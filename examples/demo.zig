//! svg2tvg demo: renders the entire feather icon set via two paths
//! side-by-side inside a shared scroll area:
//!   - left  panel: direct dvui triangle renderer (svg2tvg_dvui.renderTvg)
//!   - right panel: z2d raster → dvui Texture
//!
//! BOTH paths cache their rendered output keyed by (icon pointer, cell size).
//! The top bar reports separate stats for cache-miss (initial) renders and
//! cache-hit (subsequent) renders so the cost difference is visible.

const std = @import("std");
const builtin = @import("builtin");

const dvui = @import("dvui");
const SDLBackend = @import("sdl-backend");

const svg2tvg = @import("svg2tvg");
const svg2tvg_dvui = @import("svg2tvg_dvui");
const icons = @import("icons");

comptime {
    std.debug.assert(@hasDecl(SDLBackend, "SDLBackend"));
}

// --- Icon sets --------------------------------------------------------------
//
// Every flavor of feather/lucide/entypo/heroicons is enumerated at compile
// time into a (bytes, name) pair list.  At runtime `active_set` picks
// which list the grid renders.  `entypo` is dvui's default icon set.

const IconSet = enum { entypo, feather, lucide, heroicons_outline, heroicons_solid };

const IconList = struct {
    bytes: []const []const u8,
    names: []const []const u8,
};

fn buildList(comptime ns: type) IconList {
    @setEvalBranchQuota(200_000);
    const decls = @typeInfo(ns).@"struct".decls;
    var bytes: [decls.len][]const u8 = undefined;
    var names: [decls.len][]const u8 = undefined;
    for (decls, 0..) |d, i| {
        bytes[i] = @field(ns, d.name);
        names[i] = d.name;
    }
    const bytes_final = bytes;
    const names_final = names;
    return .{ .bytes = &bytes_final, .names = &names_final };
}

const ENTYPO = buildList(icons.tvg.entypo);
const FEATHER = buildList(icons.tvg.feather);
const LUCIDE = buildList(icons.tvg.lucide);
const HERO_O = buildList(icons.tvg.heroicons.outline);
const HERO_S = buildList(icons.tvg.heroicons.solid);

fn listFor(set: IconSet) IconList {
    return switch (set) {
        .entypo => ENTYPO,
        .feather => FEATHER,
        .lucide => LUCIDE,
        .heroicons_outline => HERO_O,
        .heroicons_solid => HERO_S,
    };
}

var active_set: IconSet = .entypo;

const GRID_COLS: usize = 8;
const CELL_SIZE: f32 = 72; // logical px per cell
const ICON_COLOR: dvui.Color = .{ .r = 0x10, .g = 0x10, .b = 0x18, .a = 0xff };

// --- Globals ----------------------------------------------------------------

var gpa_instance = std.heap.GeneralPurposeAllocator(.{}){};
const gpa = gpa_instance.allocator();

var g_backend: ?SDLBackend = null;
var g_win: ?*dvui.Window = null;

// One ScrollInfo shared between the two columns so they scroll together.
var shared_scroll: dvui.ScrollInfo = .{};

// CLI flags
var screenshot_path: ?[]const u8 = null;
var screenshot_z2d: bool = false;
var screenshot_frame_index: u32 = 0;
var single_icon: ?[]const u8 = null;

// --- Cache + stats ----------------------------------------------------------

const CacheKey = struct { ptr: usize, w: u32, h: u32 };

fn keyHash(k: CacheKey) u64 {
    var h: u64 = k.ptr;
    h ^= @as(u64, k.w) << 32;
    h ^= @as(u64, k.h);
    return h;
}

// dvui_render cache stores a self-owned triangle mesh anchored at (0,0).
// Each frame we dupe vertex data into the dvui arena, translate to the
// current cell position, and submit via `dvui.renderTriangles`.  No
// texture in sight.
const DvuiCacheEntry = struct {
    mesh: svg2tvg_dvui.MeshBuilder,
    last_seen_frame: u64,
};
const DvuiCacheMap = std.AutoHashMap(u64, DvuiCacheEntry);

// z2d cache stores a GPU texture (one PMA quad per icon).
const Z2dCacheEntry = struct {
    tex: dvui.Texture,
    last_seen_frame: u64,
};
const Z2dCacheMap = std.AutoHashMap(u64, Z2dCacheEntry);

var dvui_cache: ?DvuiCacheMap = null;
var z2d_cache: ?Z2dCacheMap = null;

// Frame counter — used to evict cache entries that weren't touched this frame.
var frame_index: u64 = 0;

const Bench = struct {
    initial_total_ns: u64 = 0,
    initial_count: u64 = 0,
    cached_total_ns: u64 = 0,
    cached_count: u64 = 0,

    fn reset(self: *Bench) void {
        self.* = .{};
    }
    fn initialAvgUs(self: Bench) f64 {
        if (self.initial_count == 0) return 0;
        return @as(f64, @floatFromInt(self.initial_total_ns)) / @as(f64, @floatFromInt(self.initial_count)) / 1000.0;
    }
    fn cachedAvgUs(self: Bench) f64 {
        if (self.cached_count == 0) return 0;
        return @as(f64, @floatFromInt(self.cached_total_ns)) / @as(f64, @floatFromInt(self.cached_count)) / 1000.0;
    }
};

var bench_dvui: Bench = .{};
var bench_z2d: Bench = .{};

// Updated each frame from cell-hover detection; shown in the top bar.
var hovered_name: ?[]const u8 = null;

fn clearCaches() void {
    {
        var it = dvui_cache.?.iterator();
        while (it.next()) |entry| entry.value_ptr.mesh.deinit();
        dvui_cache.?.clearRetainingCapacity();
    }
    {
        var it = z2d_cache.?.iterator();
        while (it.next()) |entry| entry.value_ptr.tex.destroyLater();
        z2d_cache.?.clearRetainingCapacity();
    }
    bench_dvui.reset();
    bench_z2d.reset();
}

/// Drop cache entries that weren't touched this frame.  Resizing the
/// window or scrolling changes the cache key (size-dependent) so we'd
/// otherwise accumulate stale entries forever.  Run AFTER all cells of
/// the current frame have had a chance to bump their `last_seen_frame`.
fn evictStaleCacheEntries() void {
    {
        var to_remove = std.ArrayList(u64){};
        defer to_remove.deinit(gpa);
        var it = dvui_cache.?.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.last_seen_frame != frame_index) {
                entry.value_ptr.mesh.deinit();
                to_remove.append(gpa, entry.key_ptr.*) catch {};
            }
        }
        for (to_remove.items) |k| _ = dvui_cache.?.remove(k);
    }
    {
        var to_remove = std.ArrayList(u64){};
        defer to_remove.deinit(gpa);
        var it = z2d_cache.?.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.last_seen_frame != frame_index) {
                entry.value_ptr.tex.destroyLater();
                to_remove.append(gpa, entry.key_ptr.*) catch {};
            }
        }
        for (to_remove.items) |k| _ = z2d_cache.?.remove(k);
    }
}

// --- main -------------------------------------------------------------------

pub fn main() !void {
    if (builtin.os.tag == .windows) {
        dvui.Backend.Common.windowsAttachConsole() catch {};
    }
    SDLBackend.enableSDLLogging();

    defer if (gpa_instance.deinit() != .ok) @panic("Memory leak on exit!");

    {
        var args = try std.process.argsWithAllocator(gpa);
        defer args.deinit();
        _ = args.next();
        while (args.next()) |arg| {
            if (std.mem.eql(u8, arg, "--icon")) {
                single_icon = try gpa.dupe(u8, args.next() orelse return error.MissingIconName);
                continue;
            }
            if (std.mem.eql(u8, arg, "--screenshot")) {
                const raw = args.next() orelse return error.MissingScreenshotPath;
                if (std.mem.startsWith(u8, raw, "z2d:")) {
                    screenshot_z2d = true;
                    screenshot_path = try gpa.dupe(u8, raw[4..]);
                } else if (std.mem.startsWith(u8, raw, "dvui:")) {
                    screenshot_path = try gpa.dupe(u8, raw[5..]);
                } else {
                    screenshot_path = try gpa.dupe(u8, raw);
                }
            }
        }
    }
    defer if (screenshot_path) |p| gpa.free(p);
    defer if (single_icon) |p| gpa.free(p);

    dvui_cache = DvuiCacheMap.init(gpa);
    z2d_cache = Z2dCacheMap.init(gpa);
    // NOTE: don't `defer clearCaches()` here — that would run AFTER
    // `win.deinit()` (LIFO), and Texture.destroyLater needs a live
    // currentWindow.  Cleanup happens further down inside the window's
    // scope.
    defer {
        // Maps themselves are safe to drop at any point.  Per-entry
        // texture handles are freed inside the window scope below.
        {
            var it = dvui_cache.?.iterator();
            while (it.next()) |entry| entry.value_ptr.mesh.deinit();
        }
        dvui_cache.?.deinit();
        z2d_cache.?.deinit();
    }

    var backend = try SDLBackend.initWindow(.{
        .allocator = gpa,
        .size = .{ .w = 1400.0, .h = 800.0 },
        .min_size = .{ .w = 600.0, .h = 400.0 },
        .vsync = true,
        .title = "svg2tvg - dvui_render vs z2d (cached)",
    });
    g_backend = backend;
    defer backend.deinit();

    var win = try dvui.Window.init(@src(), gpa, backend.backend(), .{
        .theme = switch (backend.preferredColorScheme() orelse .light) {
            .light => dvui.Theme.builtin.adwaita_light,
            .dark => dvui.Theme.builtin.adwaita_dark,
        },
    });
    g_win = &win;
    defer win.deinit();
    // Now that the window is up, register a defer that destroys the cached
    // z2d textures BEFORE `win.deinit()` runs.  LIFO of defers within main
    // means this runs first.
    defer {
        var it = z2d_cache.?.iterator();
        while (it.next()) |entry| entry.value_ptr.tex.destroyLater();
        z2d_cache.?.clearRetainingCapacity();
    }

    var interrupted = false;
    main_loop: while (true) {
        frame_index +%= 1;
        const nstime = win.beginWait(interrupted);
        try win.begin(nstime);
        try backend.addAllEvents(&win);

        _ = SDLBackend.c.SDL_SetRenderDrawColor(backend.renderer, 0, 0, 0, 0);
        _ = SDLBackend.c.SDL_RenderClear(backend.renderer);

        const keep_running = try gui_frame();
        if (!keep_running) break :main_loop;

        // Drop cache entries no cell touched this frame (resize/scroll
        // produces obsolete-size keys).
        evictStaleCacheEntries();

        dvui.refresh(&win, @src(), null);

        const end_micros = try win.end(.{});
        try backend.setCursor(win.cursorRequested());
        try backend.textInputRect(win.textInputRequested());
        try backend.renderPresent();

        const wait_event_micros = win.waitTime(end_micros);
        interrupted = try backend.waitEventTimeout(wait_event_micros);
    }
}

// --- gui --------------------------------------------------------------------

fn gui_frame() !bool {
    var keep_running = true;

    var outer = dvui.box(@src(), .{ .dir = .vertical }, .{ .expand = .both });
    defer outer.deinit();

    // --- top bar ---
    {
        var bar = dvui.box(@src(), .{ .dir = .horizontal }, .{
            .expand = .horizontal,
            .padding = .all(6),
            .background = true,
        });
        defer bar.deinit();

        const list = listFor(active_set);
        dvui.label(@src(), "svg2tvg demo  |  set: {s}  ({d})  |  hover: {s}", .{
            @tagName(active_set),
            list.bytes.len,
            hovered_name orelse "-",
        }, .{});

        // Icon-set selector buttons.
        inline for (std.meta.tags(IconSet), 0..) |s, i| {
            if (dvui.button(@src(), @tagName(s), .{}, .{ .id_extra = i })) {
                if (active_set != s) {
                    active_set = s;
                    // Old entries refer to OTHER sets' byte pointers; let the
                    // per-frame stale sweep collect them.  Reset bench too so
                    // the numbers reflect the new set.
                    bench_dvui.reset();
                    bench_z2d.reset();
                }
            }
        }
    }
    {
        var bar = dvui.box(@src(), .{ .dir = .horizontal }, .{
            .expand = .horizontal,
            .padding = .all(6),
            .background = true,
        });
        defer bar.deinit();
        dvui.label(@src(), "  dvui_render  initial {d:.1} us x {d}  cached {d:.1} us x {d}", .{
            bench_dvui.initialAvgUs(),
            bench_dvui.initial_count,
            bench_dvui.cachedAvgUs(),
            bench_dvui.cached_count,
        }, .{});

        dvui.label(@src(), "  z2d  initial {d:.1} us x {d}  cached {d:.1} us x {d}", .{
            bench_z2d.initialAvgUs(),
            bench_z2d.initial_count,
            bench_z2d.cachedAvgUs(),
            bench_z2d.cached_count,
        }, .{});

        if (dvui.button(@src(), "Clear cache", .{}, .{ .gravity_x = 1.0 })) {
            clearCaches();
        }
        if (dvui.button(@src(), "Quit", .{}, .{})) {
            keep_running = false;
        }
    }

    // Reset BEFORE the cell loop runs (and AFTER the top bar has already
    // displayed last frame's value).  Cells set `hovered_name` if the mouse
    // is inside.  The top bar shows the value one frame later, which is
    // imperceptible at 60Hz.
    hovered_name = null;

    // --- shared scroll area containing both columns side by side ----------
    var scroll = dvui.scrollArea(@src(), .{
        .scroll_info = &shared_scroll,
        .horizontal_bar = .hide,
        .vertical_bar = .show,
    }, .{ .expand = .both });
    defer scroll.deinit();

    const active_len = listFor(active_set).bytes.len;
    const total_rows = (active_len + GRID_COLS - 1) / GRID_COLS;
    const total_h: f32 = @as(f32, @floatFromInt(total_rows)) * CELL_SIZE + 24;

    var hbox = dvui.box(@src(), .{ .dir = .horizontal }, .{
        .expand = .horizontal,
        .min_size_content = .{ .h = total_h },
    });
    defer hbox.deinit();

    try renderColumn(0, .dvui_render, "dvui_render (triangles, cached)", &keep_running);
    try renderColumn(1, .z2d, "z2d -> texture (cached)", &keep_running);

    return keep_running;
}

const Method = enum { dvui_render, z2d };

fn renderColumn(id_extra: usize, method: Method, title: []const u8, keep_running: *bool) !void {
    var col = dvui.box(@src(), .{ .dir = .vertical }, .{
        .expand = .both,
        .padding = .all(6),
        .id_extra = id_extra,
    });
    defer col.deinit();

    dvui.label(@src(), "{s}", .{title}, .{ .id_extra = id_extra });

    // --icon mode: render a single icon at several sizes, bypass grid layout.
    if (lookupSingleIconBytes()) |bytes| {
        var icon_box = dvui.box(@src(), .{ .dir = .vertical }, .{
            .min_size_content = .{ .w = 540, .h = 970 },
            .id_extra = id_extra,
        });
        defer icon_box.deinit();
        const icon_rs = icon_box.data().contentRectScale();

        var pic_icon = blk: {
            if (screenshot_path == null) break :blk null;
            const want_z2d = screenshot_z2d;
            if ((method == .z2d) != want_z2d) break :blk null;
            break :blk dvui.Picture.start(icon_rs.r);
        };

        const sizes = [_]f32{ 64 * icon_rs.s, 128 * icon_rs.s, 256 * icon_rs.s, 512 * icon_rs.s };
        var y_off: f32 = icon_rs.r.y + 4;
        for (sizes) |s| {
            const r = dvui.Rect.Physical{ .x = icon_rs.r.x + 4, .y = y_off, .w = s, .h = s };
            switch (method) {
                .dvui_render => try drawCachedDvui(bytes, r),
                .z2d => try drawCachedZ2d(bytes, r),
            }
            y_off += s + 8;
        }

        if (pic_icon) |*p| {
            p.stop();
            if (screenshot_path) |path| {
                if (screenshot_frame_index >= 2) {
                    writePicturePng(p, path) catch |err| {
                        std.log.err("screenshot write failed: {s}", .{@errorName(err)});
                    };
                    keep_running.* = false;
                }
                screenshot_frame_index += 1;
            }
            p.deinit();
        }
        return;
    }

    const list = listFor(active_set);
    const total_rows = (list.bytes.len + GRID_COLS - 1) / GRID_COLS;
    const grid_h: f32 = @as(f32, @floatFromInt(total_rows)) * CELL_SIZE;
    const grid_w: f32 = @as(f32, @floatFromInt(GRID_COLS)) * CELL_SIZE;

    var grid_box = dvui.box(@src(), .{ .dir = .vertical }, .{
        .min_size_content = .{ .w = grid_w, .h = grid_h },
        .id_extra = id_extra,
    });
    defer grid_box.deinit();

    const rs = grid_box.data().contentRectScale();
    const cell_phys = CELL_SIZE * rs.s;

    // Picture-based screenshot of the visible panel (only in --screenshot mode).
    var pic = blk: {
        if (screenshot_path == null) break :blk null;
        const want_z2d = screenshot_z2d;
        if ((method == .z2d) != want_z2d) break :blk null;
        break :blk dvui.Picture.start(rs.r);
    };

    for (list.bytes, 0..) |bytes, i| {
        const c: f32 = @floatFromInt(i % GRID_COLS);
        const r: f32 = @floatFromInt(i / GRID_COLS);
        const cell = dvui.Rect.Physical{
            .x = rs.r.x + c * cell_phys + 4,
            .y = rs.r.y + r * cell_phys + 4,
            .w = cell_phys - 8,
            .h = cell_phys - 8,
        };
        switch (method) {
            .dvui_render => try drawCachedDvui(bytes, cell),
            .z2d => try drawCachedZ2d(bytes, cell),
        }
        // Hover detection (poor man's tooltip — FloatingTooltipWidget ignores
        // caller-supplied id_extra so it can't be looped, see dvui v0.4.0
        // FloatingTooltipWidget.zig:94).  Whichever cell the mouse is over
        // sets `hovered_name`; the top bar displays it.
        if (cell.contains(dvui.currentWindow().mouse_pt)) {
            hovered_name = list.names[i];
        }
    }

    if (pic) |*p| {
        p.stop();
        if (screenshot_path) |path| {
            if (screenshot_frame_index >= 2) {
                writePicturePng(p, path) catch |err| {
                    std.log.err("screenshot write failed: {s}", .{@errorName(err)});
                };
                keep_running.* = false;
            }
            screenshot_frame_index += 1;
        }
        p.deinit();
    }
}

// --- dvui_render with TRIANGLE-MESH cache (no texture intermediate) --------
//
// On miss we walk the TVG and accumulate every fill / stroke / vertex disc
// triangle into ONE `MeshBuilder` anchored at (0,0), then stash that
// builder in the cache (owns its vertex + index slices via gpa).  On hit
// we dupe the slices into dvui's per-frame arena, translate vertices by
// the current `cell.topLeft()`, and call `dvui.renderTriangles`.  Nothing
// touches the GPU until that one submit call, and no texture is created.

fn drawCachedDvui(bytes: []const u8, cell: dvui.Rect.Physical) !void {
    const w_i: i32 = @intFromFloat(@floor(cell.w));
    const h_i: i32 = @intFromFloat(@floor(cell.h));
    if (w_i <= 0 or h_i <= 0) return;
    const w_u: u32 = @intCast(w_i);
    const h_u: u32 = @intCast(h_i);
    const key = keyHash(.{ .ptr = @intFromPtr(bytes.ptr), .w = w_u, .h = h_u });

    if (dvui.clipGet().intersect(cell).empty()) return;

    if (dvui_cache.?.getPtr(key)) |cached| {
        cached.last_seen_frame = frame_index;
        const t0 = std.time.nanoTimestamp();
        try submitMeshTranslated(&cached.mesh, cell);
        bench_dvui.cached_total_ns += @intCast(std.time.nanoTimestamp() - t0);
        bench_dvui.cached_count += 1;
        return;
    }

    // MISS: render TVG → mesh anchored at (0,0), store, then submit translated.
    const t0 = std.time.nanoTimestamp();
    var mesh = svg2tvg_dvui.MeshBuilder.init(gpa);
    errdefer mesh.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    const local_rect = dvui.Rect.Physical{
        .x = 0,
        .y = 0,
        .w = @as(f32, @floatFromInt(w_u)),
        .h = @as(f32, @floatFromInt(h_u)),
    };
    svg2tvg_dvui.appendTvg(arena.allocator(), &mesh, bytes, local_rect, .{
        .color_override = ICON_COLOR,
        .keep_aspect = true,
    }) catch {};

    try dvui_cache.?.put(key, .{ .mesh = mesh, .last_seen_frame = frame_index });
    if (dvui_cache.?.getPtr(key)) |cached| {
        try submitMeshTranslated(&cached.mesh, cell);
    }

    bench_dvui.initial_total_ns += @intCast(std.time.nanoTimestamp() - t0);
    bench_dvui.initial_count += 1;
}

/// Dupe the cached (0,0)-anchored mesh into dvui's per-frame lifo arena,
/// translate every vertex by `cell.topLeft()`, then submit.  The lifo
/// allocation is freed automatically at end of frame.
fn submitMeshTranslated(mesh: *svg2tvg_dvui.MeshBuilder, cell: dvui.Rect.Physical) !void {
    if (mesh.idx.items.len == 0) return;
    const cw = dvui.currentWindow();
    const alloc = cw.lifo();

    var tri = mesh.toTriangles().dupe(alloc) catch return;
    defer tri.deinit(alloc);

    for (tri.vertexes) |*v| {
        v.pos.x += cell.x;
        v.pos.y += cell.y;
    }
    tri.bounds.x += cell.x;
    tri.bounds.y += cell.y;

    dvui.renderTriangles(tri, null) catch {};
}

// --- z2d with PMA-texture cache --------------------------------------------

const PixelShim = struct {
    pixels: []u8,
    width: isize,
    height: isize,

    pub fn setPixel(self: *@This(), x: isize, y: isize, c: [4]u8) void {
        const ux: usize = @intCast(x);
        const uy: usize = @intCast(y);
        const w: usize = @intCast(self.width);
        const i = (uy * w + ux) * 4;
        self.pixels[i + 0] = c[0];
        self.pixels[i + 1] = c[1];
        self.pixels[i + 2] = c[2];
        self.pixels[i + 3] = c[3];
    }
};

fn drawCachedZ2d(bytes: []const u8, cell: dvui.Rect.Physical) !void {
    const w_i: i32 = @intFromFloat(@floor(cell.w));
    const h_i: i32 = @intFromFloat(@floor(cell.h));
    if (w_i <= 0 or h_i <= 0) return;
    const w_u: u32 = @intCast(w_i);
    const h_u: u32 = @intCast(h_i);
    const key = keyHash(.{ .ptr = @intFromPtr(bytes.ptr), .w = w_u, .h = h_u });

    // Skip cells outside the visible viewport (see drawCachedDvui).
    if (dvui.clipGet().intersect(cell).empty()) return;

    if (z2d_cache.?.getPtr(key)) |cached| {
        cached.last_seen_frame = frame_index;
        const t0 = std.time.nanoTimestamp();
        dvui.renderTexture(cached.tex, .{ .r = cell, .s = 1.0 }, .{}) catch {};
        bench_z2d.cached_total_ns += @intCast(std.time.nanoTimestamp() - t0);
        bench_z2d.cached_count += 1;
        return;
    }

    const t0 = std.time.nanoTimestamp();

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const alloc = arena.allocator();

    const buf = alloc.alloc(u8, @as(usize, w_u) * h_u * 4) catch return;
    @memset(buf, 0);
    var shim = PixelShim{ .pixels = buf, .width = w_i, .height = h_i };

    var fbs = std.io.fixedBufferStream(bytes);
    svg2tvg.renderStream(alloc, &shim, fbs.reader(), .{
        .overwrite_fill = colorAsF32(ICON_COLOR),
        .overwrite_stroke = colorAsF32(ICON_COLOR),
    }) catch return;

    // sliceFromRGBA premultiplies in place and reinterprets the bytes as PMA.
    const pma = dvui.Color.PMA.sliceFromRGBA(buf);
    const tex = dvui.Texture.create(pma, w_u, h_u, .linear, .rgba_32) catch return;
    z2d_cache.?.put(key, .{ .tex = tex, .last_seen_frame = frame_index }) catch {};
    dvui.renderTexture(tex, .{ .r = cell, .s = 1.0 }, .{}) catch {};

    bench_z2d.initial_total_ns += @intCast(std.time.nanoTimestamp() - t0);
    bench_z2d.initial_count += 1;
}

fn colorAsF32(c: dvui.Color) svg2tvg.Color {
    return .{
        .r = @as(f32, @floatFromInt(c.r)) / 255.0,
        .g = @as(f32, @floatFromInt(c.g)) / 255.0,
        .b = @as(f32, @floatFromInt(c.b)) / 255.0,
        .a = @as(f32, @floatFromInt(c.a)) / 255.0,
    };
}

// --- helpers ----------------------------------------------------------------

fn lookupSingleIconBytes() ?[]const u8 {
    const name = single_icon orelse return null;
    const list = listFor(active_set);
    for (list.names, 0..) |n, i| {
        if (std.mem.eql(u8, n, name)) return list.bytes[i];
    }
    return null;
}

fn writePicturePng(pic: *dvui.Picture, path: []const u8) !void {
    var file = try std.fs.cwd().createFile(path, .{});
    defer file.close();

    var buf: [4096]u8 = undefined;
    var fw = file.writer(&buf);
    try pic.png(&fw.interface);
    try fw.interface.flush();
}
