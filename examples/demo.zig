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

// --- Icon set: every feather icon -------------------------------------------

const ICON_LIST = blk: {
    const decls = @typeInfo(icons.tvg.feather).@"struct".decls;
    var arr: [decls.len][]const u8 = undefined;
    for (decls, 0..) |d, i| arr[i] = @field(icons.tvg.feather, d.name);
    break :blk arr;
};

const ICON_NAMES = blk: {
    const decls = @typeInfo(icons.tvg.feather).@"struct".decls;
    var arr: [decls.len][]const u8 = undefined;
    for (decls, 0..) |d, i| arr[i] = d.name;
    break :blk arr;
};

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
const CacheMap = std.AutoHashMap(u64, dvui.Texture);

fn keyHash(k: CacheKey) u64 {
    var h: u64 = k.ptr;
    h ^= @as(u64, k.w) << 32;
    h ^= @as(u64, k.h);
    return h;
}

var dvui_cache: ?CacheMap = null;
var z2d_cache: ?CacheMap = null;

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
    inline for ([_]*?CacheMap{ &dvui_cache, &z2d_cache }) |cache_opt_ptr| {
        var it = cache_opt_ptr.*.?.iterator();
        while (it.next()) |entry| entry.value_ptr.destroyLater();
        cache_opt_ptr.*.?.clearRetainingCapacity();
    }
    bench_dvui.reset();
    bench_z2d.reset();
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

    dvui_cache = CacheMap.init(gpa);
    z2d_cache = CacheMap.init(gpa);
    defer dvui_cache.?.deinit();
    defer z2d_cache.?.deinit();

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

    var interrupted = false;
    main_loop: while (true) {
        const nstime = win.beginWait(interrupted);
        try win.begin(nstime);
        try backend.addAllEvents(&win);

        _ = SDLBackend.c.SDL_SetRenderDrawColor(backend.renderer, 0, 0, 0, 0);
        _ = SDLBackend.c.SDL_RenderClear(backend.renderer);

        const keep_running = try gui_frame();
        if (!keep_running) break :main_loop;

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

        dvui.label(@src(), "svg2tvg demo  |  icons: {d}  |  hover: {s}", .{
            ICON_LIST.len,
            hovered_name orelse "-",
        }, .{});

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

    const total_rows = (ICON_LIST.len + GRID_COLS - 1) / GRID_COLS;
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

    const total_rows = (ICON_LIST.len + GRID_COLS - 1) / GRID_COLS;
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

    for (ICON_LIST, 0..) |bytes, i| {
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
            hovered_name = ICON_NAMES[i];
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

// --- dvui_render with offscreen-texture cache -------------------------------

fn drawCachedDvui(bytes: []const u8, cell: dvui.Rect.Physical) !void {
    const w_i: i32 = @intFromFloat(@floor(cell.w));
    const h_i: i32 = @intFromFloat(@floor(cell.h));
    if (w_i <= 0 or h_i <= 0) return;
    const w_u: u32 = @intCast(w_i);
    const h_u: u32 = @intCast(h_i);
    const key = keyHash(.{ .ptr = @intFromPtr(bytes.ptr), .w = w_u, .h = h_u });

    // Skip cells outside the visible viewport.  Without this we'd try to
    // populate the cache for ~286×2 icons on the first frame, which exceeds
    // the backend's per-frame texture-creation budget and we end up with
    // half the grid blank.  As the user scrolls, off-screen cells fill in.
    if (dvui.clipGet().intersect(cell).empty()) return;

    const cw = dvui.currentWindow();

    if (dvui_cache.?.get(key)) |tex| {
        const t0 = std.time.nanoTimestamp();
        dvui.renderTexture(tex, .{ .r = cell, .s = 1.0 }, .{}) catch {};
        bench_dvui.cached_total_ns += @intCast(std.time.nanoTimestamp() - t0);
        bench_dvui.cached_count += 1;
        return;
    }

    // MISS: render into an offscreen target so we get a single texture we can
    // blit cheaply on subsequent frames.
    //
    // Two non-obvious requirements:
    //   1. The current clip rect is whatever the parent widget left us with
    //      (usually the visible scroll viewport).  Off-screen cells would
    //      have an empty clip and produce a blank texture.  Temporarily widen
    //      the clip to cover the whole texture target so every cell renders.
    //   2. Use a target `offset` so draws at the cell's screen position
    //      translate into the texture's local (0..w, 0..h) coords — that way
    //      the cached texture stores the icon at the origin and can later be
    //      blitted with `renderTexture` anywhere.
    const t0 = std.time.nanoTimestamp();
    const target = dvui.textureCreateTarget(w_u, h_u, .linear, .rgba_32) catch return;
    const prev_target = dvui.renderTarget(.{ .texture = target, .offset = cell.topLeft() });
    const prev_clip = dvui.clipGet();
    dvui.clipSet(.{ .x = cell.x, .y = cell.y, .w = cell.w, .h = cell.h });

    svg2tvg_dvui.renderTvg(cw.lifo(), bytes, cell, .{
        .color_override = ICON_COLOR,
        .keep_aspect = true,
    }) catch {};

    dvui.clipSet(prev_clip);
    _ = dvui.renderTarget(prev_target);

    const tex = dvui.textureFromTarget(target) catch return;
    dvui_cache.?.put(key, tex) catch {};
    dvui.renderTexture(tex, .{ .r = cell, .s = 1.0 }, .{}) catch {};

    bench_dvui.initial_total_ns += @intCast(std.time.nanoTimestamp() - t0);
    bench_dvui.initial_count += 1;
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

    if (z2d_cache.?.get(key)) |tex| {
        const t0 = std.time.nanoTimestamp();
        dvui.renderTexture(tex, .{ .r = cell, .s = 1.0 }, .{}) catch {};
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
    z2d_cache.?.put(key, tex) catch {};
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
    inline for (@typeInfo(icons.tvg.feather).@"struct".decls) |d| {
        if (std.mem.eql(u8, d.name, name)) {
            return @field(icons.tvg.feather, d.name);
        }
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
