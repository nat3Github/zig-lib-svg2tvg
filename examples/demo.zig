//! svg2tvg demo: renders the entire feather icon set via two paths
//! side-by-side inside a shared scroll area:
//!   - left  panel: direct dvui triangle renderer (svg2tvg_dvui.renderTvg)
//!   - right panel: z2d raster → dvui Texture
//!
//! BOTH paths cache their rendered output keyed by (icon pointer, cell size).
//! The top bar reports separate stats for cache-miss (initial) renders and
//! cache-hit (subsequent) renders so the cost difference is visible.
//!
//! Ported from dvui v0.4.0 to dvui main (0.16 stdlib):
//!   - main() now takes std.process.Init (Zig 0.16 entry-point convention)
//!   - GPA/allocator provided via init.gpa
//!   - Args parsed via std.process.Args.iterate
//!   - SDLBackend.initWindow now takes .io and .environ_map fields
//!   - std.io.fixedBufferStream → std.Io.Reader.fixed
//!   - renderStream takes io: std.Io as first argument
//!   - dvui.refresh(null, …) (window param is optional)
//!   - --screenshot CLI flag simplified/dropped (Picture.start returns ?Picture;
//!     the flag parsing is removed to keep porting minimal)

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

// gpa and io are set from std.process.Init in main().
var g_gpa: std.mem.Allocator = undefined;
var g_io: std.Io = undefined;

var g_backend: ?SDLBackend = null;
var g_win: ?*dvui.Window = null;

// One ScrollInfo shared between the two columns so they scroll together.
var shared_scroll: dvui.ScrollInfo = .{};

// CLI flag: --icon <name> renders a single icon at multiple sizes.
var single_icon: ?[]const u8 = null;

// --- Cache + stats ----------------------------------------------------------

const CacheKey = struct { ptr: usize, w: u32, h: u32 };

fn keyHash(k: CacheKey) u64 {
    var h: u64 = k.ptr;
    h ^= @as(u64, k.w) << 32;
    h ^= @as(u64, k.h);
    return h;
}

const DvuiCacheEntry = struct {
    mesh: svg2tvg_dvui.MeshBuilder,
    last_seen_frame: u64,
};
const DvuiCacheMap = std.AutoHashMap(u64, DvuiCacheEntry);

const Z2dCacheEntry = struct {
    tex: dvui.Texture,
    last_seen_frame: u64,
};
const Z2dCacheMap = std.AutoHashMap(u64, Z2dCacheEntry);

var dvui_cache: ?DvuiCacheMap = null;
var z2d_cache: ?Z2dCacheMap = null;

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

fn evictStaleCacheEntries() void {
    const gpa = g_gpa;
    {
        var to_remove = std.ArrayList(u64).empty;
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
        var to_remove = std.ArrayList(u64).empty;
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

pub fn main(init: std.process.Init) !void {
    g_gpa = init.gpa;
    g_io = init.io;
    const gpa = g_gpa;

    if (builtin.os.tag == .windows) {
        dvui.Backend.Common.windowsAttachConsole() catch {};
    }
    SDLBackend.enableSDLLogging();

    {
        var args = std.process.Args.iterate(init.minimal.args);
        _ = args.next(); // argv[0]
        while (args.next()) |arg| {
            if (std.mem.eql(u8, arg, "--icon")) {
                single_icon = try gpa.dupe(u8, args.next() orelse return error.MissingIconName);
            }
        }
    }
    defer if (single_icon) |p| gpa.free(p);

    dvui_cache = DvuiCacheMap.init(gpa);
    z2d_cache = Z2dCacheMap.init(gpa);
    defer {
        var it = dvui_cache.?.iterator();
        while (it.next()) |entry| entry.value_ptr.mesh.deinit();
        dvui_cache.?.deinit();
        z2d_cache.?.deinit();
    }

    var backend = try SDLBackend.initWindow(.{
        .io = init.io,
        .environ_map = init.environ_map,
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

        evictStaleCacheEntries();

        dvui.refresh(null, @src(), null);

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

        inline for (std.meta.tags(IconSet), 0..) |s, i| {
            if (dvui.button(@src(), @tagName(s), .{}, .{ .id_extra = i })) {
                if (active_set != s) {
                    active_set = s;
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

    hovered_name = null;

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
    _ = keep_running;
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
        if (cell.contains(dvui.currentWindow().mouse_pt)) {
            hovered_name = list.names[i];
        }
    }
}

// --- dvui_render with TRIANGLE-MESH cache ----------------------------------

fn nanoNow() i128 {
    if (g_backend) |*b| return b.nanoTime();
    return 0;
}

fn drawCachedDvui(bytes: []const u8, cell: dvui.Rect.Physical) !void {
    const gpa = g_gpa;
    const w_i: i32 = @intFromFloat(@floor(cell.w));
    const h_i: i32 = @intFromFloat(@floor(cell.h));
    if (w_i <= 0 or h_i <= 0) return;
    const w_u: u32 = @intCast(w_i);
    const h_u: u32 = @intCast(h_i);
    const key = keyHash(.{ .ptr = @intFromPtr(bytes.ptr), .w = w_u, .h = h_u });

    if (dvui.clipGet().intersect(cell).empty()) return;

    if (dvui_cache.?.getPtr(key)) |cached| {
        cached.last_seen_frame = frame_index;
        const t0 = nanoNow();
        try submitMeshTranslated(&cached.mesh, cell);
        bench_dvui.cached_total_ns += @intCast(nanoNow() - t0);
        bench_dvui.cached_count += 1;
        return;
    }

    const t0 = nanoNow();
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

    bench_dvui.initial_total_ns += @intCast(nanoNow() - t0);
    bench_dvui.initial_count += 1;
}

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
        const idx = (uy * w + ux) * 4;
        self.pixels[idx + 0] = c[0];
        self.pixels[idx + 1] = c[1];
        self.pixels[idx + 2] = c[2];
        self.pixels[idx + 3] = c[3];
    }
};

fn drawCachedZ2d(bytes: []const u8, cell: dvui.Rect.Physical) !void {
    const gpa = g_gpa;
    const w_i: i32 = @intFromFloat(@floor(cell.w));
    const h_i: i32 = @intFromFloat(@floor(cell.h));
    if (w_i <= 0 or h_i <= 0) return;
    const w_u: u32 = @intCast(w_i);
    const h_u: u32 = @intCast(h_i);
    const key = keyHash(.{ .ptr = @intFromPtr(bytes.ptr), .w = w_u, .h = h_u });

    if (dvui.clipGet().intersect(cell).empty()) return;

    if (z2d_cache.?.getPtr(key)) |cached| {
        cached.last_seen_frame = frame_index;
        const t0 = nanoNow();
        dvui.renderTexture(cached.tex, .{ .r = cell, .s = 1.0 }, .{}) catch {};
        bench_z2d.cached_total_ns += @intCast(nanoNow() - t0);
        bench_z2d.cached_count += 1;
        return;
    }

    const t0 = nanoNow();

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const alloc = arena.allocator();

    const buf = alloc.alloc(u8, @as(usize, w_u) * h_u * 4) catch return;
    @memset(buf, 0);
    var shim = PixelShim{ .pixels = buf, .width = w_i, .height = h_i };

    var reader = std.Io.Reader.fixed(bytes);
    svg2tvg.renderStream(g_io, alloc, &shim, &reader, .{
        .overwrite_fill = colorAsF32(ICON_COLOR),
        .overwrite_stroke = colorAsF32(ICON_COLOR),
    }) catch return;

    const pma = dvui.Color.PMA.sliceFromRGBA(buf);
    const tex = dvui.Texture.create(pma, w_u, h_u, .linear, .rgba_32) catch return;
    z2d_cache.?.put(key, .{ .tex = tex, .last_seen_frame = frame_index }) catch {};
    dvui.renderTexture(tex, .{ .r = cell, .s = 1.0 }, .{}) catch {};

    bench_z2d.initial_total_ns += @intCast(nanoNow() - t0);
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
