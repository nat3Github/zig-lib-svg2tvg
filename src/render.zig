const std = @import("std");
const assert = std.debug.assert;
const expect = std.debug.expect;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const math = std.math;

const root = @import("root.zig");
const ut = @import("util.zig");
const tvg = root.tvg;
const z2d = @import("z2d");
const ldrw = @import("stroke.zig");

const Svg = root.Svg;
const DrawCommand = tvg.parsing.DrawCommand;
const Style = tvg.Style;
const Rectangle = tvg.Rectangle;
const Header = tvg.parsing.Header;
const Color = tvg.Color;
const Path = tvg.Path;
const Segment = Path.Segment;
const Node = Path.Node;
const NodeData = Path.Node.NodeData;
const Point = tvg.Point;
const Scale = tvg.Scale;
const Range = tvg.Range;
const parse = tvg.parse;

const Shim = struct {
    pub fn setPixels(_: isize, _: isize, _: [4]u8) void {}
    width: isize,
    height: isize,
};
pub const Options = struct {
    fallback_stroke_width: ?f32 = 1, // fallback stroke_width
    overwrite_stroke_width: ?f32 = null, // overwrite stroke_width
    overwrite_fill: ?Color = null, // overwrite all colors used to color
    overwrite_stroke: ?Color = null, // overwrite stroke colors
    use_z2d_for_stroke: bool = true, // either use z2d as painter or the custom painter (Shimpainter2 -> stroke.zig) NOTE: z2d has a bug with closing paths on the same point https://github.com/vancluever/z2d/issues/116 it seems to be working with a workaround i found (search for workaround)
    // NOTE: z2d arguably has better quality but im not sure that it is correct
    disable_fill: bool = false,
};

pub fn renderStream(
    allocator: std.mem.Allocator,
    /// A struct that exports a single function `setPixel(x: isize, y: isize, color: [4]u8) void` as well as two fields width and height
    img_shim: anytype,
    /// The icon data
    reader: anytype,
    opts: Options,
) !void {
    const gpa = allocator;
    var parser = try parse(gpa, reader);
    defer parser.deinit();

    const color_table = parser.color_table;
    const og_width = parser.header.width;
    const og_height = parser.header.height;
    const og_widthf32: f32 = @floatFromInt(og_width);
    const og_heightf32: f32 = @floatFromInt(og_height);

    const new_width: usize = math.cast(usize, img_shim.width) orelse return error.SizeOutOfBounds;
    const new_height: usize = math.cast(usize, img_shim.height) orelse return error.SizeOutOfBounds;
    const new_widthf32: f32 = @floatFromInt(new_width);
    const new_heightf32: f32 = @floatFromInt(new_height);

    const scaling_x = new_widthf32 / og_widthf32;
    const scaling_y = new_heightf32 / og_heightf32;

    var sfc = try z2d.Surface.initPixel(.{ .rgba = .fromClamped(1, 0, 1, 0) }, gpa, @intCast(new_width), @intCast(new_height));
    defer sfc.deinit(allocator);

    var ctx = z2d.Context.init(gpa, &sfc);
    defer ctx.deinit();

    const lwdef: f32 = opts.fallback_stroke_width orelse 1;
    var prop = ShimProp{
        .lwdef = lwdef,
        .overwrite_lw = opts.overwrite_stroke_width,
        .col = .{ 255, 255, 255, 255 },
        .scalex = scaling_x,
        .scaley = scaling_y,
    };
    var shim_fill = ShimPainter{ .ctx = &ctx, .prop = &prop };

    var shim_stroke = ShimPainter2.init(&ctx, &prop);
    _ = &shim_stroke;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();
    while (try parser.next()) |cmd| {
        const props = try get_props(color_table, cmd);
        const segs = try flatten_into_path(arena_alloc, cmd, og_widthf32, og_heightf32);

        if (props.fill) |fill| {
            // const painter = tvg.rendering.Painter{
            //     .scale_x = scaling_x,
            //     .scale_y = scaling_y,
            // };

            // if (true) debug_print_seg(seg);
            std.log.warn("fill", .{});
            if (!opts.disable_fill) {
                // const ren = tvg.rendering;
                // ren.renderCommand2(img_shim, header: parsing.Header, color_table: []const tvg.Color, cmd: parsing.DrawCommand, allocator: ?std.mem.Allocator)

                const max_path_len = 512;
                const temp_buffer_size = 256;
                var point_store = tvg.rendering.FixedBufferList(Point, temp_buffer_size).init(allocator);
                defer point_store.deinit();
                var slice_store = tvg.rendering.FixedBufferList(tvg.rendering.IndexSlice, temp_buffer_size).init(allocator);
                defer slice_store.deinit();

                try tvg.rendering.renderPath(&point_store, null, &slice_store, Path{ .segments = segs }, 0.0);

                var slices: [max_path_len][]const Point = undefined;
                for (slice_store.items(), 0..) |src, i| {
                    slices[i] = point_store.items()[src.offset..][0..src.len];
                }
                const style = get_style(cmd);
                if (style != .flat) return error.Unsupported;
                const point_list = slices[0..slice_store.items().len];

                prop.setColor(opts.overwrite_fill orelse fill);
                shim_fill.mode_fill = true;
                shim_fill.ctx.setFillRule(.even_odd);
                for (point_list) |list| {
                    if (list.len > 3) {
                        try shim_fill.moveTo(.from(list[0]));
                        for (list[0 .. list.len - 1]) |p| {
                            try shim_fill.lineTo(.from(p), 2);
                        }
                        try shim_fill.close();
                    }
                }
                try shim_fill.flush();
            }
        }
        if (props.stroke) |stroke| {
            for (segs) |seg| {
                if (opts.use_z2d_for_stroke) {
                    shim_fill.mode_fill = false;
                    prop.setColor(opts.overwrite_fill orelse stroke);
                    try shim_fill.moveTo(.from(seg.start));
                    try z2d_draw_seg(arena_alloc, &shim_fill, seg);
                    try shim_fill.ctx.stroke();
                } else {
                    prop.setColor(opts.overwrite_stroke orelse stroke);
                    try shim_stroke.moveTo(.from(seg.start));
                    try z2d_draw_seg(arena_alloc, &shim_stroke, seg);
                }
            }
        }
        _ = arena.reset(.retain_capacity);
    }

    for (0..new_height) |y| {
        for (0..new_width) |x| {
            const pix = sfc.getPixel(
                math.cast(i32, x) orelse return error.CastOutOfBounds,
                math.cast(i32, y) orelse return error.CastOutOfBounds,
            ).?.rgba;
            const pixc: [4]u8 = .{
                pix.r,
                pix.g,
                pix.b,
                pix.a,
            };
            img_shim.setPixel(@intCast(x), @intCast(y), pixc);
        }
    }
}
fn debug_print_seg(seg: Segment) void {
    std.log.warn("LOG SEG RENDER PATH", .{});
    ut.print_point("start", seg.start);
    for (seg.commands) |n| {
        ut.print_node(n);
    }
    std.log.warn("LOG RENDER END", .{});
}
const ShimProp = struct {
    overwrite_lw: ?f32,
    lwdef: f32,
    scalex: f32,
    scaley: f32,
    col: [4]u8,
    fn setColor(self: *ShimProp, col: Color) void {
        self.col = .{
            denorm(col.r),
            denorm(col.g),
            denorm(col.b),
            denorm(col.a),
        };
    }
    fn tranform(self: *ShimProp, p: Vec2) Vec2 {
        return Vec2{
            .x = (@as(f64, @floatCast(self.scalex)) * p.x),
            .y = (@as(f64, @floatCast(self.scaley)) * p.y),
        };
    }
    fn getlw(self: *ShimProp, lw: ?f32) f32 {
        const sc = (self.scalex + self.scaley) / 2;
        if (self.overwrite_lw) |ovw| return sc * ovw;
        const f = lw orelse self.lwdef;
        return sc * f;
    }
};
fn denorm(x: f32) u8 {
    const f: f32 = math.clamp(x * 255, 0, 255);
    return @intFromFloat(f);
}
fn norm(x: u8) f64 {
    const f: f64 = @floatFromInt(x);
    return f / 255.0;
}
pub const ShimPainter2 = struct {
    ctx: *z2d.Context,
    prop: *ShimProp,
    start: Vec2,
    curr: Vec2,
    width: u32 = 0,
    height: u32 = 0,
    curr_lw: f32 = 0,
    fn init(ctx: *z2d.Context, prop: *ShimProp) ShimPainter2 {
        return @This(){
            .ctx = ctx,
            .prop = prop,
            .start = Vec2{},
            .curr = Vec2{},
            .width = @intCast(ctx.surface.getWidth()),
            .height = @intCast(ctx.surface.getWidth()),
        };
    }
    pub fn putPixel(self: *@This(), x: u32, y: u32, col: [4]u8) void {
        const pix = z2d.pixel.RGBA.fromClamped(norm(col[0]), norm(col[1]), norm(col[2]), norm(col[3]));
        self.ctx.surface.putPixel(@intCast(x), @intCast(y), .{ .rgba = pix });
    }
    pub fn getPixel(self: *@This(), x: u32, y: u32) [4]u8 {
        const pix = self.ctx.surface.getPixel(@intCast(x), @intCast(y)).?.rgba;
        return .{ pix.r, pix.g, pix.b, pix.a };
    }

    fn close(self: *@This()) !void {
        ldrw.strokeLineSegmentAA(
            self,
            self.prop.tranform(self.curr).data(),
            self.prop.tranform(self.start).data(),
            self.curr_lw,
            self.prop.col,
        );
    }
    fn moveTo(self: *@This(), p: Vec2) !void {
        self.curr = p;
        self.curr_lw = 0;
        self.start = p;
    }
    fn lineTo(self: *@This(), p: Vec2, lw: ?f32) !void {
        const tlw = self.prop.getlw(lw);
        self.curr_lw = tlw;
        ldrw.strokeLineSegmentAA(
            self,
            self.prop.tranform(self.curr).data(),
            self.prop.tranform(p).data(),
            tlw,
            self.prop.col,
        );
        self.curr = p;
    }
    fn bezier(self: *@This(), c0: Vec2, c1: Vec2, end: Vec2, lw: ?f32) !void {
        const tlw = self.prop.getlw(lw);
        self.curr_lw = tlw;
        ldrw.strokeBezierAA(
            self,
            self.prop.tranform(self.curr).data(),
            self.prop.tranform(c0).data(),
            self.prop.tranform(c1).data(),
            self.prop.tranform(end).data(),
            tlw,
            self.prop.col,
        );
        self.curr = end;
    }
    fn logline(str: []const u8, a: Vec2, b: Vec2) void {
        std.debug.print("{s} {d:.0} {d:.0} to {d:.0} {d:.0}\n", .{ str, a.x, a.y, b.x, b.y });
    }
};

pub const ShimPainter = struct {
    ctx: *z2d.Context,
    prop: *ShimProp,
    mode_fill: bool = true,
    flushed: bool = true,
    closed: bool = false,

    fn flush(self: *@This()) !void {
        if (!self.flushed) {
            if (self.mode_fill) {
                if (self.closed == false) {
                    try self.ctx.closePath();
                    self.closed = true;
                }
                try self.ctx.fill();
            } else try self.ctx.stroke();
        }
    }
    fn close(self: *@This()) !void {
        self.closed = true;
        const ctx = self.ctx;
        try ctx.closePath();
        // try self.flush();
    }
    fn moveTo(self: *@This(), p: Vec2) !void {
        const ctx = self.ctx;
        // try self.flush();
        self.closed = false;
        self.flushed = false;
        const pt = self.prop.tranform(p);
        try ctx.moveTo(pt.x, pt.y);
        self.ctx.setLineCapMode(.round);
        self.ctx.setLineJoinMode(.round);
    }
    fn lineTo(self: *@This(), p: Vec2, lw: ?f32) !void {
        const ctx = self.ctx;
        var pt = self.prop.tranform(p);
        pt.x -= 0.01; // NOTE: workaround for z2d bug
        self.ctx.setLineWidth(self.prop.getlw(lw));
        try ctx.lineTo(pt.x, pt.y);
    }
    fn bezier(self: *@This(), c0: Vec2, c1: Vec2, end: Vec2, lw: ?f32) !void {
        self.ctx.setLineWidth(self.prop.getlw(lw));
        const c0t = self.prop.tranform(c0);
        const c1t = self.prop.tranform(c1);
        const endt = self.prop.tranform(end);
        try self.ctx.curveTo(
            c0t.x,
            c0t.y,
            c1t.x,
            c1t.y,
            endt.x,
            endt.y,
        );
    }
};

pub fn z2d_draw_seg(alloc: Allocator, ctx: anytype, seg: Segment) !void {
    var p = seg.start;
    for (seg.commands) |cm| {
        switch (cm) {
            .close => |_| {
                try ctx.close();
            },
            .line => |a| {
                p = a.data;
                try ctx.lineTo(.from(p), a.line_width);
            },
            .horiz => |a| {
                p.x += a.data;
                try ctx.lineTo(.from(p), a.line_width);
            },
            .vert => |a| {
                p.y += a.data;
                try ctx.lineTo(.from(p), a.line_width);
            },
            .arc_circle => |a| {
                const arc = a.data;
                const end = arc.target;
                const cv = try arcToCubics(
                    alloc,
                    .from(p),
                    .from(end),
                    @floatCast(arc.radius),
                    @floatCast(arc.radius),
                    0,
                    arc.large_arc,
                    !arc.sweep,
                );
                for (cv) |av| {
                    try ctx.bezier(av[1], av[2], av[3], a.line_width);
                }
                p = end;
            },
            .arc_ellipse => |a| {
                const arc = a.data;
                const end = arc.target;
                const cv = try arcToCubics(
                    alloc,
                    .from(p),
                    .from(end),
                    @floatCast(arc.radius_x),
                    @floatCast(arc.radius_y),
                    @floatCast(arc.rotation),
                    arc.large_arc,
                    !arc.sweep,
                );
                for (cv) |av| {
                    try ctx.bezier(av[1], av[2], av[3], a.line_width);
                }
                p = end;
            },
            .quadratic_bezier => |a| {
                const c0 = a.data.c;
                const p1 = a.data.p1;
                const cv = quadraticToCubicBezier(
                    .from(p),
                    .from(c0),
                    .from(p1),
                );
                try ctx.bezier(cv[1], cv[2], cv[3], a.line_width);
                p = p1;
            },
            .bezier => |a| {
                const c0 = a.data.c0;
                const c1 = a.data.c1;
                const p1 = a.data.p1;
                try ctx.bezier(
                    .from(c0),
                    .from(c1),
                    .from(p1),
                    a.line_width,
                );
                p = p1;
            },
        }
    }
}

const FillProperties = struct {
    fill: ?Color = null,
    stroke: ?Color = null,
};
pub fn get_style(cmd: DrawCommand) Style {
    return switch (cmd) {
        .fill_path => |a| a.style,
        .fill_polygon => |a| a.style,
        .fill_rectangles => |a| a.style,
        .outline_fill_path => |a| a.fill_style,
        .outline_fill_polygon => |a| a.fill_style,
        .outline_fill_rectangles => |a| a.fill_style,
        .draw_lines => |a| a.style,
        .draw_line_loop => |a| a.style,
        .draw_line_strip => |a| a.style,
        .draw_line_path => |a| a.style,
    };
}
pub fn get_props(colors: []Color, cmd: DrawCommand) !FillProperties {
    return switch (cmd) {
        .fill_path => |a| FillProperties{ .fill = try get_color(colors, a.style) },
        .fill_polygon => |a| FillProperties{ .fill = try get_color(colors, a.style) },
        .fill_rectangles => |a| FillProperties{ .fill = try get_color(colors, a.style) },
        .outline_fill_path => |a| FillProperties{ .fill = try get_color(colors, a.fill_style), .stroke = try get_color(colors, a.line_style) },
        .outline_fill_polygon => |a| FillProperties{ .fill = try get_color(colors, a.fill_style), .stroke = try get_color(colors, a.line_style) },
        .outline_fill_rectangles => |a| FillProperties{ .fill = try get_color(colors, a.fill_style), .stroke = try get_color(colors, a.line_style) },
        .draw_lines => |a| FillProperties{ .stroke = try get_color(colors, a.style) },
        .draw_line_loop => |a| FillProperties{ .stroke = try get_color(colors, a.style) },
        .draw_line_strip => |a| FillProperties{ .stroke = try get_color(colors, a.style) },
        .draw_line_path => |a| FillProperties{ .stroke = try get_color(colors, a.style) },
    };
}
pub fn flatten_into_path(alloc: Allocator, cmd: DrawCommand, og_width: f32, og_height: f32) ![]Segment {
    var m = ut.NodeMaker.init(alloc, Svg{
        .viewBox = .{ .h = og_height, .w = og_width, .x = 0, .y = 0 },
        .width = og_width,
        .height = og_height,
    });
    switch (cmd) {
        .fill_path => |a| {
            return a.path.segments;
        },
        .outline_fill_path => |a| {
            return a.path.segments;
        },
        .fill_polygon => |a| {
            assert(a.vertices.len > 2);
            return try draw_points(&m, a.vertices, true);
        },
        .outline_fill_polygon => |a| {
            assert(a.vertices.len > 2);
            return try draw_points(&m, a.vertices, true);
        },
        .fill_rectangles => |a| {
            return try draw_rect(&m, a.rectangles);
        },
        .outline_fill_rectangles => |a| {
            return try draw_rect(&m, a.rectangles);
        },
        .draw_lines => |a| {
            for (a.lines) |l| {
                try m.move(l.start, false);
                try m.line(l.end, false);
            }
            try m.flush();
            return try m.segments() orelse &.{};
        },
        .draw_line_loop => |a| {
            return try draw_points(&m, a.vertices, true);
        },
        .draw_line_strip => |a| {
            return try draw_points(&m, a.vertices, false);
        },
        .draw_line_path => |a| {
            return a.path.segments;
        },
    }
}
pub fn draw_rect(m: *ut.NodeMaker, rectangles: []const Rectangle) ![]Segment {
    for (rectangles) |rect| {
        const x = rect.x;
        const y = rect.y;
        var p = Point{ .x = rect.x, .y = rect.y };
        try m.move(p, false);
        p.x += x;
        try m.line(p, false);
        p.x += y;
        try m.line(p, false);
        p.x += -x;
        try m.line(p, false);
        try m.close();
        return try m.segments() orelse unreachable;
    }
    return &.{};
}
pub fn draw_points(m: *ut.NodeMaker, points: []const Point, close: bool) ![]Segment {
    assert(points.len > 0);
    try m.move(points[0], false);
    for (points[1..]) |l| {
        try m.line(l, false);
    }
    if (close) try m.close();
    try m.flush();
    const segs = try m.segments() orelse @constCast(&.{});
    return segs;
}

pub fn get_color(table: []const Color, k: Style) !Color {
    if (k != .flat) return error.UnsupportedStyle;
    const i = math.cast(usize, k.flat) orelse return error.OOB;
    if (i >= table.len) return error.OOB;
    return table[i];
}

pub const Vec2 = struct {
    x: f64 = 0,
    y: f64 = 0,
    fn data(self: Vec2) [2]f32 {
        return .{
            @floatCast(self.x),
            @floatCast(self.y),
        };
    }
    fn lerp(a: Vec2, b: Vec2, t: f64) Vec2 {
        return Vec2{
            .x = a.x + (b.x - a.x) * t,
            .y = a.y + (b.y - a.y) * t,
        };
    }
    fn add(a: Vec2, b: Vec2) Vec2 {
        return Vec2{ .x = a.x + b.x, .y = a.y + b.y };
    }
    fn sub(a: Vec2, b: Vec2) Vec2 {
        return Vec2{ .x = a.x - b.x, .y = a.y - b.y };
    }
    fn scale(v: Vec2, s: f64) Vec2 {
        return Vec2{ .x = v.x * s, .y = v.y * s };
    }
    fn from(p: Point) Vec2 {
        return @This(){
            .x = @floatCast(p.x),
            .y = @floatCast(p.y),
        };
    }
};

/// Converts a quadratic Bezier (P0, P1, P2) to cubic Bezier (C0, C1, C2, C3)
pub fn quadraticToCubicBezier(p0: Vec2, p1: Vec2, p2: Vec2) [4]Vec2 {
    const c0 = p0;
    const c1 = Vec2.add(p0, Vec2.scale(Vec2.sub(p1, p0), 2.0 / 3.0));
    const c2 = Vec2.add(p2, Vec2.scale(Vec2.sub(p1, p2), 2.0 / 3.0));
    const c3 = p2;

    return [_]Vec2{ c0, c1, c2, c3 };
}

pub fn angle(u: Vec2, v: Vec2) f64 {
    const dot = u.x * v.x + u.y * v.y;
    const det = u.x * v.y - u.y * v.x;
    return math.atan2(det, dot);
}

/// Approximates an elliptical arc with up to 4 cubic Bézier segments.
pub fn arcToCubics(
    allocator: std.mem.Allocator,
    p0: Vec2,
    p1: Vec2,
    irx: f64,
    iry: f64,
    x_axis_rotation_deg: f64,
    large_arc: bool,
    sweep: bool,
) ![]const [4]Vec2 {
    var out = std.ArrayList([4]Vec2).init(allocator);
    var rx = irx;
    var ry = iry;

    if (rx == 0 or ry == 0 or (p0.x == p1.x and p0.y == p1.y)) {
        try out.append([_]Vec2{ p0, p0, p1, p1 });
        return out.toOwnedSlice();
    }

    const phi = math.degreesToRadians(x_axis_rotation_deg);
    const cos_phi = @cos(phi);
    const sin_phi = @sin(phi);

    const dx = (p0.x - p1.x) / 2.0;
    const dy = (p0.y - p1.y) / 2.0;

    const x1p = cos_phi * dx + sin_phi * dy;
    const y1p = -sin_phi * dx + cos_phi * dy;

    var rxsq = rx * rx;
    var rysq = ry * ry;
    const x1psq = x1p * x1p;
    const y1psq = y1p * y1p;

    const lambda = x1psq / rxsq + y1psq / rysq;
    if (lambda > 1.0) {
        const scale = @sqrt(lambda);
        rx *= scale;
        ry *= scale;
        rxsq = rx * rx;
        rysq = ry * ry;
    }

    var sign: f64 = undefined;
    if (large_arc != sweep) sign = 1.0 else sign = -1.0;
    const sq = @max(0.0, (rxsq * rysq - rxsq * y1psq - rysq * x1psq) /
        (rxsq * y1psq + rysq * x1psq));
    const coef = sign * @sqrt(sq);

    const cxp = coef * (rx * y1p / ry);
    const cyp = coef * (-ry * x1p / rx);

    const cx = cos_phi * cxp - sin_phi * cyp + (p0.x + p1.x) / 2.0;
    const cy = sin_phi * cxp + cos_phi * cyp + (p0.y + p1.y) / 2.0;

    const theta1 = angle(Vec2{ .x = 1.0, .y = 0.0 }, Vec2{ .x = (x1p - cxp) / rx, .y = (y1p - cyp) / ry });

    var delta_theta = angle(Vec2{ .x = (x1p - cxp) / rx, .y = (y1p - cyp) / ry }, Vec2{ .x = (-x1p - cxp) / rx, .y = (-y1p - cyp) / ry });

    if (!sweep and delta_theta > 0) delta_theta -= math.tau;
    if (sweep and delta_theta < 0) delta_theta += math.tau;

    const segments = @ceil(@abs(delta_theta) / (math.pi / 2.0));
    const delta = delta_theta / segments;

    var t1 = theta1;

    for (0..@intFromFloat(segments)) |_| {
        const t2 = t1 + delta;
        const alpha = (4.0 / 3.0) * @tan((t2 - t1) / 4.0);
        const cos_t1 = @cos(t1);
        const sin_t1 = @sin(t1);
        const cos_t2 = @cos(t2);
        const sin_t2 = @sin(t2);

        const p1_arc = Vec2{ .x = rx * cos_t1, .y = ry * sin_t1 };
        const p2_arc = Vec2{ .x = rx * cos_t2, .y = ry * sin_t2 };

        const d1 = Vec2{ .x = -rx * sin_t1 * alpha, .y = ry * cos_t1 * alpha };
        const d2 = Vec2{ .x = rx * sin_t2 * alpha, .y = -ry * cos_t2 * alpha };

        const c0 = apply(p1_arc, phi, cx, cy);
        const c1 = apply(Vec2.add(p1_arc, d1), phi, cx, cy);
        const c2 = apply(Vec2.add(p2_arc, d2), phi, cx, cy);
        const c3 = apply(p2_arc, phi, cx, cy);

        try out.append([_]Vec2{ c0, c1, c2, c3 });

        t1 = t2;
    }

    return out.toOwnedSlice();
}

pub fn apply(p: Vec2, phi: f64, cx: f64, cy: f64) Vec2 {
    const cos_phi = @cos(phi);
    const sin_phi = @sin(phi);
    return Vec2{
        .x = cos_phi * p.x - sin_phi * p.y + cx,
        .y = sin_phi * p.x + cos_phi * p.y + cy,
    };
}
