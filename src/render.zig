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
pub fn renderStream(
    allocator: std.mem.Allocator,
    /// A struct that exports a single function `setPixel(x: isize, y: isize, color: [4]u8) void` as well as two fields width and height
    img_shim: anytype,
    /// The icon data
    reader: anytype,
) !void {
    var parser = try parse(allocator, reader);
    defer parser.deinit();
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    const color_table = parser.color_table;
    const og_width = parser.header.width;
    const og_height = parser.header.height;
    const og_widthf32: f32 = @floatFromInt(og_width);
    const og_heightf32: f32 = @floatFromInt(og_height);

    const new_width: usize = @intCast(img_shim.width);
    const new_height: usize = @intCast(img_shim.height);
    const new_widthf32: f32 = @floatFromInt(new_width);
    const new_heightf32: f32 = @floatFromInt(new_height);

    const scaling_x = new_widthf32 / og_widthf32;
    const scaling_y = new_heightf32 / og_heightf32;
    const gpa = alloc;

    var sfc = try z2d.Surface.initPixel(.{ .rgba = .fromClamped(1, 0, 1, 1) }, gpa, @intCast(new_width), @intCast(new_height));
    // defer sfc.deinit(allocator);

    var ctx = z2d.Context.init(alloc, &sfc);
    defer ctx.deinit();
    const lwdef = 1;
    var mtx = ShimPainter{
        .ctx = &ctx,
        .lwdef = lwdef,
        .scalex = scaling_x,
        .scaley = scaling_y,
    };

    while (try parser.next()) |cmd| {
        const props = try get_props(color_table, cmd);
        const segs = try flatten_into_path(alloc, cmd, og_widthf32, og_heightf32);

        for (segs) |seg| {
            if (props.fill) |fill| {
                // ut.print_point("start", seg.start);
                // for (seg.commands) |n| {
                //     ut.print_node(n);
                // }
                mtx.close_action = .fill;
                mtx.setColor(fill);
                try mtx.moveTo(.from(seg.start));
                try z2d_draw_seg(alloc, &mtx, seg);
                try mtx.ctx.fill();
            }
            if (props.stroke) |stroke| {
                // ut.print_point("start", seg.start);
                // for (seg.commands) |n| {
                //     ut.print_node(n);
                // }
                mtx.close_action = .stroke;
                mtx.setColor(stroke);
                try mtx.moveTo(.from(seg.start));
                try z2d_draw_seg(alloc, &mtx, seg);
                try mtx.ctx.stroke();
            }
        }
        // _ = arena.reset(.retain_capacity);
    }
    for (0..new_height) |y| {
        for (0..new_width) |x| {
            const pix = sfc.getPixel(@intCast(x), @intCast(y)).?.rgba;
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
const CloseAction = enum { fill, stroke };
pub const ShimPainter = struct {
    ctx: *z2d.Context,
    lwdef: f32,
    scalex: f32,
    scaley: f32,
    close_action: CloseAction = .stroke,
    col: Color = Color{
        .r = 0,
        .g = 0,
        .b = 0,
        .a = 1,
    },
    fn setColor(self: *const ShimPainter, col: Color) void {
        self.ctx.setSourceToPixel(.{ .rgba = z2d.pixel.RGBA.fromClamped(
            @floatCast(col.r),
            @floatCast(col.g),
            @floatCast(col.b),
            @floatCast(col.a),
        ) });
    }
    fn tranform(self: *const ShimPainter, p: Vec2) Vec2 {
        return Vec2{
            .x = (@as(f64, @floatCast(self.scalex)) * p.x),
            .y = (@as(f64, @floatCast(self.scaley)) * p.y),
        };
    }
    fn setlw(self: *ShimPainter, lw: ?f32) void {
        const sc = (self.scalex + self.scaley) / 2;
        const f = lw orelse self.lwdef;
        self.ctx.setLineWidth(@floatCast(sc * f));
    }
    fn close(self: *ShimPainter) !void {
        // std.log.warn("close", .{});
        const ctx = self.ctx;
        try ctx.closePath();
        // switch (self.close_action) {
        //     .fill => try ctx.fill(),
        //     .stroke => try ctx.stroke(),
        // }
    }
    fn moveTo(self: *ShimPainter, p: Vec2) !void {
        // std.log.warn("moveTo {}, {}", .{ p.x, p.y });
        const ctx = self.ctx;
        const pt = self.tranform(p);
        try ctx.moveTo(pt.x, pt.y);
        self.ctx.setLineCapMode(.round);
        self.ctx.setLineJoinMode(.round);
    }
    fn lineTo(self: *ShimPainter, p: Vec2, lw: ?f32) !void {
        // std.log.warn("lineTo {}, {}", .{ p.x, p.y });
        const ctx = self.ctx;
        var pt = self.tranform(p);
        // workaround for z2d bug
        pt.x -= 0.01;
        self.setlw(lw);
        try ctx.lineTo(pt.x, pt.y);
    }
    fn bezier(self: *ShimPainter, c0: Vec2, c1: Vec2, end: Vec2, lw: ?f32) !void {
        // std.log.warn("bezier", .{});
        const ctx = self.ctx;
        self.setlw(lw);
        const c0t = self.tranform(c0);
        const c1t = self.tranform(c1);
        const endt = self.tranform(end);
        try ctx.curveTo(
            c0t.x,
            c0t.y,
            c1t.x,
            c1t.y,
            endt.x,
            endt.y,
        );
    }
};
// fn diff(a: f32, b: f32) bool {
//     // assert(math.isNormal(a));
//     // assert(math.isNormal(b));
//     // const k = @abs(a - b);
//     // return k > 0.1;
//     return a != b;
// }

// fn diffPoint(a: Point, b: Point) bool {
//     return diff(a.x, b.x) and diff(a.y, b.y);
// }
pub fn z2d_draw_seg(alloc: Allocator, ctx: *ShimPainter, seg: Segment) !void {
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
pub fn flatten_into_path(alloc: Allocator, cmd: DrawCommand, og_width: f32, og_height: f32) ![]const Segment {
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
pub fn draw_rect(m: *ut.NodeMaker, rectangles: []const Rectangle) ![]const Segment {
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
pub fn draw_points(m: *ut.NodeMaker, points: []const Point, close: bool) ![]const Segment {
    assert(points.len > 0);
    try m.move(points[0], false);
    for (points[1..]) |l| {
        try m.line(l, false);
    }
    if (close) try m.close();
    try m.flush();
    const segs = try m.segments() orelse &.{};
    return segs;
}

pub fn get_color(table: []const Color, k: Style) !Color {
    if (k != .flat) return error.UnsupportedStyle;
    const i = math.cast(usize, k.flat) orelse return error.OOB;
    if (i >= table.len) return error.OOB;
    return table[i];
}

pub const Vec2 = struct {
    x: f64,
    y: f64,
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
