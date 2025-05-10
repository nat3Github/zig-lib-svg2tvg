const std = @import("std");
const assert = std.debug.assert;
const expect = std.debug.expect;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const math = std.math;
const root = @import("root.zig");

const svg_parsing = @import("svg.zig");
const xml = @import("xml");
const icons = @import("icons");
const tvg = root.tvg;

const Color = tvg.Color;
const Path = tvg.Path;
const Segment = Path.Segment;
const Node = Path.Node;
const NodeData = Path.Node.NodeData;
const Point = tvg.Point;
const Scale = tvg.Scale;
const Range = tvg.Range;
const Style = tvg.Style;

const SvgColor = root.SvgColor;
const make_node_debug2 = root.make_node_debug2;
const make_node_debug = root.make_node_debug;

pub const Image = @import("image");
pub const ImageWrapper = struct {
    width: i64,
    height: i64,
    img: *Image,
    pub fn setPixel(self: *@This(), x: i64, y: i64, color: tvg.Color) void {
        const col = color.toRgba8();
        const pix: Image.Pixel = .init_from_u8_slice(&col);
        self.img.set_pixel(@intCast(x), @intCast(y), pix);
    }
};

pub fn Stack(T: type) type {
    return struct {
        data: []T,
        pos: usize = 0,
        pub fn init(alloc: Allocator, size: usize) !@This() {
            return @This(){
                .data = try alloc.alloc(T, size),
            };
        }
        pub fn deinit(self: *@This(), alloc: Allocator) void {
            alloc.free(self.data);
        }
        pub fn push(self: *@This(), t: T) !void {
            if (self.pos > self.data.len) return error.OutOfBounds;
            self.data[self.pos] = t;
            self.pos += 1;
        }
        pub fn pop(self: *@This()) ?T {
            if (self.pos == 0) return null;
            const xtop = self.top();
            self.pos -= 1;
            return xtop;
        }
        pub fn top(self: *const @This()) ?T {
            if (self.pos == 0) return null;
            return self.data[self.pos - 1];
        }
        pub fn top_mut(self: *@This()) ?*T {
            if (self.pos == 0) return null;
            return &self.data[self.pos - 1];
        }
    };
}

pub const utils = struct {
    pub fn has_field(T: type, comptime name: []const u8) bool {
        comptime {
            for (@typeInfo(T).@"struct".fields) |field| {
                if (std.mem.eql(u8, field.name, name)) return true;
            }
            return false;
        }
    }
    pub fn parseFloat(property_name: []const u8, att: []const u8, val: []const u8) !?f32 {
        if (std.mem.eql(u8, property_name, att)) {
            return try std.fmt.parseFloat(f32, val);
        }
        return null;
    }
    pub fn parsePointList(alloc: Allocator, property_name: []const u8, att: []const u8, val: []const u8) !?std.ArrayList(Point) {
        if (std.mem.eql(u8, property_name, att)) {
            var list = std.ArrayList(Point).init(alloc);
            var split = std.mem.splitAny(u8, val, " ,");
            var point = Point{ .x = 0, .y = 0 };
            var k: bool = true;
            while (split.next()) |n| {
                const v = try std.fmt.parseFloat(f32, n);
                if (k) point.x = v else point.y = v;
                if (!k) try list.append(point);
                k = !k;
            }
            if (!k) return error.ListLenUneven;
            return list;
        }
        return null;
    }
    pub fn parseColor(property_name: []const u8, att: []const u8, val: []const u8) !?SvgColor {
        if (std.mem.eql(u8, property_name, att)) {
            return SvgColor.parseColor(val);
        }
        return null;
    }
    pub fn auto_parse_def(T: type, self: *T, Def: type, att: []const u8, val: []const u8) !void {
        const decls = @typeInfo(Def).@"struct".decls;
        if (comptime decls.len == 0) @compileLog(Def);
        inline for (decls) |decl| {
            if (comptime has_field(T, decl.name)) {
                const declT = @TypeOf(@field(Def, decl.name));
                if (comptime declT == f32 or declT == ?f32) {
                    const v = try parseFloat(decl.name, att, val);
                    if (v) |a| @field(self, decl.name) = a;
                }
                if (comptime declT == SvgColor or declT == ?SvgColor) {
                    const v = try parseColor(decl.name, att, val);
                    if (v) |a| @field(self, decl.name) = a;
                }
            }
        }
    }
    pub fn toPoint(cord: svg_parsing.CoordinatePair) Point {
        return Point{
            .x = fromNumber(cord.coordinates[0].number),
            .y = fromNumber(cord.coordinates[1].number),
        };
    }
    pub fn fromNumber(number: svg_parsing.Number) f32 {
        return @floatCast(number.value);
    }
};

pub const NodeMaker = struct {
    m: make_node,
    cmds: std.ArrayList(Node),
    seg: std.ArrayList(Segment),
    iseg: std.ArrayList(IndexSegment),
    cmds_len: usize = 0,
    seg_len: usize = 0,
    flushed: bool = true,
    lw: ?f32 = null,

    pub fn init(
        alloc: Allocator,
        svg: root.Svg,
    ) NodeMaker {
        const m = make_node.init(svg);
        return NodeMaker{
            .m = m,
            .cmds = std.ArrayList(Node).init(alloc),
            .seg = std.ArrayList(Segment).init(alloc),
            .iseg = std.ArrayList(IndexSegment).init(alloc),
        };
    }
    const IndexSegment = struct {
        start: Point,
        command_idx: usize,
        command_len: usize,
    };

    pub fn flush(self: *NodeMaker) !void {
        if (make_node_debug) std.log.warn("flush", .{});
        if (self.flushed) return;
        self.flushed = true;
        const len = self.cmds.items.len;
        const command_len = len - self.cmds_len;
        if (command_len == 0) return;
        const iseg = IndexSegment{
            .start = self.m.first,
            .command_idx = self.cmds_len,
            .command_len = command_len,
        };
        self.cmds_len = len;
        try self.iseg.append(iseg);
        self.m.reset();
    }
    pub fn close(self: *NodeMaker) !void {
        if (make_node_debug) std.log.warn("close", .{});
        const nd = self.m.close(self.lw);
        try self.cmds.append(nd);
        try self.flush();
    }
    pub fn move(self: *NodeMaker, p: Point, rel: bool) !void {
        if (make_node_debug) std.log.warn("move {d:.2} {d:.2}", .{ p.x, p.y });
        try self.flush();
        self.flushed = false;
        self.m.move(p, rel);
    }
    pub fn line(self: *NodeMaker, p: Point, rel: bool) !void {
        if (make_node_debug) std.log.warn("line", .{});
        const nd = self.m.line(self.lw, p, rel);
        try self.cmds.append(nd);
    }
    pub fn vert(self: *NodeMaker, f: f32, rel: bool) !void {
        if (make_node_debug) std.log.warn("ver", .{});
        const nd = self.m.vert(self.lw, f, rel);
        try self.cmds.append(nd);
    }
    pub fn horiz(self: *NodeMaker, f: f32, rel: bool) !void {
        if (make_node_debug) std.log.warn("horiz", .{});
        const nd = self.m.horiz(self.lw, f, rel);
        try self.cmds.append(nd);
    }
    pub fn quadratic_bezier_curve_to(self: *NodeMaker, c: Point, p: Point, rel: bool) !void {
        if (make_node_debug) std.log.warn("qbez", .{});
        const nd = self.m.quadratic_bezier_curve_to(self.lw, c, p, rel);
        try self.cmds.append(nd);
    }
    pub fn curve_to(self: *NodeMaker, c1: Point, c2: Point, p: Point, rel: bool) !void {
        if (make_node_debug) std.log.warn("curve to", .{});
        const nd = self.m.curve_to(self.lw, c1, c2, p, rel);
        try self.cmds.append(nd);
    }
    pub fn smooth_quadratic_bezier_curve_to(self: *NodeMaker, p: Point, rel: bool) !void {
        if (make_node_debug) std.log.warn("smooth quad", .{});
        const nd = try self.m.smooth_quadratic_bezier_curve_to(self.lw, p, rel);
        try self.cmds.append(nd);
    }
    pub fn smooth_curve_to(self: *NodeMaker, c2: Point, p: Point, rel: bool) !void {
        if (make_node_debug) std.log.warn("smoot curve to", .{});
        const nd = try self.m.smooth_curve_to(self.lw, c2, p, rel);
        try self.cmds.append(nd);
    }
    pub fn elliptical_arc(self: *NodeMaker, rx: f32, ry: f32, rotation: f32, large_arc: bool, sweep_cw: bool, p: Point, rel: bool) !void {
        if (make_node_debug) std.log.warn("elliptical arc", .{});
        const nd = self.m.elliptical_arc(self.lw, rx, ry, rotation, large_arc, sweep_cw, p, rel);
        try self.cmds.append(nd);
    }
    pub fn circular_arc(self: *NodeMaker, r: f32, large_arc: bool, sweep_cw: bool, p: Point, rel: bool) !void {
        if (make_node_debug) std.log.warn("cirx arc", .{});
        const nd = self.m.circular_arc(self.lw, r, large_arc, sweep_cw, p, rel);
        try self.cmds.append(nd);
    }
    pub fn segments(self: *@This()) !?[]const Segment {
        for (0..self.iseg.items.len) |_| {
            try self.seg.append(Segment{ .commands = &.{}, .start = .{ .x = 0, .y = 0 } });
        }
        for (self.iseg.items, self.seg.items) |isn, *sn| {
            sn.start = isn.start;
            sn.commands = self.cmds.items[isn.command_idx .. isn.command_idx + isn.command_len];
        }
        if (self.seg.items.len == 0) return null;
        return self.seg.items;
    }
};
pub const make_node = struct {
    last_control: ?Point = null,
    first: Point = Point{ .x = 0, .y = 0 },
    cur: Point = Point{ .x = 0, .y = 0 },
    vw: root.Svg,
    fn set_last_control(self: *make_node, p: Point) void {
        self.last_control = p;
    }
    fn get_reflection(self: *make_node, current: Point) Point {
        if (self.last_control == null) return current;
        const reflected = reflect_point(self.last_control.?, current);
        return reflected;
    }
    fn reflect_point(prev_control: Point, current_pos: Point) Point {
        return Point{
            .x = 2 * current_pos.x - prev_control.x,
            .y = 2 * current_pos.y - prev_control.y,
        };
    }
    // print_point("current", current);
    // print_point("last control", self.last_control.?);
    // print_point("reflected", reflected);
    fn reset_last_control(self: *make_node) void {
        self.last_control = null;
    }
    fn reset(self: *make_node) void {
        self.reset_last_control();
        self.cur = Point{ .x = 0, .y = 0 };
    }

    pub fn init(vw: root.Svg) make_node {
        return make_node{
            .last_control = null,
            .vw = vw,
        };
    }
    fn add(x: f32, y: f32, rel: bool) f32 {
        assert(!math.isNan(x));
        assert(!math.isNan(y));
        if (rel) return x + y else return y;
    }
    fn addPoints(a: Point, b: Point, rel: bool) Point {
        return Point{
            .x = add(a.x, b.x, rel),
            .y = add(a.y, b.y, rel),
        };
    }

    pub fn close(self: *make_node, lw: ?f32) Node {
        if (make_node_debug2) std.log.warn("make_node: close", .{});
        self.cur = self.first;
        self.reset_last_control();
        return Node{ .close = NodeData(void){
            .data = {},
            .line_width = lw,
        } };
    }
    pub fn move(self: *make_node, p: Point, rel: bool) void {
        if (make_node_debug2) std.log.warn("make_node: move", .{});
        self.cur = addPoints(self.cur, p, rel);
        self.first = self.cur;
        self.reset_last_control();
    }
    pub fn line(self: *make_node, lw: ?f32, p: Point, rel: bool) Node {
        if (make_node_debug2) std.log.warn("make_node: line", .{});
        self.cur = addPoints(self.cur, p, rel);
        self.reset_last_control();
        return Node{ .line = NodeData(Point){
            .data = self.vw.transform(self.cur),
            .line_width = lw,
        } };
    }
    pub fn vert(self: *make_node, lw: ?f32, f: f32, rel: bool) Node {
        if (make_node_debug2) std.log.warn("make_node: vert", .{});
        var p = self.cur;
        p.y = add(self.cur.y, f, rel);
        return self.line(lw, p, false);
    }
    pub fn horiz(self: *make_node, lw: ?f32, f: f32, rel: bool) Node {
        if (make_node_debug2) std.log.warn("make_node: horiz", .{});
        var p = self.cur;
        p.x = add(self.cur.x, f, rel);
        return self.line(lw, p, false);
    }

    pub fn quadratic_bezier_curve_to(self: *make_node, lw: ?f32, c: Point, p: Point, rel: bool) Node {
        if (make_node_debug2) std.log.warn("make_node: quadratic_bezier_curve_to", .{});
        const compute_c = addPoints(self.cur, c, rel);
        self.cur = addPoints(self.cur, p, rel);
        self.set_last_control(compute_c);
        return Node{ .quadratic_bezier = NodeData(Node.QuadraticBezier){
            .data = Node.QuadraticBezier{
                .c = self.vw.transform(compute_c),
                .p1 = self.vw.transform(self.cur),
            },
            .line_width = lw,
        } };
    }
    pub fn curve_to(self: *make_node, lw: ?f32, c1: Point, c2: Point, p: Point, rel: bool) Node {
        if (make_node_debug2) std.log.warn("make_node: curve_to", .{});
        const compute_c0 = addPoints(self.cur, c1, rel);
        const compute_c1 = addPoints(self.cur, c2, rel);
        self.cur = addPoints(self.cur, p, rel);
        self.set_last_control(compute_c1);
        return Node{ .bezier = NodeData(Node.Bezier){
            .data = Node.Bezier{
                .c0 = self.vw.transform(compute_c0),
                .c1 = self.vw.transform(compute_c1),
                .p1 = self.vw.transform(self.cur),
            },
            .line_width = lw,
        } };
    }

    pub fn smooth_quadratic_bezier_curve_to(self: *make_node, lw: ?f32, p: Point, rel: bool) !Node {
        if (make_node_debug2) std.log.warn("make_node: smooth_quadratic_bezier_curve_to", .{});
        const reflected = self.get_reflection(self.cur);
        self.cur = addPoints(self.cur, p, rel);
        self.set_last_control(reflected);
        return Node{ .quadratic_bezier = NodeData(Node.QuadraticBezier){
            .data = Node.QuadraticBezier{
                .c = self.vw.transform(reflected),
                .p1 = self.vw.transform(self.cur),
            },
            .line_width = lw,
        } };
    }
    pub fn smooth_curve_to(self: *make_node, lw: ?f32, c2: Point, p: Point, rel: bool) !Node {
        if (make_node_debug2) std.log.warn("make_node: smooth_curve_to", .{});
        const compute_c0 = self.get_reflection(self.cur);
        const compute_c1 = addPoints(self.cur, c2, rel);
        self.cur = addPoints(self.cur, p, rel);
        self.set_last_control(compute_c1);
        return Node{ .bezier = NodeData(Node.Bezier){
            .data = Node.Bezier{
                .c0 = self.vw.transform(compute_c0),
                .c1 = self.vw.transform(compute_c1),
                .p1 = self.vw.transform(self.cur),
            },
            .line_width = lw,
        } };
    }
    pub fn elliptical_arc(self: *make_node, lw: ?f32, rx: f32, ry: f32, rotation: f32, large_arc: bool, sweep_ccw: bool, p: Point, rel: bool) Node {
        if (make_node_debug2) std.log.warn("make_node: elliptical_arc", .{});
        self.cur = addPoints(self.cur, p, rel);
        self.reset_last_control();
        return Node{ .arc_ellipse = NodeData(Node.ArcEllipse){
            .data = Node.ArcEllipse{
                .radius_x = rx,
                .radius_y = ry,
                .rotation = rotation,
                .large_arc = large_arc,
                .sweep = !sweep_ccw,
                .target = self.vw.transform(self.cur),
            },
            .line_width = lw,
        } };
    }
    pub fn circular_arc(self: *make_node, lw: ?f32, r: f32, large_arc: bool, sweep_ccw: bool, p: Point, rel: bool) Node {
        if (make_node_debug2) std.log.warn("make_node: circular_arc", .{});
        self.cur = addPoints(self.cur, p, rel);
        self.reset_last_control();
        return Node{
            .arc_circle = NodeData(Node.ArcCircle){
                .data = Node.ArcCircle{
                    .large_arc = large_arc,
                    .radius = r,
                    .sweep = !sweep_ccw,
                    .target = self.vw.transform(self.cur),
                },
                .line_width = lw,
            },
        };
    }
};

pub const ColorHash = struct {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
    pub fn fromColor(col: Color) @This() {
        return @This(){
            .r = @intFromFloat(std.math.clamp(col.r, 0, 1) * 255),
            .g = @intFromFloat(std.math.clamp(col.g, 0, 1) * 255),
            .b = @intFromFloat(std.math.clamp(col.b, 0, 1) * 255),
            .a = @intFromFloat(std.math.clamp(col.a, 0, 1) * 255),
        };
    }
    pub fn toColor(self: *const @This()) Color {
        return Color{
            .r = @as(f32, @floatFromInt(self.r)) / 255.0,
            .g = @as(f32, @floatFromInt(self.g)) / 255.0,
            .b = @as(f32, @floatFromInt(self.b)) / 255.0,
            .a = @as(f32, @floatFromInt(self.a)) / 255.0,
        };
    }
};

pub const ColMap = std.AutoArrayHashMap(ColorHash, u32);

pub fn print_point(
    name: []const u8,
    data: Point,
) void {
    return std.debug.print("{s} \n .x: {d:.2} \n .y: {d:.2}\n", .{ name, data.x, data.y });
}

fn kprint(
    name: []const u8,
    data: anytype,
) void {
    const T = @TypeOf(data);
    if (comptime T == f32) {
        return std.debug.print("{s} f:{d:.2}\n", .{ name, data });
    }
    if (comptime T == Point) {
        return print_point(name, data);
    }
    if (comptime T == Node.ArcEllipse) {
        const ell_fmt =
            \\{s}
            \\  .radius_x: {d:.2},
            \\  .radius_y: {d:.2},
            \\  .rotation: {d:.2},
            \\  .large_arc: {},
            \\  .sweep: {},
            \\  .target: x:{d:.2} y:{d:.2},
            \\
        ;
        return std.debug.print(ell_fmt, .{
            name,
            data.radius_x,
            data.radius_y,
            data.rotation,
            data.large_arc,
            data.sweep,
            data.target.x,
            data.target.y,
        });
    }
    if (comptime T == Node.Bezier) {
        const ell_fmt =
            \\{s}
            \\  .c0: x:{d:.2} y:{d:.2},
            \\  .c1: x:{d:.2} y:{d:.2},
            \\  .p1: x:{d:.2} y:{d:.2},
            \\
        ;
        return std.debug.print(ell_fmt, .{
            name,
            data.c0.x,
            data.c0.y,
            data.c1.x,
            data.c1.y,
            data.p1.x,
            data.p1.y,
        });
    }
    return std.debug.print("{s} {any}\n", .{ name, data });
}

pub fn print_node(n: Node) void {
    switch (n) {
        .line => |a| kprint(@tagName(n), a.data),
        .horiz => |a| kprint(@tagName(n), a.data),
        .vert => |a| kprint(@tagName(n), a.data),
        .bezier => |a| kprint(@tagName(n), a.data),
        .arc_circle => |a| kprint(@tagName(n), a.data),
        .arc_ellipse => |a| kprint(@tagName(n), a.data),
        .close => |a| kprint(@tagName(n), a.data),
        .quadratic_bezier => |a| kprint(@tagName(n), a.data),
    }
}
