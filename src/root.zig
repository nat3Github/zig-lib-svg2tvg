const std = @import("std");
const assert = std.debug.assert;
const expect = std.debug.expect;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
// const root = @import("../root.zig");

const svg_parsing = @import("svg.zig");
const xml = @import("xml");
const icons = @import("icons");
const tvg = @import("tvg");

const Color = tvg.Color;
const Path = tvg.Path;
const Segment = Path.Segment;
const Node = Path.Node;
const NodeData = Path.Node.NodeData;
const Point = tvg.Point;
const Scale = tvg.Scale;
const Range = tvg.Range;
const Style = tvg.Style;

const Image = @import("image");
const ImageWrapper = struct {
    width: i64,
    height: i64,
    img: Image,
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
        pub fn deinit(self: *@This(), alloc: Allocator) !@This() {
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
        pub fn top(self: *@This()) ?T {
            if (self.pos == 0) return null;
            return self.data[self.pos - 1];
        }
        pub fn top_mut(self: *@This()) ?*T {
            if (self.pos == 0) return null;
            return &self.data[self.pos - 1];
        }
    };
}
fn normalize_u8(u: u8) f32 {
    const x: f32 = @floatFromInt(u);
    return x / 255.0;
}
pub fn Color_from(col: svg_parsing.Color, alpha_normalized: f32) Color {
    return Color{
        .r = normalize_u8(col.r),
        .g = normalize_u8(col.g),
        .b = normalize_u8(col.b),
        .a = std.math.clamp(alpha_normalized, 0, 1),
    };
}
const RED = Color{
    .r = 1.0,
    .g = 0,
    .b = 0,
    .a = 1.0,
};
const WidthHeight = struct {
    w: ?f32 = null,
    h: ?f32 = null,
};
const ViewBox = struct {
    x: ?f32 = 0,
    y: ?f32 = 0,
    w: ?f32 = null,
    h: ?f32 = null,
    fn transform(
        self: *const @This(),
        width: f32,
        height: f32,
        point: Point,
    ) Point {
        return Point{
            .x = (point.x - self.x) * (width / self.w),
            .y = (point.y - self.y) * (height / self.h),
        };
    }
};

test "convert to tvg" {
    // if (true) return;
    const gpa = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const alloc = arena.allocator();
    // var output = std.io.getStdOut();

    var file = try std.fs.cwd().createFile("test/mykk.tvg", .{});
    try tvg_from_svg(alloc, file.writer(), icons.svg.lucide.play);
    file.close();

    var fileo = try std.fs.cwd().openFile("test/mykk.tvg", .{});
    const w = 300;
    const h = 300;
    var image_wrapper = ImageWrapper{
        .img = try Image.init(alloc, w, h),
        .width = w,
        .height = h,
    };
    const icon = try fileo.readToEndAlloc(alloc, 1024 * 1024);
    try tvg.render(alloc, &image_wrapper, icon);
    try image_wrapper.img.write_ppm_to_file("test/converted.ppm");
    std.log.warn("ok", .{});
}

const InheritableProperties = struct {
    fill: ?Color = null, // Fill color
    @"fill-opacity": ?Color = null, // Opacity of the fill
    stroke: ?Color = null, // Stroke (outline) color
    @"stroke-opacity": ?f32 = null, // Stroke opacity
    @"stroke-width": ?f32 = null, // Stroke thickness
    // @"stroke-dasharray": ? = null, // Dash pattern of the stroke
    // @"stroke-dashoffset": ? = null, // Offset into the dash pattern
    // @"stroke-linecap": ?C = null, // Shape of stroke ends (butt, round, square)
    // @"stroke-linejoin": ? = null, // Corner rendering (miter, round, bevel)
    // @"stroke-miterlimit": ? = null, // Miter limit for sharp corners
    opacity: ?f32 = null, // Overall opacity (applies to the whole element)
    // visibility: ?bool = null, // Whether element is visible (visible, hidden)
    // display: ?Color = null, // Whether element is rendered (inline, none)
    color: ?Color = null, // Used as base color (currentColor, filters, etc.)
    // @"font-*": ?Color = null, // If used in text elements
    // direction: ?Color = null, // Text and layout direction (e.g. ltr, rtl)
    // @"text-anchor": ?Color = null, // Text alignment
    // @"writing-mode": ?Color = null, // Vertical/horizontal text flow
    // @"clip-path": ?Color = null, // Clipping region
    // mask: ?Color = null, // Mask to apply to element
    // filter: ?Color = null, // Filter effects (blur, drop shadow, etc.)
    pub fn override(self: *const InheritableProperties, over: InheritableProperties) InheritableProperties {
        var ret = self.*;
        inline for (@typeInfo(InheritableProperties).@"struct".fields) |f| {
            if (@field(over, f.name)) |fval| {
                @field(ret, f.name) = fval;
            }
        }
        return ret;
    }
    pub fn override_from(self: *@This(), t: anytype) void {
        const T = @TypeOf(t);
        inline for (@typeInfo(InheritableProperties).@"struct".fields) |f| {
            if (comptime utils.has_field(T, f.name)) {
                comptime assert(f.type == @TypeOf(@field(T, f.name)));
                if (@field(t, f.name)) |fval| {
                    @field(self, f.name) = fval;
                }
            }
        }
    }
};

const SvgTag = enum(u8) {
    svg,
    rect,
    circle,
    ellipse,
    line,
    polyline,
    polygon,
    path,
    // text,
    // textPath,
    // tref,
    // tspan,
    // a, //link
    // image,
    // marker,
};
pub const utils = struct {
    pub fn has_field(T: type, comptime name: []const u8) T {
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
    }
    pub fn parsePointList(alloc: Allocator, property_name: []const u8, att: []const u8, val: []const u8) !?std.ArrayList(Point) {
        if (std.mem.eql(u8, property_name, att)) {
            const list = std.ArrayList(Point).init(alloc);
            var split = std.mem.splitAny(u8, val, " ");
            var point = Point{};
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
    }
    pub fn parseColor(property_name: []const u8, att: []const u8, val: []const u8) !?Color {
        if (std.mem.eql(u8, property_name, att)) {
            return svg_parsing.Color.parse(val).color;
        }
    }
    pub fn auto_parse_def(T: type, self: *T, Def: type, att: []const u8, val: []const u8) !void {
        inline for (@typeInfo(Def).@"struct".decls) |decl| {
            if (comptime has_field(T, decl.name)) {
                const declT = @typeInfo(@field(Def, decl.name));
                if (comptime declT == f32 or declT == ?f32) {
                    const v = try parseFloat(decl.name, att, val);
                    if (v) |a| @field(self, decl.name) = a;
                }
                if (comptime declT == Color or declT == ?Color) {
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
    cmds: *std.ArrayList(Node),
    seg: *std.ArrayList(Segment),
    cmds_len: usize,
    seg_len: usize,
    flushed: bool = true,
    lw: f32 = 1,

    pub fn init(
        m: make_node,
        cmds: *std.ArrayList(Node),
        seg: *std.ArrayList(Segment),
    ) NodeMaker {
        return NodeMaker{
            .cmds = cmds,
            .seg = seg,
            .cmds_len = cmds.items.len,
            .seg_len = seg.items.len,
            .m = m,
        };
    }
    pub fn flush(self: *NodeMaker) !void {
        if (self.flushed) return;
        self.flushed = true;
        const cm = self.cmds.items[self.cmd_len..];
        self.cmd_len = self.cmds.items.len;
        const seg = Segment{
            .start = self.m.__first,
            .commands = cm,
        };
        try self.seg.append(seg);
    }
    pub fn move(self: *NodeMaker, p: Point) !void {
        try self.flush();
        self.flushed = false;
        const nd = self.m.move(p);
        try self.cmds.append(nd);
    }
    pub fn close(self: *NodeMaker) !void {
        const nd = self.m.close(self.lw);
        try self.cmds.append(nd);
        try self.flush();
    }
    pub fn line(self: *NodeMaker, p: Point) !void {
        const nd = self.m.line(self.lw, p);
        try self.cmds.append(nd);
    }
    pub fn vert(self: *NodeMaker, f: f32) !void {
        const nd = self.m.vert(self.lw, f);
        try self.cmds.append(nd);
    }
    pub fn horiz(self: *NodeMaker, f: f32) !void {
        const nd = self.m.horiz(self.lw, f);
        try self.cmds.append(nd);
    }
    pub fn quadratic_bezier_curve_to(self: *NodeMaker, c: Point, p: Point) !void {
        const nd = self.m.quadratic_bezier_curve_to(self.lw, c, p);
        try self.cmds.append(nd);
    }
    pub fn curve_to(self: *NodeMaker, c1: Point, c2: Point, p: Point) !void {
        const nd = self.m.curve_to(self.lw, c1, c2, p);
        try self.cmds.append(nd);
    }
    pub fn smooth_quadratic_bezier_curve_to(self: *NodeMaker, p: Point) !void {
        const nd = self.m.smooth_quadratic_bezier_curve_to(self.lw, p);
        try self.cmds.append(nd);
    }
    pub fn smooth_curve_to(self: *NodeMaker, c2: Point, p: Point) !void {
        const nd = self.m.smooth_curve_to(self.lw, c2, p);
        try self.cmds.append(nd);
    }
    pub fn elliptical_arc(self: *NodeMaker, rx: f32, ry: f32, rotation: f32, large_arc: bool, sweep_ccw: bool, p: Point) !void {
        const nd = self.m.elliptical_arc(self.lw, rx, ry, rotation, large_arc, sweep_ccw, p);
        try self.cmds.append(nd);
    }
    pub fn circular_arc(self: *NodeMaker, r: f32, large_arc: bool, sweep_ccw: bool, p: Point) !void {
        const nd = self.m.circular_arc(self.lw, r, large_arc, sweep_ccw, p);
        try self.cmds.append(nd);
    }
};

pub const make_node = struct {
    __c: ?Point = null,
    __first: Point,
    __pos: Point,
    vw: Svg,

    pub fn init(p: Point) make_node {
        return make_node{
            .__first = p,
            .__pos = p,
            .__c = null,
        };
    }
    pub fn move(self: *make_node, p: Point) void {
        self.__first = p;
        self.__pos = p;
        self.__c = null;
    }
    pub fn close(self: *make_node, lw: f32) Node {
        self.__pos = self.__first;
        self.__c = null;
        return Node{ .close = NodeData(void){
            .data = {},
            .line_width = lw,
        } };
    }
    pub fn line(self: *make_node, lw: f32, p: Point) Node {
        self.__pos = p;
        return Node{ .line = NodeData(Point){
            .data = self.vw.point_from(p),
            .line_width = lw,
        } };
    }
    pub fn vert(self: *make_node, lw: f32, f: f32) Node {
        self.__pos.y += f;
        return self.line(lw, self.__pos);
    }
    pub fn horiz(self: *make_node, lw: f32, f: f32) Node {
        self.__pos.x += f;
        return self.line(lw, self.__pos);
    }

    pub fn quadratic_bezier_curve_to(self: *make_node, lw: f32, c: Point, p: Point) Node {
        self.__pos = p;
        self.__c = c;
        return Node{ .quadratic_bezier = NodeData(Node.QuadraticBezier){
            .data = Node.QuadraticBezier{
                .c = self.vw.transform(c),
                .p1 = self.vw.transform(p),
            },
            .line_width = lw,
        } };
    }
    pub fn curve_to(self: *make_node, lw: f32, c1: Point, c2: Point, p: Point) Node {
        self.__pos = p;
        self.__c = c2;

        return Node{ .bezier = NodeData(Node.Bezier){
            .data = Node.Bezier{
                .c0 = self.vw.transform(c1),
                .c1 = self.vw.transform(c2),
                .p1 = self.vw.transform(p),
            },
            .line_width = lw,
        } };
    }

    pub fn smooth_quadratic_bezier_curve_to(self: *make_node, lw: f32, p: Point) Node {
        self.__pos = p;

        return Node{ .quadratic_bezier = NodeData(Node.QuadraticBezier){
            .data = Node.QuadraticBezier{
                .c = self.vw.transform(self.__c orelse return error.NoPrevControlPoint),
                .p1 = self.vw.transform(p),
            },
            .line_width = lw,
        } };
    }
    pub fn smooth_curve_to(self: *make_node, lw: f32, c2: Point, p: Point) Node {
        self.__pos = p;
        self.__c = c2;

        return Node{ .bezier = NodeData(Node.Bezier){
            .data = Node.Bezier{
                .c0 = self.vw.transform(reflect_point(self.__c, self.__pos)),
                .c1 = self.vw.transform(c2),
                .p1 = self.vw.transform(p),
            },
            .line_width = lw,
        } };
    }
    pub fn elliptical_arc(self: *make_node, lw: f32, rx: f32, ry: f32, rotation: f32, large_arc: bool, sweep_ccw: bool, p: Point) Node {
        self.__pos = p;
        return Node{ .arc_ellipse = NodeData(Node.ArcEllipse){
            .data = Node.ArcEllipse{
                .radius_x = rx,
                .radius_y = ry,
                .rotation = rotation,
                .large_arc = large_arc,
                .sweep = sweep_ccw,
                .target = self.vw.transform(p),
            },
            .line_width = lw,
        } };
    }
    pub fn circular_arc(self: *make_node, lw: f32, r: f32, large_arc: bool, sweep_ccw: bool, p: Point) Node {
        self.__pos = p;
        return Node{
            .arc_circle = NodeData(Node.ArcCircle){
                .data = Node.ArcCircle{
                    .large_arc = large_arc,
                    .radius = r,
                    .sweep = sweep_ccw,
                    .target = self.vw.transform(p),
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

const ColMap = std.AutoArrayHashMap(ColorHash, usize);

const Svg = struct {
    pub const xmlns = "http://www.w3.org/2000/svg";
    x: f32 = 0,
    y: f32 = 0,
    width: ?f32 = null, // default is auto ?! wtf is auto
    height: ?f32 = null, // default is auto ?! wtf is auto
    viewBox: ViewBox,
    // preserveAspectRatio="How the svg fragment is deformed if it is displayed with a different aspect ratio". Can be none| xMinYMin| xMidYMin| xMaxYMin| xMinYMid| xMidYMid| xMaxYMid| xMinYMax| xMidYMax| xMaxYMax. Default is xMidYMid
    pub fn parse(self: *@This(), alloc: Allocator, att: []const u8, val: []const u8) !void {
        if (std.mem.eql(u8, att, "width")) {
            const v = try std.fmt.parseFloat(f32, val);
            self.width = v;
        }
        if (std.mem.eql(u8, att, "height")) {
            const v = try std.fmt.parseFloat(f32, val);
            self.height = v;
        }
        if (try utils.parsePointList(alloc, "viewBox", att, val)) |res| {
            self.viewBox.x = res.items[0].x;
            self.viewBox.y = res.items[0].y;
            self.viewBox.w = res.items[1].x;
            self.viewBox.h = res.items[1].y;
        }
    }
    pub fn check(self: *@This()) !void {
        if (self.width == null or self.height == null) return error.NoWidthHeightDefined;
        if (self.viewBox.h == null or self.viewBox.w == null) {
            self.viewBox.h = self.height;
            self.viewBox.w = self.width;
        }
    }
    pub fn transform(self: *@This(), pp: Point) Point {
        const p = Point{
            .x = @floatCast(pp.x),
            .y = @floatCast(pp.y),
        };
        return self.viewBox.transform(@floatFromInt(self.width.?), @floatFromInt(self.height.?), p);
    }
    pub fn point_from(self: *@This(), coord: svg_parsing.CoordinatePair) Point {
        const p = Point{
            .x = @floatCast(coord.coordinates[0].number.value),
            .y = @floatCast(coord.coordinates[1].number.value),
        };
        return self.viewBox.transform(@floatFromInt(self.width.?), @floatFromInt(self.height.?), p);
    }
};
fn reflect_point(prev_control: Point, current_pos: Point) Point {
    return Point{
        .x = 2 * current_pos.x - prev_control.x,
        .y = 2 * current_pos.y - prev_control.y,
    };
}
pub const ColorProperties: []const []const u8 = &.{
    "fill",
    "stroke",
};
pub const f32Properties = struct {
    // not supported dor now!
    pub const Opacity: []const []const u8 = &.{
        "stroke-opacity",
        "fill-opacity",
        "opacity",
    };
};
const Rect = struct {
    width: ?f32 = null, //the width of the rectangle. Required.
    height: ?f32 = null, //the height of the rectangle Required.
    x: f32 = 0, //the x-position for the top-left corner of the rectangle
    y: f32 = 0, //the y-position for the top-left corner of the rectangle
    rx: f32 = 0, //The x radius of the corners of the rectangle (used to round the corners). Default is 0
    ry: f32 = 0, //The y radius of the corners of the rectangle (used to round the corners). Default is 0
    // pathLength = "the total length for the rectangle's perimeter",
    fill: ?Color = null,
    @"fill-opacity": ?f32 = null, // between 0 and 1
    stroke: ?Color = null,
    @"stroke-width": ?f32 = null,
    @"stroke-opacity": ?f32 = null, // between 0 and 1
    pub fn parse(
        self: *@This(),
        maker: *NodeMaker,
        att: []const []const u8,
        val: []const []const u8,
    ) !void {
        const Def = struct {
            const width: ?f32 = undefined;
            const height: ?f32 = undefined;
            const x: f32 = undefined;
            const y: f32 = undefined;
            const rx: f32 = undefined;
            const ry: f32 = undefined;
            const fill: ?Color = undefined;
            const @"fill-opacity": ?f32 = undefined;
            const stroke: ?Color = undefined;
            const @"stroke-width": ?f32 = undefined;
            const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
        }
        const lw = default.or_stroke_width(self.@"stroke-width");
        maker.lw = lw;
        const yline_len = @min(0, self.height - self.ry * 2);
        const xline_len = @min(0, self.width - self.rx * 2);
        var p = Point{
            .x = self.x + self.rx,
            .y = self.y,
        };
        try maker.move(p);
        p.x += xline_len;
        try maker.line(p);
        p.x += self.rx;
        p.y += self.ry;
        try maker.elliptical_arc(self.rx, self.ry, 0, false, false, p);
        p.y += yline_len;
        try maker.line(p);
        p.x += -self.rx;
        p.y += self.ry;
        try maker.elliptical_arc(self.rx, self.ry, 0, false, false, p);
        p.x += -xline_len;
        try maker.line(p);
        p.x += -self.rx;
        p.y += -self.ry;
        try maker.elliptical_arc(self.rx, self.ry, 0, false, false, p);
        p.y += -yline_len;
        try maker.line(p);
        p.x += self.rx;
        p.y += -self.ry;
        try maker.elliptical_arc(self.rx, self.ry, 0, false, false, p);
        try maker.flush();
    }
};
const Circle = struct {
    r: ?f32 = null, //The radius of the circle. Required
    cx: f32 = 0, //the x-axis center of the circle
    cy: f32 = 0, //the y-axis center of the circle
    fill: ?Color = null,
    @"fill-opacity": ?f32 = null, // between 0 and 1
    stroke: ?Color = null,
    @"stroke-width": ?f32 = null,
    @"stroke-opacity": ?f32 = null, // between 0 and 1
    pub fn parse(
        self: *@This(),
        maker: *NodeMaker,
        att: []const []const u8,
        val: []const []const u8,
    ) !void {
        const Def = struct {
            const r: ?f32 = undefined;
            const cx: f32 = undefined;
            const cy: f32 = undefined;
            const fill: ?Color = undefined;
            const @"fill-opacity": ?f32 = undefined;
            const stroke: ?Color = undefined;
            const @"stroke-width": ?f32 = undefined;
            const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
        }
        const r = self.r orelse return error.NoRadius;
        const lw = default.or_stroke_width(self.@"stroke-width");
        maker.lw = lw;

        try maker.move(.{ .x = self.cx - r, .y = self.cy });
        try maker.circular_arc(r, true, false, .{ .x = self.cx + r, .y = self.cy });
        try maker.circular_arc(r, true, false, .{ .x = self.cx - r, .y = self.cy });
        try maker.flush();
    }
};
pub const default = struct {
    pub const stroke_width: f32 = 2;
    pub const stroke_opacity: f32 = 1; // between 0 and 1
    pub const fill_opacity: f32 = 1; // between 0 and 1
    pub fn or_stroke_width(stroke: ?f32) f32 {
        return stroke orelse stroke_width;
    }
};

const Ellipse = struct {
    rx: ?f32 = null, //the x radius of the ellipse. Required.
    ry: ?f32 = null, //the y radius of the ellipse. Required.
    cx: f32 = 0, //the x-axis center of the ellipse
    cy: f32 = 0, //the y-axis center of the ellipse
    fill: ?Color = null,
    @"fill-opacity": ?f32 = null, // between 0 and 1
    stroke: ?Color = null,
    @"stroke-width": ?f32 = null,
    @"stroke-opacity": ?f32 = null, // between 0 and 1
    pub fn parse(
        self: *@This(),
        maker: *NodeMaker,
        att: []const []const u8,
        val: []const []const u8,
    ) !void {
        const Def = struct {
            const rx: ?f32 = undefined;
            const ry: ?f32 = undefined;
            const cx: f32 = undefined;
            const cy: f32 = undefined;
            const fill: ?Color = undefined;
            const @"fill-opacity": ?f32 = undefined;
            const stroke: ?Color = undefined;
            const @"stroke-width": ?f32 = undefined;
            const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
        }
        const rrx = self.rx orelse return error.NoRadius;
        const rry = self.ry orelse return error.NoRadius;

        const lw = default.or_stroke_width(self.@"stroke-width");
        maker.lw = lw;

        try maker.move(.{ .x = self.cx - rrx, .y = self.cy });
        try maker.elliptical_arc(rrx, rry, 0, false, false, .{ .x = self.cx + rrx, .y = self.cy });
        try maker.elliptical_arc(rrx, rry, 0, false, false, .{ .x = self.cx - rrx, .y = self.cy });
        try maker.flush();
    }
};
const Line = struct {
    x1: ?f32 = null, //"The start of the line on the x-axis"
    y1: ?f32 = null, //"The start of the line on the y-axis"
    x2: ?f32 = null, //"The end of the line on the x-axis"
    y2: ?f32 = null, //"The end of the line on the y-axis"
    stroke: ?Color = null,
    @"stroke-width": ?f32 = null,
    @"stroke-opacity": ?f32 = null, // between 0 and 1
    pub fn parse(
        self: *@This(),
        maker: *NodeMaker,
        att: []const []const u8,
        val: []const []const u8,
    ) !void {
        const Def = struct {
            pub const x1: ?f32 = undefined;
            pub const y1: ?f32 = undefined;
            pub const x2: ?f32 = undefined;
            pub const y2: ?f32 = undefined;
            pub const stroke: ?Color = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
        }
        const ax = self.x1 orelse return error.MissingLineParameter;
        const ay = self.y1 orelse return error.MissingLineParameter;
        const bx = self.x2 orelse return error.MissingLineParameter;
        const by = self.y2 orelse return error.MissingLineParameter;
        const lw = default.or_stroke_width(self.@"stroke-width");
        maker.lw = lw;

        try maker.move(.{ .x = ax, .y = ay });
        try maker.line(.{ .x = bx, .y = by });
        try maker.flush();
    }
};
// Defines any shape that consists of only straight lines. The shape is open
const PolyLine = struct {
    points: []const Point, //The list of points of the polygon. Each point must contain an x coordinate and a y coordinate. Required.
    fill: ?Color = null,
    @"fill-opacity": ?f32 = null, // between 0 and 1
    stroke: ?Color = null,
    @"stroke-width": ?f32 = null,
    @"stroke-opacity": ?f32 = null, // between 0 and 1

    pub fn parse(
        self: *@This(),
        maker: *NodeMaker,
        alloc: Allocator,
        att: []const []const u8,
        val: []const []const u8,
    ) !void {
        const Def = struct {
            pub const fill: ?Color = undefined;
            pub const @"fill-opacity": ?f32 = undefined;
            pub const stroke: ?Color = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
            if (try utils.parsePointList(alloc, "points", a, v)) |res| {
                self.points = res.items;
            }
        }
        const lw = default.or_stroke_width(self.@"stroke-width");
        maker.lw = lw;
        for (self.points, 0..) |p, i| {
            if (i == 0) {
                try maker.move(.{ .x = p.x, .y = p.y });
            } else {
                try maker.line(.{ .x = p.x, .y = p.y });
            }
        }
        try maker.flush();
    }
};
// Creates a graphic that contains at least three sides. Polygons are made of straight lines, and the shape is "closed"
const Polygon = struct {
    points: []const Point, //The list of points of the polygon. Each point must contain an x-  and a y-coordinate. Required.
    fill: ?Color = null,
    @"fill-opacity": ?f32 = null, // between 0 and 1
    stroke: ?Color = null,
    @"stroke-width": ?f32 = null,
    @"stroke-opacity": ?f32 = null, // between 0 and 1
    pub fn parse(
        self: *@This(),
        maker: *NodeMaker,
        alloc: Allocator,
        att: []const []const u8,
        val: []const []const u8,
    ) !void {
        const Def = struct {
            pub const fill: ?Color = undefined;
            pub const @"fill-opacity": ?f32 = undefined;
            pub const stroke: ?Color = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
            if (try utils.parsePointList(alloc, "points", a, v)) |res| {
                self.points = res.items;
            }
        }
        const lw = default.or_stroke_width(self.@"stroke-width");
        maker.lw = lw;
        for (self.points, 0..) |p, i| {
            if (i == 0) {
                try maker.move(.{ .x = p.x, .y = p.y });
            } else {
                try maker.line(.{ .x = p.x, .y = p.y });
            }
        }
        try maker.close();
    }
};
const SvgPath = struct {
    d: ParsePath, //The list of points of the polygon. Each point must contain an x-  and a y-coordinate. Required.
    fill: ?Color = null,
    @"fill-opacity": ?f32 = null, // between 0 and 1
    stroke: ?Color = null,
    @"stroke-width": ?f32 = null,
    @"stroke-opacity": ?f32 = null, // between 0 and 1
    pub fn parse(
        self: *@This(),
        maker: *NodeMaker,
        alloc: Allocator,
        att: []const []const u8,
        val: []const []const u8,
    ) !void {
        const Def = struct {
            pub const fill: ?Color = undefined;
            pub const @"fill-opacity": ?f32 = undefined;
            pub const stroke: ?Color = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, vx| {
            try utils.auto_parse_def(@This(), self, Def, a, vx);
        }
        for (att, val) |a, vx| {
            if (std.mem.eql(u8, a, "d")) {
                const lw = default.or_stroke_width(self.@"stroke-width");
                maker.lw = lw;
                var path_d = try svg_parsing.Path.parse(alloc, vx);
                defer path_d.path.deinit();
                for (path_d.path.nodes) |pnode| {
                    switch (pnode) {
                        .move_to => |v| {
                            for (v.args) |coord| {
                                const p = utils.toPoint(coord);
                                try maker.move(p);
                            }
                        },

                        .close_path => |_| {
                            try maker.close();
                        },
                        .line_to => |v| {
                            for (v.args) |coord| {
                                const p = utils.toPoint(coord);
                                try maker.line(p);
                            }
                        },
                        .vertical_line_to => |v| {
                            for (v.args) |coord| {
                                const f = utils.fromNumber(coord);
                                try maker.vert(f);
                            }
                        },
                        .horizontal_line_to => |v| {
                            for (v.args) |coord| {
                                const f = utils.fromNumber(coord);
                                try maker.horiz(f);
                            }
                        },
                        .quadratic_bezier_curve_to => |v| {
                            for (v.args) |arg| {
                                const c = utils.toPoint(arg.p1);
                                const p = utils.toPoint(arg.end);
                                try maker.quadratic_bezier_curve_to(c, p);
                            }
                        },
                        .curve_to => |v| {
                            for (v.args) |arg| {
                                const c1 = utils.toPoint(arg.p1);
                                const c2 = utils.toPoint(arg.p2);
                                const p = utils.toPoint(arg.end);
                                try maker.curve_to(c1, c2, p);
                            }
                        },
                        .smooth_quadratic_bezier_curve_to => |v| {
                            for (v.args) |arg| {
                                const p = utils.toPoint(arg);
                                try maker.smooth_quadratic_bezier_curve_to(p);
                            }
                        },
                        .smooth_curve_to => |v| {
                            for (v.args) |arg| {
                                const c2 = utils.toPoint(arg.p2);
                                const p = utils.toPoint(arg.end);
                                try maker.smooth_curve_to(c2, p);
                            }
                        },
                        .elliptical_arc => |v| {
                            for (v.args) |arg| {
                                const rx: f32 = @floatCast(arg.rx.value);
                                const ry: f32 = @floatCast(arg.ry.value);
                                const rotation: f32 = @floatCast(arg.x_axis_rotation.value);
                                const large_arc = arg.large_arc_flag.value;
                                const sweep = arg.sweep_flag.value;
                                const target = utils.toPoint(arg.point);
                                try maker.elliptical_arc(rx, ry, rotation, large_arc, sweep, target);
                            }
                        },
                    }
                }
                try maker.flush();
            }
        }
    }
};
const G = struct {
    fill: ?Color = null,
    @"fill-opacity": ?f32 = null, // between 0 and 1
    stroke: ?Color = null,
    @"stroke-width": ?f32 = null,
    @"stroke-opacity": ?f32 = null, // between 0 and 1
    pub fn parse(
        self: *@This(),
        att: []const []const u8,
        val: []const []const u8,
    ) !void {
        const Def = struct {
            pub const fill: ?Color = undefined;
            pub const @"fill-opacity": ?f32 = undefined;
            pub const stroke: ?Color = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
        }
    }
};
const ParsePath = struct {};
pub fn SvgConverter(W: type) type {
    return struct {
        const Converter = @This();
        const Builder = tvg.builder.Builder(W);
        arena1: std.heap.ArenaAllocator,
        arena2: std.heap.ArenaAllocator,
        colortable_len: usize = 0,
        colormap: ColMap,
        svg: Svg = Svg{},
        builder: Builder,
        maker: NodeMaker = undefined,
        cmds: std.ArrayList(Node),
        seg: std.ArrayList(Segment),

        pub fn init(alloc: Allocator, writer: W) @This() {
            return @This(){
                .builder = Builder{ .writer = writer },
                .arena1 = std.heap.ArenaAllocator.init(alloc),
                .arena2 = std.heap.ArenaAllocator.init(alloc),
                .colormap = ColMap.init(alloc),
                .cmds = try std.ArrayList(Node).init(alloc),
                .seg = try std.ArrayList(Segment).init(alloc),
            };
        }
        pub fn deinit(self: *@This()) @This() {
            self.arena1.deinit();
            self.arena2.deinit();
            self.colormap.deinit();
            self.cmds.deinit();
            self.seg.deinit();
        }
        pub fn write_path(
            self: *@This(),
            pathlist: []const Segment,
            fill: ?Color,
            stroke: ?Color,
            stroke_width: ?f32,
        ) !void {
            if (stroke) |col| {
                const col_idx = self.colormap.get(.fromColor(col)).?;
                try self.builder.writeDrawPath(.{ .flat = col_idx }, stroke_width orelse default.stroke_width, pathlist);
            }
            if (fill) |col| {
                const col_idx = self.colormap.get(.fromColor(col)).?;
                try self.builder.writeFillPath(.{ .flat = col_idx }, pathlist);
            }
        }

        pub fn write_path_T(
            self: *@This(),
            pathlist: []const Segment,
            t: anytype,
        ) !void {
            const T = @TypeOf(t);
            if (comptime utils.has_field(T, "stroke") and
                utils.has_field(T, "fill") and
                utils.has_field(T, "stroke-width"))
            {
                const fill = @field(t, "fill");
                const stroke = @field(t, "stroke");
                const stroke_width = @field(t, "stroke-width");
                try self.write_path(pathlist, fill, stroke, stroke_width);
            } else @compileError("why U call this?");
        }
        pub fn parse_colors_and_svg(self: *@This(), svg_bytes: []const u8) !void {
            const alloc = self.arena1.allocator();
            var fbuffs = std.io.fixedBufferStream(svg_bytes);
            var xml_res = xml.streamingDocument(alloc, fbuffs.reader());
            defer xml_res.deinit();
            var reader = xml_res.reader(alloc, .{});
            defer reader.deinit();

            while (true) {
                const node = reader.read() catch |err| switch (err) {
                    error.MalformedXml => {
                        const loc = reader.errorLocation();
                        std.log.err("{}:{}: {}", .{ loc.line, loc.column, reader.errorCode() });
                        return error.MalformedXml;
                    },
                    else => |other| return other,
                };
                switch (node) {
                    .element_start => {
                        const element_name = reader.elementNameNs();
                        const element_tag = element_name.local;

                        const att_count = reader.reader.attributeCount();

                        if (std.mem.eql(u8, "svg", element_tag)) {
                            const att_names = try alloc.alloc([]const u8, att_count);
                            const att_vals = try alloc.alloc([]const u8, att_count);
                            for (att_names, att_vals, 0..) |*n, *v, i| {
                                n.* = try alloc.dupe(u8, reader.attributeNameNs(i).local);
                                v.* = try alloc.dupe(u8, reader.attributeValue(i));
                            }
                            var svg = Svg{};
                            try svg.parse(att_names, att_vals);
                            try svg.check();
                            self.svg = svg;
                        }
                        for (0..att_count) |i| {
                            const att_name = reader.attributeNameNs(i).local;
                            const att_val = try reader.attributeValue(i);
                            inline for (ColorProperties) |p| {
                                const c = try utils.parseColor(p, att_name, att_val);
                                if (c) |col| {
                                    if (self.colormap.getKey(.fromColor(col))) |k| {
                                        try self.colormap.put(k, self.colortable_len);
                                        self.colortable_len += 1;
                                    }
                                }
                            }
                        }
                    },
                    else => {},
                    .eof => break,
                }
            }
        }
        pub fn svg_width(self: *@This()) u32 {
            return @intFromFloat(self.svg.width);
        }
        pub fn svg_height(self: *@This()) u32 {
            return @intFromFloat(self.svg.height);
        }

        pub fn run(self: *@This(), svg_bytes: []const u8) !void {
            try self.parse_colors_and_svg(svg_bytes);
            _ = self.arena1.reset(.retain_capacity);
            _ = self.arena2.reset(.retain_capacity);

            const gpa = self.arena1.child_allocator;
            var fbuffs = std.io.fixedBufferStream(svg_bytes);
            var xml_res = xml.streamingDocument(gpa, fbuffs.reader());
            defer xml_res.deinit();
            var reader = xml_res.reader(gpa, .{});
            defer reader.deinit();

            try self.builder.writeHeader(self.svg_width(), self.svg_height(), Scale.@"1/4096", .u8888, Range.enhanced);

            const colors_hash = self.colormap.keys();
            const colors = try self.arena1.allocator().alloc(Color, colors_hash.len);
            for (colors, colors_hash) |*v, v2| {
                v.* = v2.toColor();
            }
            try self.builder.writeColorTable(colors);

            var stack = try Stack(InheritableProperties).init(gpa, 128);
            defer stack.deinit(gpa);

            var layer_arena = std.heap.ArenaAllocator.init(gpa);
            defer layer_arena.deinit();
            var found_svg_tag = false;

            while (true) {
                const node = reader.read() catch |err| switch (err) {
                    error.MalformedXml => {
                        const loc = reader.errorLocation();
                        std.log.err("{}:{}: {}", .{ loc.line, loc.column, reader.errorCode() });
                        return error.MalformedXml;
                    },
                    else => |other| return other,
                };
                switch (node) {
                    .element_start => {
                        const element_name = reader.elementNameNs();
                        const element_tag = element_name.local;
                        const current_properties = stack.top() orelse InheritableProperties{};
                        if (!found_svg_tag) {
                            if (std.mem.eql(u8, "svg", element_tag)) {
                                found_svg_tag = true;
                                const makenode = make_node.init(Point{ .x = 0, .y = 0 });
                                self.maker = NodeMaker.init(makenode, &self.cmds, &self.seg);
                                try stack.push(current_properties);
                                const top_mut = stack.top_mut().?;
                                top_mut.override_from(self.svg);
                                continue;
                            }
                        } else {
                            try stack.push(current_properties);
                            const top_mut = stack.top_mut().?;

                            // std.log.warn(
                            //     \\element_start:  {}
                            // , .{
                            //     std.zig.fmtEscapes(element_name.local),
                            // });

                            const att_count = reader.reader.attributeCount();
                            const att_names = try self.arena2.allocator().alloc([]const u8, att_count);
                            const att_vals = try self.arena2.allocator().alloc([]const u8, att_count);

                            for (att_names, att_vals, 0..) |*n, *v, i| {
                                n.* = try self.arena2.allocator().dupe(u8, reader.attributeNameNs(i).local);
                                v.* = try self.arena2.allocator().dupe(u8, reader.attributeValue(i));
                            }
                            // Container
                            if (std.mem.eql(u8, "g", element_tag)) {
                                var element = G{};
                                try element.parse(att_names, att_vals);
                                top_mut.override_from(element);
                            } else
                            // Drawing Primitives
                            if (std.mem.eql(u8, "rect", element_tag)) {
                                var element = Rect{};
                                try element.parse(att_names, att_vals);
                                top_mut.override_from(element);
                            } else if (std.mem.eql(u8, "circle", element_tag)) {
                                var element = Circle{};
                                try element.parse(att_names, att_vals);
                                top_mut.override_from(element);
                            } else if (std.mem.eql(u8, "ellipse", element_tag)) {
                                var element = Ellipse{};
                                try element.parse(att_names, att_vals);
                                top_mut.override_from(element);
                            } else if (std.mem.eql(u8, "line", element_tag)) {
                                var element = Line{};
                                try element.parse(att_names, att_vals);
                                top_mut.override_from(element);
                            } else if (std.mem.eql(u8, "polyline", element_tag)) {
                                var element = PolyLine{};
                                try element.parse(att_names, att_vals);
                                top_mut.override_from(element);
                            } else if (std.mem.eql(u8, "polygon", element_tag)) {
                                var element = Polygon{};
                                try element.parse(att_names, att_vals);
                                top_mut.override_from(element);
                            } else if (std.mem.eql(u8, "path", element_tag)) {
                                var element = SvgPath{};
                                try element.parse();
                                top_mut.override_from(element);
                            } else {
                                std.log.warn("unrecognized element", .{element_tag});
                            }

                            _ = self.arena2.reset(.retain_capacity);
                        }
                    },
                    .element_end => {
                        _ = stack.pop();
                    },
                    else => {},
                    .eof => break,
                }
            }
            try self.builder.writeEndOfFile();
        }
    };
}

pub fn tvg_from_svg(alloc: Allocator, writer: anytype, svg_bytes: []const u8) !void {
    var con = try SvgConverter(@TypeOf(writer)).init(alloc);
    try con.run(svg_bytes);
}

pub fn make_tvg(writer: anytype) !void {
    var builder = tvg.builder.create(writer);
    const col_custom = try Color.fromString("202020");
    _ = col_custom;
    const colors: []const Color = &.{RED};
    try builder.writeHeader(25, 25, Scale.@"1/4096", .u8888, Range.enhanced);

    try builder.writeColorTable(colors);
    const points: []const Point = &.{
        Point{ .x = 5, .y = 5 },
        Point{ .x = 20, .y = 5 },
        Point{ .x = 20, .y = 20 },
        Point{ .x = 5, .y = 20 },
    };
    const com: []const Node = &.{
        Node{
            .line = NodeData(Point){ .data = points[2], .line_width = 2.0 },
        },
        Node{
            .line = NodeData(Point){ .data = points[3], .line_width = 2.0 },
        },
        Node{
            .close = NodeData(void){ .data = {} },
        },
    };

    const seg = Segment{ .start = points[0], .commands = com };

    // try builder.writeDrawLineStrip(.{ .flat = 0 }, 2, points);
    try builder.writeDrawPath(Style{ .flat = 0 }, 2.0, &.{seg});

    // const path = &.{Segment};
    // builder.writeDrawPath(.{ .flat = 0 }, 2, path);
    try builder.writeEndOfFile();
}

test "make tvg og" {
    if (true) return;
    const gpa = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const alloc = arena.allocator();
    // var output = std.io.getStdOut();

    var file = try std.fs.cwd().createFile("test/gen.tvg", .{});
    try make_tvg(file.writer());
    file.close();

    var fileo = try std.fs.cwd().openFile("test/gen.tvg", .{});
    const w = 300;
    const h = 300;
    var image_wrapper = ImageWrapper{
        .img = try Image.init(alloc, w, h),
        .width = w,
        .height = h,
    };
    const icon = try fileo.readToEndAlloc(alloc, 1024 * 1024);
    try tvg.render(alloc, &image_wrapper, icon);
    try image_wrapper.img.write_ppm_to_file("test/gen.ppm");
    std.log.warn("ok", .{});
}
