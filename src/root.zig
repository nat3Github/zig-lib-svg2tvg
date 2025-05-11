const std = @import("std");
const assert = std.debug.assert;
const expect = std.debug.expect;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const math = std.math;
// const root = @import("../root.zig");
const ut = @import("util.zig");
const utils = ut.utils;
const ColorHash = ut.ColorHash;
const Stack = ut.Stack;
const NodeMaker = ut.NodeMaker;
const ColMap = ut.ColMap;

const svg_parsing = @import("svg.zig");
const xml = @import("xml");
const icons = @import("icons");
const tvg_og = @import("tvg");
pub const z2d = @import("z2d");

const tinyvg2 = @import("tinyvg/tinyvg.zig");
pub const tvg = tinyvg2;
const librender = @import("render.zig");
pub const renderStream = librender.renderStream;
pub const RenderOptions = librender.Options;

pub const Color = tvg.Color;
const Path = tvg.Path;
const Segment = Path.Segment;
const Node = Path.Node;
const NodeData = Path.Node.NodeData;
const Point = tvg.Point;
const Scale = tvg.Scale;
const Range = tvg.Range;
const Style = tvg.Style;

pub const SvgColorAttribute = enum {
    none,
    inherit,
    currentColor,
};

pub const SvgColorTag = enum {
    att,
    col,
};
/// TODO: Svg Color inheritance handling of overriding values inside containers
/// (Stack) -> Color Resolving
pub const SvgColor = union(SvgColorTag) {
    att: SvgColorAttribute,
    col: Color,
    pub const none: SvgColor = .{ .att = .none };
    pub const inherit: SvgColor = .{ .att = .inherit };
    pub const currentColor: SvgColor = .{ .att = .currentColor };

    pub fn parseColor(val: []const u8) SvgColor {
        const trimmed = std.mem.trim(u8, val, " ");

        if (std.mem.eql(u8, trimmed, "none")) {
            return none;
        }
        if (std.mem.eql(u8, trimmed, "inherit")) {
            return inherit;
        }
        if (std.mem.eql(u8, trimmed, "currentColor")) {
            return currentColor;
        }
        return SvgColor{
            .col = Color_from(svg_parsing.Color.parse(val).color, 1.0),
        };
    }
    fn normalize_u8(u: u8) f32 {
        const x: f32 = @floatFromInt(u);
        return x / 255.0;
    }
    fn Color_from(col: svg_parsing.Color, alpha_normalized: f32) Color {
        return Color{
            .r = normalize_u8(col.r),
            .g = normalize_u8(col.g),
            .b = normalize_u8(col.b),
            .a = std.math.clamp(alpha_normalized, 0, 1),
        };
    }
    pub fn get_hash_key(self: *const SvgColor) ?ColorHash {
        if (self.* == .att) return null;
        return ColorHash.fromColor(self.col);
    }
};

const WidthHeight = struct {
    w: ?f32 = null,
    h: ?f32 = null,
};
const ViewBox = struct {
    x: f32 = 0,
    y: f32 = 0,
    w: ?f32 = null,
    h: ?f32 = null,
    fn transform(
        self: *const @This(),
        width: f32,
        height: f32,
        point: Point,
    ) Point {
        assert(width != 0);
        assert(height != 0);
        assert(self.w.? != 0);
        assert(self.h.? != 0);
        return Point{
            .x = (point.x - self.x) * (width / self.w.?),
            .y = (point.y - self.y) * (height / self.h.?),
        };
    }
};

const InheritableProperties = struct {
    fill: ?SvgColor = null, // Fill color
    stroke: ?SvgColor = null, // Stroke (outline) color
    color: ?SvgColor = null, // Used as base color (currentColor, filters, etc.)
    @"fill-opacity": ?f32 = null, // Opacity of the fill
    @"stroke-opacity": ?f32 = null, // Stroke opacity
    @"stroke-width": ?f32 = null, // Stroke thickness
    // @"stroke-dasharray": ? = null, // Dash pattern of the stroke
    // @"stroke-dashoffset": ? = null, // Offset into the dash pattern
    // @"stroke-linecap": ?C = null, // Shape of stroke ends (butt, round, square)
    // @"stroke-linejoin": ? = null, // Corner rendering (miter, round, bevel)
    // @"stroke-miterlimit": ? = null, // Miter limit for sharp corners
    opacity: ?f32 = null, // Overall opacity (applies to the whole element)
    // visibility: ?bool = null, // Whether element is visible (visible, hidden)
    // display: ?SvgColor = null, // Whether element is rendered (inline, none)
    // @"font-*": ?SvgColor = null, // If used in text elements
    // direction: ?SvgColor = null, // Text and layout direction (e.g. ltr, rtl)
    // @"text-anchor": ?SvgColor = null, // Text alignment
    // @"writing-mode": ?SvgColor = null, // Vertical/horizontal text flow
    // @"clip-path": ?SvgColor = null, // Clipping region
    // mask: ?SvgColor = null, // Mask to apply to element
    // filter: ?SvgColor = null, // Filter effects (blur, drop shadow, etc.)
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
                // @compileLog(T, f.name);
                if (comptime f.type == @TypeOf(@field(t, f.name))) {
                    if (@field(t, f.name)) |fval| {
                        @field(self, f.name) = fval;
                    }
                }
            }
        }
    }
    pub fn resolve_color_property(stack: *const Stack(InheritableProperties), comptime name: []const u8) ?Color {
        var i = stack.data.len;
        while (i > 0) {
            i -= 1;
            const top: InheritableProperties = stack.data[i];
            const mcol: ?SvgColor = @field(top, name);
            if (mcol == null) return null;
            if (mcol.? == .col) return mcol.?.col;
            const att: SvgColorAttribute = mcol.?.att;
            switch (att) {
                .none => return null,
                .inherit => continue,
                .currentColor => continue,
            }
        }
        unreachable;
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

pub const Svg = struct {
    pub const xmlns = "http://www.w3.org/2000/svg";
    x: f32 = 0,
    y: f32 = 0,
    width: ?f32 = null, // default is auto ?! wtf is auto
    height: ?f32 = null, // default is auto ?! wtf is auto
    viewBox: ViewBox = ViewBox{},
    // preserveAspectRatio="How the svg fragment is deformed if it is displayed with a different aspect ratio". Can be none| xMinYMin| xMidYMin| xMaxYMin| xMinYMid| xMidYMid| xMaxYMid| xMinYMax| xMidYMax| xMaxYMax. Default is xMidYMid
    fill: ?SvgColor = null,
    @"fill-opacity": ?f32 = null, // between 0 and 1
    stroke: ?SvgColor = null,
    @"stroke-width": ?f32 = null,
    @"stroke-opacity": ?f32 = null, // between 0 and 1
    pub fn parse(
        self: *@This(),
        alloc: Allocator,
        att: []const []const u8,
        val: []const []const u8,
    ) !void {
        const Def = struct {
            pub const width: ?f32 = undefined;
            pub const height: ?f32 = undefined;
            pub const fill: ?SvgColor = undefined;
            pub const @"fill-opacity": ?f32 = undefined;
            pub const stroke: ?SvgColor = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
            if (try utils.parsePointList(alloc, "viewBox", a, v)) |res| {
                self.viewBox.x = res.items[0].x;
                self.viewBox.y = res.items[0].y;
                self.viewBox.w = res.items[1].x;
                self.viewBox.h = res.items[1].y;
            }
        }
    }
    pub fn check(self: *@This()) !void {
        if (self.width == null or self.height == null) return error.NoWidthHeightDefined;
        if (self.viewBox.h == null or self.viewBox.w == null) {
            self.viewBox.h = self.height;
            self.viewBox.w = self.width;
        }
    }
    pub fn transform(self: *const @This(), pp: Point) Point {
        const p = Point{
            .x = @floatCast(pp.x),
            .y = @floatCast(pp.y),
        };
        const ret = self.viewBox.transform(self.width.?, self.height.?, p);
        assert(math.isNormal(ret.x) or ret.x == 0);
        assert(math.isNormal(ret.y) or ret.y == 0);
        return ret;
    }
    pub fn point_from(self: *const @This(), coord: svg_parsing.CoordinatePair) Point {
        const p = Point{
            .x = @floatCast(coord.coordinates[0].number.value),
            .y = @floatCast(coord.coordinates[1].number.value),
        };
        return self.viewBox.transform(@floatFromInt(self.width.?), @floatFromInt(self.height.?), p);
    }
};
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
    fill: ?SvgColor = null,
    @"fill-opacity": ?f32 = null, // between 0 and 1
    stroke: ?SvgColor = null,
    @"stroke-width": ?f32 = null,
    @"stroke-opacity": ?f32 = null, // between 0 and 1
    pub fn parse(
        self: *@This(),
        maker: *NodeMaker,
        att: []const []const u8,
        val: []const []const u8,
    ) !void {
        const Def = struct {
            pub const width: ?f32 = undefined;
            pub const height: ?f32 = undefined;
            pub const x: f32 = undefined;
            pub const y: f32 = undefined;
            pub const rx: f32 = undefined;
            pub const ry: f32 = undefined;
            pub const fill: ?SvgColor = undefined;
            pub const @"fill-opacity": ?f32 = undefined;
            pub const stroke: ?SvgColor = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
        }
        if (self.width == null or self.height == null) return error.RectWithoutDimensions;
        const yline_len = @max(0, self.height.? - self.ry * 2);
        const xline_len = @max(0, self.width.? - self.rx * 2);
        var p = Point{
            .x = self.x + self.rx,
            .y = self.y,
        };
        try maker.move(p, false);
        p.x += xline_len;
        try maker.line(p, false);
        p.x += self.rx;
        p.y += self.ry;
        try maker.elliptical_arc(self.rx, self.ry, 0, false, true, p, false);
        p.y += yline_len;
        try maker.line(p, false);
        p.x += -self.rx;
        p.y += self.ry;
        try maker.elliptical_arc(self.rx, self.ry, 0, false, true, p, false);
        p.x += -xline_len;
        try maker.line(p, false);
        p.x += -self.rx;
        p.y += -self.ry;
        try maker.elliptical_arc(self.rx, self.ry, 0, false, true, p, false);
        p.y += -yline_len;
        try maker.line(p, false);
        p.x += self.rx;
        p.y += -self.ry;
        try maker.elliptical_arc(self.rx, self.ry, 0, false, true, p, false);
        try maker.flush();
    }
};
const Circle = struct {
    r: ?f32 = null, //The radius of the circle. Required
    cx: f32 = 0, //the x-axis center of the circle
    cy: f32 = 0, //the y-axis center of the circle
    fill: ?SvgColor = null,
    @"fill-opacity": ?f32 = null, // between 0 and 1
    stroke: ?SvgColor = null,
    @"stroke-width": ?f32 = null,
    @"stroke-opacity": ?f32 = null, // between 0 and 1
    pub fn parse(
        self: *@This(),
        maker: *NodeMaker,
        att: []const []const u8,
        val: []const []const u8,
    ) !void {
        const Def = struct {
            pub const r: ?f32 = undefined;
            pub const cx: f32 = undefined;
            pub const cy: f32 = undefined;
            pub const fill: ?SvgColor = undefined;
            pub const @"fill-opacity": ?f32 = undefined;
            pub const stroke: ?SvgColor = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
        }
        const r = self.r orelse return error.NoRadius;

        try maker.move(.{ .x = self.cx - r, .y = self.cy }, false);
        try maker.circular_arc(r, true, false, .{ .x = self.cx + r, .y = self.cy }, false);
        try maker.circular_arc(r, true, false, .{ .x = self.cx - r, .y = self.cy }, false);
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
    fill: ?SvgColor = null,
    @"fill-opacity": ?f32 = null, // between 0 and 1
    stroke: ?SvgColor = null,
    @"stroke-width": ?f32 = null,
    @"stroke-opacity": ?f32 = null, // between 0 and 1
    pub fn parse(
        self: *@This(),
        maker: *NodeMaker,
        att: []const []const u8,
        val: []const []const u8,
    ) !void {
        const Def = struct {
            pub const rx: ?f32 = undefined;
            pub const ry: ?f32 = undefined;
            pub const cx: f32 = undefined;
            pub const cy: f32 = undefined;
            pub const fill: ?SvgColor = undefined;
            pub const @"fill-opacity": ?f32 = undefined;
            pub const stroke: ?SvgColor = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
        }
        const rrx = self.rx orelse return error.NoRadius;
        const rry = self.ry orelse return error.NoRadius;

        try maker.move(.{ .x = self.cx - rrx, .y = self.cy }, false);
        try maker.elliptical_arc(rrx, rry, 0, false, false, .{ .x = self.cx + rrx, .y = self.cy }, false);
        try maker.elliptical_arc(rrx, rry, 0, false, false, .{ .x = self.cx - rrx, .y = self.cy }, false);
        try maker.flush();
    }
};
const Line = struct {
    x1: ?f32 = null, //"The start of the line on the x-axis"
    y1: ?f32 = null, //"The start of the line on the y-axis"
    x2: ?f32 = null, //"The end of the line on the x-axis"
    y2: ?f32 = null, //"The end of the line on the y-axis"
    stroke: ?SvgColor = null,
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
            pub const stroke: ?SvgColor = undefined;
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
        const p1 = Point{
            .x = ax,
            .y = ay,
        };
        const p2 = Point{
            .x = bx,
            .y = by,
        };
        try maker.move(p1, false);
        try maker.line(p2, false);
        try maker.flush();
    }
};
// Defines any shape that consists of only straight lines. The shape is open
const PolyLine = struct {
    points: []const Point = &.{}, //The list of points of the polygon. Each point must contain an x coordinate and a y coordinate. Required.
    fill: ?SvgColor = null,
    @"fill-opacity": ?f32 = null, // between 0 and 1
    stroke: ?SvgColor = null,
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
            pub const fill: ?SvgColor = undefined;
            pub const @"fill-opacity": ?f32 = undefined;
            pub const stroke: ?SvgColor = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
            if (try utils.parsePointList(alloc, "points", a, v)) |res| {
                self.points = res.items;
            }
        }
        for (self.points, 0..) |p, i| {
            if (i == 0) {
                try maker.move(.{ .x = p.x, .y = p.y }, false);
            } else {
                try maker.line(.{ .x = p.x, .y = p.y }, false);
            }
        }
        try maker.flush();
    }
};
// Creates a graphic that contains at least three sides. Polygons are made of straight lines, and the shape is "closed"
const Polygon = struct {
    points: []const Point = &.{}, //The list of points of the polygon. Each point must contain an x coordinate and a y coordinate. Required.
    fill: ?SvgColor = null,
    @"fill-opacity": ?f32 = null, // between 0 and 1
    stroke: ?SvgColor = null,
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
            pub const fill: ?SvgColor = undefined;
            pub const @"fill-opacity": ?f32 = undefined;
            pub const stroke: ?SvgColor = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
            if (try utils.parsePointList(alloc, "points", a, v)) |res| {
                self.points = res.items;
            }
        }
        if (self.points.len < 3) return error.PolygonHasLessThan3Points;
        for (self.points, 0..) |p, i| {
            if (i == 0) {
                try maker.move(.{ .x = p.x, .y = p.y }, false);
            } else {
                try maker.line(.{ .x = p.x, .y = p.y }, false);
            }
        }
        try maker.close();
    }
};
const ParsePath = struct {};
const SvgPath = struct {
    d: ParsePath = ParsePath{}, //The list of points of the polygon. Each point must contain an x-  and a y-coordinate. Required.
    fill: ?SvgColor = null,
    @"fill-opacity": ?f32 = null, // between 0 and 1
    stroke: ?SvgColor = null,
    @"stroke-width": ?f32 = null,
    @"stroke-opacity": ?f32 = null, // between 0 and 1
    pub fn parse_dep(
        self: *@This(),
        maker: *NodeMaker,
        alloc: Allocator,
        att: []const []const u8,
        val: []const []const u8,
    ) !void {
        const Def = struct {
            pub const fill: ?SvgColor = undefined;
            pub const @"fill-opacity": ?f32 = undefined;
            pub const stroke: ?SvgColor = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, vx| {
            try utils.auto_parse_def(@This(), self, Def, a, vx);
        }
        for (att, val) |a, vx| {
            if (std.mem.eql(u8, a, "d")) {
                var path_d = try svg_parsing.Path.parse(alloc, vx);
                defer path_d.path.deinit();
                for (path_d.path.nodes) |pnode| {
                    switch (pnode) {
                        .move_to => |v| {
                            for (v.args) |coord| {
                                const p = utils.toPoint(coord);
                                try maker.move(p, v.relative);
                            }
                        },
                        .close_path => |_| {
                            try maker.close();
                        },
                        .line_to => |v| {
                            for (v.args) |coord| {
                                const p = utils.toPoint(coord);
                                try maker.line(p, v.relative);
                            }
                        },
                        .vertical_line_to => |v| {
                            for (v.args) |coord| {
                                const f = utils.fromNumber(coord);
                                try maker.vert(f, v.relative);
                            }
                        },
                        .horizontal_line_to => |v| {
                            for (v.args) |coord| {
                                const f = utils.fromNumber(coord);
                                try maker.horiz(f, v.relative);
                            }
                        },
                        .quadratic_bezier_curve_to => |v| {
                            for (v.args) |arg| {
                                const c = utils.toPoint(arg.p1);
                                const p = utils.toPoint(arg.end);
                                try maker.quadratic_bezier_curve_to(c, p, v.relative);
                            }
                        },
                        .curve_to => |v| {
                            for (v.args) |arg| {
                                const c1 = utils.toPoint(arg.p1);
                                const c2 = utils.toPoint(arg.p2);
                                const p = utils.toPoint(arg.end);
                                try maker.curve_to(c1, c2, p, v.relative);
                            }
                        },
                        .smooth_quadratic_bezier_curve_to => |v| {
                            for (v.args) |arg| {
                                const p = utils.toPoint(arg);
                                try maker.smooth_quadratic_bezier_curve_to(p, v.relative);
                            }
                        },
                        .smooth_curve_to => |v| {
                            for (v.args) |arg| {
                                const c2 = utils.toPoint(arg.p2);
                                const p = utils.toPoint(arg.end);
                                try maker.smooth_curve_to(c2, p, v.relative);
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
                                try maker.elliptical_arc(rx, ry, rotation, large_arc, sweep, target, v.relative);
                            }
                        },
                    }
                }
                try maker.flush();
            }
        }
    }
    fn t2p(x: f32, y: f32) Point {
        return Point{ .x = x, .y = y };
    }
    pub fn parse(
        self: *@This(),
        maker: *NodeMaker,
        alloc: Allocator,
        att: []const []const u8,
        val: []const []const u8,
    ) !void {
        const Def = struct {
            pub const fill: ?SvgColor = undefined;
            pub const @"fill-opacity": ?f32 = undefined;
            pub const stroke: ?SvgColor = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, vx| {
            try utils.auto_parse_def(@This(), self, Def, a, vx);
        }
        for (att, val) |a, vx| {
            if (std.mem.eql(u8, a, "d")) {
                const nodes = try @import("svg2.zig").parse_path_data(alloc, vx);

                for (nodes) |cmd| {
                    switch (cmd.node_type) {
                        .move_to, .line_to => {
                            // [x, y]
                            if (cmd.values.len % 2 != 0) return error.InvalidValueCount;
                            for (cmd.values, 0..) |_, i| {
                                if (i % 2 == 0) {
                                    const x = cmd.values[i];
                                    const y = cmd.values[i + 1];
                                    if (i == 0 and cmd.node_type == .move_to) {
                                        try maker.move(t2p(x, y), cmd.rel);
                                    } else {
                                        try maker.line(t2p(x, y), cmd.rel);
                                    }
                                }
                            }
                        },
                        .horizontal_line_to, .vertical_line_to => {
                            // [x] or [y]
                            for (cmd.values) |v| {
                                if (cmd.node_type == .horizontal_line_to) {
                                    try maker.horiz(v, cmd.rel);
                                } else try maker.vert(v, cmd.rel);
                            }
                        },
                        .curve_to => {
                            // [x1 y1 x2 y2 x y] (multiple sets allowed)
                            if (cmd.values.len % 6 != 0) return error.InvalidValueCount;
                            var i: usize = 0;
                            while (i + 5 < cmd.values.len) : (i += 6) {
                                const x1 = cmd.values[i];
                                const y1 = cmd.values[i + 1];
                                const x2 = cmd.values[i + 2];
                                const y2 = cmd.values[i + 3];
                                const x = cmd.values[i + 4];
                                const y = cmd.values[i + 5];
                                try maker.curve_to(
                                    t2p(x1, y1),
                                    t2p(x2, y2),
                                    t2p(x, y),
                                    cmd.rel,
                                );
                            }
                        },
                        .smooth_curve_to => {
                            // [x2 y2 x y]
                            if (cmd.values.len % 4 != 0) return error.InvalidValueCount;
                            var i: usize = 0;
                            while (i + 3 < cmd.values.len) : (i += 4) {
                                const x2 = cmd.values[i];
                                const y2 = cmd.values[i + 1];
                                const x = cmd.values[i + 2];
                                const y = cmd.values[i + 3];
                                try maker.smooth_curve_to(
                                    t2p(x2, y2),
                                    t2p(x, y),
                                    cmd.rel,
                                );
                            }
                        },
                        .quadratic_bezier_curve_to => {
                            // [x1 y1 x y]
                            if (cmd.values.len % 4 != 0) return error.InvalidValueCount;
                            var i: usize = 0;
                            while (i + 3 < cmd.values.len) : (i += 4) {
                                const x1 = cmd.values[i];
                                const y1 = cmd.values[i + 1];
                                const x = cmd.values[i + 2];
                                const y = cmd.values[i + 3];
                                try maker.quadratic_bezier_curve_to(
                                    t2p(x1, y1),
                                    t2p(x, y),
                                    cmd.rel,
                                );
                            }
                        },
                        .smooth_quadratic_bezier_curve_to => {
                            // [x y]
                            if (cmd.values.len % 2 != 0) return error.InvalidValueCount;
                            for (cmd.values, 0..) |_, i| {
                                if (i % 2 == 0) {
                                    const x = cmd.values[i];
                                    const y = cmd.values[i + 1];
                                    try maker.smooth_quadratic_bezier_curve_to(
                                        t2p(x, y),
                                        cmd.rel,
                                    );
                                }
                            }
                        },
                        .elliptical_arc => {
                            // [rx ry x-axis-rotation large-arc-flag sweep-flag x y]
                            if (cmd.values.len % 7 != 0) return error.InvalidValueCount;
                            var i: usize = 0;
                            while (i + 6 < cmd.values.len) : (i += 7) {
                                const rx = cmd.values[i];
                                const ry = cmd.values[i + 1];
                                const rotation = cmd.values[i + 2];
                                const large_arc = cmd.values[i + 3] != 0;
                                const sweep = cmd.values[i + 4] != 0;
                                const x = cmd.values[i + 5];
                                const y = cmd.values[i + 6];
                                try maker.elliptical_arc(rx, ry, rotation, large_arc, sweep, t2p(x, y), cmd.rel);
                            }
                        },
                        .close_path => {
                            try maker.close();
                        },
                    }
                }
            }
        }
        try maker.flush();
    }
};
const G = struct {
    fill: ?SvgColor = null,
    @"fill-opacity": ?f32 = null, // between 0 and 1
    stroke: ?SvgColor = null,
    @"stroke-width": ?f32 = null,
    @"stroke-opacity": ?f32 = null, // between 0 and 1
    pub fn parse(
        self: *@This(),
        att: []const []const u8,
        val: []const []const u8,
    ) !void {
        const Def = struct {
            pub const fill: ?SvgColor = undefined;
            pub const @"fill-opacity": ?f32 = undefined;
            pub const stroke: ?SvgColor = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
        }
    }
};
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

        default_stroke_width: f32 = 2,
        default_color: SvgColor =
            .{
                .col = Color{ .r = 0.0, .g = 0.0, .b = 0.0, .a = 1.0 },
            },

        pub fn init(alloc: Allocator, writer: W) !@This() {
            return @This(){
                .builder = Builder{ .writer = writer },
                .arena1 = std.heap.ArenaAllocator.init(alloc),
                .arena2 = std.heap.ArenaAllocator.init(alloc),
                .colormap = ColMap.init(alloc),
            };
        }
        pub fn deinit(self: *@This()) @This() {
            self.arena1.deinit();
            self.arena2.deinit();
            self.colormap.deinit();
        }
        pub fn write_path(
            self: *@This(),
            pathlist: []const Segment,
            stack: *const Stack(InheritableProperties),
        ) !void {
            const stroke_width = stack.top().?.@"stroke-width" orelse self.default_stroke_width;
            const fill = InheritableProperties.resolve_color_property(stack, "fill");
            const stroke = InheritableProperties.resolve_color_property(stack, "stroke");

            if (fill) |col| {
                const col_idx = self.colormap.get(.fromColor(col)).?;
                try self.builder.writeFillPath(.{ .flat = col_idx }, pathlist);
            }
            if (stroke) |col| {
                for (@constCast(pathlist)) |*seg| {
                    for (@constCast(seg.commands)) |*n| {
                        switch (n.*) {
                            .line => |_| n.line.line_width = stroke_width,
                            .horiz => |_| n.horiz.line_width = stroke_width,
                            .vert => |_| n.vert.line_width = stroke_width,
                            .bezier => |_| n.bezier.line_width = stroke_width,
                            .arc_circle => |_| n.arc_circle.line_width = stroke_width,
                            .arc_ellipse => |_| n.arc_ellipse.line_width = stroke_width,
                            .close => |_| n.close.line_width = stroke_width,
                            .quadratic_bezier => |_| n.quadratic_bezier.line_width = stroke_width,
                        }
                    }
                }
                const col_idx = self.colormap.get(.fromColor(col)).?;
                try self.builder.writeDrawPath(.{ .flat = col_idx }, stroke_width, pathlist);
            }
        }

        pub fn parse_colors_and_svg(self: *@This(), svg_bytes: []const u8) !void {
            const alloc = self.arena1.allocator();
            var fbuffs = std.io.fixedBufferStream(svg_bytes);
            var xml_res = xml.streamingDocument(alloc, fbuffs.reader());
            defer xml_res.deinit();
            var reader = xml_res.reader(alloc, .{});
            defer reader.deinit();

            try self.colormap.put(.fromColor(self.default_color.col), math.cast(u32, self.colortable_len) orelse return error.IndexOOB);
            self.colortable_len += 1;

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
                                v.* = try alloc.dupe(u8, try reader.attributeValue(i));
                            }
                            var svg = Svg{};
                            try svg.parse(alloc, att_names, att_vals);
                            try svg.check();
                            self.svg = svg;
                        }
                        for (0..att_count) |i| {
                            const att_name = reader.attributeNameNs(i).local;
                            const att_val = try reader.attributeValue(i);
                            inline for (ColorProperties) |p| {
                                const c = try utils.parseColor(p, att_name, att_val);
                                if (c) |col| {
                                    const maybe_key = col.get_hash_key();
                                    if (maybe_key) |key| {
                                        if (self.colormap.getKey(key) == null) {
                                            try self.colormap.put(key, math.cast(u32, self.colortable_len) orelse return error.IndexOOB);
                                            self.colortable_len += 1;
                                        }
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
            return @intFromFloat(self.svg.width.?);
        }
        pub fn svg_height(self: *@This()) u32 {
            return @intFromFloat(self.svg.height.?);
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
            if (debug)
                std.log.warn("writing colortable: RGBA", .{});
            for (colors, colors_hash, 0..) |*v, v2, i| {
                const c = v2.toColor();
                v.* = c;
                if (debug)
                    std.log.warn("- {} | [{d:.1} {d:.1} {d:.1} {d:.1}]", .{ i, c.r, c.g, c.b, c.a });
            }
            try self.builder.writeColorTable(colors);

            var stack = try Stack(InheritableProperties).init(gpa, 128);
            defer stack.deinit(gpa);
            try stack.push(InheritableProperties{
                .opacity = 1.0,
                .@"fill-opacity" = 1.0,
                .@"stroke-opacity" = 1.0,
                .@"stroke-width" = self.default_stroke_width,
                .color = self.default_color,
                .fill = self.default_color,
                .stroke = self.default_color,
            });

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
                                try stack.push(current_properties);
                                const top_mut = stack.top_mut().?;
                                top_mut.override_from(self.svg);
                                continue;
                            }
                        } else {
                            try stack.push(current_properties);
                            const top_mut = stack.top_mut().?;
                            const garbage_alloc = self.arena2.allocator();
                            var maker = NodeMaker.init(garbage_alloc, self.svg);

                            // std.log.warn(
                            //     \\element_start:  {}
                            // , .{
                            //     std.zig.fmtEscapes(element_name.local),
                            // });

                            const att_count = reader.reader.attributeCount();
                            const att_names = try self.arena2.allocator().alloc([]const u8, att_count);
                            const att_vals = try self.arena2.allocator().alloc([]const u8, att_count);

                            for (att_names, att_vals, 0..) |*n, *v, i| {
                                n.* = try garbage_alloc.dupe(u8, reader.attributeNameNs(i).local);
                                v.* = try garbage_alloc.dupe(u8, try reader.attributeValue(i));
                            }
                            // Container
                            if (std.mem.eql(u8, "g", element_tag)) {
                                var element = G{};
                                try element.parse(att_names, att_vals);
                                top_mut.override_from(element);
                                continue;
                            } else
                            // Drawing Primitives
                            if (std.mem.eql(u8, "rect", element_tag)) {
                                var element = Rect{};
                                try element.parse(&maker, att_names, att_vals);
                                top_mut.override_from(element);
                                if (try maker.segments()) |segs| {
                                    try self.write_path(segs, &stack);
                                }
                            } else if (std.mem.eql(u8, "circle", element_tag)) {
                                var element = Circle{};
                                try element.parse(&maker, att_names, att_vals);
                                top_mut.override_from(element);
                                if (try maker.segments()) |segs| {
                                    try self.write_path(segs, &stack);
                                }
                            } else if (std.mem.eql(u8, "ellipse", element_tag)) {
                                var element = Ellipse{};
                                try element.parse(&maker, att_names, att_vals);
                                top_mut.override_from(element);
                                if (try maker.segments()) |segs| {
                                    try self.write_path(segs, &stack);
                                }
                            } else if (std.mem.eql(u8, "line", element_tag)) {
                                var element = Line{};
                                try element.parse(&maker, att_names, att_vals);
                                top_mut.override_from(element);
                                if (try maker.segments()) |segs| {
                                    try self.write_path(segs, &stack);
                                }
                            } else if (std.mem.eql(u8, "polyline", element_tag)) {
                                var element = PolyLine{};
                                try element.parse(&maker, garbage_alloc, att_names, att_vals);
                                top_mut.override_from(element);
                                if (try maker.segments()) |segs| {
                                    try self.write_path(segs, &stack);
                                }
                            } else if (std.mem.eql(u8, "polygon", element_tag)) {
                                var element = Polygon{};
                                try element.parse(&maker, garbage_alloc, att_names, att_vals);
                                top_mut.override_from(element);
                                if (try maker.segments()) |segs| {
                                    try self.write_path(segs, &stack);
                                }
                            } else if (std.mem.eql(u8, "path", element_tag)) {
                                var element = SvgPath{};
                                try element.parse(&maker, garbage_alloc, att_names, att_vals);
                                top_mut.override_from(element);

                                if (try maker.segments()) |segs| {
                                    try self.write_path(segs, &stack);
                                    try self.write_path(segs, &stack);
                                    if (debug) {
                                        std.log.warn("PRINT SEG BEGIN", .{});
                                        for (segs) |sg| {
                                            ut.print_point("start", sg.start);
                                            for (sg.commands) |sn| {
                                                ut.print_node(sn);
                                            }
                                        }
                                        std.log.warn("PRINT SEG END", .{});
                                    }
                                } else std.log.warn("no segments", .{});
                            } else {
                                std.log.warn("unrecognized element: {s}", .{element_tag});
                            }
                            // dispose of the garbage
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
    var con = try SvgConverter(@TypeOf(writer)).init(alloc, writer);
    try con.run(svg_bytes);
}

pub const make_node_debug = true and debug;
pub const make_node_debug2 = false and debug;
const debug = false;

test "coverage" {
    _ = .{ tvg_from_svg, SvgConverter, renderStream };
    std.log.warn("ok", .{});
}
