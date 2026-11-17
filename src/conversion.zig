const std = @import("std");
const assert = std.debug.assert;
const expect = std.debug.expect;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const math = std.math;
const ut = @import("util.zig");
const utils = ut.utils;
const ColorHash = ut.ColorHash;
const Stack = ut.Stack;
const NodeMaker = ut.NodeMaker;
const svg_ut = @import("svg-util.zig");
const arrangement = @import("tessellation").arrangement;
const SvgColor = svg_ut.SvgColor;
const SvgColorAttribute = svg_ut.SvgColorAttribute;

const xml = @import("xml");

pub const tvg = @import("tessellation").tvg;

pub const Color = tvg.Color;
const Path = tvg.Path;
const Segment = Path.Segment;
const Node = Path.Node;
const NodeData = Path.Node.NodeData;
const Point = tvg.Point;
const Scale = tvg.Scale;
const Range = tvg.Range;
const WrittenUnits = tvg.builder.WrittenUnits;
const Style = tvg.Style;

const WidthHeight = struct {
    w: ?f32 = null,
    h: ?f32 = null,
};
const ViewBox = struct {
    x: f32 = 0,
    y: f32 = 0,
    w: ?f32 = null,
    h: ?f32 = null,
    /// preserveAspectRatio="xMidYMid meet", the SVG default: one scale for both axes, centered.
    fn scale(self: *const @This(), width: f32, height: f32) f32 {
        assert(width != 0);
        assert(height != 0);
        assert(self.w.? != 0);
        assert(self.h.? != 0);
        return @min(width / self.w.?, height / self.h.?);
    }
    fn transform(
        self: *const @This(),
        width: f32,
        height: f32,
        point: Point,
    ) Point {
        const s = self.scale(width, height);
        return Point{
            .x = (point.x - self.x) * s + (width - self.w.? * s) / 2,
            .y = (point.y - self.y) * s + (height - self.h.? * s) / 2,
        };
    }
};

/// Keywords as written in SVG.
const FillRule = enum { nonzero, evenodd };

const InheritableProperties = struct {
    fill: ?SvgColor = .{ .col = .{
        .r = 0,
        .g = 0,
        .b = 0,
        .a = 1,
    } }, // Fill color
    stroke: ?SvgColor = null, // Stroke (outline) color
    color: ?SvgColor = null, // Used as base color (currentColor, filters, etc.)
    @"fill-opacity": ?f32 = null, // Opacity of the fill
    @"fill-rule": ?FillRule = null,
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
        inline for (@typeInfo(InheritableProperties).@"struct".field_names) |f_name| {
            if (@field(over, f_name)) |fval| {
                @field(ret, f_name) = fval;
            }
        }
        return ret;
    }
    pub fn override_from(self: *@This(), t: anytype) void {
        const T = @TypeOf(t);
        const info = @typeInfo(InheritableProperties).@"struct";
        inline for (info.field_names, info.field_types) |f_name, f_type| {
            if (comptime utils.has_field(T, f_name)) {
                // @compileLog(T, f.name);
                if (comptime f_type == @TypeOf(@field(t, f_name))) {
                    if (@field(t, f_name)) |fval| {
                        @field(self, f_name) = fval;
                    }
                }
            }
        }
    }
    pub fn resolve_color_property(stack: *const Stack(InheritableProperties), comptime name: []const u8) !?Color {
        var it = stack.rev_iter();
        while (it.next()) |top| {
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
        @panic("bottom of stack should have color");
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
    @"fill-rule": ?FillRule = null,
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
            pub const @"fill-rule": ?FillRule = undefined;
            pub const stroke: ?SvgColor = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
            if (try utils.parsePointList(alloc, "viewBox", a, v)) |res| {
                if (res.items.len != 2) return error.ViewBoxMisformed;
                self.viewBox.x = res.items[0].x;
                self.viewBox.y = res.items[0].y;
                self.viewBox.w = res.items[1].x;
                self.viewBox.h = res.items[1].y;
            }
        }
    }
    pub fn check(self: *@This()) !void {
        if (self.width == null) self.width = self.viewBox.w;
        if (self.height == null) self.height = self.viewBox.h;
        if (self.viewBox.h == null or self.viewBox.w == null) {
            self.viewBox.w = self.width;
            self.viewBox.h = self.height;
        }
        if (self.width == null or self.height == null) return error.NoWidthHeightDefined;
        for ([_]?f32{ self.width, self.height, self.viewBox.w, self.viewBox.h }) |size| {
            if (!(size.? > 0 and size.? < 0x1p32)) return error.InvalidSize;
        }
    }
    pub fn transform(self: *const @This(), pp: Point) Point {
        const p = Point{
            .x = @floatCast(pp.x),
            .y = @floatCast(pp.y),
        };
        return self.viewBox.transform(self.width.?, self.height.?, p);
    }
    pub fn lengthScale(self: *const @This()) f32 {
        return self.viewBox.scale(self.width.?, self.height.?);
    }
    // pub fn point_from(self: *const @This(), coord: svg_parsing.CoordinatePair) Point {
    //     const p = Point{
    //         .x = @floatCast(coord.coordinates[0].number.value),
    //         .y = @floatCast(coord.coordinates[1].number.value),
    //     };
    //     return self.viewBox.transform(@floatFromInt(self.width.?), @floatFromInt(self.height.?), p);
    // }
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
    rx: ?f32 = null, //The x radius of the corners of the rectangle (used to round the corners). Default is ry
    ry: ?f32 = null, //The y radius of the corners of the rectangle (used to round the corners). Default is rx
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
            pub const rx: ?f32 = undefined;
            pub const ry: ?f32 = undefined;
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
        const width = self.width.?;
        const height = self.height.?;
        if (!(width > 0 and height > 0)) return;
        var rx = std.math.clamp(self.rx orelse self.ry orelse 0, 0, width / 2);
        var ry = std.math.clamp(self.ry orelse self.rx orelse 0, 0, height / 2);
        const rounded = rx > 0 and ry > 0;
        if (!rounded) {
            rx = 0;
            ry = 0;
        }
        const left = self.x;
        const top = self.y;
        const right = self.x + width;
        const bottom = self.y + height;
        try maker.move(.{ .x = left + rx, .y = top }, false);
        try maker.line(.{ .x = right - rx, .y = top }, false);
        if (rounded) try maker.elliptical_arc(rx, ry, 0, false, true, .{ .x = right, .y = top + ry }, false);
        try maker.line(.{ .x = right, .y = bottom - ry }, false);
        if (rounded) try maker.elliptical_arc(rx, ry, 0, false, true, .{ .x = right - rx, .y = bottom }, false);
        try maker.line(.{ .x = left + rx, .y = bottom }, false);
        if (rounded) {
            try maker.elliptical_arc(rx, ry, 0, false, true, .{ .x = left, .y = bottom - ry }, false);
            try maker.line(.{ .x = left, .y = top + ry }, false);
            try maker.elliptical_arc(rx, ry, 0, false, true, .{ .x = left + rx, .y = top }, false);
        }
        try maker.close();
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
        if (!(r > 0)) return;

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
        if (!(rrx > 0 and rry > 0)) return;

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
    @"fill-rule": ?FillRule = null,
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
            pub const @"fill-rule": ?FillRule = undefined;
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
    @"fill-rule": ?FillRule = null,
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
            pub const @"fill-rule": ?FillRule = undefined;
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
        if (self.points.len == 0) return;
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
    @"fill-rule": ?FillRule = null,
    stroke: ?SvgColor = null,
    @"stroke-width": ?f32 = null,
    @"stroke-opacity": ?f32 = null, // between 0 and 1
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
            pub const @"fill-rule": ?FillRule = undefined;
            pub const stroke: ?SvgColor = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, vx| {
            try utils.auto_parse_def(@This(), self, Def, a, vx);
        }
        for (att, val) |a, vx| {
            if (std.mem.eql(u8, a, "d")) {
                const nodes = try svg_ut.parse_path_data(alloc, vx);

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
    @"fill-rule": ?FillRule = null,
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
            pub const @"fill-rule": ?FillRule = undefined;
            pub const stroke: ?SvgColor = undefined;
            pub const @"stroke-width": ?f32 = undefined;
            pub const @"stroke-opacity": ?f32 = undefined;
        };
        for (att, val) |a, v| {
            try utils.auto_parse_def(@This(), self, Def, a, v);
        }
    }
};

svg: Svg = Svg{},
xml_max_nesting: usize = 256,
default_stroke_width: f32 = 2,
default_color: SvgColor = .{ .col = Color{
    .r = 0.0,
    .g = 0.0,
    .b = 0.0,
    .a = 1.0,
} },
scale: Scale = .@"1/4096",

const DrawCommand = tvg.parsing.DrawCommand;

/// Kept until the whole SVG is read, because the TVG header and color table come first.
const Recorder = struct {
    allocator: Allocator,
    commands: std.ArrayList(DrawCommand) = .empty,
    colors: std.array_hash_map.Auto(ColorHash, void) = .empty,
    arrangement_work: arrangement.Work = .init(.{}),

    fn colorStyle(self: *Recorder, color: Color) !Style {
        const entry = try self.colors.getOrPut(self.allocator, .fromColor(color));
        return .{ .flat = math.cast(u32, entry.index) orelse return error.ColorOOB };
    }

    fn dupeSegments(self: *Recorder, segments: []const Segment) ![]Segment {
        const copies = try self.allocator.dupe(Segment, segments);
        for (copies) |*copy| copy.commands = try self.allocator.dupe(Node, copy.commands);
        return copies;
    }
};

const Bounds = struct {
    min: Point,
    max: Point,

    fn around(point: Point, reach: f32) Bounds {
        return .{ .min = .{ .x = point.x - reach, .y = point.y - reach }, .max = .{ .x = point.x + reach, .y = point.y + reach } };
    }

    fn unite(a: Bounds, b: Bounds) Bounds {
        return .{ .min = .{ .x = @min(a.min.x, b.min.x), .y = @min(a.min.y, b.min.y) }, .max = .{ .x = @max(a.max.x, b.max.x), .y = @max(a.max.y, b.max.y) } };
    }

    /// A pixel between both keeps their anti-aliasing ramps apart too.
    fn separatedFrom(a: Bounds, b: Bounds) bool {
        return a.max.x + 1 <= b.min.x or b.max.x + 1 <= a.min.x or a.max.y + 1 <= b.min.y or b.max.y + 1 <= a.min.y;
    }
};

/// Box around everything the segments cover: Bézier control hulls, a circle arc within twice its
/// radius of its start, the radius grows to half the chord when too small. Null with elliptic arcs.
fn segmentsBounds(segments: []const Segment) ?Bounds {
    assert(segments.len > 0);
    var bounds: Bounds = .around(segments[0].start, 0);
    for (segments) |segment| {
        var current = segment.start;
        bounds = bounds.unite(.around(current, 0));
        for (segment.commands) |node| {
            switch (node) {
                .line => |data| current = data.data,
                .horiz => |data| current.x = data.data,
                .vert => |data| current.y = data.data,
                .bezier => |data| {
                    bounds = bounds.unite(.around(data.data.c0, 0)).unite(.around(data.data.c1, 0));
                    current = data.data.p1;
                },
                .quadratic_bezier => |data| {
                    bounds = bounds.unite(.around(data.data.c, 0));
                    current = data.data.p1;
                },
                .arc_circle => |data| {
                    const chord = @sqrt((data.data.target.x - current.x) * (data.data.target.x - current.x) + (data.data.target.y - current.y) * (data.data.target.y - current.y));
                    bounds = bounds.unite(.around(current, @max(2 * data.data.radius, chord)));
                    current = data.data.target;
                },
                .arc_ellipse => return null,
                .close => current = segment.start,
            }
            bounds = bounds.unite(.around(current, 0));
        }
    }
    return bounds;
}

/// outline_fill_path stores its segment count in 6 bits.
const outline_fill_max_segments = 64;

pub fn write_path(
    self: *const @This(),
    arena_alloc: Allocator,
    recorder: *Recorder,
    pathlist: []Segment,
    stack: *const Stack(InheritableProperties),
) !void {
    const stroke_width = (stack.top().?.@"stroke-width" orelse self.default_stroke_width) * self.svg.lengthScale();
    const fill = try InheritableProperties.resolve_color_property(stack, "fill");
    const stroke = try InheritableProperties.resolve_color_property(stack, "stroke");
    var fill_segments: []const Segment = &.{};
    if (fill != null) {
        fill_segments = switch (stack.top().?.@"fill-rule" orelse .nonzero) {
            .evenodd => pathlist,
            .nonzero => try self.even_odd_segments_for_nonzero(arena_alloc, pathlist, &recorder.arrangement_work),
        };
    }
    const fill_style: ?Style = if (fill != null and fill_segments.len > 0) try recorder.colorStyle(fill.?) else null;
    const line_style: ?Style = if (stroke) |col| try recorder.colorStyle(col) else null;
    const same_geometry = fill_segments.ptr == pathlist.ptr and fill_segments.len == pathlist.len;
    if (fill_style != null and line_style != null and same_geometry and pathlist.len <= outline_fill_max_segments) {
        try recorder.commands.append(recorder.allocator, .{ .outline_fill_path = .{
            .fill_style = fill_style.?,
            .line_style = line_style.?,
            .line_width = stroke_width,
            .path = .{ .segments = try recorder.dupeSegments(pathlist) },
        } });
        return;
    }
    if (fill_style) |style| {
        if (debug_write_path) std.log.warn("writeFillPath: {any}", .{fill_segments});
        try recorder.commands.append(recorder.allocator, .{ .fill_path = .{ .style = style, .path = .{ .segments = try recorder.dupeSegments(fill_segments) } } });
    }
    if (line_style) |style| {
        if (debug_write_path) std.log.warn("writeDrawPath: {any}", .{pathlist});
        try recorder.commands.append(recorder.allocator, .{ .draw_line_path = .{ .style = style, .line_width = stroke_width, .path = .{ .segments = try recorder.dupeSegments(pathlist) } } });
    }
}

/// Units per pixel: the largest power of two whose unit is at least 1/4096 of the larger image
/// side, so the precision relative to the image is the same at every image size.
fn scaleForImage(width: f32, height: f32) Scale {
    const larger_side = @max(width, height);
    assert(std.math.isFinite(larger_side) and larger_side > 0);
    const unit_exponent = std.math.frexp(larger_side / 4096).exponent - 1;
    return @fromBackingInt(@intCast(std.math.clamp(-unit_exponent, 0, 15)));
}

const Encoding = struct { range: Range, output_shift: u4 = 0 };

fn encode(gpa: Allocator, width: u32, height: u32, scale: Scale, encoding: Encoding, tolerance: f32, colors: []const Color, commands: []const DrawCommand, written_units: ?*WrittenUnits) ![]const u8 {
    var writer: std.Io.Writer.Allocating = .init(gpa);
    defer writer.deinit();
    var arcs = std.heap.ArenaAllocator.init(gpa);
    defer arcs.deinit();
    var builder = tvg.builder.create(&writer.writer);
    builder.output_shift = encoding.output_shift;
    try builder.writeHeader(width, height, scale, .u8888, encoding.range);
    try builder.writeColorTable(colors);
    var start: usize = 0;
    while (start < commands.len) {
        const end = try mergedEnd(scale, colors, commands, start);
        var segments: std.ArrayList(Segment) = .empty;
        for (commands[start..end]) |command| {
            const path = switch (command) {
                inline .fill_path, .draw_line_path, .outline_fill_path => |data| data.path,
                else => unreachable,
            };
            try segments.appendSlice(arcs.allocator(), try arcsFromBeziers(arcs.allocator(), path.segments, scale, tolerance));
        }
        switch (commands[start]) {
            .fill_path => |data| try builder.writeFillPath(data.style, segments.items),
            .draw_line_path => |data| try builder.writeDrawPath(data.style, data.line_width, segments.items),
            .outline_fill_path => |data| try builder.writeOutlineFillPath(data.fill_style, data.line_style, data.line_width, segments.items),
            else => unreachable,
        }
        start = end;
    }
    try builder.writeEndOfFile();
    if (written_units) |units| units.* = builder.written_units;
    return writer.toOwnedSlice();
}

/// End of the run of commands from `start` that one command draws the same: opaque strokes of
/// the same style and width in units, painting an overlap twice gives the same color, and fills of
/// the same style whose boxes stay apart, so the even-odd rule covers no area twice.
fn mergedEnd(scale: Scale, colors: []const Color, commands: []const DrawCommand, start: usize) !usize {
    var end = start + 1;
    switch (commands[start]) {
        .draw_line_path => |first| {
            const color_index = switch (first.style) {
                .flat => |index| index,
                else => return end,
            };
            if (colors[color_index].a != 1) return end;
            const width_units = (try scale.map(first.line_width)).raw();
            while (end < commands.len) : (end += 1) {
                const next = switch (commands[end]) {
                    .draw_line_path => |data| data,
                    else => break,
                };
                if (!std.meta.eql(next.style, first.style) or (try scale.map(next.line_width)).raw() != width_units) break;
            }
        },
        .fill_path => |first| {
            var bounds = segmentsBounds(first.path.segments) orelse return end;
            while (end < commands.len) : (end += 1) {
                const next = switch (commands[end]) {
                    .fill_path => |data| data,
                    else => break,
                };
                if (!std.meta.eql(next.style, first.style)) break;
                const next_bounds = segmentsBounds(next.path.segments) orelse break;
                if (!bounds.separatedFrom(next_bounds)) break;
                bounds = bounds.unite(next_bounds);
            }
        },
        else => {},
    }
    return end;
}

/// The arrangement and the arc detection stay this many pixels close to the SVG geometry.
/// A fixed fraction of the image looks alike at every display size. 1/4096 gives a circle
/// spanning the image about 100 segments, as many as the reference renderer draws.
fn toleranceForImage(width: f32, height: f32) f32 {
    return @max(width, height) / 4096;
}

const UnitVector = [2]f64;

fn unitVector(scale: Scale, point: Point) !UnitVector {
    return .{ @floatFromInt((try scale.map(point.x)).raw()), @floatFromInt((try scale.map(point.y)).raw()) };
}

fn cross(a: UnitVector, b: UnitVector) f64 {
    return a[0] * b[1] - a[1] * b[0];
}

fn difference(a: UnitVector, b: UnitVector) UnitVector {
    return .{ a[0] - b[0], a[1] - b[1] };
}

/// Larger radii stay Béziers, the arc radius has to fit the 16 bit range like the coordinates.
const arc_max_radius_units = math.maxInt(i16);
/// Bounds the quadratic cost of growing a run of Béziers into one arc.
const arc_max_beziers = 16;
const arc_samples_per_bezier = 32;

/// Every cubic Bézier, or run of them, that stays within `tolerance` pixels of a circle arc of
/// less than a full turn becomes one `arc_circle`. Checked after rounding to the units of
/// `scale`, against the arc rendering.circleArc draws.
pub fn arcsFromBeziers(arena: Allocator, segments: []const Segment, scale: Scale, tolerance: f32) ![]const Segment {
    assert(tolerance > 0);
    const result = try arena.alloc(Segment, segments.len);
    for (result, segments) |*out, segment| {
        var nodes: std.ArrayList(Node) = .empty;
        const start = try unitVector(scale, segment.start);
        var current = start;
        var index: usize = 0;
        while (index < segment.commands.len) {
            var arc: ?Node = null;
            var bezier_count: usize = 0;
            if (segment.commands[index] == .bezier) {
                const fit = try circleThroughBezier(scale, current, segment.commands[index].bezier.data);
                if (fit) |circle| {
                    const run_end = @min(segment.commands.len, index + arc_max_beziers);
                    for (segment.commands[index..run_end], 1..) |node, count| {
                        if (node != .bezier or (count > 1 and node.bezier.line_width != null)) break;
                        arc = try arcAlongBeziers(scale, tolerance, current, circle, segment.commands[index..][0..count]) orelse break;
                        bezier_count = count;
                    }
                }
            }
            const node = if (arc) |found| found else segment.commands[index];
            index += @max(1, bezier_count);
            try nodes.append(arena, node);
            current = switch (node) {
                .line => |data| try unitVector(scale, data.data),
                .horiz => |data| .{ @floatFromInt((try scale.map(data.data)).raw()), current[1] },
                .vert => |data| .{ current[0], @floatFromInt((try scale.map(data.data)).raw()) },
                .bezier => |data| try unitVector(scale, data.data.p1),
                .quadratic_bezier => |data| try unitVector(scale, data.data.p1),
                .arc_circle => |data| try unitVector(scale, data.data.target),
                .arc_ellipse => |data| try unitVector(scale, data.data.target),
                .close => start,
            };
        }
        out.* = .{ .start = segment.start, .commands = nodes.items };
    }
    return result;
}

const FittedCircle = struct { radius: f64, sweep: bool };

/// The circle through both ends of the Bézier that is tangent to it at the start, in units.
fn circleThroughBezier(scale: Scale, p0: UnitVector, bezier: Node.Bezier) !?FittedCircle {
    const c0 = try unitVector(scale, bezier.c0);
    const p1 = try unitVector(scale, bezier.p1);
    var tangent = difference(c0, p0);
    if (tangent[0] == 0 and tangent[1] == 0) tangent = difference(try unitVector(scale, bezier.c1), p0);
    const chord = difference(p1, p0);
    const normal: UnitVector = .{ -tangent[1], tangent[0] };
    const normal_along_chord = 2 * (normal[0] * chord[0] + normal[1] * chord[1]);
    if (normal_along_chord == 0) return null;
    const center_distance = (chord[0] * chord[0] + chord[1] * chord[1]) / normal_along_chord;
    const radius = @abs(center_distance) * @sqrt(normal[0] * normal[0] + normal[1] * normal[1]);
    if (!(radius >= 1 and radius <= arc_max_radius_units)) return null;
    // A center to the right of the tangent turns the arc counterclockwise on screen, rendering's turn_left.
    return .{ .radius = radius, .sweep = center_distance < 0 };
}

/// The arc from `p0` to the end of `beziers` on `circle`, if every sample of the Béziers lies
/// within `tolerance` pixels of the arc renderCircle draws and moves along it in its direction.
fn arcAlongBeziers(scale: Scale, tolerance: f32, p0: UnitVector, circle: FittedCircle, beziers: []const Node) !?Node {
    assert(beziers.len > 0);
    const factor: f64 = @floatFromInt(scale.getScaleFactor());
    const tolerance_units = tolerance * factor;
    const target = beziers[beziers.len - 1].bezier.data.p1;
    const p1 = try unitVector(scale, target);
    if (p1[0] == p0[0] and p1[1] == p0[1]) return null;
    const radius_units = @round(circle.radius);
    assert(radius_units >= 1 and radius_units <= arc_max_radius_units);
    const p0_pixel: Point = .{ .x = @floatCast(p0[0] / factor), .y = @floatCast(p0[1] / factor) };
    const p1_pixel: Point = .{ .x = @floatCast(p1[0] / factor), .y = @floatCast(p1[1] / factor) };
    if (p0_pixel.x == p1_pixel.x and p0_pixel.y == p1_pixel.y) return null;
    for ([_]bool{ false, true }) |large_arc| {
        const drawn = tvg.rendering.circleArc(p0_pixel, p1_pixel, @floatCast(radius_units / factor), large_arc, circle.sweep);
        const center: UnitVector = .{ drawn.center.x * factor, drawn.center.y * factor };
        var previous = difference(p0, center);
        const drawn_radius = @sqrt(previous[0] * previous[0] + previous[1] * previous[1]);
        const direction: f64 = if (circle.sweep) -1 else 1;
        var turned: f64 = 0;
        var start = p0;
        const on_arc = samples: for (beziers) |node| {
            const controls = [4]UnitVector{ start, try unitVector(scale, node.bezier.data.c0), try unitVector(scale, node.bezier.data.c1), try unitVector(scale, node.bezier.data.p1) };
            for (1..arc_samples_per_bezier + 1) |sample| {
                const radial = difference(cubicAt(controls, @as(f64, @floatFromInt(sample)) / arc_samples_per_bezier), center);
                const step = direction * math.atan2(cross(previous, radial), previous[0] * radial[0] + previous[1] * radial[1]);
                const distance = @sqrt(radial[0] * radial[0] + radial[1] * radial[1]);
                if (step < 0 or @abs(distance - drawn_radius) > tolerance_units) break :samples false;
                turned += step;
                previous = radial;
            }
            start = controls[3];
        } else true;
        if (on_arc and drawn_radius * @abs(turned - drawn.arc) <= tolerance_units) return .{ .arc_circle = .init(beziers[0].bezier.line_width, .{
            .radius = @floatCast(radius_units / factor),
            .large_arc = large_arc,
            .sweep = circle.sweep,
            .target = target,
        }) };
    }
    return null;
}

fn cubicAt(controls: [4]UnitVector, t: f64) UnitVector {
    const u = 1 - t;
    var result: UnitVector = undefined;
    for (&result, 0..) |*coordinate, axis| {
        coordinate.* = u * u * u * controls[0][axis] + 3 * u * u * t * controls[1][axis] + 3 * u * t * t * controls[2][axis] + t * t * t * controls[3][axis];
    }
    return result;
}

/// TinyVG fills every path even-odd. Returns segments whose even-odd fill is the nonzero fill of
/// `pathlist`: the path itself if both rules agree, without the contours between two filled
/// regions if no edges touch, otherwise the flattened boundary of the filled regions.
fn even_odd_segments_for_nonzero(self: *const @This(), arena_alloc: Allocator, pathlist: []Segment, work: *arrangement.Work) ![]const Segment {
    const tolerance = toleranceForImage(self.svg.width.?, self.svg.height.?);
    const limits: arrangement.Limits = .{};
    const contours = try arrangement.flattenSegments(arena_alloc, pathlist, self.scale, tolerance, limits.max_edges);
    assert(contours.count() == pathlist.len);
    const regions = try arrangement.Arrangement.init(arena_alloc, contours, limits, work);
    if (regions.fillRulesAgree()) return pathlist;

    if (regions.input_edges_unchanged) {
        const ContourRole = enum { empty, boundary, inner };
        const roles = try arena_alloc.alloc(ContourRole, pathlist.len);
        @memset(roles, .empty);
        const consistent = for (regions.edges) |edge| {
            const role: ContourRole = if (edge.separates(.nonzero)) .boundary else .inner;
            if (roles[edge.contour] == .empty) roles[edge.contour] = role;
            if (roles[edge.contour] != role) break false;
        } else true;
        if (consistent) {
            var kept: std.ArrayList(Segment) = .empty;
            for (pathlist, roles) |segment, role| {
                if (role == .boundary) try kept.append(arena_alloc, segment);
            }
            return kept.items;
        }
    }

    const boundary = try regions.boundary(arena_alloc, .nonzero);
    const grid_per_pixel: f32 = @floatFromInt(self.scale.getScaleFactor());
    const segments = try arena_alloc.alloc(Segment, boundary.count());
    for (segments, 0..) |*segment, contour_index| {
        const points = boundary.get(contour_index);
        const nodes = try arena_alloc.alloc(Node, points.len - 1);
        for (nodes, points[1..]) |*node, point| {
            node.* = .{ .line = .init(null, .{ .x = @as(f32, @floatFromInt(point[0])) / grid_per_pixel, .y = @as(f32, @floatFromInt(point[1])) / grid_per_pixel }) };
        }
        segment.* = .{ .start = .{ .x = @as(f32, @floatFromInt(points[0][0])) / grid_per_pixel, .y = @as(f32, @floatFromInt(points[0][1])) / grid_per_pixel }, .commands = nodes };
    }
    return segments;
}

pub fn parse_svg(gpa: Allocator, svg_bytes: []const u8) !Svg {
    // xml.Reader.Streaming.init asserts a reader buffer of at least 2 bytes.
    if (svg_bytes.len < 2) return error.InvalidSvg;
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const alloc = arena.allocator();

    var in: std.Io.Reader = .fixed(svg_bytes);
    var readerImpl: xml.Reader.Streaming = .init(alloc, &in, .{});
    defer readerImpl.deinit();
    var reader = &readerImpl.interface;

    var svg = Svg{};

    while (true) {
        const xml_node = reader.read() catch |err| switch (err) {
            error.MalformedXml => {
                const loc = reader.errorLocation();
                std.log.err("{}:{}: {}", .{ loc.line, loc.column, reader.errorCode() });
                return error.MalformedXml;
            },
            else => |other| return other,
        };
        switch (xml_node) {
            .element_start => {
                const element_name = reader.elementNameNs();
                const element_tag = element_name.local;
                const att_count = reader.attributeCount();

                if (std.mem.eql(u8, "svg", element_tag)) {
                    const att_names = try alloc.alloc([]const u8, att_count);
                    const att_vals = try alloc.alloc([]const u8, att_count);
                    for (att_names, att_vals, 0..) |*n, *v, i| {
                        n.* = try alloc.dupe(u8, reader.attributeNameNs(i).local);
                        v.* = try alloc.dupe(u8, try reader.attributeValue(i));
                    }
                    try svg.parse(alloc, att_names, att_vals);
                }
            },
            else => {},
            .eof => break,
        }
    }
    try svg.check();
    return svg;
}

pub fn tvg_from_svg(gpa: Allocator, svg_bytes: []const u8, opts: @This()) ![]const u8 {
    var popts = opts;
    const svg = try parse_svg(gpa, svg_bytes);
    popts.svg = svg;
    popts.scale = scaleForImage(svg.width.?, svg.height.?);
    var recorded = std.heap.ArenaAllocator.init(gpa);
    defer recorded.deinit();
    var recorder: Recorder = .{ .allocator = recorded.allocator() };

    assert(svg_bytes.len >= 2);
    var in: std.Io.Reader = .fixed(svg_bytes);
    var readerImpl: xml.Reader.Streaming = .init(gpa, &in, .{});
    defer readerImpl.deinit();
    var reader = &readerImpl.interface;

    const sw: u32 = @intFromFloat(@max(1, @round(svg.width.?)));
    const sh: u32 = @intFromFloat(@max(1, @round(svg.height.?)));
    var stack = try Stack(InheritableProperties).init(gpa, popts.xml_max_nesting, InheritableProperties{});
    defer stack.deinit(gpa);

    // the fallback values
    try stack.push(InheritableProperties{
        .opacity = 1.0,
        .@"fill-opacity" = 1.0,
        .@"stroke-opacity" = 1.0,
        .@"stroke-width" = popts.default_stroke_width,
        .color = popts.default_color,
        .fill = popts.default_color,
        .stroke = popts.default_color,
    });

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
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
                const garbage_alloc = arena.allocator();
                const element_name = reader.elementNameNs();
                const element_tag = element_name.local;
                if (!found_svg_tag) {
                    if (std.mem.eql(u8, "svg", element_tag)) {
                        found_svg_tag = true;
                        const initial_properties = InheritableProperties{};
                        var props = initial_properties;
                        props.override_from(svg);
                        // The fallback level below only serves currentColor, root `inherit` takes the initial value.
                        if (std.meta.eql(props.fill, @as(?SvgColor, .inherit))) props.fill = initial_properties.fill;
                        if (std.meta.eql(props.stroke, @as(?SvgColor, .inherit))) props.stroke = initial_properties.stroke;
                        try stack.push(props);
                    }
                } else {
                    const current_properties = stack.top() orelse InheritableProperties{};
                    try stack.push(current_properties);
                    const top_mut = stack.top_mut().?;
                    var maker = NodeMaker.init(garbage_alloc, svg);

                    const att_count = reader.attributeCount();
                    const att_names = try garbage_alloc.alloc([]const u8, att_count);
                    const att_vals = try garbage_alloc.alloc([]const u8, att_count);

                    for (att_names, att_vals, 0..) |*n, *v, i| {
                        n.* = try garbage_alloc.dupe(u8, reader.attributeNameNs(i).local);
                        v.* = try garbage_alloc.dupe(u8, try reader.attributeValue(i));
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
                        try element.parse(&maker, att_names, att_vals);
                        top_mut.override_from(element);
                        if (try maker.segments()) |segs| {
                            try write_path(&popts, garbage_alloc, &recorder, segs, &stack);
                        }
                    } else if (std.mem.eql(u8, "circle", element_tag)) {
                        var element = Circle{};
                        try element.parse(&maker, att_names, att_vals);
                        top_mut.override_from(element);
                        if (try maker.segments()) |segs| {
                            try write_path(&popts, garbage_alloc, &recorder, segs, &stack);
                        }
                    } else if (std.mem.eql(u8, "ellipse", element_tag)) {
                        var element = Ellipse{};
                        try element.parse(&maker, att_names, att_vals);
                        top_mut.override_from(element);
                        if (try maker.segments()) |segs| {
                            try write_path(&popts, garbage_alloc, &recorder, segs, &stack);
                        }
                    } else if (std.mem.eql(u8, "line", element_tag)) {
                        var element = Line{};
                        try element.parse(&maker, att_names, att_vals);
                        top_mut.override_from(element);
                        top_mut.fill = .none;
                        if (try maker.segments()) |segs| {
                            try write_path(&popts, garbage_alloc, &recorder, segs, &stack);
                        }
                    } else if (std.mem.eql(u8, "polyline", element_tag)) {
                        var element = PolyLine{};
                        try element.parse(&maker, garbage_alloc, att_names, att_vals);
                        top_mut.override_from(element);
                        if (try maker.segments()) |segs| {
                            try write_path(&popts, garbage_alloc, &recorder, segs, &stack);
                        }
                    } else if (std.mem.eql(u8, "polygon", element_tag)) {
                        var element = Polygon{};
                        try element.parse(&maker, garbage_alloc, att_names, att_vals);
                        top_mut.override_from(element);
                        if (try maker.segments()) |segs| {
                            try write_path(&popts, garbage_alloc, &recorder, segs, &stack);
                        }
                    } else if (std.mem.eql(u8, "path", element_tag)) {
                        var element = SvgPath{};
                        try element.parse(&maker, garbage_alloc, att_names, att_vals);
                        top_mut.override_from(element);

                        if (try maker.segments()) |segs| {
                            try write_path(&popts, garbage_alloc, &recorder, segs, &stack);
                            if (debug) log_seg(segs);
                        } else std.log.debug("no segments", .{});
                    } else {
                        std.log.warn("unrecognized element: {s}", .{element_tag});
                    }
                }
                _ = arena.reset(.retain_capacity);
            },
            .element_end => {
                _ = stack.pop();
            },
            else => {},
            .eof => break,
        }
    }
    const colors = try recorded.allocator().alloc(Color, recorder.colors.count());
    for (colors, recorder.colors.keys()) |*color, key| color.* = key.toColor();
    // Finest unit whose 16 bit range spans the image, coarser down to the arrangement grid while values overflow.
    const larger_side: f32 = @floatFromInt(@max(sw, sh));
    const tolerance = toleranceForImage(svg.width.?, svg.height.?);
    var unit_exponent: u4 = math.maxInt(u4);
    while (unit_exponent > @backingInt(popts.scale) and larger_side * @as(f32, @floatFromInt(@as(u16, 1) << unit_exponent)) > math.maxInt(i16)) unit_exponent -= 1;
    while (true) : (unit_exponent -= 1) {
        var written_units: WrittenUnits = .{};
        const default_bytes = encode(gpa, sw, sh, @fromBackingInt(unit_exponent), .{ .range = .default }, tolerance, colors, recorder.commands.items, &written_units) catch |err| switch (err) {
            error.OutOfRange => if (unit_exponent > @backingInt(popts.scale)) continue else break,
            else => |other| return other,
        };
        // Same units at a coarser scale, one byte each, when they all lie on its grid.
        if (sw > 256 or sh > 256) return default_bytes;
        const output_shift = written_units.reducedShift(unit_exponent) orelse return default_bytes;
        defer gpa.free(default_bytes);
        return encode(gpa, sw, sh, @fromBackingInt(unit_exponent), .{ .range = .reduced, .output_shift = output_shift }, tolerance, colors, recorder.commands.items, null) catch |err| switch (err) {
            error.OutOfRange => unreachable,
            else => |other| other,
        };
    }
    return encode(gpa, sw, sh, popts.scale, .{ .range = .enhanced }, tolerance, colors, recorder.commands.items, null);
}
fn log_seg(segs: []const Segment) void {
    std.log.warn("PRINT SEG BEGIN", .{});
    for (segs) |sg| {
        ut.print_point("start", sg.start);
        for (sg.commands) |sn| {
            ut.print_node(sn);
        }
    }
    std.log.warn("PRINT SEG END", .{});
}

pub const make_node_debug = true and debug;
pub const debug_write_path = false;
pub const make_node_debug2 = false and debug;
const debug = false;
