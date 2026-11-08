//! Implements TinyVG to text conversion
//!
//! TODO: Port over the SVG parser to TinyVG and implement SVG parsing in Zig!
//! => https://github.com/fubark/cosmic/blob/master/graphics/src/svg.zig
//!
const std = @import("std");
const tvg = @import("tinyvg.zig");

/// Renders a binary TinyVG graphic to SVG.
/// - `allocator` will be used for temporary allocations in both the TVG parser and the SVG renderer.
/// - `tvg_buffer` provides a binary TinyVG file
/// - `writer` will receive the UTF-8 encoded SVG text.
pub fn renderBinary(allocator: std.mem.Allocator, tvg_buffer: []const u8, writer: *std.Io.Writer) !void {
    var reader: std.Io.Reader = .fixed(tvg_buffer);

    var parser = try tvg.parse(allocator, &reader);
    defer parser.deinit();

    return try renderStream(allocator, &parser, writer);
}

/// Renders a TinyVG command stream into a SVG file.
/// - `allocator` is used for temporary allocations
/// - `parser` is a pointer to a `tvg.parsing.Parser(Reader)`
/// - `writer` will receive the UTF-8 encoded SVG text.
pub fn renderStream(allocator: std.mem.Allocator, parser: anytype, writer: *std.Io.Writer) !void {
    var cache = SvgStyleCache{
        .allocator = allocator,
        .color_table = parser.color_table,
    };
    defer cache.list.deinit(allocator);

    try writer.print(
        \\<svg xmlns="http://www.w3.org/2000/svg" width="{0d}" height="{1d}" viewBox="0 0 {0d} {1d}">
    , .{
        parser.header.width,
        parser.header.height,
    });

    while (try parser.next()) |command| {
        switch (command) {
            .fill_rectangles => |data| {
                for (data.rectangles) |rect| {
                    try writer.print(
                        \\<rect style="{f}" x="{d}" y="{d}" width="{d}" height="{d}"/>
                    ,
                        .{
                            svgStyle(&cache, data.style, null, null),
                            rect.x,
                            rect.y,
                            rect.width,
                            rect.height,
                        },
                    );
                }
            },

            .outline_fill_rectangles => |data| {
                for (data.rectangles) |rect| {
                    try writer.print(
                        \\<rect style="{f}" x="{d}" y="{d}" width="{d}" height="{d}"/>
                    ,
                        .{
                            svgStyle(&cache, data.fill_style, data.line_style, data.line_width),
                            rect.x,
                            rect.y,
                            rect.width,
                            rect.height,
                        },
                    );
                }
            },

            .draw_lines => |data| {
                for (data.lines) |line| {
                    try writer.print(
                        \\<line style="{f}" x1="{d}" y1="{d}" x2="{d}" y2="{d}"/>
                    ,
                        .{
                            svgStyle(&cache, null, data.style, data.line_width),
                            line.start.x,
                            line.start.y,
                            line.end.x,
                            line.end.y,
                        },
                    );
                }
            },

            .draw_line_loop => |data| {
                try writer.print(
                    \\<polygon style="{f}" points="
                , .{
                    svgStyle(&cache, null, data.style, data.line_width),
                });
                for (data.vertices, 0..) |vertex, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("{d},{d}", .{ vertex.x, vertex.y });
                }
                try writer.writeAll(
                    \\"/>
                );
            },

            .draw_line_strip => |data| {
                try writer.print(
                    \\<polyline style="{f}" points="
                , .{
                    svgStyle(&cache, null, data.style, data.line_width),
                });
                for (data.vertices, 0..) |vertex, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("{d},{d}", .{ vertex.x, vertex.y });
                }
                try writer.writeAll(
                    \\"/>
                );
            },

            .fill_polygon => |data| {
                try writer.print(
                    \\<polygon style="{f}" points="
                , .{
                    svgStyle(&cache, data.style, null, null),
                });
                for (data.vertices, 0..) |vertex, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("{d},{d}", .{ vertex.x, vertex.y });
                }
                try writer.writeAll(
                    \\"/>
                );
            },

            .outline_fill_polygon => |data| {
                try writer.print(
                    \\<polygon style="{f}" points="
                , .{
                    svgStyle(&cache, data.fill_style, data.line_style, data.line_width),
                });
                for (data.vertices, 0..) |vertex, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("{d},{d}", .{ vertex.x, vertex.y });
                }
                try writer.writeAll(
                    \\"/>
                );
            },

            .draw_line_path => |data| {
                const style = svgStyle(&cache, null, data.style, data.line_width);
                const path = SvgPath{ .path = data.path };
                try writer.print(
                    \\<path style="{f}" d="{f}"/>
                , .{ style, path });
            },

            .fill_path => |data| {
                const style = svgStyle(&cache, data.style, null, null);
                const path = SvgPath{ .path = data.path };
                try writer.print(
                    \\<path style="{f}" d="{f}"/>
                , .{ style, path });
            },

            .outline_fill_path => |data| {
                const style = svgStyle(&cache, data.fill_style, data.line_style, data.line_width);
                const path = SvgPath{ .path = data.path };

                try writer.print(
                    \\<path style="{f}" d="{f}"/>
                , .{ style, path });
            },
        }
        if (cache.out_of_memory) return error.OutOfMemory;
    }

    if (cache.list.items.len > 0) {
        try writer.writeAll("<defs>");

        for (cache.list.items, 0..) |style, i| {
            switch (style) {
                .linear => |grad| {
                    try writer.print(
                        \\<linearGradient id="grad{d}" gradientUnits="userSpaceOnUse" x1="{d}" y1="{d}" x2="{d}" y2="{d}">
                    , .{ i, grad.point_0.x, grad.point_0.y, grad.point_1.x, grad.point_1.y });
                    try writer.print(
                        \\<stop offset="0" style="stop-opacity:{d}; stop-color:
                    , .{cache.color_table[grad.color_0].a});
                    try cache.printColor3AndPrefix(writer, "", grad.color_0, "\" />");
                    try writer.print(
                        \\<stop offset="100%" style="stop-opacity:{d}; stop-color:
                    , .{cache.color_table[grad.color_1].a});
                    try cache.printColor3AndPrefix(writer, "", grad.color_1, "\" />");
                    try writer.writeAll("</linearGradient>");
                },
                .radial => |grad| {
                    const dx = grad.point_1.x - grad.point_0.x;
                    const dy = grad.point_1.y - grad.point_0.y;
                    const r = std.math.sqrt(dx * dx + dy * dy);

                    try writer.print(
                        \\<radialGradient id="grad{d}" gradientUnits="userSpaceOnUse" cx="{d}" cy="{d}" r="{d}">
                    , .{ i, grad.point_0.x, grad.point_0.y, r });
                    try writer.print(
                        \\<stop offset="0" style="stop-opacity:{d}; stop-color:
                    , .{cache.color_table[grad.color_0].a});
                    try cache.printColor3AndPrefix(writer, "", grad.color_0, "\"/>");
                    try writer.print(
                        \\<stop offset="100%" style="stop-opacity:{d}; stop-color:
                    , .{cache.color_table[grad.color_1].a});
                    try cache.printColor3AndPrefix(writer, "", grad.color_1, "\"/>");
                    try writer.writeAll("</radialGradient>");
                },
                .flat => @panic("implementation fault"),
            }
        }
        try writer.writeAll("</defs>");
    }

    try writer.writeAll("</svg>");
}

const SvgStyle = struct {
    cache: *SvgStyleCache,
    fill_style: ?tvg.Style,
    line_style: ?tvg.Style,
    line_width: ?f32,

    pub fn format(self: SvgStyle, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        if (self.fill_style) |style| {
            switch (style) {
                .flat => |ind| try self.cache.printColorForStyle(writer, "fill", ind),
                .linear, .radial => try writer.print("fill:url(#grad{d});", .{
                    self.cache.insert(style),
                }),
            }
        } else {
            try writer.writeAll("fill:none;");
        }

        if (self.line_style) |style| {
            try writer.writeAll("stroke-linecap:round;");
            switch (style) {
                .flat => |ind| try self.cache.printColorForStyle(writer, "stroke", ind),
                .linear, .radial => try writer.print("stroke:url(#grad{d});", .{
                    self.cache.insert(style),
                }),
            }
        } else {
            try writer.writeAll("stroke:none;");
        }

        if (self.line_width) |lw| {
            try writer.print("stroke-width:{d};", .{lw});
        }
    }
};

const SvgPath = struct {
    path: tvg.Path,

    pub fn format(self: SvgPath, writer: *std.Io.Writer) std.Io.Writer.Error!void {

        for (self.path.segments) |segment| {
            try writer.print("M{d},{d}", .{ segment.start.x, segment.start.y });
            for (segment.commands) |cmd| {
                switch (cmd) {
                    .line => |data| try writer.print("L{d},{d}", .{ data.data.x, data.data.y }),
                    .horiz => |data| try writer.print("H{d}", .{data.data}),
                    .vert => |data| try writer.print("V{d}", .{data.data}),
                    .bezier => |data| try writer.print("C{d},{d},{d},{d},{d},{d}", .{ data.data.c0.x, data.data.c0.y, data.data.c1.x, data.data.c1.y, data.data.p1.x, data.data.p1.y }),
                    .arc_circle => |data| try writer.print("A{d},{d},{d},{d},{d},{d},{d}", .{
                        data.data.radius,
                        data.data.radius,
                        0,
                        @intFromBool(data.data.large_arc),
                        @intFromBool(!data.data.sweep),
                        data.data.target.x,
                        data.data.target.y,
                    }),
                    .arc_ellipse => |data| try writer.print("A{d},{d},{d},{d},{d},{d},{d}", .{
                        data.data.radius_x,
                        data.data.radius_y,
                        data.data.rotation,
                        @intFromBool(data.data.large_arc),
                        @intFromBool(!data.data.sweep),
                        data.data.target.x,
                        data.data.target.y,
                    }),
                    .close => try writer.writeAll("Z"),
                    .quadratic_bezier => |data| try writer.print("Q{d},{d},{d},{d}", .{ data.data.c.x, data.data.c.y, data.data.p1.x, data.data.p1.y }),
                }
            }
        }
    }
};

fn svgStyle(
    cache: *SvgStyleCache,
    fill_style: ?tvg.Style,
    line_style: ?tvg.Style,
    line_width: ?f32,
) SvgStyle {
    return SvgStyle{
        .cache = cache,
        .fill_style = fill_style,
        .line_style = line_style,
        .line_width = line_width,
    };
}

const SvgStyleCache = struct {
    allocator: std.mem.Allocator,
    color_table: []const tvg.Color,
    list: std.ArrayList(tvg.Style) = .empty,
    out_of_memory: bool = false,

    /// Returns the gradient id. Out of memory is remembered and reported after the format call,
    /// because format can only fail with error.WriteFailed.
    pub fn insert(self: *SvgStyleCache, style: tvg.Style) usize {
        self.list.append(self.allocator, style) catch {
            self.out_of_memory = true;
            return 0;
        };
        return self.list.items.len - 1;
    }

    fn printColorForStyle(self: SvgStyleCache, writer: *std.Io.Writer, prefix: []const u8, i: usize) !void {
        std.debug.assert(i < self.color_table.len);
        const color = self.color_table[i];

        const r: u8 = @intFromFloat(std.math.clamp(255.0 * color.r, 0.0, 255.0));
        const g = @as(u8, @intFromFloat(std.math.clamp(255.0 * color.g, 0.0, 255.0)));
        const b = @as(u8, @intFromFloat(std.math.clamp(255.0 * color.b, 0.0, 255.0)));
        try writer.print("{s}:#{X:0>2}{X:0>2}{X:0>2};", .{ prefix, r, g, b });
        if (color.a != 1.0) {
            try writer.print("{s}-opacity:{d};", .{ prefix, color.a });
        }
    }

    fn printColor3AndPrefix(self: SvgStyleCache, writer: *std.Io.Writer, prefix: []const u8, i: usize, postfix: []const u8) !void {
        std.debug.assert(i < self.color_table.len);
        const color = self.color_table[i];

        const r: u8 = @intFromFloat(std.math.clamp(255.0 * color.r, 0.0, 255.0));
        const g: u8 = @intFromFloat(std.math.clamp(255.0 * color.g, 0.0, 255.0));
        const b = @as(u8, @intFromFloat(std.math.clamp(255.0 * color.b, 0.0, 255.0)));
        try writer.print("{s}#{X:0>2}{X:0>2}{X:0>2}{s}", .{ prefix, r, g, b, postfix });
    }
};
