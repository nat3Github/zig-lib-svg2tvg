//! TinyVG documents as meshes: every fill and every stroke of a command becomes one part, in
//! drawing order. Strokes always take the outline, so a mesh colored later with any alpha covers
//! every pixel once.

const std = @import("std");
const math = std.math;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const mesh = @import("mesh.zig");
const tvg = mesh.tvg;
const DrawCommand = tvg.parsing.DrawCommand;
const Point = mesh.Point;

pub fn tvgMeshes(allocator: Allocator, tvg_bytes: []const u8, options: mesh.TvgOptions) mesh.Error!mesh.TvgMeshes {
    if (!(options.tolerance > 0)) return error.InvalidInput;
    for (options.origin ++ options.size) |value| if (!(@abs(value) <= mesh.max_coordinate)) return error.InvalidInput;
    if (options.line_width) |width| if (!(width >= 0 and width <= mesh.max_coordinate)) return error.InvalidInput;

    var reader: std.Io.Reader = .fixed(tvg_bytes);
    var parser = tvg.parse(allocator, &reader) catch |err| return invalidUnlessOutOfMemory(err);
    defer parser.deinit();
    var scratch_state: std.heap.ArenaAllocator = .init(allocator);
    defer scratch_state.deinit();
    var work: mesh.Work = .{ .remaining = options.limits.max_work };
    var document: Document = .{
        .allocator = allocator,
        .scratch = scratch_state.allocator(),
        .color_table = parser.color_table,
        .transform = .init(options, @floatFromInt(parser.header.width), @floatFromInt(parser.header.height)),
        .options = options,
        .work = &work,
    };
    errdefer {
        for (document.parts.items) |part| part.mesh.deinit(allocator);
        document.parts.deinit(allocator);
    }
    while (parser.next() catch |err| return invalidUnlessOutOfMemory(err)) |command| {
        _ = scratch_state.reset(.retain_capacity);
        try document.appendCommand(command);
    }
    return .{ .parts = try document.parts.toOwnedSlice(allocator) };
}

fn invalidUnlessOutOfMemory(err: anyerror) mesh.Error {
    return if (err == error.OutOfMemory) error.OutOfMemory else error.InvalidInput;
}

/// Document units to output pixels: a scale per axis, then the offset.
const Transform = struct {
    scale: [2]f32,
    offset: [2]f32,

    fn init(options: mesh.TvgOptions, width: f32, height: f32) Transform {
        assert(width > 0 and height > 0);
        var scale: [2]f32 = .{ options.size[0] / width, options.size[1] / height };
        var offset = options.origin;
        if (options.keep_aspect) {
            const uniform = @min(scale[0], scale[1]);
            scale = @splat(uniform);
            offset[0] += (options.size[0] - width * uniform) / 2;
            offset[1] += (options.size[1] - height * uniform) / 2;
        }
        return .{ .scale = scale, .offset = offset };
    }

    fn apply(transform: Transform, point: tvg.Point) Point {
        return .{ transform.offset[0] + point.x * transform.scale[0], transform.offset[1] + point.y * transform.scale[1] };
    }

    fn meanScale(transform: Transform) f32 {
        return (@abs(transform.scale[0]) + @abs(transform.scale[1])) / 2;
    }

    /// The image of an elliptical arc: radii and rotation in radians from the singular value
    /// decomposition of the scaled ellipse axes.
    fn arc(transform: Transform, signed_radius_x: f32, signed_radius_y: f32, rotation_degrees: f32, large_arc: bool, sweep: bool, target: Point) mesh.Segment {
        const rotation = rotation_degrees * math.pi / 180;
        const radius_x = @abs(signed_radius_x);
        const radius_y = @abs(signed_radius_y);
        const cos = @cos(rotation);
        const sin = @sin(rotation);
        const a = transform.scale[0] * radius_x * cos;
        const c = transform.scale[1] * radius_x * sin;
        const b = -transform.scale[0] * radius_y * sin;
        const d = transform.scale[1] * radius_y * cos;
        const e = (a + d) / 2;
        const f = (a - d) / 2;
        const g = (c + b) / 2;
        const h = (c - b) / 2;
        const q = @sqrt(e * e + h * h);
        const r = @sqrt(f * f + g * g);
        return .{ .arc = .{
            .radius = .{ q + r, @abs(q - r) },
            .rotation = (math.atan2(g, f) + math.atan2(h, e)) / 2,
            .large_arc = large_arc,
            .sweep = sweep,
            .end = target,
        } };
    }
};

const Document = struct {
    allocator: Allocator,
    /// Contours of the current command.
    scratch: Allocator,
    color_table: []const tvg.Color,
    transform: Transform,
    options: mesh.TvgOptions,
    work: *mesh.Work,
    parts: std.ArrayList(mesh.TvgPart) = .empty,

    fn appendCommand(document: *Document, command: DrawCommand) mesh.Error!void {
        switch (command) {
            .fill_polygon => |command_data| try document.fill(command_data.style, try document.polygon(command_data.vertices, true), .even_odd),
            .fill_rectangles => |command_data| try document.fill(command_data.style, try document.rectangles(command_data.rectangles), .nonzero),
            .fill_path => |command_data| try document.fill(command_data.style, try document.path(command_data.path), .even_odd),
            .draw_lines => |draw| try document.stroke(draw.style, draw.line_width, try document.lines(draw.lines)),
            .draw_line_loop => |draw| try document.stroke(draw.style, draw.line_width, try document.polygon(draw.vertices, true)),
            .draw_line_strip => |draw| try document.stroke(draw.style, draw.line_width, try document.polygon(draw.vertices, false)),
            .draw_line_path => |draw| try document.stroke(draw.style, draw.line_width, try document.path(draw.path)),
            .outline_fill_polygon => |outline| {
                const contours = try document.polygon(outline.vertices, true);
                try document.fill(outline.fill_style, contours, .even_odd);
                try document.stroke(outline.line_style, outline.line_width, contours);
            },
            .outline_fill_rectangles => |outline| {
                const contours = try document.rectangles(outline.rectangles);
                try document.fill(outline.fill_style, contours, .nonzero);
                try document.stroke(outline.line_style, outline.line_width, contours);
            },
            .outline_fill_path => |outline| {
                const contours = try document.path(outline.path);
                try document.fill(outline.fill_style, contours, .even_odd);
                try document.stroke(outline.line_style, outline.line_width, contours);
            },
        }
    }

    fn fill(document: *Document, style: tvg.Style, contours: []const mesh.Contour, fill_rule: mesh.FillRule) mesh.Error!void {
        if (!document.options.fills or contours.len == 0) return;
        const built = try mesh.fill(document.allocator, contours, .{
            .fill_rule = fill_rule,
            .anti_alias = document.options.anti_alias,
            .tolerance = document.options.tolerance,
            .limits = document.options.limits,
            .work = document.work,
        });
        try document.appendPart(.fill, style, built);
    }

    /// TinyVG lines have round joins and caps.
    fn stroke(document: *Document, style: tvg.Style, line_width: f32, contours: []const mesh.Contour) mesh.Error!void {
        const width = (document.options.line_width orelse line_width) * document.transform.meanScale();
        if (!(width > 0) or contours.len == 0) return;
        const built = try mesh.stroke(document.allocator, contours, .{
            .width = width,
            .join = .round,
            .cap = .round,
            .anti_alias = document.options.anti_alias,
            .translucent = true,
            .tolerance = document.options.tolerance,
            .limits = document.options.limits,
            .work = document.work,
        });
        try document.appendPart(.stroke, style, built);
    }

    fn appendPart(document: *Document, kind: @FieldType(mesh.TvgPart, "kind"), style: tvg.Style, built: mesh.Mesh) mesh.Error!void {
        if (built.indices.len == 0) return built.deinit(document.allocator);
        errdefer built.deinit(document.allocator);
        try document.parts.append(document.allocator, .{ .kind = kind, .paint = document.paint(style), .mesh = built });
    }

    fn paint(document: *const Document, style: tvg.Style) mesh.Paint {
        return switch (style) {
            .flat => |index| .{ .flat = document.color(index) },
            .linear => |gradient| .{ .linear = .{
                .color_0 = document.color(gradient.color_0),
                .color_1 = document.color(gradient.color_1),
                .point_0 = document.transform.apply(gradient.point_0),
                .point_1 = document.transform.apply(gradient.point_1),
            } },
            .radial => |gradient| .{ .radial = .{
                .color_0 = document.color(gradient.color_0),
                .color_1 = document.color(gradient.color_1),
                .center = document.transform.apply(gradient.point_0),
                .edge = document.transform.apply(gradient.point_1),
            } },
        };
    }

    fn color(document: *const Document, index: u32) mesh.Color {
        const entry = document.color_table[index];
        return .{ entry.r, entry.g, entry.b, entry.a };
    }

    /// One contour through `vertices`, none without vertices.
    fn polygon(document: *Document, vertices: []const tvg.Point, closed: bool) mesh.Error![]const mesh.Contour {
        if (vertices.len == 0) return &.{};
        const segments = try document.scratch.alloc(mesh.Segment, vertices.len - 1);
        for (segments, vertices[1..]) |*segment, vertex| segment.* = .{ .line = document.transform.apply(vertex) };
        const contours = try document.scratch.alloc(mesh.Contour, 1);
        contours[0] = .{ .start = document.transform.apply(vertices[0]), .segments = segments, .closed = closed };
        return contours;
    }

    /// Clockwise on screen whatever the sign of width and height, so overlaps add up under nonzero.
    fn rectangles(document: *Document, rects: []const tvg.Rectangle) mesh.Error![]const mesh.Contour {
        const contours = try document.scratch.alloc(mesh.Contour, rects.len);
        for (contours, rects) |*contour, rect| {
            const left = @min(rect.x, rect.x + rect.width);
            const right = @max(rect.x, rect.x + rect.width);
            const top = @min(rect.y, rect.y + rect.height);
            const bottom = @max(rect.y, rect.y + rect.height);
            const segments = try document.scratch.alloc(mesh.Segment, 3);
            segments[0..3].* = .{
                .{ .line = document.transform.apply(.{ .x = right, .y = top }) },
                .{ .line = document.transform.apply(.{ .x = right, .y = bottom }) },
                .{ .line = document.transform.apply(.{ .x = left, .y = bottom }) },
            };
            contour.* = .{ .start = document.transform.apply(.{ .x = left, .y = top }), .segments = segments, .closed = true };
        }
        return contours;
    }

    fn lines(document: *Document, tvg_lines: []const tvg.Line) mesh.Error![]const mesh.Contour {
        const contours = try document.scratch.alloc(mesh.Contour, tvg_lines.len);
        for (contours, tvg_lines) |*contour, line| {
            const segments = try document.scratch.alloc(mesh.Segment, 1);
            segments[0] = .{ .line = document.transform.apply(line.end) };
            contour.* = .{ .start = document.transform.apply(line.start), .segments = segments };
        }
        return contours;
    }

    /// One contour per path segment, closed when its last node closes it. TinyVG arcs store the
    /// sweep flag inverted against SVG. Line widths per node are ignored.
    fn path(document: *Document, tvg_path: tvg.Path) mesh.Error![]const mesh.Contour {
        const contours = try document.scratch.alloc(mesh.Contour, tvg_path.segments.len);
        const transform = document.transform;
        for (contours, tvg_path.segments) |*contour, path_segment| {
            const segments = try document.scratch.alloc(mesh.Segment, path_segment.commands.len);
            var current = path_segment.start;
            for (segments, path_segment.commands) |*segment, node| {
                segment.* = switch (node) {
                    .line => |line| line: {
                        current = line.data;
                        break :line .{ .line = transform.apply(current) };
                    },
                    .horiz => |horizontal| horizontal: {
                        current.x = horizontal.data;
                        break :horizontal .{ .line = transform.apply(current) };
                    },
                    .vert => |vertical| vertical: {
                        current.y = vertical.data;
                        break :vertical .{ .line = transform.apply(current) };
                    },
                    .bezier => |bezier| bezier: {
                        current = bezier.data.p1;
                        break :bezier .{ .cubic = .{ .control_0 = transform.apply(bezier.data.c0), .control_1 = transform.apply(bezier.data.c1), .end = transform.apply(current) } };
                    },
                    .quadratic_bezier => |bezier| bezier: {
                        current = bezier.data.p1;
                        break :bezier .{ .quadratic = .{ .control = transform.apply(bezier.data.c), .end = transform.apply(current) } };
                    },
                    .arc_circle => |arc| arc: {
                        current = arc.data.target;
                        break :arc transform.arc(arc.data.radius, arc.data.radius, 0, arc.data.large_arc, !arc.data.sweep, transform.apply(current));
                    },
                    .arc_ellipse => |arc| arc: {
                        current = arc.data.target;
                        break :arc transform.arc(arc.data.radius_x, arc.data.radius_y, arc.data.rotation, arc.data.large_arc, !arc.data.sweep, transform.apply(current));
                    },
                    .close => close: {
                        current = path_segment.start;
                        break :close .{ .line = transform.apply(current) };
                    },
                };
            }
            const commands = path_segment.commands;
            contour.* = .{
                .start = transform.apply(path_segment.start),
                .segments = segments,
                .closed = commands.len > 0 and commands[commands.len - 1] == .close,
            };
        }
        return contours;
    }
};
