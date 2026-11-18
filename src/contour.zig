//! Flattening of `mesh.Contour` into points: one stream for every segment kind, uniform steps for
//! the allocation free convex fill, Wang plus halving for Béziers everywhere else.

const std = @import("std");
const math = std.math;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const mesh = @import("mesh.zig");
const arrangement = @import("arrangement.zig");
const Point = mesh.Point;
const Segment = mesh.Segment;
const Contour = mesh.Contour;
const Vector = @Vector(2, f32);

/// dvui's `Path.Builder.addArc` flattens within this many pixels.
const circle_arc_tolerance = 0.5;
/// fillConvex flattens Béziers and elliptical arcs within this many pixels.
pub const convex_tolerance = 0.5;
/// A circle_arc end point this close to the next point is left out, as dvui's `skip_end`.
const circle_arc_end_merge = 0.5;
const max_uniform_steps = 1 << 16;

pub const Error = error{ OutOfMemory, TooComplex };

/// Coordinates, radii and angles finite and within `mesh.max_coordinate`.
pub fn isValid(contour: Contour) bool {
    if (!pointIsValid(contour.start)) return false;
    for (contour.segments) |segment| switch (segment) {
        .line => |end| if (!pointIsValid(end)) return false,
        .quadratic => |curve| if (!pointIsValid(curve.control) or !pointIsValid(curve.end)) return false,
        .cubic => |curve| if (!pointIsValid(curve.control_0) or !pointIsValid(curve.control_1) or !pointIsValid(curve.end)) return false,
        .arc => |arc| if (!pointIsValid(arc.end) or !valueIsValid(arc.radius[0]) or !valueIsValid(arc.radius[1]) or !valueIsValid(arc.rotation)) return false,
        .circle_arc => |arc| if (!pointIsValid(arc.center) or !(arc.radius >= 0 and arc.radius <= mesh.max_coordinate) or
            !valueIsValid(arc.start_angle) or !valueIsValid(arc.end_angle)) return false,
    };
    return true;
}

fn pointIsValid(point: Point) bool {
    return valueIsValid(point[0]) and valueIsValid(point[1]);
}

fn valueIsValid(value: f32) bool {
    return @abs(value) <= mesh.max_coordinate;
}

/// Appends the flattened contour to `points` without its closing point. Fails with TooComplex once
/// `points` would grow past `max_points`.
pub fn append(allocator: Allocator, contour: Contour, tolerance: f32, max_points: usize, points: *std.ArrayList(Point)) Error!void {
    assert(tolerance > 0);
    var scratch: std.ArrayList(Point) = .empty;
    defer scratch.deinit(allocator);
    var stream: Points = .{ .contour = contour, .tolerance = tolerance, .bezier_scratch = .{ .allocator = allocator, .points = &scratch, .max_points = max_points } };
    while (try stream.next()) |point| {
        if (points.items.len >= max_points) return error.TooComplex;
        try points.append(allocator, point);
    }
}

/// The points of a closed contour in order: no exact repeats, no closing point equal to the start,
/// and no circle_arc end point within half a pixel of the next point.
pub const Points = struct {
    contour: Contour,
    tolerance: f32,
    /// Béziers take Wang plus halving through this list, else uniform Wang steps.
    bezier_scratch: ?BezierScratch = null,

    segment_index: usize = 0,
    segment: SegmentPoints = .{ .single = null },
    started: bool = false,
    current: Point = undefined,
    ahead: ?Item = null,
    last_emitted: ?Point = null,

    pub const BezierScratch = struct {
        allocator: Allocator,
        points: *std.ArrayList(Point),
        max_points: usize,
    };

    const Item = struct { point: Point, mergeable: bool = false };

    pub fn next(stream: *Points) Error!?Point {
        while (true) {
            const item = stream.ahead orelse (try stream.raw()) orelse return null;
            stream.ahead = try stream.raw();
            const following = if (stream.ahead) |ahead| ahead.point else stream.contour.start;
            if (stream.last_emitted) |last| {
                if (equal(item.point, last)) continue;
                if (stream.ahead == null and equal(item.point, stream.contour.start)) continue;
            }
            if (item.mergeable and distanceSquared(item.point, following) < circle_arc_end_merge * circle_arc_end_merge) continue;
            stream.last_emitted = item.point;
            return item.point;
        }
    }

    pub fn count(stream: *Points) Error!usize {
        var point_count: usize = 0;
        while (try stream.next()) |_| point_count += 1;
        return point_count;
    }

    fn raw(stream: *Points) Error!?Item {
        if (!stream.started) {
            stream.started = true;
            stream.current = stream.contour.start;
            return .{ .point = stream.contour.start };
        }
        while (true) {
            if (stream.segment.next()) |item| {
                stream.current = item.point;
                return item;
            }
            if (stream.segment_index == stream.contour.segments.len) return null;
            const segment = stream.contour.segments[stream.segment_index];
            stream.segment_index += 1;
            stream.segment = try stream.startSegment(segment);
        }
    }

    fn startSegment(stream: *Points, segment: Segment) Error!SegmentPoints {
        const from = stream.current;
        return switch (segment) {
            .line => |end| .{ .single = end },
            .circle_arc => |arc| .{ .circle_arc = .init(arc.center, arc.radius, arc.start_angle, arc.end_angle) },
            .arc => |arc| .{ .ellipse_arc = .init(from, arc.radius, arc.rotation, arc.large_arc, arc.sweep, arc.end, stream.tolerance) },
            .quadratic => |curve| try stream.startBezier(&.{ from, curve.control, curve.end }),
            .cubic => |curve| try stream.startBezier(&.{ from, curve.control_0, curve.control_1, curve.end }),
        };
    }

    fn startBezier(stream: *Points, controls: []const Point) Error!SegmentPoints {
        const scratch = stream.bezier_scratch orelse return .{ .uniform_bezier = .init(controls, stream.tolerance) };
        scratch.points.clearRetainingCapacity();
        var tvg_controls: [4]mesh.tvg.Point = undefined;
        for (tvg_controls[0..controls.len], controls) |*tvg_control, control| tvg_control.* = .{ .x = control[0], .y = control[1] };
        var flat: std.ArrayList(mesh.tvg.Point) = .empty;
        defer flat.deinit(scratch.allocator);
        try arrangement.flattenBezier(scratch.allocator, tvg_controls[0..controls.len], stream.tolerance, scratch.max_points, &flat);
        assert(flat.items.len > 0);
        try scratch.points.ensureTotalCapacity(scratch.allocator, flat.items.len);
        var previous: Vector = controls[0];
        // Inner points move half the local chord sag outward, so the polyline straddles the curve.
        for (flat.items[0 .. flat.items.len - 1], flat.items[1..]) |point, following| {
            const here: Vector = .{ point.x, point.y };
            const middle = (previous + Vector{ following.x, following.y }) * @as(Vector, @splat(0.5));
            scratch.points.appendAssumeCapacity(here + (here - middle) / @as(Vector, @splat(8)));
            previous = here;
        }
        const end = flat.items[flat.items.len - 1];
        scratch.points.appendAssumeCapacity(.{ end.x, end.y });
        return .{ .list = .{ .points = scratch.points.items } };
    }
};

const SegmentPoints = union(enum) {
    single: ?Point,
    list: struct { points: []const Point, index: usize = 0 },
    circle_arc: CircleArc,
    ellipse_arc: EllipseArc,
    uniform_bezier: UniformBezier,

    fn next(segment: *SegmentPoints) ?Points.Item {
        switch (segment.*) {
            .single => |*point| {
                const value = point.* orelse return null;
                point.* = null;
                return .{ .point = value };
            },
            .list => |*list| {
                if (list.index == list.points.len) return null;
                list.index += 1;
                return .{ .point = list.points[list.index - 1] };
            },
            inline .circle_arc, .ellipse_arc, .uniform_bezier => |*state| return state.next(),
        }
    }
};

/// The steps of dvui's `Path.Builder.addArc` with the same f32 arithmetic: the start, every multiple
/// of the step angle strictly between, then the end, which may merge into the next point.
const CircleArc = struct {
    center: Point,
    radius: f32,
    angle: f32,
    end_angle: f32,
    step: f32,
    stage: enum { start, grid, done } = .start,

    fn init(center: Point, radius: f32, start_angle: f32, end_angle: f32) CircleArc {
        return .{
            .center = center,
            .radius = radius,
            .angle = start_angle,
            .end_angle = end_angle,
            .step = math.acos(radius / (radius + circle_arc_tolerance)),
        };
    }

    fn pointAt(arc: *const CircleArc, angle: f32) Point {
        return .{ arc.center[0] + arc.radius * @cos(angle), arc.center[1] + arc.radius * @sin(angle) };
    }

    fn next(arc: *CircleArc) ?Points.Item {
        switch (arc.stage) {
            .start => {
                if (arc.radius == 0) {
                    arc.stage = .done;
                    return .{ .point = arc.center };
                }
                arc.stage = .grid;
                return .{ .point = arc.pointAt(arc.angle) };
            },
            .grid => {
                if (arc.angle - arc.end_angle > arc.step) {
                    const grid_angle = @floor((arc.angle - 0.001) / arc.step) * arc.step;
                    if (grid_angle < arc.angle) {
                        arc.angle = grid_angle;
                        return .{ .point = arc.pointAt(arc.angle) };
                    }
                }
                arc.stage = .done;
                return .{ .point = arc.pointAt(arc.end_angle), .mergeable = true };
            },
            .done => return null,
        }
    }
};

/// SVG endpoint arc in center form, uniform angle steps within the tolerance.
/// https://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes
const EllipseArc = struct {
    center: Vector = @splat(0),
    radius: Vector = @splat(0),
    cos_rotation: f32 = 1,
    sin_rotation: f32 = 0,
    start_angle: f32 = 0,
    sweep_angle: f32 = 0,
    step_count: u32 = 0,
    step: u32 = 1,
    end: ?Point,

    fn init(from: Point, radius_in: [2]f32, rotation: f32, large_arc: bool, sweep: bool, end: Point, tolerance: f32) EllipseArc {
        if (equal(from, end)) return .{ .end = null };
        var radius: Vector = .{ @abs(radius_in[0]), @abs(radius_in[1]) };
        if (radius[0] == 0 or radius[1] == 0) return .{ .end = end };

        const cos_rotation = @cos(rotation);
        const sin_rotation = @sin(rotation);
        const half_difference = (@as(Vector, from) - @as(Vector, end)) * @as(Vector, @splat(0.5));
        const rotated: Vector = .{
            cos_rotation * half_difference[0] + sin_rotation * half_difference[1],
            -sin_rotation * half_difference[0] + cos_rotation * half_difference[1],
        };
        const lambda = @reduce(.Add, (rotated * rotated) / (radius * radius));
        if (lambda > 1) radius *= @splat(@sqrt(lambda));

        const rx2 = radius[0] * radius[0];
        const ry2 = radius[1] * radius[1];
        const denominator = rx2 * rotated[1] * rotated[1] + ry2 * rotated[0] * rotated[0];
        const numerator = @max(0, rx2 * ry2 - denominator);
        const sign: f32 = if (large_arc == sweep) -1 else 1;
        const factor = sign * @sqrt(numerator / denominator);
        const center_rotated: Vector = .{ factor * radius[0] * rotated[1] / radius[1], -factor * radius[1] * rotated[0] / radius[0] };
        const middle = (@as(Vector, from) + @as(Vector, end)) * @as(Vector, @splat(0.5));
        const center: Vector = .{
            cos_rotation * center_rotated[0] - sin_rotation * center_rotated[1] + middle[0],
            sin_rotation * center_rotated[0] + cos_rotation * center_rotated[1] + middle[1],
        };

        const start_unit = (rotated - center_rotated) / radius;
        const end_unit = (-rotated - center_rotated) / radius;
        const start_angle = math.atan2(start_unit[1], start_unit[0]);
        var sweep_angle = math.atan2(start_unit[0] * end_unit[1] - start_unit[1] * end_unit[0], @reduce(.Add, start_unit * end_unit));
        if (!sweep and sweep_angle > 0) sweep_angle -= 2 * math.pi;
        if (sweep and sweep_angle < 0) sweep_angle += 2 * math.pi;

        const largest_radius = @max(radius[0], radius[1]);
        const max_step = if (largest_radius <= tolerance) math.pi / 2.0 else 2 * math.acos(1 - tolerance / largest_radius);
        const steps = @ceil(@abs(sweep_angle) / max_step);
        if (!(steps >= 1 and steps <= max_uniform_steps)) return .{ .end = end };
        return .{
            .center = center,
            .radius = radius,
            .cos_rotation = cos_rotation,
            .sin_rotation = sin_rotation,
            .start_angle = start_angle,
            .sweep_angle = sweep_angle,
            .step_count = @intFromFloat(steps),
            .end = end,
        };
    }

    fn next(arc: *EllipseArc) ?Points.Item {
        if (arc.step < arc.step_count) {
            const angle = arc.start_angle + arc.sweep_angle * @as(f32, @floatFromInt(arc.step)) / @as(f32, @floatFromInt(arc.step_count));
            arc.step += 1;
            const x = arc.radius[0] * @cos(angle);
            const y = arc.radius[1] * @sin(angle);
            return .{ .point = .{
                arc.cos_rotation * x - arc.sin_rotation * y + arc.center[0],
                arc.sin_rotation * x + arc.cos_rotation * y + arc.center[1],
            } };
        }
        const end = arc.end orelse return null;
        arc.end = null;
        return .{ .point = end };
    }
};

/// Wang's uniform steps, the polyline stays within the tolerance of the curve.
const UniformBezier = struct {
    controls: [4]Vector,
    control_count: u32,
    step_count: u32,
    step: u32 = 1,

    fn init(controls: []const Point, tolerance: f32) UniformBezier {
        assert(controls.len == 3 or controls.len == 4);
        var bezier: UniformBezier = .{ .controls = undefined, .control_count = @intCast(controls.len), .step_count = 1 };
        for (bezier.controls[0..controls.len], controls) |*value, control| value.* = control;
        var largest_second_difference: f32 = 0;
        for (bezier.controls[0 .. controls.len - 2], bezier.controls[1 .. controls.len - 1], bezier.controls[2..controls.len]) |a, b, c| {
            const difference = a - b - b + c;
            largest_second_difference = @max(largest_second_difference, @sqrt(@reduce(.Add, difference * difference)));
        }
        const degree: f32 = @floatFromInt(controls.len - 1);
        const steps = @ceil(@sqrt(degree * (degree - 1) / 8 * largest_second_difference / tolerance));
        if (steps >= 1 and steps <= max_uniform_steps) bezier.step_count = @intFromFloat(steps);
        return bezier;
    }

    fn next(bezier: *UniformBezier) ?Points.Item {
        if (bezier.step > bezier.step_count) return null;
        defer bezier.step += 1;
        const count = bezier.control_count;
        if (bezier.step == bezier.step_count) return .{ .point = bezier.controls[count - 1] };
        const t: Vector = @splat(@as(f32, @floatFromInt(bezier.step)) / @as(f32, @floatFromInt(bezier.step_count)));
        var reduced = bezier.controls;
        var remaining = count;
        while (remaining > 1) : (remaining -= 1) {
            for (reduced[0 .. remaining - 1], reduced[1..remaining]) |*value, following| value.* += t * (following - value.*);
        }
        return .{ .point = reduced[0] };
    }
};

fn equal(a: Point, b: Point) bool {
    return a[0] == b[0] and a[1] == b[1];
}

fn distanceSquared(a: Point, b: Point) f32 {
    const difference = @as(Vector, a) - @as(Vector, b);
    return @reduce(.Add, difference * difference);
}
