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

/// Appends the flattened contour to `points`, without a closing point equal to the start when
/// `drop_closing_point`. Fails with TooComplex once `points` would grow past `max_points`.
pub fn append(allocator: Allocator, contour: Contour, drop_closing_point: bool, tolerance: f32, max_points: usize, points: *std.ArrayList(Point)) Error!void {
    assert(tolerance > 0);
    const Appender = struct {
        allocator: Allocator,
        points: *std.ArrayList(Point),
        max_points: usize,

        fn emit(appender: @This(), point: Point) Error!void {
            if (appender.points.items.len >= appender.max_points) return error.TooComplex;
            try appender.points.append(appender.allocator, point);
        }
    };
    try forEachPoint(contour, .{
        .tolerance = tolerance,
        .drop_closing_point = drop_closing_point,
        .bezier_scratch = .{ .allocator = allocator, .max_points = max_points },
    }, Appender{ .allocator = allocator, .points = points, .max_points = max_points }, Appender.emit);
}

pub const Flattening = struct {
    tolerance: f32,
    drop_closing_point: bool = true,
    /// Béziers take Wang plus halving with this allocator, else uniform Wang steps.
    bezier_scratch: ?BezierScratch = null,
};

pub const BezierScratch = struct {
    allocator: Allocator,
    max_points: usize,
};

const Item = struct { point: Point, mergeable: bool = false };

/// Calls `emit` with the points of a contour in order: no exact repeats, no closing point equal to
/// the start unless kept for an open stroke, and no circle_arc end point within half a pixel of the
/// next point. A push loop, so the state stays in registers on the hot path of `fillConvex`.
pub fn forEachPoint(contour: Contour, flattening: Flattening, context: anytype, comptime emit: fn (@TypeOf(context), Point) Error!void) Error!void {
    var filter: Filter(@TypeOf(context), emit) = .{ .context = context, .start = contour.start, .drop_closing_point = flattening.drop_closing_point };
    try filter.push(.{ .point = contour.start });
    for (contour.segments) |segment| {
        const from = filter.pending.?.point;
        switch (segment) {
            .line => |end| try filter.push(.{ .point = end }),
            .circle_arc => |arc| {
                var points: CircleArc = .init(arc.center, arc.radius, arc.start_angle, arc.end_angle);
                while (points.next()) |item| try filter.push(item);
            },
            .arc => |arc| {
                var points: EllipseArc = .init(from, arc.radius, arc.rotation, arc.large_arc, arc.sweep, arc.end, flattening.tolerance);
                while (points.next()) |item| try filter.push(item);
            },
            .quadratic => |curve| try pushBezier(&filter, flattening, &.{ from, curve.control, curve.end }),
            .cubic => |curve| try pushBezier(&filter, flattening, &.{ from, curve.control_0, curve.control_1, curve.end }),
        }
    }
    try filter.finish();
}

/// At least as many points as `forEachPoint` emits, without trigonometry for lines and circle
/// arcs. For a valid contour.
pub fn maxPointCount(contour: Contour, tolerance: f32) usize {
    var count: usize = 1;
    var previous_end = contour.start;
    var previous_circle_arc: ?@FieldType(Segment, "circle_arc") = null;
    for (contour.segments) |segment| {
        if (segment == .circle_arc) {
            const arc = segment.circle_arc;
            count += CircleArc.maxPointCount(arc.radius, arc.start_angle, arc.end_angle);
            previous_circle_arc = arc;
            continue;
        }
        const from: Point = if (previous_circle_arc) |arc| .{ arc.center[0] + arc.radius * @cos(arc.end_angle), arc.center[1] + arc.radius * @sin(arc.end_angle) } else previous_end;
        previous_circle_arc = null;
        switch (segment) {
            .circle_arc => unreachable,
            .line => |end| {
                count += 1;
                previous_end = end;
            },
            .arc => |arc| {
                const points: EllipseArc = .init(from, arc.radius, arc.rotation, arc.large_arc, arc.sweep, arc.end, tolerance);
                count += points.step_count + 1;
                previous_end = arc.end;
            },
            .quadratic => |curve| {
                count += UniformBezier.init(&.{ from, curve.control, curve.end }, tolerance).step_count;
                previous_end = curve.end;
            },
            .cubic => |curve| {
                count += UniformBezier.init(&.{ from, curve.control_0, curve.control_1, curve.end }, tolerance).step_count;
                previous_end = curve.end;
            },
        }
    }
    return count;
}

/// Holds back the newest raw point until the one after it is known.
fn Filter(comptime Context: type, comptime emit: fn (Context, Point) Error!void) type {
    return struct {
        context: Context,
        start: Point,
        drop_closing_point: bool,
        pending: ?Item = null,
        last_emitted: ?Point = null,

        inline fn push(filter: *@This(), item: Item) Error!void {
            if (filter.pending) |pending| try filter.decide(pending, item.point, false);
            filter.pending = item;
        }

        fn finish(filter: *@This()) Error!void {
            const pending = filter.pending orelse return;
            try filter.decide(pending, if (filter.drop_closing_point) filter.start else null, true);
        }

        inline fn decide(filter: *@This(), item: Item, following: ?Point, is_last: bool) Error!void {
            if (filter.last_emitted) |last| {
                if (equal(item.point, last)) return;
                if (is_last and filter.drop_closing_point and equal(item.point, filter.start)) return;
            }
            if (item.mergeable) if (following) |point| if (distanceSquared(item.point, point) < circle_arc_end_merge * circle_arc_end_merge) return;
            filter.last_emitted = item.point;
            try emit(filter.context, item.point);
        }
    };
}

fn pushBezier(filter: anytype, flattening: Flattening, controls: []const Point) Error!void {
    const scratch = flattening.bezier_scratch orelse {
        var points: UniformBezier = .init(controls, flattening.tolerance);
        while (points.next()) |item| try filter.push(item);
        return;
    };
    var tvg_controls: [4]mesh.tvg.Point = undefined;
    for (tvg_controls[0..controls.len], controls) |*tvg_control, control| tvg_control.* = .{ .x = control[0], .y = control[1] };
    var flat: std.ArrayList(mesh.tvg.Point) = .empty;
    defer flat.deinit(scratch.allocator);
    try arrangement.flattenBezier(scratch.allocator, tvg_controls[0..controls.len], flattening.tolerance, scratch.max_points, &flat);
    assert(flat.items.len > 0);
    var previous: Vector = controls[0];
    // Inner points move half the local chord sag outward, so the polyline straddles the curve.
    for (flat.items[0 .. flat.items.len - 1], flat.items[1..]) |point, following| {
        const here: Vector = .{ point.x, point.y };
        const middle = (previous + Vector{ following.x, following.y }) * @as(Vector, @splat(0.5));
        try filter.push(.{ .point = here + (here - middle) / @as(Vector, @splat(8)) });
        previous = here;
    }
    const end = flat.items[flat.items.len - 1];
    try filter.push(.{ .point = .{ end.x, end.y } });
}

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

    /// Start, end and the multiples of the step strictly between, which lie above the end angle
    /// minus 0.001. The step is bounded from below by acos(x) >= sqrt(2 (1 - x)), with a margin
    /// for the rounding of x near 1.
    fn maxPointCount(radius: f32, start_angle: f32, end_angle: f32) usize {
        if (radius == 0) return 1;
        const smallest_step = 0.99 * @sqrt(2 * circle_arc_tolerance / (radius + circle_arc_tolerance));
        const grid_points = @floor((start_angle - end_angle + 0.001) / smallest_step) + 1;
        assert(grid_points < 1 << 30);
        return 2 + @as(usize, @intFromFloat(@max(0, grid_points)));
    }

    fn pointAt(arc: *const CircleArc, angle: f32) Point {
        return .{ arc.center[0] + arc.radius * @cos(angle), arc.center[1] + arc.radius * @sin(angle) };
    }

    fn next(arc: *CircleArc) ?Item {
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

/// SVG endpoint arc in center form, uniform angle steps within half the tolerance.
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

        // Half the tolerance as in dvui's former flattenArc, circles looked faceted at the full one.
        const sag = tolerance / 2;
        const largest_radius = @max(radius[0], radius[1]);
        const max_step = if (largest_radius <= sag) math.pi / 2.0 else 2 * math.acos(1 - sag / largest_radius);
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

    fn next(arc: *EllipseArc) ?Item {
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

    fn next(bezier: *UniformBezier) ?Item {
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
