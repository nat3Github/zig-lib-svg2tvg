//! Outline of a round-joined, round-capped stroke: the Minkowski sum of a polyline with a disc,
//! as contours for one nonzero fill.

const std = @import("std");
const math = std.math;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const tessellation = @import("tessellation.zig");
const Point = tessellation.Point;
const Vector = @Vector(2, f32);

/// How far an outer miter point may lie outside the join circle, in the units of the polyline.
const miter_overshoot = 0.03;

/// Contours whose nonzero fill is the union of the edge quads and vertex discs of `polyline`
/// stroked with `radius`, so one fill gives the stroke a single anti-aliased edge. A polyline whose
/// last point repeats its first is closed. Join and cap arcs stay within `tolerance` of the circle.
/// Appends to `points`, which grows to at most `max_points`, and one past the last point of every
/// contour to `ends`. Coordinates and the radius beyond `tessellation.max_coordinate_pixels` are
/// invalid input.
pub fn strokeOutline(
    allocator: Allocator,
    polyline: []const Point,
    radius: f32,
    tolerance: f32,
    max_points: usize,
    points: *std.ArrayList(Point),
    ends: *std.ArrayList(usize),
) tessellation.Error!void {
    assert(tolerance > 0);
    if (!(radius > 0 and radius <= tessellation.max_coordinate_pixels)) return error.InvalidInput;
    for (polyline) |point| for (point) |coordinate| {
        if (!(@abs(coordinate) <= tessellation.max_coordinate_pixels)) return error.InvalidInput;
    };
    if (polyline.len == 0) return;

    var distinct: std.ArrayList(Vector) = try .initCapacity(allocator, polyline.len);
    defer distinct.deinit(allocator);
    for (polyline) |point| {
        if (distinct.items.len == 0 or !nearlyEqual(distinct.getLast(), point)) distinct.appendAssumeCapacity(point);
    }
    while (distinct.items.len > 1 and nearlyEqual(distinct.items[0], distinct.getLast())) _ = distinct.pop();
    const closed = distinct.items.len > 2 and nearlyEqual(distinct.items[0], polyline[polyline.len - 1]);
    const pts = distinct.items;

    var outliner: Outliner = .{ .allocator = allocator, .radius = radius, .tolerance = tolerance, .max_points = max_points, .points = points, .ends = ends };
    if (pts.len == 1) {
        const right: Vector = .{ radius, 0 };
        try outliner.append(pts[0] + right);
        try outliner.appendArc(pts[0], right, -2 * math.pi);
        _ = points.pop();
    } else if (closed) {
        for (0..pts.len) |i| try outliner.appendJoin(pts[(i + pts.len - 1) % pts.len], pts[i], pts[(i + 1) % pts.len]);
        try outliner.endContour();
        var i = pts.len;
        while (i > 0) : (i -= 1) try outliner.appendJoin(pts[i % pts.len], pts[i - 1], pts[(i + pts.len - 2) % pts.len]);
    } else {
        const last = pts.len - 1;
        try outliner.append(pts[0] + outliner.leftOffset(pts[0], pts[1]));
        for (1..last) |i| try outliner.appendJoin(pts[i - 1], pts[i], pts[i + 1]);
        const end_offset = outliner.leftOffset(pts[last - 1], pts[last]);
        try outliner.append(pts[last] + end_offset);
        try outliner.appendArc(pts[last], end_offset, -math.pi);
        var i = last;
        while (i > 1) : (i -= 1) try outliner.appendJoin(pts[i], pts[i - 1], pts[i - 2]);
        const start_offset = outliner.leftOffset(pts[1], pts[0]);
        try outliner.append(pts[0] + start_offset);
        try outliner.appendArc(pts[0], start_offset, -math.pi);
        _ = points.pop();
    }
    try outliner.endContour();
}

const Outliner = struct {
    allocator: Allocator,
    radius: f32,
    tolerance: f32,
    max_points: usize,
    points: *std.ArrayList(Point),
    ends: *std.ArrayList(usize),

    fn append(outliner: *Outliner, point: Vector) error{ OutOfMemory, TooComplex }!void {
        if (outliner.points.items.len >= outliner.max_points) return error.TooComplex;
        try outliner.points.append(outliner.allocator, point);
    }

    fn endContour(outliner: *Outliner) error{OutOfMemory}!void {
        assert(outliner.ends.items.len == 0 or outliner.ends.getLast() < outliner.points.items.len);
        try outliner.ends.append(outliner.allocator, outliner.points.items.len);
    }

    /// Left offset at `vertex` from the edge `previous`-`vertex` to the edge `vertex`-`next`. Outlines
    /// run clockwise on screen. A join takes the miter point, where both offset lines cross, when it
    /// stays inside both edge quads (inner side) or less than `miter_overshoot` outside the circle (outer side).
    /// Otherwise an outer join is a clockwise arc and an inner join runs through the vertex.
    fn appendJoin(outliner: *Outliner, previous: Vector, vertex: Vector, next: Vector) error{ OutOfMemory, TooComplex }!void {
        const incoming = vertex - previous;
        const outgoing = next - vertex;
        const incoming_offset = outliner.leftOffset(previous, vertex);
        const outgoing_offset = outliner.leftOffset(vertex, next);
        var turn = math.atan2(incoming[0] * outgoing[1] - incoming[1] * outgoing[0], @reduce(.Add, incoming * outgoing));
        if (@abs(turn) > math.pi - 1e-3) turn = -math.pi;
        const half_turn_cos = @cos(turn / 2);
        const miter_fits = if (turn < 0)
            outliner.radius * (1 - half_turn_cos) <= miter_overshoot * half_turn_cos
        else
            turn < math.pi / 2.0 and outliner.radius * @tan(turn / 2) <= @min(length(incoming), length(outgoing));
        if (miter_fits) {
            const miter_scale: Vector = @splat(1 / (1 + @cos(turn)));
            try outliner.append(vertex + (incoming_offset + outgoing_offset) * miter_scale);
            return;
        }
        try outliner.append(vertex + incoming_offset);
        if (turn < 0) {
            try outliner.appendArc(vertex, incoming_offset, turn);
        } else {
            try outliner.append(vertex);
            try outliner.append(vertex + outgoing_offset);
        }
    }

    /// Points after `center + start_offset` along the circle through it, turned by `sweep` radians.
    fn appendArc(outliner: *Outliner, center: Vector, start_offset: Vector, sweep: f32) error{ OutOfMemory, TooComplex }!void {
        assert(math.isFinite(sweep) and @abs(sweep) <= 2 * math.pi);
        const max_step = if (outliner.radius <= outliner.tolerance) math.pi / 2.0 else 2 * math.acos(1 - outliner.tolerance / outliner.radius);
        const steps: usize = @intFromFloat(@min(1024, @max(1, @ceil(@abs(sweep) / max_step))));
        // Inner points move out by half the sag, so the chords straddle the circle.
        const outward_reach = 2 / (1 + @cos(sweep / @as(f32, @floatFromInt(steps)) / 2));
        for (1..steps + 1) |step| {
            const angle = sweep * @as(f32, @floatFromInt(step)) / @as(f32, @floatFromInt(steps));
            const reach = if (step == steps) 1 else outward_reach;
            const cos = @cos(angle) * reach;
            const sin = @sin(angle) * reach;
            try outliner.append(center + Vector{ start_offset[0] * cos - start_offset[1] * sin, start_offset[0] * sin + start_offset[1] * cos });
        }
    }

    fn leftOffset(outliner: *const Outliner, from: Vector, to: Vector) Vector {
        const direction = to - from;
        const direction_length = length(direction);
        assert(direction_length > 0);
        return Vector{ -direction[1], direction[0] } * @as(Vector, @splat(outliner.radius / direction_length));
    }
};

fn length(vector: Vector) f32 {
    return @sqrt(@reduce(.Add, vector * vector));
}

fn nearlyEqual(a: Vector, b: Vector) bool {
    const difference = a - b;
    return @reduce(.Add, difference * difference) < 1e-6;
}
