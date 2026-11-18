//! Strokes of polylines, two ways. `strokeOutline` gives contours whose nonzero fill is exactly the
//! stroke, so every pixel is covered once. `strip` writes the mesh directly, faster, but its pieces
//! overlap, which only an opaque stroke hides.

const std = @import("std");
const math = std.math;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const tessellation = @import("tessellation.zig");
const mesh = @import("mesh.zig");
const Point = tessellation.Point;
const Vector = @Vector(2, f32);

/// How far an outer miter point may lie outside the join circle, in the units of the polyline.
const miter_overshoot = 0.03;
const max_arc_steps = 1024;

/// Width, miter limit and points within `tessellation.max_coordinate_pixels`, else invalid input.
pub fn validate(polyline: []const Point, options: mesh.StrokeOptions) tessellation.Error!void {
    if (!(options.width > 0 and options.width / 2 <= tessellation.max_coordinate_pixels)) return error.InvalidInput;
    if (!(options.miter_limit >= 1 and options.miter_limit <= tessellation.max_coordinate_pixels)) return error.InvalidInput;
    for (polyline) |point| for (point) |coordinate| {
        if (!(@abs(coordinate) <= tessellation.max_coordinate_pixels)) return error.InvalidInput;
    };
}

/// Points of `polyline` without near repeats, and for a closed one without a last point on the
/// first. Owned by `allocator`.
fn distinctPoints(allocator: Allocator, polyline: []const Point, closed: bool) error{OutOfMemory}![]Vector {
    var distinct: std.ArrayList(Vector) = try .initCapacity(allocator, polyline.len);
    for (polyline) |point| {
        if (distinct.items.len == 0 or !nearlyEqual(distinct.getLast(), point)) distinct.appendAssumeCapacity(point);
    }
    if (closed) while (distinct.items.len > 1 and nearlyEqual(distinct.items[0], distinct.getLast())) {
        _ = distinct.pop();
    };
    return distinct.toOwnedSlice(allocator);
}

/// Contours whose nonzero fill is the stroke of `polyline` in the style of `options`: edge quads,
/// join wedges and caps as SVG draws them. A single point is a disc for round caps, a square for
/// square caps and nothing for butt caps. Join and cap arcs stay within `options.tolerance` of the
/// circle. Appends to `points`, which grows to at most `max_points`, and one past the last point
/// of every contour to `ends`.
pub fn strokeOutline(
    allocator: Allocator,
    polyline: []const Point,
    closed: bool,
    options: mesh.StrokeOptions,
    max_points: usize,
    points: *std.ArrayList(Point),
    ends: *std.ArrayList(usize),
) tessellation.Error!void {
    assert(options.tolerance > 0);
    try validate(polyline, options);
    if (polyline.len == 0) return;

    const pts = try distinctPoints(allocator, polyline, closed);
    defer allocator.free(pts);
    const radius = options.width / 2;
    var outliner: Outliner = .{ .allocator = allocator, .radius = radius, .options = options, .max_points = max_points, .points = points, .ends = ends };
    if (pts.len == 1) {
        switch (options.cap) {
            .butt => return,
            .round => {
                const right: Vector = .{ radius, 0 };
                try outliner.append(pts[0] + right);
                try outliner.appendArc(pts[0], right, -2 * math.pi);
                _ = points.pop();
            },
            .square => for ([_]Vector{ .{ 1, 1 }, .{ 1, -1 }, .{ -1, -1 }, .{ -1, 1 } }) |corner| {
                try outliner.append(pts[0] + corner * @as(Vector, @splat(radius)));
            },
        }
    } else if (closed) {
        for (0..pts.len) |i| try outliner.appendJoin(pts[(i + pts.len - 1) % pts.len], pts[i], pts[(i + 1) % pts.len]);
        try outliner.endContour();
        var i = pts.len;
        while (i > 0) : (i -= 1) try outliner.appendJoin(pts[i % pts.len], pts[i - 1], pts[(i + pts.len - 2) % pts.len]);
    } else {
        const last = pts.len - 1;
        for (1..last) |i| try outliner.appendJoin(pts[i - 1], pts[i], pts[i + 1]);
        try outliner.appendCap(pts[last], outliner.leftOffset(pts[last - 1], pts[last]));
        var i = last;
        while (i > 1) : (i -= 1) try outliner.appendJoin(pts[i], pts[i - 1], pts[i - 2]);
        try outliner.appendCap(pts[0], outliner.leftOffset(pts[1], pts[0]));
    }
    try outliner.endContour();
}

const Outliner = struct {
    allocator: Allocator,
    radius: f32,
    options: mesh.StrokeOptions,
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
    /// stays inside both edge quads (inner side), or on the outer side within the miter limit of a
    /// miter join or less than `miter_overshoot` outside the circle of a round join. Otherwise an
    /// outer join is a clockwise arc or a bevel and an inner join runs through the vertex.
    fn appendJoin(outliner: *Outliner, previous: Vector, vertex: Vector, next: Vector) error{ OutOfMemory, TooComplex }!void {
        const incoming = vertex - previous;
        const outgoing = next - vertex;
        const incoming_offset = outliner.leftOffset(previous, vertex);
        const outgoing_offset = outliner.leftOffset(vertex, next);
        const turn = turnAngle(incoming, outgoing);
        const half_turn_cos = @cos(turn / 2);
        const miter_fits = if (turn < 0) switch (outliner.options.join) {
            .round => outliner.radius * (1 - half_turn_cos) <= miter_overshoot * half_turn_cos,
            .miter => half_turn_cos * outliner.options.miter_limit >= 1,
            .bevel => false,
        } else turn < math.pi / 2.0 and outliner.radius * @tan(turn / 2) <= @min(length(incoming), length(outgoing));
        if (miter_fits) {
            const miter_scale: Vector = @splat(1 / (1 + @cos(turn)));
            try outliner.append(vertex + (incoming_offset + outgoing_offset) * miter_scale);
            return;
        }
        try outliner.append(vertex + incoming_offset);
        if (turn < 0) {
            if (outliner.options.join == .round) try outliner.appendArc(vertex, incoming_offset, turn) else try outliner.append(vertex + outgoing_offset);
        } else {
            try outliner.append(vertex);
            try outliner.append(vertex + outgoing_offset);
        }
    }

    /// From `center + offset` around the end to `center - offset`, `offset` being the left offset
    /// of the last edge.
    fn appendCap(outliner: *Outliner, center: Vector, offset: Vector) error{ OutOfMemory, TooComplex }!void {
        switch (outliner.options.cap) {
            .butt => {
                try outliner.append(center + offset);
                try outliner.append(center - offset);
            },
            .round => {
                try outliner.append(center + offset);
                try outliner.appendArc(center, offset, -math.pi);
            },
            .square => {
                const extension: Vector = .{ offset[1], -offset[0] };
                try outliner.append(center + offset + extension);
                try outliner.append(center - offset + extension);
            },
        }
    }

    /// Points after `center + start_offset` along the circle through it, turned by `sweep` radians.
    fn appendArc(outliner: *Outliner, center: Vector, start_offset: Vector, sweep: f32) error{ OutOfMemory, TooComplex }!void {
        assert(math.isFinite(sweep) and @abs(sweep) <= 2 * math.pi);
        const steps = arcSteps(outliner.radius, outliner.options.tolerance, sweep);
        // Inner points move out by half the sag, so the chords straddle the circle.
        const outward_reach = 2 / (1 + @cos(sweep / @as(f32, @floatFromInt(steps)) / 2));
        for (1..steps + 1) |step| {
            const angle = sweep * @as(f32, @floatFromInt(step)) / @as(f32, @floatFromInt(steps));
            const reach = if (step == steps) 1 else outward_reach;
            try outliner.append(center + rotate(start_offset, angle) * @as(Vector, @splat(reach)));
        }
    }

    fn leftOffset(outliner: *const Outliner, from: Vector, to: Vector) Vector {
        return leftNormal(from, to) * @as(Vector, @splat(outliner.radius));
    }
};

/// Steps that keep the chords of an arc with `radius` within `tolerance` of it.
fn arcSteps(radius: f32, tolerance: f32, sweep: f32) usize {
    const max_step = if (radius <= tolerance) math.pi / 2.0 else 2 * math.acos(1 - tolerance / radius);
    return @intFromFloat(@min(max_arc_steps, @max(1, @ceil(@abs(sweep) / max_step))));
}

/// Signed turn from `incoming` to `outgoing`, a reversal counts as -π.
fn turnAngle(incoming: Vector, outgoing: Vector) f32 {
    const turn = math.atan2(incoming[0] * outgoing[1] - incoming[1] * outgoing[0], @reduce(.Add, incoming * outgoing));
    return if (@abs(turn) > math.pi - 1e-3) -math.pi else turn;
}

/// The mesh of an opaque stroke written directly: one station of core and fade vertices across
/// every point, quads between stations, and fans for wide joins and round caps. Overlapping pieces
/// cover a pixel twice, so `options.translucent` strokes take the outline. Stations at gentle
/// joins are shared by both edges; where the inner side would fold back the strip splits and a
/// patch fills the outer wedge. The fade is centered on the edge, strokes thinner than it get
/// core coverage width / fade.
pub fn strip(
    allocator: Allocator,
    polylines: []const []const Point,
    closed: []const bool,
    options: mesh.StrokeOptions,
) tessellation.Error!mesh.Mesh {
    assert(polylines.len == closed.len);
    assert(options.tolerance > 0);
    for (polylines) |polyline| try validate(polyline, options);

    const radius = options.width / 2;
    const fade: f32 = if (options.anti_alias) 1 else 0;
    var builder: StripBuilder = .{
        .allocator = allocator,
        .core = @max(0, radius - fade / 2),
        .outer = radius + fade / 2,
        .core_coverage = if (fade > 0) @min(1, options.width / fade) else 1,
        .has_fade = fade > 0,
        .radius = radius,
        .options = options,
        .max_vertices = options.limits.max_edges,
    };
    defer builder.deinit();
    var point_total: usize = 0;
    for (polylines) |polyline| point_total += polyline.len;
    try builder.positions.ensureTotalCapacity(allocator, point_total * 4 + 16);
    try builder.coverages.ensureTotalCapacity(allocator, point_total * 4 + 16);
    try builder.indices.ensureTotalCapacity(allocator, point_total * 18 + 48);

    for (polylines, closed) |polyline, is_closed| {
        const pts = try distinctPoints(allocator, polyline, is_closed);
        defer allocator.free(pts);
        if (pts.len == 0) continue;
        if (pts.len == 1) {
            if (options.cap == .butt) continue;
            const one_point = [2]Vector{ pts[0], pts[0] };
            try builder.openPolyline(&one_point, &.{.{ 1, 0 }}, &.{0});
            continue;
        }
        const segment_count = if (is_closed) pts.len else pts.len - 1;
        const directions = try allocator.alloc(Vector, segment_count);
        defer allocator.free(directions);
        const lengths = try allocator.alloc(f32, segment_count);
        defer allocator.free(lengths);
        for (directions, lengths, 0..) |*direction, *segment_length, i| {
            const chord = pts[(i + 1) % pts.len] - pts[i];
            segment_length.* = length(chord);
            assert(segment_length.* > 0);
            direction.* = chord / @as(Vector, @splat(segment_length.*));
        }
        if (is_closed) try builder.closedPolyline(pts, directions, lengths) else try builder.openPolyline(pts, directions, lengths);
    }
    const positions = try builder.positions.toOwnedSlice(allocator);
    errdefer allocator.free(positions);
    const coverages = try builder.coverages.toOwnedSlice(allocator);
    errdefer allocator.free(coverages);
    return .{ .positions = positions, .coverages = coverages, .indices = try builder.indices.toOwnedSlice(allocator) };
}

/// Vertices across a point from the right fade edge over the right and left core edges to the left
/// fade edge. Without fade the fade entries repeat the core ones.
const Station = [4]u32;

const StripBuilder = struct {
    allocator: Allocator,
    /// Half width of full coverage and of the fade's outer edge.
    core: f32,
    outer: f32,
    core_coverage: f32,
    has_fade: bool,
    radius: f32,
    options: mesh.StrokeOptions,
    max_vertices: usize,
    positions: std.ArrayList(Point) = .empty,
    coverages: std.ArrayList(f32) = .empty,
    indices: std.ArrayList(u32) = .empty,

    fn deinit(builder: *StripBuilder) void {
        builder.positions.deinit(builder.allocator);
        builder.coverages.deinit(builder.allocator);
        builder.indices.deinit(builder.allocator);
    }

    fn reserveVertices(builder: *StripBuilder, count: usize) error{ OutOfMemory, TooComplex }!void {
        if (builder.positions.items.len + count > builder.max_vertices) return error.TooComplex;
        try builder.positions.ensureUnusedCapacity(builder.allocator, count);
        try builder.coverages.ensureUnusedCapacity(builder.allocator, count);
    }

    fn vertex(builder: *StripBuilder, position: Vector, coverage: f32) error{ OutOfMemory, TooComplex }!u32 {
        try builder.reserveVertices(1);
        return builder.vertexAssumeCapacity(position, coverage);
    }

    fn vertexAssumeCapacity(builder: *StripBuilder, position: Vector, coverage: f32) u32 {
        builder.positions.appendAssumeCapacity(position);
        builder.coverages.appendAssumeCapacity(coverage);
        return @intCast(builder.positions.items.len - 1);
    }

    /// Counterclockwise on screen whatever the order, slivers of no area left out.
    fn triangle(builder: *StripBuilder, a: u32, b: u32, c: u32) error{OutOfMemory}!void {
        try builder.indices.ensureUnusedCapacity(builder.allocator, 3);
        builder.triangleAssumeCapacity(a, b, c);
    }

    fn triangleAssumeCapacity(builder: *StripBuilder, a: u32, b: u32, c: u32) void {
        const positions = builder.positions.items;
        const pa: Vector = positions[a];
        const ab = @as(Vector, positions[b]) - pa;
        const ac = @as(Vector, positions[c]) - pa;
        const cross = ab[0] * ac[1] - ab[1] * ac[0];
        if (!(@abs(cross) > 1e-6)) return;
        builder.indices.appendSliceAssumeCapacity(if (cross < 0) &.{ a, b, c } else &.{ a, c, b });
    }

    fn quad(builder: *StripBuilder, a: u32, b: u32, c: u32, d: u32) error{OutOfMemory}!void {
        try builder.triangle(a, b, c);
        try builder.triangle(a, c, d);
    }

    /// Core vertices `core_offset` and fade vertices `outer_offset` to the left of `center`, the
    /// same to the right. `coverage` scales the core, cap stations fade out with 0.
    fn station(builder: *StripBuilder, center: Vector, normal: Vector, core_scale: f32, outer_scale: f32, coverage: f32) error{ OutOfMemory, TooComplex }!Station {
        try builder.reserveVertices(4);
        const core_offset = normal * @as(Vector, @splat(core_scale));
        const right_core = builder.vertexAssumeCapacity(center - core_offset, coverage);
        const left_core = builder.vertexAssumeCapacity(center + core_offset, coverage);
        if (!builder.has_fade) return .{ right_core, right_core, left_core, left_core };
        const outer_offset = normal * @as(Vector, @splat(outer_scale));
        return .{ builder.vertexAssumeCapacity(center - outer_offset, 0), right_core, left_core, builder.vertexAssumeCapacity(center + outer_offset, 0) };
    }

    fn plainStation(builder: *StripBuilder, center: Vector, normal: Vector, coverage: f32) error{ OutOfMemory, TooComplex }!Station {
        return builder.station(center, normal, builder.core, builder.outer, coverage);
    }

    fn connect(builder: *StripBuilder, from: Station, to: Station) error{OutOfMemory}!void {
        try builder.indices.ensureUnusedCapacity(builder.allocator, 18);
        const sides = [_]usize{ 1, 0, 2 };
        for (sides[0..if (builder.has_fade) 3 else 1]) |side| {
            builder.triangleAssumeCapacity(from[side], from[side + 1], to[side + 1]);
            builder.triangleAssumeCapacity(from[side], to[side + 1], to[side]);
        }
    }

    /// Fan around `center` from the core and fade vertices of `first` on one side to those of
    /// `last`, through `ring` offsets of unit length (scaled by core and outer) between them.
    fn fan(builder: *StripBuilder, center: Vector, first: [2]u32, last: [2]u32, ring: RingOffsets) error{ OutOfMemory, TooComplex }!void {
        const middle = try builder.vertex(center, builder.core_coverage);
        var previous = first;
        var iterator = ring;
        while (iterator.next()) |offset| {
            const current: [2]u32 = .{
                try builder.vertex(center + offset * @as(Vector, @splat(builder.core)), builder.core_coverage),
                if (builder.has_fade) try builder.vertex(center + offset * @as(Vector, @splat(builder.outer)), 0) else undefined,
            };
            try builder.fanPiece(middle, previous, current);
            previous = current;
        }
        try builder.fanPiece(middle, previous, last);
    }

    fn fanPiece(builder: *StripBuilder, middle: u32, from: [2]u32, to: [2]u32) error{OutOfMemory}!void {
        try builder.triangle(middle, from[0], to[0]);
        if (builder.has_fade) try builder.quad(from[0], from[1], to[1], to[0]);
    }

    fn openPolyline(builder: *StripBuilder, pts: []const Vector, directions: []const Vector, lengths: []const f32) error{ OutOfMemory, TooComplex }!void {
        assert(pts.len == directions.len + 1 and lengths.len == directions.len);
        var current = try builder.startCap(pts[0], directions[0]);
        for (1..pts.len - 1) |i| {
            const stations = try builder.join(pts[i], directions[i - 1], directions[i], lengths[i - 1], lengths[i]);
            try builder.connect(current, stations.incoming);
            current = stations.outgoing;
        }
        const end = try builder.endCap(pts[pts.len - 1], directions[directions.len - 1]);
        try builder.connect(current, end);
    }

    fn closedPolyline(builder: *StripBuilder, pts: []const Vector, directions: []const Vector, lengths: []const f32) error{ OutOfMemory, TooComplex }!void {
        assert(pts.len == directions.len and pts.len >= 2);
        const count = pts.len;
        const first = try builder.join(pts[0], directions[count - 1], directions[0], lengths[count - 1], lengths[0]);
        var current = first.outgoing;
        for (1..count) |i| {
            const stations = try builder.join(pts[i], directions[i - 1], directions[i], lengths[i - 1], lengths[i]);
            try builder.connect(current, stations.incoming);
            current = stations.outgoing;
        }
        try builder.connect(current, first.incoming);
    }

    /// The station the first edge starts from, after the cap behind `point`.
    fn startCap(builder: *StripBuilder, point: Vector, direction: Vector) error{ OutOfMemory, TooComplex }!Station {
        const cap_station = try builder.endCap(point, -direction);
        return .{ cap_station[3], cap_station[2], cap_station[1], cap_station[0] };
    }

    /// The station the last edge ends at, with the cap beyond `point` in `direction`.
    fn endCap(builder: *StripBuilder, point: Vector, direction: Vector) error{ OutOfMemory, TooComplex }!Station {
        const normal: Vector = .{ -direction[1], direction[0] };
        if (builder.options.cap == .round) {
            const end = try builder.plainStation(point, normal, builder.core_coverage);
            try builder.fan(point, .{ end[2], end[3] }, .{ end[1], end[0] }, .init(normal, -math.pi, builder.radius, builder.options.tolerance));
            return end;
        }
        const extension: f32 = if (builder.options.cap == .square) builder.radius else 0;
        const half_fade = (builder.outer - builder.radius);
        const end = try builder.plainStation(point + direction * @as(Vector, @splat(extension - half_fade)), normal, builder.core_coverage);
        if (!builder.has_fade) return end;
        const beyond = try builder.plainStation(point + direction * @as(Vector, @splat(extension + half_fade)), normal, 0);
        try builder.connect(end, beyond);
        return end;
    }

    const Join = struct { incoming: Station, outgoing: Station };

    fn join(builder: *StripBuilder, point: Vector, incoming: Vector, outgoing: Vector, incoming_length: f32, outgoing_length: f32) error{ OutOfMemory, TooComplex }!Join {
        const dot = @reduce(.Add, incoming * outgoing);
        const cross = incoming[0] * outgoing[1] - incoming[1] * outgoing[0];
        // Within 1e-3 of a reversal counts as one, as in `turnAngle`.
        const reversal = dot < -0.9999995;
        const half_turn_cos: f32 = if (reversal) 0 else @sqrt(@max(0, (1 + dot) / 2));
        const half_turn_sin = @sqrt(@max(0, (1 - dot) / 2));
        const incoming_normal: Vector = .{ -incoming[1], incoming[0] };
        const outgoing_normal: Vector = .{ -outgoing[1], outgoing[0] };
        const outer_fits = half_turn_cos > 0 and switch (builder.options.join) {
            .miter => half_turn_cos * builder.options.miter_limit >= 1,
            .round => builder.radius * (1 - half_turn_cos) <= builder.options.tolerance * half_turn_cos,
            .bevel => builder.radius * (1 - half_turn_cos * half_turn_cos) <= builder.options.tolerance * half_turn_cos,
        };
        // The inner miter moves back along both edges, half an edge each leaves room for the other end.
        const inner_fits = half_turn_cos > 0 and
            builder.outer * half_turn_sin <= 0.5 * @min(incoming_length, outgoing_length) * half_turn_cos;
        // Scaled to the miter: an offset of 1 along it lies 1 from both edges.
        const miter = (incoming_normal + outgoing_normal) * @as(Vector, @splat(if (reversal) 0 else 1 / (1 + dot)));
        if (outer_fits and inner_fits) {
            const shared = try builder.plainStation(point, miter, builder.core_coverage);
            return .{ .incoming = shared, .outgoing = shared };
        }
        const incoming_station = try builder.plainStation(point, incoming_normal, builder.core_coverage);
        const outgoing_station = try builder.plainStation(point, outgoing_normal, builder.core_coverage);
        // A right turn on screen has its outer side on the left, as in the outline.
        const left_outer = reversal or cross < 0;
        const turn: f32 = if (reversal) -math.pi else math.atan2(cross, dot);
        const first: [2]u32 = if (left_outer) .{ incoming_station[2], incoming_station[3] } else .{ incoming_station[1], incoming_station[0] };
        const last: [2]u32 = if (left_outer) .{ outgoing_station[2], outgoing_station[3] } else .{ outgoing_station[1], outgoing_station[0] };
        const side: Vector = @splat(if (left_outer) 1 else -1);
        const ring: RingOffsets = switch (builder.options.join) {
            .round => .init(incoming_normal * side, turn, builder.radius, builder.options.tolerance),
            .bevel => .none,
            .miter => if (outer_fits) .{ .single = miter * side } else .none,
        };
        try builder.fan(point, first, last, ring);
        return .{ .incoming = incoming_station, .outgoing = outgoing_station };
    }
};

/// Unit offsets strictly between the ends of a fan.
const RingOffsets = union(enum) {
    none,
    single: ?Vector,
    arc: struct { start: Vector, sweep: f32, steps: usize, step: usize = 1 },

    fn init(start: Vector, sweep: f32, radius: f32, tolerance: f32) RingOffsets {
        return .{ .arc = .{ .start = start, .sweep = sweep, .steps = arcSteps(radius, tolerance, sweep) } };
    }

    fn next(ring: *RingOffsets) ?Vector {
        switch (ring.*) {
            .none => return null,
            .single => |*offset| {
                defer offset.* = null;
                return offset.*;
            },
            .arc => |*arc| {
                if (arc.step >= arc.steps) return null;
                defer arc.step += 1;
                return rotate(arc.start, arc.sweep * @as(f32, @floatFromInt(arc.step)) / @as(f32, @floatFromInt(arc.steps)));
            },
        }
    }
};

fn rotate(vector: Vector, angle: f32) Vector {
    const cos = @cos(angle);
    const sin = @sin(angle);
    return .{ vector[0] * cos - vector[1] * sin, vector[0] * sin + vector[1] * cos };
}

fn leftNormal(from: Vector, to: Vector) Vector {
    const direction = to - from;
    const direction_length = length(direction);
    assert(direction_length > 0);
    return Vector{ -direction[1], direction[0] } / @as(Vector, @splat(direction_length));
}

fn length(vector: Vector) f32 {
    return @sqrt(@reduce(.Add, vector * vector));
}

fn nearlyEqual(a: Vector, b: Vector) bool {
    const difference = a - b;
    return @reduce(.Add, difference * difference) < 1e-6;
}
