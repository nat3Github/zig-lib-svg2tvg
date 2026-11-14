//! Planar arrangement of closed polygons on an integer grid, the first stages of the sweep
//! tessellator: flattening, exact crossings snapped to the grid, the winding on both sides of
//! every edge, and the boundary contours of the filled regions. Every limit returns an error.

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const tvg = @import("tinyvg/tinyvg.zig");
const rendering = @import("tinyvg/rendering.zig");

pub const GridPoint = [2]i32;
pub const FillRule = enum { nonzero, even_odd };
pub const Error = error{ OutOfMemory, TooComplex };

pub const Limits = struct {
    max_edges: usize = 1 << 16,
    /// Pair tests and winding steps, bounds the quadratic parts.
    max_work: usize = 1 << 27,
};

pub fn isFilled(rule: FillRule, winding: i64) bool {
    return switch (rule) {
        .nonzero => winding != 0,
        .even_odd => @mod(winding, 2) != 0,
    };
}

/// Closed polygons, the last point connects back to the first.
pub const Contours = struct {
    points: []const GridPoint,
    /// One past the last point of each contour.
    ends: []const usize,

    pub fn deinit(contours: Contours, allocator: Allocator) void {
        allocator.free(contours.points);
        allocator.free(contours.ends);
    }

    pub fn count(contours: Contours) usize {
        return contours.ends.len;
    }

    pub fn get(contours: Contours, index: usize) []const GridPoint {
        const start = if (index == 0) 0 else contours.ends[index - 1];
        return contours.points[start..contours.ends[index]];
    }
};

/// One contour per segment in grid units of `scale`, rounded like the TinyVG builder rounds.
/// Béziers stay within `tolerance` pixels, arcs take the reference renderer's fixed steps.
pub fn flattenSegments(allocator: Allocator, segments: []const tvg.Path.Segment, scale: tvg.Scale, tolerance: f32, max_points: usize) !Contours {
    assert(tolerance > 0);
    var flattener: Flattener = .{ .allocator = allocator, .scale = scale, .max_points = max_points };
    defer flattener.points.deinit(allocator);
    var ends: std.ArrayList(usize) = .empty;
    defer ends.deinit(allocator);
    for (segments) |segment| {
        try flattener.append(segment.start, 0);
        for (segment.commands) |node| switch (node) {
            .line => |line| try flattener.append(line.data, 0),
            .horiz => |horiz| try flattener.append(.{ .x = horiz.data, .y = flattener.last.y }, 0),
            .vert => |vert| try flattener.append(.{ .x = flattener.last.x, .y = vert.data }, 0),
            .bezier => |bezier| try flattener.appendBezier(&.{ flattener.last, bezier.data.c0, bezier.data.c1, bezier.data.p1 }, tolerance),
            .quadratic_bezier => |bezier| try flattener.appendBezier(&.{ flattener.last, bezier.data.c, bezier.data.p1 }, tolerance),
            .arc_circle => |arc| {
                if (Flattener.nearlyEqual(flattener.last, arc.data.target)) continue;
                if (arc.data.radius == 0) {
                    try flattener.append(arc.data.target, 0);
                } else try rendering.renderCircle(&flattener, flattener.last, arc.data.target, arc.data.radius, arc.data.large_arc, arc.data.sweep, 0, 0);
            },
            .arc_ellipse => |arc| {
                if (Flattener.nearlyEqual(flattener.last, arc.data.target)) continue;
                if (arc.data.radius_x == 0 or arc.data.radius_y == 0) {
                    try flattener.append(arc.data.target, 0);
                } else try rendering.renderEllipse(&flattener, flattener.last, arc.data.target, arc.data.radius_x, arc.data.radius_y, arc.data.rotation, arc.data.large_arc, arc.data.sweep, 0, 0);
            },
            .close => try flattener.append(segment.start, 0),
        };
        try ends.append(allocator, flattener.points.items.len);
    }
    const points = try flattener.points.toOwnedSlice(allocator);
    errdefer allocator.free(points);
    return .{ .points = points, .ends = try ends.toOwnedSlice(allocator) };
}

const Flattener = struct {
    allocator: Allocator,
    scale: tvg.Scale,
    max_points: usize,
    points: std.ArrayList(GridPoint) = .empty,
    last: tvg.Point = undefined,

    /// The point list interface of rendering.renderEllipse, the width is unused.
    pub fn append(flattener: *Flattener, point: tvg.Point, width: f32) !void {
        _ = width;
        if (flattener.points.items.len >= flattener.max_points) return error.TooComplex;
        const x = (try flattener.scale.map(point.x)).raw();
        const y = (try flattener.scale.map(point.y)).raw();
        try flattener.points.append(flattener.allocator, .{ x, y });
        flattener.last = point;
    }

    /// renderPath skips arcs this short.
    fn nearlyEqual(a: tvg.Point, b: tvg.Point) bool {
        return std.math.approxEqAbs(f32, a.x, b.x, 1e-5) and std.math.approxEqAbs(f32, a.y, b.y, 1e-5);
    }

    /// Wang's formula: n segments keep the polyline within `tolerance` of the curve.
    fn appendBezier(flattener: *Flattener, controls: []const tvg.Point, tolerance: f32) !void {
        assert(controls.len == 3 or controls.len == 4);
        var largest_second_difference: f64 = 0;
        for (controls[0 .. controls.len - 2], controls[1 .. controls.len - 1], controls[2..]) |a, b, c| {
            const x = @as(f64, a.x) - 2 * @as(f64, b.x) + @as(f64, c.x);
            const y = @as(f64, a.y) - 2 * @as(f64, b.y) + @as(f64, c.y);
            largest_second_difference = @max(largest_second_difference, std.math.hypot(x, y));
        }
        const degree: f64 = @floatFromInt(controls.len - 1);
        const divisions = @ceil(@sqrt(degree * (degree - 1) / 8 * largest_second_difference / tolerance));
        if (!(divisions <= @as(f64, @floatFromInt(flattener.max_points)))) return error.TooComplex;
        const division_count: usize = @max(1, @as(usize, @intFromFloat(divisions)));
        for (1..division_count) |step| {
            const t = @as(f64, @floatFromInt(step)) / @as(f64, @floatFromInt(division_count));
            var reduced: [4][2]f64 = undefined;
            for (reduced[0..controls.len], controls) |*value, control| value.* = .{ control.x, control.y };
            var remaining = controls.len;
            while (remaining > 1) : (remaining -= 1) {
                for (reduced[0 .. remaining - 1], reduced[1..remaining]) |*value, next| {
                    value.* = .{ value[0] + t * (next[0] - value[0]), value[1] + t * (next[1] - value[1]) };
                }
            }
            try flattener.append(.{ .x = @floatCast(reduced[0][0]), .y = @floatCast(reduced[0][1]) }, 0);
        }
        try flattener.append(controls[controls.len - 1], 0);
    }
};

pub const Edge = struct {
    /// Lower y first, then lower x.
    from: GridPoint,
    to: GridPoint,
    /// Signed count of input edges along from → to.
    multiplicity: i32,
    /// Input contour, meaningful while `Arrangement.input_edges_unchanged`.
    contour: u32,
    /// Winding next to the edge where orientation(from, to, point) > 0.
    winding_positive_side: i64 = 0,
    winding_negative_side: i64 = 0,

    pub fn separates(edge: Edge, rule: FillRule) bool {
        return isFilled(rule, edge.winding_positive_side) != isFilled(rule, edge.winding_negative_side);
    }
};

pub const Arrangement = struct {
    /// Edges meet only at their end points, sorted by `from`.
    edges: []Edge,
    /// Every edge is exactly one input edge: nothing was split, merged or cancelled.
    input_edges_unchanged: bool,

    /// `work` may be shared by several arrangements to bound their total.
    pub fn init(allocator: Allocator, contours: Contours, limits: Limits, work: *Work) Error!Arrangement {
        assert(limits.max_edges <= std.math.maxInt(u32));
        if (contours.points.len > limits.max_edges or contours.count() > limits.max_edges) return error.TooComplex;
        var directed: std.ArrayList(DirectedEdge) = .empty;
        defer directed.deinit(allocator);
        for (0..contours.count()) |contour_index| {
            const points = contours.get(contour_index);
            for (points, 0..) |from, index| {
                const to = points[(index + 1) % points.len];
                if (!pointsEqual(from, to)) try directed.append(allocator, .{ .from = from, .to = to, .contour = @intCast(contour_index) });
            }
        }
        var input_edges_unchanged = true;
        while (try splitRound(allocator, &directed, work, limits.max_edges)) input_edges_unchanged = false;

        var edges: std.ArrayList(Edge) = try .initCapacity(allocator, directed.items.len);
        defer edges.deinit(allocator);
        for (directed.items) |edge| {
            const forward = pointLessThan(edge.from, edge.to);
            edges.appendAssumeCapacity(.{
                .from = if (forward) edge.from else edge.to,
                .to = if (forward) edge.to else edge.from,
                .multiplicity = if (forward) 1 else -1,
                .contour = edge.contour,
            });
        }
        std.mem.sort(Edge, edges.items, {}, edgeLessThan);
        var kept: usize = 0;
        for (edges.items) |edge| {
            if (kept > 0 and pointsEqual(edges.items[kept - 1].from, edge.from) and pointsEqual(edges.items[kept - 1].to, edge.to)) {
                edges.items[kept - 1].multiplicity += edge.multiplicity;
                input_edges_unchanged = false;
                continue;
            }
            edges.items[kept] = edge;
            kept += 1;
        }
        edges.shrinkRetainingCapacity(kept);
        kept = 0;
        for (edges.items) |edge| {
            if (edge.multiplicity == 0) continue;
            edges.items[kept] = edge;
            kept += 1;
        }
        edges.shrinkRetainingCapacity(kept);
        try computeWindings(edges.items, work);
        return .{ .edges = try edges.toOwnedSlice(allocator), .input_edges_unchanged = input_edges_unchanged };
    }

    pub fn deinit(arrangement: Arrangement, allocator: Allocator) void {
        allocator.free(arrangement.edges);
    }

    /// No region where nonzero and even-odd fill differently.
    pub fn fillRulesAgree(arrangement: Arrangement) bool {
        for (arrangement.edges) |edge| {
            for ([_]i64{ edge.winding_positive_side, edge.winding_negative_side }) |winding| {
                if (isFilled(.nonzero, winding) != isFilled(.even_odd, winding)) return false;
            }
        }
        return true;
    }

    /// Closed contours of the edges between filled and unfilled regions, each with the filled
    /// region on its positive side. Filled under even-odd and under nonzero exactly where the
    /// arrangement is filled under `rule`.
    pub fn boundary(arrangement: Arrangement, allocator: Allocator, rule: FillRule) Error!Contours {
        var directed: std.ArrayList([2]GridPoint) = .empty;
        defer directed.deinit(allocator);
        for (arrangement.edges) |edge| {
            const positive_filled = isFilled(rule, edge.winding_positive_side);
            if (positive_filled == isFilled(rule, edge.winding_negative_side)) continue;
            try directed.append(allocator, if (positive_filled) .{ edge.from, edge.to } else .{ edge.to, edge.from });
        }
        std.mem.sort([2]GridPoint, directed.items, {}, directedLessThan);
        const used = try allocator.alloc(bool, directed.items.len);
        defer allocator.free(used);
        @memset(used, false);
        const next_candidate = try allocator.alloc(usize, directed.items.len);
        defer allocator.free(next_candidate);
        for (next_candidate, 0..) |*candidate, index| candidate.* = index;

        var points: std.ArrayList(GridPoint) = .empty;
        defer points.deinit(allocator);
        var ends: std.ArrayList(usize) = .empty;
        defer ends.deinit(allocator);
        for (directed.items, used) |first, *first_used| {
            if (first_used.*) continue;
            first_used.* = true;
            try points.append(allocator, first[0]);
            var current = first[1];
            while (!pointsEqual(current, first[0])) {
                try points.append(allocator, current);
                const group = std.sort.partitionPoint([2]GridPoint, directed.items, current, startsBefore);
                var candidate = next_candidate[group];
                while (candidate < directed.items.len and pointsEqual(directed.items[candidate][0], current) and used[candidate]) candidate += 1;
                // Every vertex has as many boundary edges in as out.
                assert(candidate < directed.items.len and pointsEqual(directed.items[candidate][0], current));
                next_candidate[group] = candidate + 1;
                used[candidate] = true;
                current = directed.items[candidate][1];
            }
            assert(points.items.len - (if (ends.items.len == 0) 0 else ends.getLast()) >= 3);
            try ends.append(allocator, points.items.len);
        }
        const owned_points = try points.toOwnedSlice(allocator);
        errdefer allocator.free(owned_points);
        return .{ .points = owned_points, .ends = try ends.toOwnedSlice(allocator) };
    }
};

pub const Work = struct {
    remaining: usize,

    pub fn init(limits: Limits) Work {
        return .{ .remaining = limits.max_work };
    }

    fn charge(work: *Work) error{TooComplex}!void {
        if (work.remaining == 0) return error.TooComplex;
        work.remaining -= 1;
    }
};

const DirectedEdge = struct { from: GridPoint, to: GridPoint, contour: u32 };
const Split = struct { edge: u32, point: GridPoint };

fn pointsEqual(a: GridPoint, b: GridPoint) bool {
    return a[0] == b[0] and a[1] == b[1];
}

fn pointLessThan(a: GridPoint, b: GridPoint) bool {
    return a[1] < b[1] or (a[1] == b[1] and a[0] < b[0]);
}

fn edgeLessThan(_: void, a: Edge, b: Edge) bool {
    if (!pointsEqual(a.from, b.from)) return pointLessThan(a.from, b.from);
    return pointLessThan(a.to, b.to);
}

fn directedLessThan(_: void, a: [2]GridPoint, b: [2]GridPoint) bool {
    if (!pointsEqual(a[0], b[0])) return pointLessThan(a[0], b[0]);
    return pointLessThan(a[1], b[1]);
}

fn startsBefore(point: GridPoint, edge: [2]GridPoint) bool {
    return pointLessThan(edge[0], point);
}

fn orientation(from: [2]i64, to: [2]i64, point: [2]i64) i128 {
    return @as(i128, to[0] - from[0]) * (point[1] - from[1]) - @as(i128, to[1] - from[1]) * (point[0] - from[0]);
}

fn widen(point: GridPoint) [2]i64 {
    return .{ point[0], point[1] };
}

fn projection(from: GridPoint, to: GridPoint, point: GridPoint) i128 {
    return @as(i128, @as(i64, to[0]) - from[0]) * (@as(i64, point[0]) - from[0]) + @as(i128, @as(i64, to[1]) - from[1]) * (@as(i64, point[1]) - from[1]);
}

fn oppositeSigns(a: i128, b: i128) bool {
    return (a < 0 and b > 0) or (a > 0 and b < 0);
}

/// Splits every edge at the crossings, snapped to the grid, and at the vertices on its interior.
/// Returns whether anything was split. Snapping can create new crossings, so the caller repeats.
fn splitRound(allocator: Allocator, edges: *std.ArrayList(DirectedEdge), work: *Work, max_edges: usize) Error!bool {
    const by_lowest_y = try allocator.alloc(u32, edges.items.len);
    defer allocator.free(by_lowest_y);
    for (by_lowest_y, 0..) |*index, position| index.* = @intCast(position);
    std.mem.sort(u32, by_lowest_y, edges.items, lowestYLessThan);

    var active: std.ArrayList(u32) = .empty;
    defer active.deinit(allocator);
    var splits: std.ArrayList(Split) = .empty;
    defer splits.deinit(allocator);
    for (by_lowest_y) |index| {
        const lowest_y = @min(edges.items[index].from[1], edges.items[index].to[1]);
        var kept: usize = 0;
        for (active.items) |active_index| {
            const active_edge = edges.items[active_index];
            if (@max(active_edge.from[1], active_edge.to[1]) < lowest_y) continue;
            active.items[kept] = active_index;
            kept += 1;
            try work.charge();
            try findSplits(allocator, &splits, edges.items, active_index, index);
        }
        active.shrinkRetainingCapacity(kept);
        try active.append(allocator, index);
    }
    if (splits.items.len == 0) return false;

    std.mem.sort(Split, splits.items, edges.items, splitLessThan);
    var result: std.ArrayList(DirectedEdge) = .empty;
    errdefer result.deinit(allocator);
    var split_index: usize = 0;
    for (edges.items, 0..) |edge, edge_index| {
        var from = edge.from;
        while (split_index < splits.items.len and splits.items[split_index].edge == edge_index) : (split_index += 1) {
            const point = splits.items[split_index].point;
            if (pointsEqual(point, from) or pointsEqual(point, edge.to)) continue;
            try result.append(allocator, .{ .from = from, .to = point, .contour = edge.contour });
            from = point;
        }
        try result.append(allocator, .{ .from = from, .to = edge.to, .contour = edge.contour });
        if (result.items.len > max_edges) return error.TooComplex;
    }
    assert(result.items.len > edges.items.len);
    edges.deinit(allocator);
    edges.* = result;
    return true;
}

fn lowestYLessThan(edges: []const DirectedEdge, a: u32, b: u32) bool {
    return @min(edges[a].from[1], edges[a].to[1]) < @min(edges[b].from[1], edges[b].to[1]);
}

fn splitLessThan(edges: []const DirectedEdge, a: Split, b: Split) bool {
    if (a.edge != b.edge) return a.edge < b.edge;
    const edge = edges[a.edge];
    const a_projection = projection(edge.from, edge.to, a.point);
    const b_projection = projection(edge.from, edge.to, b.point);
    if (a_projection != b_projection) return a_projection < b_projection;
    return pointLessThan(a.point, b.point);
}

fn findSplits(allocator: Allocator, splits: *std.ArrayList(Split), edges: []const DirectedEdge, first_index: u32, second_index: u32) !void {
    const first = edges[first_index];
    const second = edges[second_index];
    if (@max(first.from[0], first.to[0]) < @min(second.from[0], second.to[0])) return;
    if (@max(second.from[0], second.to[0]) < @min(first.from[0], first.to[0])) return;
    const second_from_side = orientation(widen(first.from), widen(first.to), widen(second.from));
    const second_to_side = orientation(widen(first.from), widen(first.to), widen(second.to));
    const first_from_side = orientation(widen(second.from), widen(second.to), widen(first.from));
    const first_to_side = orientation(widen(second.from), widen(second.to), widen(first.to));
    if (oppositeSigns(second_from_side, second_to_side) and oppositeSigns(first_from_side, first_to_side)) {
        const crossing = snappedCrossing(first.from, first.to, first_from_side, first_to_side);
        try splits.append(allocator, .{ .edge = first_index, .point = crossing });
        try splits.append(allocator, .{ .edge = second_index, .point = crossing });
        return;
    }
    if (second_from_side == 0 and strictlyInside(first, second.from)) try splits.append(allocator, .{ .edge = first_index, .point = second.from });
    if (second_to_side == 0 and strictlyInside(first, second.to)) try splits.append(allocator, .{ .edge = first_index, .point = second.to });
    if (first_from_side == 0 and strictlyInside(second, first.from)) try splits.append(allocator, .{ .edge = second_index, .point = first.from });
    if (first_to_side == 0 and strictlyInside(second, first.to)) try splits.append(allocator, .{ .edge = second_index, .point = first.to });
}

/// For a point on the line of the edge.
fn strictlyInside(edge: DirectedEdge, point: GridPoint) bool {
    return projection(edge.from, edge.to, point) > 0 and projection(edge.to, edge.from, point) > 0;
}

/// from + (to - from) * from_side / (from_side - to_side), rounded to the nearest grid point.
/// The exact crossing lies inside the bounding box of both edges, and so does the rounded one.
pub fn snappedCrossing(from: GridPoint, to: GridPoint, from_side: i128, to_side: i128) GridPoint {
    var numerator_scale = from_side;
    var denominator = from_side - to_side;
    if (denominator < 0) {
        numerator_scale = -numerator_scale;
        denominator = -denominator;
    }
    var crossing: GridPoint = undefined;
    for (&crossing, from, to) |*coordinate, from_coordinate, to_coordinate| {
        const numerator = @as(i128, from_coordinate) * denominator + (@as(i128, to_coordinate) - from_coordinate) * numerator_scale;
        const rounded = @divFloor(2 * numerator + denominator, 2 * denominator);
        assert(rounded >= @min(from_coordinate, to_coordinate) and rounded <= @max(from_coordinate, to_coordinate));
        coordinate.* = @intCast(rounded);
    }
    return crossing;
}

/// Winding next to each edge: crossings of a ray from its midpoint towards +x, half-open in y so
/// vertices on the ray count once. The ray from a horizontal edge sees the +y side, from any
/// other edge the +x side.
fn computeWindings(edges: []Edge, work: *Work) error{TooComplex}!void {
    for (edges, 0..) |*edge, edge_index| {
        const midpoint = [2]i64{ @as(i64, edge.from[0]) + edge.to[0], @as(i64, edge.from[1]) + edge.to[1] };
        const candidate_count = std.sort.partitionPoint(Edge, edges, midpoint[1], startsAtOrBelow);
        var winding: i64 = 0;
        for (edges[0..candidate_count], 0..) |other, other_index| {
            try work.charge();
            if (other_index == edge_index or 2 * @as(i64, other.to[1]) <= midpoint[1]) continue;
            const side = orientation(doubled(other.from), doubled(other.to), midpoint);
            // Edges meet only at end points, so no other edge passes through the midpoint.
            assert(side != 0);
            if (side > 0) winding += other.multiplicity;
        }
        if (edge.from[1] != edge.to[1]) {
            edge.winding_negative_side = winding;
            edge.winding_positive_side = winding + edge.multiplicity;
        } else {
            edge.winding_positive_side = winding;
            edge.winding_negative_side = winding - edge.multiplicity;
        }
    }
}

fn startsAtOrBelow(midpoint_y: i64, edge: Edge) bool {
    return 2 * @as(i64, edge.from[1]) <= midpoint_y;
}

fn doubled(point: GridPoint) [2]i64 {
    return .{ 2 * @as(i64, point[0]), 2 * @as(i64, point[1]) };
}
