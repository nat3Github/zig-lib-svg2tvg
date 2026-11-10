//! Anti-aliasing for the sweep tessellator, a port of Skia's GrAATriangulator onto the integer grid.
//! The filled region's boundary is offset half a pixel inward (coverage 1) and outward
//! (coverage 0). Coverage is the winding number of the inner rings plus the linear ramps between
//! the rings, summed where ramps overlap and clamped to [0, 1]. For parallel edges closer than a
//! pixel this is the exact box filtered coverage, where Skia collapses the overlap along its
//! straight skeleton.
//!
//! Ported from Skia, src/gpu/ganesh/geometry/GrAATriangulator.{h,cpp}:
//!
//! Copyright 2020 Google LLC
//!
//! Redistribution and use in source and binary forms, with or without modification, are
//! permitted provided that the following conditions are met:
//!
//!   * Redistributions of source code must retain the above copyright notice, this list of
//!     conditions and the following disclaimer.
//!   * Redistributions in binary form must reproduce the above copyright notice, this list of
//!     conditions and the following disclaimer in the documentation and/or other materials
//!     provided with the distribution.
//!   * Neither the name of the copyright holder nor the names of its contributors may be used
//!     to endorse or promote products derived from this software without specific prior
//!     written permission.
//!
//! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
//! OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
//! MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
//! COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
//! EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
//! GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
//! AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
//! NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
//! ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

const std = @import("std");
const assert = std.debug.assert;
const arrangement = @import("arrangement.zig");
const tessellation = @import("tessellation.zig");
const GridPoint = arrangement.GridPoint;
const FillRule = tessellation.FillRule;
const InternalError = tessellation.InternalError;
const Tessellator = tessellation.Tessellator;
const Vertex = tessellation.Vertex;
const Edge = tessellation.Edge;
const VertexList = tessellation.VertexList;
const EdgeList = tessellation.EdgeList;
const Poly = tessellation.Poly;
const LinearCoverage = tessellation.LinearCoverage;

const grid_per_pixel: f64 = tessellation.grid_per_pixel;
const ramp_half_width = 0.5 * grid_per_pixel;
const pointy_vertex_distance: i128 = tessellation.grid_per_pixel / 4;
const max_grid_coordinate = 1 << 24;
/// Directions between grid points stay below this, so squared dot and cross products fit in i128.
const max_direction_component = 1 << 25;

/// Ramps steeper than this per grid unit, narrower than 1/16 px, are dropped. Their gradients
/// would cost the coverage sums their precision.
const max_coverage_gradient = 16.0 / grid_per_pixel;

/// Corner pyramids fall to 0 within half a pixel diagonal of the corner.
const corner_radius = 0.5 * std.math.sqrt2 * grid_per_pixel;
const min_corner_correction = 1.0 / 32.0;

/// `mesh` is simplified. The polys fill with the nonzero rule and carry their coverage relative to
/// `origin`.
pub fn tessellate(tessellator: *Tessellator, mesh: VertexList, origin: GridPoint, fill_rule: FillRule) InternalError!?*Poly {
    var ramp_mesh: VertexList = .{};
    try removeNonBoundaryEdges(tessellator, mesh, fill_rule);
    var vertex = mesh.head;
    while (vertex) |current| : (vertex = current.next) {
        while (current.first_edge_below) |edge| {
            const boundary = try extractBoundary(tessellator, edge, fill_rule);
            //try simplifyBoundary(tessellator, &boundary);
            try strokeBoundary(tessellator, boundary, origin, &ramp_mesh);
        }
    }
    try tessellator.sortMesh(&ramp_mesh);
    _ = try tessellator.mergeCoincidentVertices(&ramp_mesh);
    _ = try tessellator.simplify(&ramp_mesh);
    return tessellator.tessellate(ramp_mesh);
}

fn antialiasDirection(from: GridPoint, to: GridPoint) [2]i64 {
    const result = [2]i64{ @as(i64, to[0]) - from[0], @as(i64, to[1]) - from[1] };
    assert(@abs(result[0]) <= max_direction_component and @abs(result[1]) <= max_direction_component);
    return result;
}

fn dotProduct(a: [2]i64, b: [2]i64) i64 {
    return a[0] * b[0] + a[1] * b[1];
}

fn crossProduct(a: [2]i64, b: [2]i64) i64 {
    return a[0] * b[1] - a[1] * b[0];
}

/// Same direction within an angle of about 2^-16, exact. Skia's nearParallel allows 1e-5.
fn isNearlySameDirection(a: [2]i64, b: [2]i64) bool {
    if (dotProduct(a, b) <= 0) return false;
    const cross: i256 = crossProduct(a, b);
    return (cross * cross) << 32 < @as(i256, dotProduct(a, a)) * dotProduct(b, b);
}

fn vectorLength(vector: [2]i64) f64 {
    return std.math.hypot(@as(f64, @floatFromInt(vector[0])), @as(f64, @floatFromInt(vector[1])));
}

fn unit(vector: [2]i64) [2]f64 {
    const length = vectorLength(vector);
    assert(length > 0);
    return .{ @as(f64, @floatFromInt(vector[0])) / length, @as(f64, @floatFromInt(vector[1])) / length };
}

fn relativePoint(origin: GridPoint, point: GridPoint) [2]f64 {
    const offset = antialiasDirection(origin, point);
    return .{ @floatFromInt(offset[0]), @floatFromInt(offset[1]) };
}

/// `origin` moved by `offset` rounded to the grid, null past the grid. Rounding only the offset
/// keeps constructions exactly invariant under translation.
fn offsetPoint(origin: GridPoint, offset: [2]f64) ?GridPoint {
    var point: GridPoint = undefined;
    for (&point, origin, offset) |*coordinate, base, delta| {
        const value = @as(f64, @floatFromInt(base)) + @round(delta);
        if (!(@abs(value) <= max_grid_coordinate)) return null;
        coordinate.* = @intFromFloat(value);
    }
    return point;
}

fn makeVertex(tessellator: *Tessellator, point: GridPoint) error{OutOfMemory}!*Vertex {
    const vertex = try tessellator.arena.create(Vertex);
    vertex.* = .{ .point = point };
    return vertex;
}

/// A piece of the filled region's boundary, the fill lies on its right.
const BoundaryEdge = struct {
    from: *Vertex,
    to: *Vertex,
    prev: ?*BoundaryEdge = null,
    next: ?*BoundaryEdge = null,

    fn make(tessellator: *Tessellator, from: *Vertex, to: *Vertex) error{OutOfMemory}!*BoundaryEdge {
        assert(!tessellation.pointsEqual(from.point, to.point));
        const edge = try tessellator.arena.create(BoundaryEdge);
        edge.* = .{ .from = from, .to = to };
        return edge;
    }

    fn direction(edge: *const BoundaryEdge) [2]i64 {
        return antialiasDirection(edge.from.point, edge.to.point);
    }

    /// Within a quarter pixel of the line through the edge, exact.
    fn isPointyNeighbor(edge: *const BoundaryEdge, vertex: *const Vertex) bool {
        if (vertex == edge.from or vertex == edge.to) return true;
        const side: i128 = tessellation.orientation(edge.from.point, edge.to.point, vertex.point);
        const edge_direction = edge.direction();
        return side * side <= pointy_vertex_distance * pointy_vertex_distance * dotProduct(edge_direction, edge_direction);
    }
};

const Boundary = struct {
    head: ?*BoundaryEdge = null,
    tail: ?*BoundaryEdge = null,

    fn insertAfter(boundary: *Boundary, edge: *BoundaryEdge, prev: ?*BoundaryEdge) void {
        const next = if (prev) |before| before.next else boundary.head;
        tessellation.listInsert(BoundaryEdge, "prev", "next", edge, prev, next, &boundary.head, &boundary.tail);
    }

    fn remove(boundary: *Boundary, edge: *BoundaryEdge) void {
        tessellation.listRemove(BoundaryEdge, "prev", "next", edge, &boundary.head, &boundary.tail);
    }
};

/// Leaves only edges with fill on exactly one side, each winding becomes the winding on its right.
fn removeNonBoundaryEdges(tessellator: *Tessellator, mesh: VertexList, fill_rule: FillRule) InternalError!void {
    var active: EdgeList = .{};
    var vertex = mesh.head;
    while (vertex) |current| : (vertex = current.next) {
        if (!current.isConnected()) continue;
        const enclosing = try tessellator.findEnclosingEdges(current, &active);
        var prev_filled = if (enclosing[0]) |left| arrangement.isFilled(fill_rule, left.winding) else false;
        var above = current.first_edge_above;
        while (above) |edge| {
            try tessellator.charge();
            above = edge.next_edge_above;
            assert(active.remove(edge));
            const filled = arrangement.isFilled(fill_rule, edge.winding);
            if (filled == prev_filled) edge.disconnect();
            prev_filled = filled;
        }
        var prev = enclosing[0];
        var below = current.first_edge_below;
        while (below) |edge| : (below = edge.next_edge_below) {
            try tessellator.charge();
            if (prev) |left| edge.winding += left.winding;
            assert(active.insertAfter(edge, prev));
            prev = edge;
        }
    }
}

/// Walks the boundary through `first` with the fill on the right and disconnects its edges.
fn extractBoundary(tessellator: *Tessellator, first: *Edge, fill_rule: FillRule) InternalError!Boundary {
    var boundary: Boundary = .{};
    var down = arrangement.isFilled(fill_rule, first.winding);
    const start = if (down) first.top.? else first.bottom.?;
    var edge = first;
    while (true) {
        try tessellator.charge();
        const top = edge.top.?;
        const bottom = edge.bottom.?;
        boundary.insertAfter(try BoundaryEdge.make(tessellator, if (down) top else bottom, if (down) bottom else top), boundary.tail);
        var next: ?*Edge = null;
        var next_down = down;
        if (down) {
            if (edge.next_edge_above) |candidate| {
                next = candidate;
                next_down = false;
            } else if (bottom.last_edge_below) |candidate| {
                next = candidate;
            } else if (edge.prev_edge_above) |candidate| {
                next = candidate;
                next_down = false;
            }
        } else {
            if (edge.prev_edge_below) |candidate| {
                next = candidate;
                next_down = true;
            } else if (top.first_edge_above) |candidate| {
                next = candidate;
            } else if (edge.next_edge_below) |candidate| {
                next = candidate;
                next_down = true;
            }
        }
        edge.disconnect();
        edge = next orelse break;
        down = next_down;
        if ((if (down) edge.top.? else edge.bottom.?) == start) break;
    }
    return boundary;
}

/// Replaces pointy vertices, whose neighbors lie within a quarter pixel of the opposite edge, by a
/// straight join. They would invert when offset.
fn simplifyBoundary(tessellator: *Tessellator, boundary: *Boundary) InternalError!void {
    var prev_edge = boundary.tail orelse return;
    var next_edge = boundary.head;
    while (next_edge) |edge| {
        try tessellator.charge();
        const prev = prev_edge.from;
        const next = edge.to;
        if (prev == next) {
            assert(prev_edge != edge);
            boundary.remove(prev_edge);
            boundary.remove(edge);
            prev_edge = boundary.tail orelse return;
            next_edge = boundary.head;
        } else if (dotProduct(prev_edge.direction(), edge.direction()) < 0 and (edge.isPointyNeighbor(prev) or prev_edge.isPointyNeighbor(next))) {
            const join = try BoundaryEdge.make(tessellator, prev, next);
            boundary.insertAfter(join, edge);
            boundary.remove(prev_edge);
            boundary.remove(edge);
            if (join.prev != null and join.next != null) {
                prev_edge = join.prev.?;
                next_edge = join;
            } else {
                prev_edge = boundary.tail.?;
                next_edge = boundary.head;
            }
        } else {
            prev_edge = edge;
            next_edge = edge.next;
        }
    }
}

/// Ring corners at a join, each inner corner partners the outer corner at the same index.
const Ramp = struct {
    inner: [2]GridPoint,
    outer: [2]GridPoint,
    count: usize,
    /// The ramps' coverage at the boundary vertex.
    vertex_coverage: f64,
};

/// One end of a ring segment, the boundary vertex lies halfway between the corners.
const RingEntry = struct {
    inner: GridPoint,
    outer: GridPoint,
    vertex: GridPoint,
    /// Added to the segment's coverage of 0.5 at the vertex, falling to 0 within corner_radius.
    lift: f64,
    /// Second entry of a join with two, the segment from the first one stays inside the join.
    continues_join: bool,
};

/// Offsets the boundary half a pixel to both sides and adds the ramps between the rings, with
/// coverage 0 on the outer ring and 1 on the inner ring.
fn strokeBoundary(tessellator: *Tessellator, boundary: Boundary, origin: GridPoint, ramp_mesh: *VertexList) InternalError!void {
    const head = boundary.head orelse return;
    if (head.next == null or head.next.?.next == null) return;
    var prev_edge = boundary.tail.?;
    var ring: std.ArrayList(RingEntry) = .empty;
    var next_edge: ?*BoundaryEdge = head;
    while (next_edge) |edge| : ({
        next_edge = edge.next;
        prev_edge = edge;
    }) {
        try tessellator.charge();
        const ramp = try joinRamp(prev_edge, edge) orelse continue;
        const peak = cornerPeak(prev_edge, edge, ramp.vertex_coverage);
        // Sharp joins stay uncorrected like in Skia and NanoVG, a correction there would cross both overlapping fans.
        const corrected = ramp.count == 1 and @abs(peak) >= min_corner_correction;
        const shorter_edge = @min(vectorLength(prev_edge.direction()), vectorLength(edge.direction()));
        const lift = if (corrected) peak * @min(1, shorter_edge / (2 * corner_radius)) else 0;
        for (ramp.inner[0..ramp.count], ramp.outer[0..ramp.count], 0..) |inner, outer, index| {
            try ring.append(tessellator.arena, .{ .inner = inner, .outer = outer, .vertex = edge.from.point, .lift = lift, .continues_join = index == 1 });
        }
    }
    const count = ring.items.len;
    for (ring.items, 0..) |entry, index| {
        try tessellator.charge();
        const next = ring.items[(index + 1) % count];
        if (next.continues_join) {
            const outer = try meshVertex(tessellator, ramp_mesh, entry.outer);
            const next_outer = try meshVertex(tessellator, ramp_mesh, next.outer);
            const next_inner = try meshVertex(tessellator, ramp_mesh, next.inner);
            const inner = try meshVertex(tessellator, ramp_mesh, entry.inner);
            try addRampTriangle(tessellator, origin, .{ outer, next_outer, next_inner }, .{ 0, 0, 1 });
            try addRampTriangle(tessellator, origin, .{ outer, next_inner, inner }, .{ 0, 1, 1 });
        } else try addRampSegment(tessellator, origin, entry, next, ramp_mesh);
    }
}

fn meshVertex(tessellator: *Tessellator, ramp_mesh: *VertexList, point: GridPoint) error{OutOfMemory}!*Vertex {
    const vertex = try makeVertex(tessellator, point);
    ramp_mesh.append(vertex);
    return vertex;
}

/// The ramp between two joins. Lifted ends split it along the boundary into an outer and an inner
/// strip, both fanned so the lift sits on the boundary vertex without new crossing edges.
fn addRampSegment(tessellator: *Tessellator, origin: GridPoint, start: RingEntry, end: RingEntry, ramp_mesh: *VertexList) InternalError!void {
    const outer_start = try meshVertex(tessellator, ramp_mesh, start.outer);
    const outer_end = try meshVertex(tessellator, ramp_mesh, end.outer);
    const inner_end = try meshVertex(tessellator, ramp_mesh, end.inner);
    const inner_start = try meshVertex(tessellator, ramp_mesh, start.inner);
    if (start.lift == 0 and end.lift == 0) {
        try addRampTriangle(tessellator, origin, .{ outer_start, outer_end, inner_end }, .{ 0, 0, 1 });
        try addRampTriangle(tessellator, origin, .{ outer_start, inner_end, inner_start }, .{ 0, 1, 1 });
        return;
    }
    var midline: [4]*Vertex = undefined;
    var values: [4]f64 = undefined;
    var midline_count: usize = 0;
    const length = vectorLength(antialiasDirection(start.vertex, end.vertex));
    const room_for_falloff = length >= 2 * corner_radius;
    if (start.lift != 0) {
        midline[midline_count] = try meshVertex(tessellator, ramp_mesh, start.vertex);
        values[midline_count] = 0.5 + start.lift;
        midline_count += 1;
        if (room_for_falloff) {
            midline[midline_count] = try meshVertex(tessellator, ramp_mesh, try pointAlong(start.vertex, end.vertex, corner_radius / length));
            values[midline_count] = 0.5;
            midline_count += 1;
        }
    }
    if (end.lift != 0) {
        if (room_for_falloff) {
            midline[midline_count] = try meshVertex(tessellator, ramp_mesh, try pointAlong(end.vertex, start.vertex, corner_radius / length));
            values[midline_count] = 0.5;
            midline_count += 1;
        }
        midline[midline_count] = try meshVertex(tessellator, ramp_mesh, end.vertex);
        values[midline_count] = 0.5 + end.lift;
        midline_count += 1;
    }
    assert(midline_count > 0);
    const last = midline_count - 1;
    if (start.lift == 0) try addRampTriangle(tessellator, origin, .{ outer_start, midline[0], inner_start }, .{ 0, values[0], 1 });
    if (end.lift == 0) try addRampTriangle(tessellator, origin, .{ outer_end, inner_end, midline[last] }, .{ 0, 1, values[last] });
    // The long triangles spanning the segment end on its last unlifted midline point.
    const span_end = if (end.lift != 0 and last > 0) last - 1 else last;
    try addRampTriangle(tessellator, origin, .{ outer_start, outer_end, midline[span_end] }, .{ 0, 0, values[span_end] });
    try addRampTriangle(tessellator, origin, .{ inner_start, midline[span_end], inner_end }, .{ 1, values[span_end], 1 });
    for (0..last) |index| {
        const outer = if (index < span_end) outer_start else outer_end;
        const inner = if (index < span_end) inner_start else inner_end;
        try addRampTriangle(tessellator, origin, .{ outer, midline[index + 1], midline[index] }, .{ 0, values[index + 1], values[index] });
        try addRampTriangle(tessellator, origin, .{ inner, midline[index], midline[index + 1] }, .{ 1, values[index], values[index + 1] });
    }
}

/// `from` moved towards `to` by `fraction` of the way.
fn pointAlong(from: GridPoint, to: GridPoint, fraction: f64) error{TooComplex}!GridPoint {
    assert(fraction > 0 and fraction < 1);
    const direction = relativePoint(from, to);
    return offsetPoint(from, .{ direction[0] * fraction, direction[1] * fraction }) orelse error.TooComplex;
}

/// Adds the triangle's coverage, linear from 0 at outer to 1 at inner corners, as edges whose
/// coverage steps sum to it. An edge between inner corners also steps by -1, the winding of the
/// inner ring, which keeps the sum continuous. Winding counts the outer ring.
fn addRampTriangle(tessellator: *Tessellator, origin: GridPoint, corners: [3]*Vertex, values: [3]f64) InternalError!void {
    const coverage = rampCoverage(origin, corners, values);
    for (corners, 0..) |from, index| {
        const next = (index + 1) % 3;
        const to = corners[next];
        const inner_edge = values[index] == 1 and values[next] == 1;
        if (coverage) |linear| {
            const step = if (inner_edge) linear.plus(.{ .c = -1 }) else linear;
            try tessellator.makeConnectingEdge(from, to, if (inner_edge) 0 else 1, step);
        } else if (inner_edge) try tessellator.makeConnectingEdge(from, to, -1, .{ .c = -1 });
    }
}

/// The linear function through the corners, null for flat or too steep triangles. The corners
/// are taken in the given order, so the steps are right minus left for either orientation.
fn rampCoverage(origin: GridPoint, corners: [3]*Vertex, values: [3]f64) ?LinearCoverage {
    const first = corners[0].point;
    const second = antialiasDirection(first, corners[1].point);
    const third = antialiasDirection(first, corners[2].point);
    const determinant = crossProduct(second, third);
    if (determinant == 0) return null;
    const second_rise = values[1] - values[0];
    const third_rise = values[2] - values[0];
    const denominator: f64 = @floatFromInt(determinant);
    const a = (second_rise * @as(f64, @floatFromInt(third[1])) - third_rise * @as(f64, @floatFromInt(second[1]))) / denominator;
    const b = (third_rise * @as(f64, @floatFromInt(second[0])) - second_rise * @as(f64, @floatFromInt(third[0]))) / denominator;
    if (a * a + b * b > max_coverage_gradient * max_coverage_gradient) return null;
    const relative = relativePoint(origin, first);
    return .{ .a = a, .b = b, .c = values[0] - a * relative[0] - b * relative[1] };
}

/// Where the offset lines of two consecutive boundary edges meet, null where the boundary runs
/// straight on. Points are built relative to the vertex in unit vectors and rounded once.
fn joinRamp(prev_edge: *const BoundaryEdge, edge: *const BoundaryEdge) error{TooComplex}!?Ramp {
    const vertex = edge.from.point;
    assert(prev_edge.to == edge.from);
    const incoming = prev_edge.direction();
    const outgoing = edge.direction();
    if (isNearlySameDirection(incoming, outgoing)) return null;
    const incoming_unit = unit(incoming);
    const outgoing_unit = unit(outgoing);
    // Unit normals towards the fill.
    const incoming_normal = [2]f64{ incoming_unit[1], -incoming_unit[0] };
    const outgoing_normal = [2]f64{ outgoing_unit[1], -outgoing_unit[0] };
    if (dotProduct(incoming, outgoing) > 0) {
        const cosine = dotFloat(incoming_unit, outgoing_unit);
        const scale = ramp_half_width / (1 + cosine);
        const offset = [2]f64{ (incoming_normal[0] + outgoing_normal[0]) * scale, (incoming_normal[1] + outgoing_normal[1]) * scale };
        const inner = offsetPoint(vertex, offset) orelse return error.TooComplex;
        const outer = offsetPoint(vertex, .{ -offset[0], -offset[1] }) orelse return error.TooComplex;
        return .{ .inner = .{ inner, inner }, .outer = .{ outer, outer }, .count = 1, .vertex_coverage = 0.5 };
    }
    // Turns of 90° and more: both ramps run on to the line through the vertex along the sum of the
    // directions. Their coverages add up to 1 on it, before it they overlap and sum as for two
    // close edges. The inner corner of one ramp is the outer corner of the other.
    const offset: [2]f64 = if (crossProduct(incoming, outgoing) == 0)
        .{ ramp_half_width * incoming_normal[0], ramp_half_width * incoming_normal[1] }
    else offset: {
        const scale = ramp_half_width / dotFloat(incoming_normal, outgoing_unit);
        break :offset .{ (incoming_unit[0] + outgoing_unit[0]) * scale, (incoming_unit[1] + outgoing_unit[1]) * scale };
    };
    const inner = offsetPoint(vertex, offset) orelse return error.TooComplex;
    const outer = offsetPoint(vertex, .{ -offset[0], -offset[1] }) orelse return error.TooComplex;
    const concave = crossProduct(incoming, outgoing) > 0;
    return .{ .inner = .{ inner, outer }, .outer = .{ outer, inner }, .count = 2, .vertex_coverage = if (concave) 1 else 0 };
}

/// What lifts the ramps' coverage at a corner to α/2π, α the interior angle.
fn cornerPeak(prev_edge: *const BoundaryEdge, edge: *const BoundaryEdge, vertex_coverage: f64) f64 {
    const incoming_direction = prev_edge.direction();
    const outgoing_direction = edge.direction();
    const incoming = unit(incoming_direction);
    const outgoing = unit(outgoing_direction);
    const turn = std.math.atan2(@abs(incoming[0] * outgoing[1] - incoming[1] * outgoing[0]), dotFloat(incoming, outgoing));
    const concave = crossProduct(incoming_direction, outgoing_direction) > 0;
    const interior_angle = if (concave) std.math.pi + turn else std.math.pi - turn;
    return interior_angle / (2 * std.math.pi) - vertex_coverage;
}

fn dotFloat(a: [2]f64, b: [2]f64) f64 {
    return a[0] * b[0] + a[1] * b[1];
}
