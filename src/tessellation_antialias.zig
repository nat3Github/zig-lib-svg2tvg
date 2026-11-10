//! Anti-aliasing for the sweep tessellator, a port of Skia's GrAATriangulator onto the integer grid.
//! The filled region's boundary is offset half a pixel inward (coverage 1) and outward
//! (coverage 0), triangles between the rings interpolate coverage per vertex. Where the rings of
//! close edges overlap, the overlap collapses along its straight skeleton as in Skia, and each
//! collapsed vertex gets the summed coverage of the two ramps meeting there.
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
const EdgeKind = tessellation.EdgeKind;
const VertexList = tessellation.VertexList;
const EdgeList = tessellation.EdgeList;
const Poly = tessellation.Poly;
const sweepLessThan = tessellation.sweepLessThan;
const coverageAlong = tessellation.coverageAlong;

const grid_per_pixel: f64 = tessellation.grid_per_pixel;
const ramp_half_width = 0.5 * grid_per_pixel;
const pointy_vertex_distance: i128 = tessellation.grid_per_pixel / 4;
const max_grid_coordinate = 1 << 24;
/// Directions between grid points stay below this, so squared dot and cross products fit in i128.
const max_direction_component = 1 << 25;
/// Skia's miter angle, turns past about 166° pull the far corner back.
const pull_back_cosine = 0.97;
/// Skia's limit for bisectors at crossings, about 2.6°.
const max_bisector_cosine = 0.999;

/// Corner lifts fall to 0 within half a pixel diagonal of the boundary vertex.
const corner_radius = 0.5 * std.math.sqrt2 * grid_per_pixel;
const min_corner_correction = 1.0 / 32.0;

pub const Result = struct {
    /// Filled with the nonzero rule.
    polys: ?*Poly,
    /// The ramps between the rings, when no rings overlap. Otherwise the polys contain them.
    triangles: []const [3]*Vertex,
};

/// `mesh` is simplified.
pub fn tessellate(tessellator: *Tessellator, mesh: VertexList, fill_rule: FillRule) InternalError!Result {
    var inner_mesh: VertexList = .{};
    var outer_mesh: VertexList = .{};
    var rings: std.ArrayList([]const RingEntry) = .empty;
    try removeNonBoundaryEdges(tessellator, mesh, fill_rule);
    var vertex = mesh.head;
    while (vertex) |current| : (vertex = current.next) {
        while (current.first_edge_below) |edge| {
            var boundary = try extractBoundary(tessellator, edge, fill_rule);
            try simplifyBoundary(tessellator, &boundary);
            const ring = try strokeBoundary(tessellator, boundary, &inner_mesh, &outer_mesh);
            if (ring.len > 0) try rings.append(tessellator.arena, ring);
        }
    }
    try tessellator.sortMesh(&inner_mesh);
    try tessellator.sortMesh(&outer_mesh);
    _ = try tessellator.mergeCoincidentVertices(&inner_mesh);
    var complex = try tessellator.mergeCoincidentVertices(&outer_mesh);
    if (try tessellator.simplify(&inner_mesh)) complex = true;
    if (try tessellator.simplify(&outer_mesh)) complex = true;
    if (try collapseOverlapRegions(tessellator, &inner_mesh, .inner)) complex = true;
    if (try collapseOverlapRegions(tessellator, &outer_mesh, .outer)) complex = true;
    if (!complex) {
        var triangles: std.ArrayList([3]*Vertex) = .empty;
        for (rings.items) |ring| for (ring, 0..) |entry, index| {
            try tessellator.charge();
            try addRampSegment(tessellator, entry, ring[(index + 1) % ring.len], &triangles);
        };
        return .{ .polys = try tessellator.tessellate(inner_mesh), .triangles = triangles.items };
    }
    try connectPartners(tessellator, outer_mesh);
    try connectPartners(tessellator, inner_mesh);
    outer_mesh.appendList(inner_mesh);
    try tessellator.sortMesh(&outer_mesh);
    _ = try tessellator.mergeCoincidentVertices(&outer_mesh);
    _ = try tessellator.simplify(&outer_mesh);
    return .{ .polys = try tessellator.tessellate(outer_mesh), .triangles = &.{} };
}

/// Connects remaining ring partners with structural edges, so no triangle spans the ramp with
/// coverage 0 at all corners.
fn connectPartners(tessellator: *Tessellator, mesh: VertexList) InternalError!void {
    var vertex = mesh.head;
    while (vertex) |outer| : (vertex = outer.next) {
        try tessellator.charge();
        const inner = outer.partner orelse continue;
        const target = inner.collapsed_into orelse inner;
        if (!isListed(target) or !isListed(outer)) continue;
        try tessellator.makeConnectingEdge(outer, target, .connector, 0);
        inner.partner = null;
        outer.partner = null;
    }
}

fn isListed(vertex: *const Vertex) bool {
    return vertex.prev != null or vertex.next != null;
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

fn makeVertex(tessellator: *Tessellator, point: GridPoint, coverage: f32) error{OutOfMemory}!*Vertex {
    const vertex = try tessellator.arena.create(Vertex);
    vertex.* = .{ .point = point, .coverage = coverage };
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
};

/// Partnered ring corners, the boundary vertex lies between them.
const RingEntry = struct {
    inner: *Vertex,
    outer: *Vertex,
    vertex: GridPoint,
    /// Added to the ramp's coverage of 0.5 at the vertex, falling to 0 within corner_radius.
    lift: f64,
};

/// Offsets the boundary half a pixel to both sides into the ring meshes, inner corners with
/// coverage 1 and outer corners with coverage 0. Returns the ring in boundary order.
fn strokeBoundary(tessellator: *Tessellator, boundary: Boundary, inner_mesh: *VertexList, outer_mesh: *VertexList) InternalError![]const RingEntry {
    const head = boundary.head orelse return &.{};
    if (head.next == null or head.next.?.next == null) return &.{};
    var ring: std.ArrayList(RingEntry) = .empty;
    var inner_inversion = true;
    var outer_inversion = true;
    var prev_edge = boundary.tail.?;
    var next_edge: ?*BoundaryEdge = head;
    while (next_edge) |edge| : ({
        next_edge = edge.next;
        prev_edge = edge;
    }) {
        try tessellator.charge();
        const ramp = joinRamp(prev_edge, edge) orelse continue;
        const lift = if (ramp.count == 1) cornerLift(prev_edge, edge) else 0;
        for (ramp.inner[0..ramp.count], ramp.outer[0..ramp.count], 0..) |inner_point, outer_point, index| {
            // A miter's corner on the side without the tangent is one vertex for both entries, a
            // merge would make it synthetic and keep it out of the collapse.
            const shared_inner = index == 1 and tessellation.pointsEqual(inner_point, ramp.inner[0]);
            const shared_outer = index == 1 and tessellation.pointsEqual(outer_point, ramp.outer[0]);
            const inner = if (shared_inner) ring.getLast().inner else try ringVertex(tessellator, inner_mesh, inner_point, 1);
            const outer = if (shared_outer) ring.getLast().outer else try ringVertex(tessellator, outer_mesh, outer_point, 0);
            if (!shared_inner) inner.partner = outer;
            if (!shared_outer) outer.partner = inner;
            if (index == 0 and ring.items.len > 0) {
                const last = ring.getLast();
                if (!runsAgainst(last.inner, inner, prev_edge)) inner_inversion = false;
                if (!runsAgainst(last.outer, outer, prev_edge)) outer_inversion = false;
            }
            try ring.append(tessellator.arena, .{ .inner = inner, .outer = outer, .vertex = edge.from.point, .lift = lift });
        }
    }
    const count = ring.items.len;
    if (count == 0) return &.{};
    if (!runsAgainst(ring.getLast().inner, ring.items[0].inner, boundary.tail.?)) inner_inversion = false;
    if (!runsAgainst(ring.getLast().outer, ring.items[0].outer, boundary.tail.?)) outer_inversion = false;
    // Inner -2 and outer 1 fill the ramp with 1 and the interior with -1, or 3 where a thin part
    // inverts. A wholly inverted ring flips them, so collapseOverlapRegions finds it.
    const inner_winding: i32 = if (inner_inversion) 2 else -2;
    const outer_winding: i32 = if (outer_inversion) -1 else 1;
    for (ring.items, 0..) |entry, index| {
        try tessellator.charge();
        const next = ring.items[(index + 1) % count];
        try tessellator.makeConnectingEdge(entry.inner, next.inner, .inner, inner_winding);
        try tessellator.makeConnectingEdge(entry.outer, next.outer, .outer, outer_winding);
    }
    return ring.items;
}

fn ringVertex(tessellator: *Tessellator, mesh: *VertexList, point: GridPoint, coverage: f32) error{OutOfMemory}!*Vertex {
    const vertex = try makeVertex(tessellator, point, coverage);
    mesh.append(vertex);
    return vertex;
}

/// Skia's inversion test: the ring piece runs against the boundary edge in sweep order.
fn runsAgainst(from: *const Vertex, to: *const Vertex, edge: *const BoundaryEdge) bool {
    return sweepLessThan(from.point, to.point) != sweepLessThan(edge.from.point, edge.to.point);
}

/// Where the offset lines of two consecutive boundary edges meet. Null where the boundary runs
/// straight on, turns back exactly, or a corner leaves the grid. Points are built relative to the
/// vertex in unit vectors and rounded once.
fn joinRamp(prev_edge: *const BoundaryEdge, edge: *const BoundaryEdge) ?Ramp {
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
    const normal_sum = [2]f64{ incoming_normal[0] + outgoing_normal[0], incoming_normal[1] + outgoing_normal[1] };
    const cosine = dotFloat(incoming_unit, outgoing_unit);
    if (dotProduct(incoming, outgoing) > 0) {
        const offset = scaled(normal_sum, ramp_half_width / (1 + cosine));
        const inner = offsetPoint(vertex, offset) orelse return null;
        const outer = offsetPoint(vertex, scaled(offset, -1)) orelse return null;
        return .{ .inner = .{ inner, inner }, .outer = .{ outer, outer }, .count = 1 };
    }
    const cross = crossProduct(incoming, outgoing);
    if (cross == 0) return null;
    const sum_length = std.math.hypot(normal_sum[0], normal_sum[1]);
    if (!(sum_length > 0)) return null;
    // Mitered from 90° on. The convex side's corners lie on the tangent half a pixel past the
    // vertex, the other side's corner where the offset lines meet. Past Skia's miter angle that
    // corner is pulled back along the bisector to the perpendiculars through the neighbors.
    const bisector = scaled(normal_sum, 1 / sum_length);
    const concave = cross > 0;
    const tangent_distance: f64 = if (concave) ramp_half_width else -ramp_half_width;
    const first_offset = solveLines(bisector, tangent_distance, incoming_normal, tangent_distance) orelse return null;
    const second_offset = solveLines(bisector, tangent_distance, outgoing_normal, tangent_distance) orelse return null;
    const direction = scaled(bisector, if (concave) -1 else 1);
    var distance = ramp_half_width * sum_length / (1 + cosine);
    if (cosine < -pull_back_cosine) {
        const incoming_projection = dotFloat(incoming_unit, direction);
        if (incoming_projection < 0) distance = @min(distance, vectorLength(incoming) / -incoming_projection);
        const outgoing_projection = dotFloat(outgoing_unit, direction);
        if (outgoing_projection > 0) distance = @min(distance, vectorLength(outgoing) / outgoing_projection);
    }
    const opposite = offsetPoint(vertex, scaled(direction, distance)) orelse return null;
    const first = offsetPoint(vertex, first_offset) orelse return null;
    const second = offsetPoint(vertex, second_offset) orelse return null;
    if (concave) return .{ .inner = .{ first, second }, .outer = .{ opposite, opposite }, .count = 2 };
    return .{ .inner = .{ opposite, opposite }, .outer = .{ first, second }, .count = 2 };
}

/// The point q with normal · q = distance and other_normal · q = other_distance, null if the
/// lines are parallel.
fn solveLines(normal: [2]f64, distance: f64, other_normal: [2]f64, other_distance: f64) ?[2]f64 {
    const determinant = normal[0] * other_normal[1] - normal[1] * other_normal[0];
    if (determinant == 0) return null;
    return .{
        (distance * other_normal[1] - other_distance * normal[1]) / determinant,
        (normal[0] * other_distance - other_normal[0] * distance) / determinant,
    };
}

/// What lifts the ramps' coverage of 0.5 at a flat corner to α/2π, α the interior angle, scaled
/// down where the shorter edge leaves no room for the falloff.
fn cornerLift(prev_edge: *const BoundaryEdge, edge: *const BoundaryEdge) f64 {
    const incoming_direction = prev_edge.direction();
    const outgoing_direction = edge.direction();
    const incoming = unit(incoming_direction);
    const outgoing = unit(outgoing_direction);
    const turn = std.math.atan2(@abs(incoming[0] * outgoing[1] - incoming[1] * outgoing[0]), dotFloat(incoming, outgoing));
    const concave = crossProduct(incoming_direction, outgoing_direction) > 0;
    const interior_angle = if (concave) std.math.pi + turn else std.math.pi - turn;
    const peak = interior_angle / (2 * std.math.pi) - 0.5;
    if (@abs(peak) < min_corner_correction) return 0;
    const shorter_edge = @min(vectorLength(incoming_direction), vectorLength(outgoing_direction));
    return peak * @min(1, shorter_edge / (2 * corner_radius));
}

/// The ramp between two ring entries. Lifted ends split it along the boundary into an outer and
/// an inner strip, both fanned so the lift sits on the boundary vertex.
fn addRampSegment(tessellator: *Tessellator, start: RingEntry, end: RingEntry, triangles: *std.ArrayList([3]*Vertex)) InternalError!void {
    const arena = tessellator.arena;
    const outer_start = start.outer;
    const outer_end = end.outer;
    const inner_start = start.inner;
    const inner_end = end.inner;
    if (start.lift == 0 and end.lift == 0) {
        try triangles.append(arena, .{ outer_start, outer_end, inner_end });
        try triangles.append(arena, .{ outer_start, inner_end, inner_start });
        return;
    }
    var midline: [4]*Vertex = undefined;
    var midline_count: usize = 0;
    const length = vectorLength(antialiasDirection(start.vertex, end.vertex));
    const room_for_falloff = length >= 2 * corner_radius;
    if (start.lift != 0) {
        midline[midline_count] = try makeVertex(tessellator, start.vertex, @floatCast(0.5 + start.lift));
        midline_count += 1;
        if (room_for_falloff) {
            midline[midline_count] = try makeVertex(tessellator, try pointAlong(start.vertex, end.vertex, corner_radius / length), 0.5);
            midline_count += 1;
        }
    }
    if (end.lift != 0) {
        if (room_for_falloff) {
            midline[midline_count] = try makeVertex(tessellator, try pointAlong(end.vertex, start.vertex, corner_radius / length), 0.5);
            midline_count += 1;
        }
        midline[midline_count] = try makeVertex(tessellator, end.vertex, @floatCast(0.5 + end.lift));
        midline_count += 1;
    }
    assert(midline_count > 0);
    const last = midline_count - 1;
    if (start.lift == 0) try triangles.append(arena, .{ outer_start, midline[0], inner_start });
    if (end.lift == 0) try triangles.append(arena, .{ outer_end, inner_end, midline[last] });
    // The long triangles spanning the segment end on its last unlifted midline point.
    const span_end = if (end.lift != 0 and last > 0) last - 1 else last;
    try triangles.append(arena, .{ outer_start, outer_end, midline[span_end] });
    try triangles.append(arena, .{ inner_start, midline[span_end], inner_end });
    for (0..last) |index| {
        const outer = if (index < span_end) outer_start else outer_end;
        const inner = if (index < span_end) inner_start else inner_end;
        try triangles.append(arena, .{ outer, midline[index + 1], midline[index] });
        try triangles.append(arena, .{ inner, midline[index], midline[index + 1] });
    }
}

/// `from` moved towards `to` by `fraction` of the way.
fn pointAlong(from: GridPoint, to: GridPoint, fraction: f64) error{TooComplex}!GridPoint {
    assert(fraction > 0 and fraction < 1);
    return offsetPoint(from, scaled(relativePoint(from, to), fraction)) orelse error.TooComplex;
}

/// Gives the crossing `vertex` of two ring edges a partner where the lines one pixel off both
/// edges meet, with coverage 1 behind outer and 0 behind inner edges. Nearly parallel edges get none.
pub fn computeBisector(tessellator: *Tessellator, edge: *const Edge, other: *const Edge, vertex: *Vertex) InternalError!void {
    if (!edge.isAlive() or !other.isAlive()) return;
    const direction = unit(antialiasDirection(edge.top.?.point, edge.bottom.?.point));
    const other_direction = unit(antialiasDirection(other.top.?.point, other.bottom.?.point));
    if (dotFloat(direction, other_direction) > max_bisector_cosine) return;
    const normal = [2]f64{ direction[1], -direction[0] };
    const other_normal = [2]f64{ other_direction[1], -other_direction[0] };
    const side: f64 = if (edge.winding > 0) grid_per_pixel else -grid_per_pixel;
    const other_side: f64 = if (other.winding > 0) grid_per_pixel else -grid_per_pixel;
    const distance = side - dotFloat(normal, relativePoint(edge.top.?.point, vertex.point));
    const other_distance = other_side - dotFloat(other_normal, relativePoint(other.top.?.point, vertex.point));
    const offset = solveLines(normal, distance, other_normal, other_distance) orelse return;
    const point = offsetPoint(vertex.point, offset) orelse return;
    vertex.partner = try makeVertex(tessellator, point, if (edge.kind == .outer) 1 else 0);
}

/// Skia's test for edges between a region where rings overlap and one where they do not.
fn isOverlapEdge(edge: *const Edge) bool {
    return switch (edge.kind) {
        .outer => edge.winding != 0 and edge.winding != 1,
        .inner => edge.winding != 0 and edge.winding != -2,
        .connector => false,
    };
}

/// Finds the edges around overlapping rings in a ring mesh and collapses each overlap along its
/// straight skeleton. True if there was any.
fn collapseOverlapRegions(tessellator: *Tessellator, mesh: *VertexList, ring: EdgeKind) InternalError!bool {
    assert(ring != .connector);
    var skeleton: Skeleton = .{ .tessellator = tessellator, .mesh = mesh, .ring = ring, .events = .initContext(ring) };
    var active: EdgeList = .{};
    var vertex = mesh.head;
    while (vertex) |current| : (vertex = current.next) {
        if (!current.isConnected()) continue;
        const enclosing = try tessellator.findEnclosingEdges(current, &active);
        var above = current.last_edge_above;
        while (above) |edge| {
            if (edge == enclosing[0]) break;
            try tessellator.charge();
            const prev = edge.prev_edge_above orelse enclosing[0];
            assert(active.remove(edge));
            const left_overlap = if (prev) |left| isOverlapEdge(left) else false;
            const right_overlap = isOverlapEdge(edge);
            const outer_boundary = edge.kind == .outer and (prev == null or prev.?.winding == 0 or edge.winding == 0);
            if (prev) |left| edge.winding -= left.winding;
            if (left_overlap and right_overlap) {
                edge.disconnect();
            } else if (left_overlap or right_overlap) {
                try skeleton.addEdge(edge);
                if (outer_boundary) {
                    // Collinear merges sum windings, the fill needs the outer ring's scale back.
                    edge.winding = if (edge.winding < 0) -1 else 1;
                } else edge.disconnect();
            }
            above = prev;
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
    while (skeleton.events.pop()) |event| {
        try tessellator.charge();
        try skeleton.applyEvent(event);
    }
    // Unlike Skia's ring edges, connectors give crossings on them interpolated coverage.
    for (skeleton.edges.items) |skeleton_edge| if (skeleton_edge.edge != null) {
        try tessellator.makeConnectingEdge(skeleton_edge.prev.vertex, skeleton_edge.next.vertex, .connector, 0);
    };
    return skeleton.edges.items.len > 0;
}

const SkeletonVertex = struct {
    vertex: *Vertex,
    prev: ?*SkeletonEdge = null,
    next: ?*SkeletonEdge = null,
};

const SkeletonEdge = struct {
    /// The overlap edge, null once collapsed.
    edge: ?*Edge,
    event: ?*CollapseEvent = null,
    prev: *SkeletonVertex,
    next: *SkeletonVertex,
};

const CollapseEvent = struct {
    /// Null once a neighbor's collapse replaced the event.
    skeleton_edge: ?*SkeletonEdge,
    point: GridPoint,
    coverage: f32,
    priority: f32,
    /// The skeleton edge's ends when the event was made, they break ties.
    ends: [2]GridPoint,
};

/// Inner rings collapse higher coverage first, outer rings lower coverage first, as in Skia.
/// Ties go by sweep order, so the order is total.
fn eventOrder(ring: EdgeKind, event: *CollapseEvent, other: *CollapseEvent) std.math.Order {
    if (event.priority != other.priority) {
        const first = if (ring == .inner) event.priority > other.priority else event.priority < other.priority;
        return if (first) .lt else .gt;
    }
    const pairs = [_][2]GridPoint{ .{ event.point, other.point }, .{ event.ends[0], other.ends[0] }, .{ event.ends[1], other.ends[1] } };
    for (pairs) |pair| {
        if (sweepLessThan(pair[0], pair[1])) return .lt;
        if (sweepLessThan(pair[1], pair[0])) return .gt;
    }
    return .eq;
}

const Skeleton = struct {
    tessellator: *Tessellator,
    mesh: *VertexList,
    ring: EdgeKind,
    events: std.PriorityQueue(*CollapseEvent, EdgeKind, eventOrder),
    vertices: std.AutoHashMapUnmanaged(*Vertex, *SkeletonVertex) = .empty,
    edges: std.ArrayList(*SkeletonEdge) = .empty,

    fn vertexFor(skeleton: *Skeleton, vertex: *Vertex) error{OutOfMemory}!*SkeletonVertex {
        const arena = skeleton.tessellator.arena;
        const entry = try skeleton.vertices.getOrPut(arena, vertex);
        if (!entry.found_existing) {
            entry.value_ptr.* = try arena.create(SkeletonVertex);
            entry.value_ptr.*.* = .{ .vertex = vertex };
        }
        return entry.value_ptr.*;
    }

    fn addEdge(skeleton: *Skeleton, edge: *Edge) InternalError!void {
        const arena = skeleton.tessellator.arena;
        const prev = try skeleton.vertexFor(if (edge.winding < 0) edge.bottom.? else edge.top.?);
        const next = try skeleton.vertexFor(if (edge.winding < 0) edge.top.? else edge.bottom.?);
        const skeleton_edge = try arena.create(SkeletonEdge);
        skeleton_edge.* = .{ .edge = edge, .prev = prev, .next = next };
        try skeleton.edges.append(arena, skeleton_edge);
        prev.next = skeleton_edge;
        next.prev = skeleton_edge;
        try skeleton.makeEvent(skeleton_edge);
    }

    /// Where the bisectors at both ends of the skeleton edge meet. Inner ramps sum there, outer
    /// ramps take the larger value, which stays on the ramp of each edge where only one reaches.
    fn makeEvent(skeleton: *Skeleton, skeleton_edge: *SkeletonEdge) InternalError!void {
        const prev = skeleton_edge.prev.vertex;
        const next = skeleton_edge.next.vertex;
        if (prev == next) return;
        const prev_partner = prev.partner orelse return;
        const next_partner = next.partner orelse return;
        const bisector = bisectorEdge(prev, prev_partner) orelse return;
        const other_bisector = bisectorEdge(next, next_partner) orelse return;
        const point = tessellation.segmentCrossing(&bisector, &other_bisector) orelse return;
        const coverage = coverageAlong(prev, prev_partner, point);
        const other_coverage = coverageAlong(next, next_partner, point);
        const larger = @max(coverage, other_coverage);
        const collapsed = if (skeleton.ring == .inner) std.math.clamp(coverage + other_coverage - 1, 0, 1) else larger;
        try skeleton.pushEvent(skeleton_edge, point, collapsed, larger);
    }

    /// Where the bisector at `vertex` meets the line through `destination` parallel to the
    /// skeleton edge, within the edge's sweep range.
    fn makeEventAlong(skeleton: *Skeleton, skeleton_edge: *SkeletonEdge, vertex: *Vertex, destination: *Vertex) InternalError!void {
        const partner = vertex.partner orelse return;
        const edge = skeleton_edge.edge orelse return;
        if (!edge.isAlive()) return;
        const top = edge.top.?.point;
        const bottom = edge.bottom.?.point;
        const parallel_end: GridPoint = .{ destination.point[0] + (bottom[0] - top[0]), destination.point[1] + (bottom[1] - top[1]) };
        const crossing = tessellation.lineCrossing(.{ destination.point, parallel_end }, .{ vertex.point, partner.point }) orelse return;
        for (crossing) |coordinate| if (@abs(coordinate) > max_grid_coordinate) return;
        const point: GridPoint = .{ @intCast(crossing[0]), @intCast(crossing[1]) };
        if (sweepLessThan(point, top) or !sweepLessThan(point, bottom)) return;
        try skeleton.pushEvent(skeleton_edge, point, destination.coverage, destination.coverage);
    }

    fn pushEvent(skeleton: *Skeleton, skeleton_edge: *SkeletonEdge, point: GridPoint, coverage: f32, priority: f32) InternalError!void {
        assert(!std.math.isNan(priority));
        const arena = skeleton.tessellator.arena;
        const event = try arena.create(CollapseEvent);
        const ends = [2]GridPoint{ skeleton_edge.prev.vertex.point, skeleton_edge.next.vertex.point };
        event.* = .{ .skeleton_edge = skeleton_edge, .point = point, .coverage = coverage, .priority = priority, .ends = ends };
        skeleton_edge.event = event;
        try skeleton.events.push(arena, event);
    }

    /// Collapses the skeleton edge into one vertex and makes events for its neighbors.
    fn applyEvent(skeleton: *Skeleton, event: *CollapseEvent) InternalError!void {
        const skeleton_edge = event.skeleton_edge orelse return;
        assert(skeleton_edge.event == event);
        if (skeleton_edge.edge == null) return;
        const prev = skeleton_edge.prev.vertex;
        const next = skeleton_edge.next.vertex;
        const prev_edge = skeleton_edge.prev.prev orelse return;
        const next_edge = skeleton_edge.next.next orelse return;
        if (prev_edge.edge == null or next_edge.edge == null) return;
        const tessellator = skeleton.tessellator;
        const destination = try tessellator.makeSortedVertex(event.point, skeleton.mesh, prev, event.coverage);
        destination.synthetic = true;
        const joined = try tessellator.arena.create(SkeletonVertex);
        joined.* = .{ .vertex = destination, .prev = prev_edge, .next = next_edge };
        skeleton_edge.edge = null;
        try skeleton.connect(prev, destination);
        try skeleton.connect(next, destination);
        prev_edge.next = joined;
        next_edge.prev = joined;
        if (prev_edge.event) |stale| stale.skeleton_edge = null;
        if (next_edge.event) |stale| stale.skeleton_edge = null;
        if (prev_edge.prev == next_edge.next) {
            try skeleton.connect(prev_edge.prev.vertex, destination);
            prev_edge.edge = null;
            next_edge.edge = null;
            return;
        }
        try computeBisector(tessellator, prev_edge.edge.?, next_edge.edge.?, destination);
        if (destination.partner != null) {
            try skeleton.makeEvent(prev_edge);
            try skeleton.makeEvent(next_edge);
        } else {
            try skeleton.makeEventAlong(prev_edge, prev_edge.prev.vertex, destination);
            try skeleton.makeEventAlong(next_edge, next_edge.next.vertex, destination);
        }
    }

    fn connect(skeleton: *Skeleton, vertex: *Vertex, destination: *Vertex) InternalError!void {
        if (vertex == destination) return;
        if (vertex.synthetic) return skeleton.tessellator.makeConnectingEdge(vertex, destination, .connector, 0);
        const partner = vertex.partner orelse return;
        partner.partner = destination;
        vertex.partner = null;
        vertex.collapsed_into = destination;
    }
};

/// A temporary edge from a vertex to its partner, null if they coincide.
fn bisectorEdge(vertex: *Vertex, partner: *Vertex) ?Edge {
    if (tessellation.pointsEqual(vertex.point, partner.point)) return null;
    const downward = sweepLessThan(vertex.point, partner.point);
    const top = if (downward) vertex else partner;
    const bottom = if (downward) partner else vertex;
    return .{ .winding = 1, .kind = .connector, .top = top, .bottom = bottom, .line = .{ top.point, bottom.point } };
}

fn dotFloat(a: [2]f64, b: [2]f64) f64 {
    return a[0] * b[0] + a[1] * b[1];
}

fn scaled(vector: [2]f64, factor: f64) [2]f64 {
    return .{ vector[0] * factor, vector[1] * factor };
}
