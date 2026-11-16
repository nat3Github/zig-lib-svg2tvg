//! Sweep-line tessellator for filled contours in pixel coordinates, a port of Skia's
//! GrTriangulator onto an integer grid. Points snap to 1/256 px, every topology decision is an
//! exact integer predicate, and crossings round to the grid. Every limit returns an error.
//!
//! Ported from Skia, src/gpu/ganesh/geometry/GrTriangulator.{h,cpp}:
//!
//! Copyright 2015 Google Inc.
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
const Allocator = std.mem.Allocator;
pub const arrangement = @import("arrangement.zig");
pub const tvg = @import("tinyvg/tinyvg.zig");
const antialias = @import("tessellation_antialias.zig");
const GridPoint = arrangement.GridPoint;

pub const Point = [2]f32;
pub const FillRule = arrangement.FillRule;
pub const Work = arrangement.Work;
pub const Error = error{ OutOfMemory, InvalidInput, TooComplex };

pub const grid_per_pixel = 256;
/// Grid points up to 2^24 are exact in f32, the rest leaves room for anti-aliasing.
pub const max_coordinate_pixels = 1 << 15;

pub const Limits = struct {
    max_points: u32 = 1 << 16,
    max_edges: u32 = 1 << 20,
    /// Active edge scans, rewind steps and list walks.
    max_work: usize = 1 << 22,
};

/// Measured on icons: 50 to 100 KiB for 8 to 80 points.
const min_scratch_bytes = 64 << 10;
const scratch_bytes_per_point = 1 << 10;
const max_scratch_bytes = 1 << 20;

pub const Options = struct {
    fill_rule: FillRule,
    anti_alias: bool,
    limits: Limits = .{},
    /// A budget shared by several fills, in place of `limits.max_work` per fill.
    work: ?*Work = null,
};

/// Owned by the allocator given to `fill`.
pub const Mesh = struct {
    positions: []const Point,
    coverages: []const f32,
    /// Triangles with (b - a) × (c - a) > 0, y down.
    indices: []const u32,

    pub fn deinit(mesh: Mesh, allocator: Allocator) void {
        allocator.free(mesh.positions);
        allocator.free(mesh.coverages);
        allocator.free(mesh.indices);
    }
};

pub fn fill(allocator: Allocator, contours: []const []const Point, options: Options) Error!Mesh {
    var point_count: usize = 0;
    for (contours) |contour| point_count += contour.len;
    if (point_count > options.limits.max_points) return error.TooComplex;

    // A bump buffer in front of the arena spares its atomic bookkeeping on every vertex and edge.
    const scratch = try allocator.alloc(u8, @min(max_scratch_bytes, min_scratch_bytes + point_count * scratch_bytes_per_point));
    defer allocator.free(scratch);
    var arena_state: std.heap.ArenaAllocator = .init(allocator);
    defer arena_state.deinit();
    var scratch_state: std.heap.BufferFirstAllocator = .init(scratch, arena_state.allocator());
    const arena = scratch_state.allocator();

    const grid_contours = try arena.alloc([]GridPoint, contours.len);
    var minimum: GridPoint = @splat(std.math.maxInt(i32));
    var maximum: GridPoint = @splat(std.math.minInt(i32));
    for (grid_contours, contours) |*grid_contour, contour| {
        grid_contour.* = try arena.alloc(GridPoint, contour.len);
        for (grid_contour.*, contour) |*grid_point, point| {
            for (grid_point, point, &minimum, &maximum) |*coordinate, value, *low, *high| {
                if (!(@abs(value) <= max_coordinate_pixels)) return error.InvalidInput;
                coordinate.* = @intFromFloat(@round(value * grid_per_pixel));
                low.* = @min(low.*, coordinate.*);
                high.* = @max(high.*, coordinate.*);
            }
        }
    }
    // Skia sweeps wide paths along x. Rotating by 90° keeps orientations and the y sweep.
    const rotated = point_count > 0 and @as(i64, maximum[0]) - minimum[0] > @as(i64, maximum[1]) - minimum[1];
    if (rotated) for (grid_contours) |grid_contour| for (grid_contour) |*point| {
        const original = point.*;
        point.* = .{ -original[1], original[0] };
    };

    var own_work: Work = .{ .remaining = options.limits.max_work };
    var tessellator: Tessellator = .{ .arena = arena, .limits = options.limits, .work = options.work orelse &own_work };
    var mesh: VertexList = .{};
    for (grid_contours) |grid_contour| try tessellator.buildContourEdges(try sanitizedContour(arena, grid_contour), &mesh);
    try tessellator.sortMesh(&mesh);
    _ = try tessellator.mergeCoincidentVertices(&mesh);
    _ = try tessellator.simplify(&mesh);

    var output: Output = .{ .allocator = allocator, .rotated = rotated };
    errdefer output.deinit();
    if (options.anti_alias) {
        const result = try antialias.tessellate(&tessellator, mesh, options.fill_rule);
        try output.reserve(tessellator.edge_count + result.triangles.len);
        try output.emitPolys(result.polys, .nonzero);
        for (result.triangles) |triangle| try output.emitTriangle(triangle[0], triangle[1], triangle[2]);
    } else {
        const polys = try tessellator.tessellate(mesh);
        try output.reserve(tessellator.edge_count);
        try output.emitPolys(polys, options.fill_rule);
    }

    const positions = try output.positions.toOwnedSlice(allocator);
    errdefer allocator.free(positions);
    const coverages = try output.coverages.toOwnedSlice(allocator);
    errdefer allocator.free(coverages);
    const indices = try output.indices.toOwnedSlice(allocator);
    return .{ .positions = positions, .coverages = coverages, .indices = indices };
}

pub const InternalError = error{ OutOfMemory, TooComplex };

/// Skia's bound on one chain of collinear merges.
const max_merge_collinear_calls = 64;

const Side = enum { left, right };

/// Anti-aliasing: edges of the inner and outer ring, and structural connectors with winding 0.
pub const EdgeKind = enum { inner, outer, connector };

pub const Vertex = struct {
    point: GridPoint,
    /// Contour order, then sweep order in the mesh, then monotone polygon order.
    prev: ?*Vertex = null,
    next: ?*Vertex = null,
    first_edge_above: ?*Edge = null,
    last_edge_above: ?*Edge = null,
    first_edge_below: ?*Edge = null,
    last_edge_below: ?*Edge = null,
    left_enclosing_edge: ?*Edge = null,
    right_enclosing_edge: ?*Edge = null,
    output_index: ?u32 = null,
    /// Anti-aliasing: coverage at the vertex, triangles interpolate it.
    coverage: f32 = 1,
    /// Anti-aliasing: the corresponding vertex on the other ring.
    partner: ?*Vertex = null,
    /// Anti-aliasing: made by a crossing or a merge, not by stroking the boundary.
    synthetic: bool = false,
    /// Anti-aliasing: the skeleton vertex replacing this ring vertex, for partners it shares.
    collapsed_into: ?*Vertex = null,
    /// Anti-aliasing: the filled region's boundary passes this vertex more than once.
    pinch: bool = false,

    pub fn isConnected(vertex: *const Vertex) bool {
        return vertex.first_edge_above != null or vertex.first_edge_below != null;
    }
};

pub const Edge = struct {
    /// +1 when the input runs from top to bottom.
    winding: i32,
    kind: EdgeKind = .inner,
    /// Both null once merged away.
    top: ?*Vertex,
    bottom: ?*Vertex,
    /// The input segment this edge is a piece of. Crossings come from it, so rounding errors do not
    /// accumulate across splits.
    line: [2]GridPoint,
    left: ?*Edge = null,
    right: ?*Edge = null,
    prev_edge_above: ?*Edge = null,
    next_edge_above: ?*Edge = null,
    prev_edge_below: ?*Edge = null,
    next_edge_below: ?*Edge = null,
    left_poly: ?*Poly = null,
    right_poly: ?*Poly = null,
    left_poly_prev: ?*Edge = null,
    left_poly_next: ?*Edge = null,
    right_poly_prev: ?*Edge = null,
    right_poly_next: ?*Edge = null,
    used_in_left_poly: bool = false,
    used_in_right_poly: bool = false,

    pub fn isAlive(edge: *const Edge) bool {
        assert((edge.top == null) == (edge.bottom == null));
        return edge.top != null;
    }

    fn side(edge: *const Edge, point: GridPoint) i64 {
        return orientation(edge.top.?.point, edge.bottom.?.point, point);
    }

    /// The edge passes strictly left of the point.
    fn isLeftOf(edge: *const Edge, point: GridPoint) bool {
        return edge.side(point) < 0;
    }

    fn isRightOf(edge: *const Edge, point: GridPoint) bool {
        return edge.side(point) > 0;
    }

    pub fn disconnect(edge: *Edge) void {
        removeEdgeAbove(edge);
        removeEdgeBelow(edge);
    }
};

pub const VertexList = struct {
    head: ?*Vertex = null,
    tail: ?*Vertex = null,

    pub fn insert(list: *VertexList, vertex: *Vertex, prev: ?*Vertex, next: ?*Vertex) void {
        listInsert(Vertex, "prev", "next", vertex, prev, next, &list.head, &list.tail);
    }

    pub fn append(list: *VertexList, vertex: *Vertex) void {
        list.insert(vertex, list.tail, null);
    }

    pub fn appendList(list: *VertexList, other: VertexList) void {
        const other_head = other.head orelse return;
        if (list.tail) |tail| {
            tail.next = other_head;
            other_head.prev = tail;
        } else list.head = other_head;
        list.tail = other.tail;
    }

    fn prepend(list: *VertexList, vertex: *Vertex) void {
        list.insert(vertex, null, list.head);
    }

    fn remove(list: *VertexList, vertex: *Vertex) void {
        listRemove(Vertex, "prev", "next", vertex, &list.head, &list.tail);
    }
};

pub const EdgeList = struct {
    head: ?*Edge = null,
    tail: ?*Edge = null,

    fn contains(list: *const EdgeList, edge: *const Edge) bool {
        return edge.left != null or edge.right != null or list.head == edge;
    }

    pub fn insertAfter(list: *EdgeList, edge: *Edge, prev: ?*Edge) bool {
        if (list.contains(edge)) return false;
        const next = if (prev) |left| left.right else list.head;
        listInsert(Edge, "left", "right", edge, prev, next, &list.head, &list.tail);
        return true;
    }

    pub fn remove(list: *EdgeList, edge: *Edge) bool {
        if (!list.contains(edge)) return false;
        listRemove(Edge, "left", "right", edge, &list.head, &list.tail);
        return true;
    }
};

const MonotonePoly = struct {
    side: Side,
    first_edge: ?*Edge = null,
    last_edge: ?*Edge = null,
    prev: ?*MonotonePoly = null,
    next: ?*MonotonePoly = null,
    winding: i32,

    fn addEdge(monotone: *MonotonePoly, edge: *Edge) void {
        switch (monotone.side) {
            .right => {
                assert(!edge.used_in_right_poly);
                listInsert(Edge, "right_poly_prev", "right_poly_next", edge, monotone.last_edge, null, &monotone.first_edge, &monotone.last_edge);
                edge.used_in_right_poly = true;
            },
            .left => {
                assert(!edge.used_in_left_poly);
                listInsert(Edge, "left_poly_prev", "left_poly_next", edge, monotone.last_edge, null, &monotone.first_edge, &monotone.last_edge);
                edge.used_in_left_poly = true;
            },
        }
    }
};

pub const Poly = struct {
    first_vertex: *Vertex,
    winding: i32,
    head: ?*MonotonePoly = null,
    tail: ?*MonotonePoly = null,
    next: ?*Poly = null,
    partner: ?*Poly = null,
    count: u32 = 0,

    fn lastVertex(poly: *const Poly) *Vertex {
        const tail = poly.tail orelse return poly.first_vertex;
        return tail.last_edge.?.bottom.?;
    }
};

/// The active edge list and the vertex the sweep is at.
const Sweep = struct {
    active: EdgeList = .{},
    current: ?*Vertex,
};

pub const Tessellator = struct {
    arena: Allocator,
    limits: Limits,
    edge_count: usize = 0,
    work: *Work,
    merge_collinear_calls: u32 = 0,

    pub fn charge(tessellator: *Tessellator) error{TooComplex}!void {
        return tessellator.work.charge();
    }

    fn allocateEdge(tessellator: *Tessellator, top: *Vertex, bottom: *Vertex, winding: i32, kind: EdgeKind) InternalError!*Edge {
        if (tessellator.edge_count >= tessellator.limits.max_edges) return error.TooComplex;
        tessellator.edge_count += 1;
        const edge = try tessellator.arena.create(Edge);
        edge.* = .{ .winding = winding, .kind = kind, .top = top, .bottom = bottom, .line = .{ top.point, bottom.point } };
        return edge;
    }

    fn buildContourEdges(tessellator: *Tessellator, points: []const GridPoint, mesh: *VertexList) InternalError!void {
        if (points.len == 0) return;
        const vertices = try tessellator.arena.alloc(Vertex, points.len);
        for (vertices, points) |*vertex, point| {
            vertex.* = .{ .point = point };
            mesh.append(vertex);
        }
        var prev = &vertices[vertices.len - 1];
        for (vertices) |*vertex| {
            try tessellator.makeConnectingEdge(prev, vertex, .inner, 1);
            prev = vertex;
        }
    }

    /// `winding_scale` is right minus left when walking from `from` to `to`.
    pub fn makeConnectingEdge(tessellator: *Tessellator, from: *Vertex, to: *Vertex, kind: EdgeKind, winding_scale: i32) InternalError!void {
        if (pointsEqual(from.point, to.point)) return;
        const downward = sweepLessThan(from.point, to.point);
        const winding: i32 = if (downward) winding_scale else -winding_scale;
        const edge = try tessellator.allocateEdge(if (downward) from else to, if (downward) to else from, winding, kind);
        try tessellator.insertBelow(edge, edge.top.?);
        try tessellator.insertAbove(edge, edge.bottom.?);
        tessellator.merge_collinear_calls = 0;
        try tessellator.mergeCollinearEdges(edge, null);
    }

    pub fn sortMesh(tessellator: *Tessellator, mesh: *VertexList) InternalError!void {
        var vertices: std.ArrayList(*Vertex) = .empty;
        var vertex = mesh.head;
        while (vertex) |current| : (vertex = current.next) try vertices.append(tessellator.arena, current);
        std.mem.sort(*Vertex, vertices.items, {}, vertexLessThan);
        mesh.* = .{};
        for (vertices.items) |current| mesh.append(current);
    }

    pub fn mergeCoincidentVertices(tessellator: *Tessellator, mesh: *VertexList) InternalError!bool {
        var merged = false;
        var vertex = if (mesh.head) |head| head.next else null;
        while (vertex) |current| {
            const next = current.next;
            const prev = current.prev.?;
            assert(!sweepLessThan(current.point, prev.point));
            if (pointsEqual(current.point, prev.point)) {
                try tessellator.mergeVertices(current, prev, mesh);
                merged = true;
            }
            vertex = next;
        }
        return merged;
    }

    fn mergeVertices(tessellator: *Tessellator, source: *Vertex, destination: *Vertex, mesh: *VertexList) InternalError!void {
        assert(source != destination);
        destination.coverage = @max(destination.coverage, source.coverage);
        if (source.partner) |partner| partner.partner = destination;
        while (source.first_edge_above) |edge| {
            tessellator.merge_collinear_calls = 0;
            try tessellator.setBottom(edge, destination, null);
        }
        while (source.first_edge_below) |edge| {
            tessellator.merge_collinear_calls = 0;
            try tessellator.setTop(edge, destination, null);
        }
        mesh.remove(source);
        destination.synthetic = true;
    }

    fn insertAbove(tessellator: *Tessellator, edge: *Edge, vertex: *Vertex) InternalError!void {
        const top = edge.top.?;
        assert(sweepLessThan(top.point, edge.bottom.?.point));
        var prev: ?*Edge = null;
        var next = vertex.first_edge_above;
        while (next) |candidate| : (next = candidate.next_edge_above) {
            try tessellator.charge();
            if (candidate.isRightOf(top.point)) break;
            prev = candidate;
        }
        listInsert(Edge, "prev_edge_above", "next_edge_above", edge, prev, next, &vertex.first_edge_above, &vertex.last_edge_above);
    }

    fn insertBelow(tessellator: *Tessellator, edge: *Edge, vertex: *Vertex) InternalError!void {
        const bottom = edge.bottom.?;
        assert(sweepLessThan(edge.top.?.point, bottom.point));
        var prev: ?*Edge = null;
        var next = vertex.first_edge_below;
        while (next) |candidate| : (next = candidate.next_edge_below) {
            try tessellator.charge();
            if (candidate.isRightOf(bottom.point)) break;
            prev = candidate;
        }
        listInsert(Edge, "prev_edge_below", "next_edge_below", edge, prev, next, &vertex.first_edge_below, &vertex.last_edge_below);
    }

    /// Undoes the sweep back to `destination`, further if a reinserted edge is out of order.
    fn rewind(tessellator: *Tessellator, sweep_or_null: ?*Sweep, destination: *Vertex) InternalError!void {
        const sweep = sweep_or_null orelse return;
        const current = sweep.current.?;
        if (current == destination or sweepLessThan(current.point, destination.point)) return;
        var target = destination;
        var vertex = current;
        while (vertex != target) {
            try tessellator.charge();
            vertex = vertex.prev.?;
            var below = vertex.first_edge_below;
            while (below) |edge| : (below = edge.next_edge_below) {
                try tessellator.charge();
                assert(sweep.active.remove(edge));
            }
            var left_edge = vertex.left_enclosing_edge;
            var above = vertex.first_edge_above;
            while (above) |edge| : (above = edge.next_edge_above) {
                try tessellator.charge();
                assert(sweep.active.insertAfter(edge, left_edge));
                left_edge = edge;
                const top = edge.top.?;
                if (!sweepLessThan(top.point, target.point)) continue;
                const left_out_of_order = if (top.left_enclosing_edge) |enclosing| !enclosing.isLeftOf(top.point) else false;
                const right_out_of_order = if (top.right_enclosing_edge) |enclosing| !enclosing.isRightOf(top.point) else false;
                if (left_out_of_order or right_out_of_order) target = top;
            }
        }
        sweep.current = vertex;
    }

    fn rewindIfNecessary(tessellator: *Tessellator, edge: *Edge, sweep_or_null: ?*Sweep) InternalError!void {
        if (sweep_or_null == null) return;
        const top = edge.top.?;
        const bottom = edge.bottom.?;
        if (edge.left) |left| {
            const left_top = left.top.?;
            const left_bottom = left.bottom.?;
            if (sweepLessThan(left_top.point, top.point) and !left.isLeftOf(top.point)) {
                try tessellator.rewind(sweep_or_null, left_top);
            } else if (sweepLessThan(top.point, left_top.point) and !edge.isRightOf(left_top.point)) {
                try tessellator.rewind(sweep_or_null, top);
            } else if (sweepLessThan(bottom.point, left_bottom.point) and !left.isLeftOf(bottom.point)) {
                try tessellator.rewind(sweep_or_null, left_top);
            } else if (sweepLessThan(left_bottom.point, bottom.point) and !edge.isRightOf(left_bottom.point)) {
                try tessellator.rewind(sweep_or_null, top);
            }
        }
        if (edge.right) |right| {
            const right_top = right.top.?;
            const right_bottom = right.bottom.?;
            if (sweepLessThan(right_top.point, top.point) and !right.isRightOf(top.point)) {
                try tessellator.rewind(sweep_or_null, right_top);
            } else if (sweepLessThan(top.point, right_top.point) and !edge.isLeftOf(right_top.point)) {
                try tessellator.rewind(sweep_or_null, top);
            } else if (sweepLessThan(bottom.point, right_bottom.point) and !right.isRightOf(bottom.point)) {
                try tessellator.rewind(sweep_or_null, right_top);
            } else if (sweepLessThan(right_bottom.point, bottom.point) and !edge.isLeftOf(right_bottom.point)) {
                try tessellator.rewind(sweep_or_null, top);
            }
        }
    }

    fn setTop(tessellator: *Tessellator, edge: *Edge, vertex: *Vertex, sweep: ?*Sweep) InternalError!void {
        assert(edge.isAlive());
        removeEdgeBelow(edge);
        edge.top = vertex;
        try tessellator.insertBelow(edge, vertex);
        // The new top turns the edge, so its place among the edges above its bottom can change.
        removeEdgeAbove(edge);
        try tessellator.insertAbove(edge, edge.bottom.?);
        try tessellator.rewindIfNecessary(edge, sweep);
        try tessellator.mergeCollinearEdges(edge, sweep);
    }

    fn setBottom(tessellator: *Tessellator, edge: *Edge, vertex: *Vertex, sweep: ?*Sweep) InternalError!void {
        assert(edge.isAlive());
        removeEdgeAbove(edge);
        edge.bottom = vertex;
        try tessellator.insertAbove(edge, vertex);
        // The new bottom turns the edge, so its place among the edges below its top can change.
        removeEdgeBelow(edge);
        try tessellator.insertBelow(edge, edge.top.?);
        try tessellator.rewindIfNecessary(edge, sweep);
        try tessellator.mergeCollinearEdges(edge, sweep);
    }

    /// `edge` and `other` share their bottom vertex and lie on one line.
    fn mergeEdgesAbove(tessellator: *Tessellator, edge: *Edge, other: *Edge, sweep: ?*Sweep) InternalError!void {
        assert(edge.isAlive() and other.isAlive());
        if (pointsEqual(edge.top.?.point, other.top.?.point)) {
            try tessellator.rewind(sweep, edge.top.?);
            other.winding += edge.winding;
            kill(edge, sweep);
        } else if (sweepLessThan(edge.top.?.point, other.top.?.point)) {
            try tessellator.rewind(sweep, edge.top.?);
            other.winding += edge.winding;
            try tessellator.setBottom(edge, other.top.?, sweep);
        } else {
            try tessellator.rewind(sweep, other.top.?);
            edge.winding += other.winding;
            try tessellator.setBottom(other, edge.top.?, sweep);
        }
    }

    /// `edge` and `other` share their top vertex and lie on one line.
    fn mergeEdgesBelow(tessellator: *Tessellator, edge: *Edge, other: *Edge, sweep: ?*Sweep) InternalError!void {
        assert(edge.isAlive() and other.isAlive());
        if (pointsEqual(edge.bottom.?.point, other.bottom.?.point)) {
            try tessellator.rewind(sweep, edge.top.?);
            other.winding += edge.winding;
            kill(edge, sweep);
        } else if (sweepLessThan(edge.bottom.?.point, other.bottom.?.point)) {
            try tessellator.rewind(sweep, other.top.?);
            edge.winding += other.winding;
            try tessellator.setTop(other, edge.bottom.?, sweep);
        } else {
            try tessellator.rewind(sweep, edge.top.?);
            other.winding += edge.winding;
            try tessellator.setTop(edge, other.bottom.?, sweep);
        }
    }

    fn mergeCollinearEdges(tessellator: *Tessellator, edge: *Edge, sweep: ?*Sweep) InternalError!void {
        tessellator.merge_collinear_calls += 1;
        if (tessellator.merge_collinear_calls > max_merge_collinear_calls) return error.TooComplex;
        while (edge.isAlive()) {
            try tessellator.charge();
            if (topCollinear(edge.prev_edge_above, edge)) {
                try tessellator.mergeEdgesAbove(edge.prev_edge_above.?, edge, sweep);
            } else if (topCollinear(edge, edge.next_edge_above)) {
                try tessellator.mergeEdgesAbove(edge.next_edge_above.?, edge, sweep);
            } else if (bottomCollinear(edge.prev_edge_below, edge)) {
                try tessellator.mergeEdgesBelow(edge.prev_edge_below.?, edge, sweep);
            } else if (bottomCollinear(edge, edge.next_edge_below)) {
                try tessellator.mergeEdgesBelow(edge.next_edge_below.?, edge, sweep);
            } else break;
        }
    }

    fn splitEdge(tessellator: *Tessellator, edge: *Edge, vertex: *Vertex, sweep: *Sweep) InternalError!bool {
        if (!edge.isAlive()) return false;
        const edge_top = edge.top.?;
        const edge_bottom = edge.bottom.?;
        if (vertex == edge_top or vertex == edge_bottom) return false;
        var top: *Vertex = undefined;
        var bottom: *Vertex = undefined;
        var winding = edge.winding;
        if (sweepLessThan(vertex.point, edge_top.point)) {
            top = vertex;
            bottom = edge_top;
            winding = -winding;
            try tessellator.setTop(edge, vertex, sweep);
        } else if (sweepLessThan(edge_bottom.point, vertex.point)) {
            top = edge_bottom;
            bottom = vertex;
            winding = -winding;
            try tessellator.setBottom(edge, vertex, sweep);
        } else {
            top = vertex;
            bottom = edge_bottom;
            try tessellator.setBottom(edge, vertex, sweep);
        }
        const new_edge = try tessellator.allocateEdge(top, bottom, winding, edge.kind);
        new_edge.line = edge.line;
        try tessellator.insertBelow(new_edge, top);
        try tessellator.insertAbove(new_edge, bottom);
        tessellator.merge_collinear_calls = 0;
        try tessellator.mergeCollinearEdges(new_edge, sweep);
        return true;
    }

    /// Splits an edge at an end point of the other where the two touch or overlap.
    fn intersectEdgePair(tessellator: *Tessellator, left: *Edge, right: *Edge, sweep: *Sweep) InternalError!bool {
        if (left.top == right.top or left.bottom == right.bottom) return false;
        const left_top = left.top.?;
        const left_bottom = left.bottom.?;
        const right_top = right.top.?;
        const right_bottom = right.bottom.?;
        var split: ?*Edge = null;
        var split_at: *Vertex = undefined;
        if (sweepLessThan(left_top.point, right_top.point)) {
            if (!left.isLeftOf(right_top.point)) {
                split = left;
                split_at = right_top;
            }
        } else if (!right.isRightOf(left_top.point)) {
            split = right;
            split_at = left_top;
        }
        if (sweepLessThan(right_bottom.point, left_bottom.point)) {
            if (!left.isLeftOf(right_bottom.point)) {
                split = left;
                split_at = right_bottom;
            }
        } else if (!right.isRightOf(left_bottom.point)) {
            split = right;
            split_at = left_bottom;
        }
        const edge = split orelse return false;
        try tessellator.rewind(sweep, edge.top.?);
        return tessellator.splitEdge(edge, split_at, sweep);
    }

    fn checkForIntersection(tessellator: *Tessellator, left_or_null: ?*Edge, right_or_null: ?*Edge, sweep: *Sweep, mesh: *VertexList) InternalError!bool {
        const left = left_or_null orelse return false;
        const right = right_or_null orelse return false;
        assert(left.isAlive() and right.isAlive());
        const crossing = segmentCrossing(left, right) orelse return tessellator.intersectEdgePair(left, right, sweep);
        var top = sweep.current;
        while (top) |vertex| : (top = vertex.prev) {
            try tessellator.charge();
            if (!sweepLessThan(crossing, vertex.point)) break;
        }
        const coverage = crossingCoverage(left, right, crossing);
        const end_vertex = for ([_]*Vertex{ left.top.?, left.bottom.?, right.top.?, right.bottom.? }) |end| {
            if (pointsEqual(crossing, end.point)) break end;
        } else null;
        const vertex = end_vertex orelse vertex: {
            const vertex = try tessellator.makeSortedVertex(crossing, mesh, top, coverage);
            if (left.top.?.partner != null) {
                vertex.synthetic = true;
                try antialias.computeBisector(tessellator, left, right, vertex);
            }
            break :vertex vertex;
        };
        // A split off the chord turns it, so vertices the sweep already passed can now touch it.
        var rewind_target = top orelse vertex;
        for ([_]*Edge{ left, right }) |edge| {
            const edge_top = edge.top.?;
            if (edge.side(vertex.point) != 0 and sweepLessThan(edge_top.point, rewind_target.point)) rewind_target = edge_top;
        }
        try tessellator.rewind(sweep, rewind_target);
        _ = try tessellator.splitEdge(left, vertex, sweep);
        _ = try tessellator.splitEdge(right, vertex, sweep);
        vertex.coverage = @max(vertex.coverage, coverage);
        return true;
    }

    pub fn makeSortedVertex(tessellator: *Tessellator, point: GridPoint, mesh: *VertexList, reference: ?*Vertex, coverage: f32) InternalError!*Vertex {
        var prev = reference;
        while (prev) |candidate| {
            if (!sweepLessThan(point, candidate.point)) break;
            try tessellator.charge();
            prev = candidate.prev;
        }
        var next = if (prev) |candidate| candidate.next else mesh.head;
        while (next) |candidate| {
            if (!sweepLessThan(candidate.point, point)) break;
            try tessellator.charge();
            prev = candidate;
            next = candidate.next;
        }
        if (prev) |candidate| if (pointsEqual(candidate.point, point)) return candidate;
        if (next) |candidate| if (pointsEqual(candidate.point, point)) return candidate;
        const vertex = try tessellator.arena.create(Vertex);
        vertex.* = .{ .point = point, .coverage = coverage };
        mesh.insert(vertex, prev, next);
        return vertex;
    }

    pub fn findEnclosingEdges(tessellator: *Tessellator, vertex: *const Vertex, active: *const EdgeList) error{TooComplex}![2]?*Edge {
        if (vertex.first_edge_above) |first_above| return .{ first_above.left, vertex.last_edge_above.?.right };
        var next: ?*Edge = null;
        var prev = active.tail;
        while (prev) |edge| : (prev = edge.left) {
            try tessellator.charge();
            if (edge.isLeftOf(vertex.point)) break;
            next = edge;
        }
        return .{ prev, next };
    }

    /// Bentley-Ottmann: splits crossing edges until edges meet only at vertices. True if it split any.
    pub fn simplify(tessellator: *Tessellator, mesh: *VertexList) InternalError!bool {
        var found_crossing = false;
        var sweep: Sweep = .{ .current = mesh.head };
        while (sweep.current) |start| {
            try tessellator.charge();
            if (!start.isConnected()) {
                sweep.current = start.next;
                continue;
            }
            var enclosing: [2]?*Edge = undefined;
            while (true) {
                const vertex = sweep.current.?;
                enclosing = try tessellator.findEnclosingEdges(vertex, &sweep.active);
                vertex.left_enclosing_edge = enclosing[0];
                vertex.right_enclosing_edge = enclosing[1];
                const restart = if (vertex.first_edge_below) |first_below| restart: {
                    var below: ?*Edge = first_below;
                    while (below) |edge| : (below = edge.next_edge_below) {
                        try tessellator.charge();
                        if (try tessellator.checkForIntersection(enclosing[0], edge, &sweep, mesh)) break :restart true;
                        if (try tessellator.checkForIntersection(edge, enclosing[1], &sweep, mesh)) break :restart true;
                    }
                    break :restart false;
                } else try tessellator.checkForIntersection(enclosing[0], enclosing[1], &sweep, mesh);
                if (!restart) break;
                found_crossing = true;
            }
            const vertex = sweep.current.?;
            if (std.debug.runtime_safety) validateActiveEdges(sweep.active);
            var above = vertex.first_edge_above;
            while (above) |edge| : (above = edge.next_edge_above) {
                try tessellator.charge();
                assert(sweep.active.remove(edge));
            }
            var left_edge = enclosing[0];
            var below = vertex.first_edge_below;
            while (below) |edge| : (below = edge.next_edge_below) {
                try tessellator.charge();
                assert(edge.isAlive());
                assert(sweep.active.insertAfter(edge, left_edge));
                left_edge = edge;
            }
            sweep.current = vertex.next;
        }
        assert(sweep.active.head == null and sweep.active.tail == null);
        return found_crossing;
    }

    /// Fournier and Montuno: splits the simplified mesh into monotone polygons.
    pub fn tessellate(tessellator: *Tessellator, mesh: VertexList) InternalError!?*Poly {
        var active: EdgeList = .{};
        var polys: ?*Poly = null;
        var next_vertex = mesh.head;
        while (next_vertex) |vertex| : (next_vertex = vertex.next) {
            if (!vertex.isConnected()) continue;
            const enclosing = try tessellator.findEnclosingEdges(vertex, &active);
            var left_poly: ?*Poly = null;
            var right_poly: ?*Poly = null;
            if (vertex.first_edge_above) |first_above| {
                left_poly = first_above.left_poly;
                right_poly = vertex.last_edge_above.?.right_poly;
            } else {
                if (enclosing[0]) |edge| left_poly = edge.right_poly;
                if (enclosing[1]) |edge| right_poly = edge.left_poly;
            }
            if (vertex.first_edge_above) |first_above| {
                const last_above = vertex.last_edge_above.?;
                if (left_poly) |poly| left_poly = try tessellator.addPolyEdge(poly, first_above, .right);
                if (right_poly) |poly| right_poly = try tessellator.addPolyEdge(poly, last_above, .left);
                var edge = first_above;
                while (edge != last_above) {
                    try tessellator.charge();
                    const right_edge = edge.next_edge_above.?;
                    _ = active.remove(edge);
                    if (edge.right_poly) |poly| _ = try tessellator.addPolyEdge(poly, edge, .left);
                    if (right_edge.left_poly) |poly| if (poly != edge.right_poly) {
                        _ = try tessellator.addPolyEdge(poly, edge, .right);
                    };
                    edge = right_edge;
                }
                _ = active.remove(last_above);
                if (vertex.first_edge_below == null) if (left_poly) |left| if (right_poly) |right| if (left != right) {
                    assert(left.partner == null and right.partner == null);
                    left.partner = right;
                    right.partner = left;
                };
            }
            const first_below = vertex.first_edge_below orelse continue;
            if (vertex.first_edge_above == null) if (left_poly) |left_start| if (right_poly) |right_start| {
                var left = left_start;
                var right = right_start;
                if (left == right) {
                    if (left.tail != null and left.tail.?.side == .left) {
                        left = try tessellator.makePoly(&polys, left.lastVertex(), left.winding);
                        enclosing[0].?.right_poly = left;
                    } else {
                        right = try tessellator.makePoly(&polys, right.lastVertex(), right.winding);
                        enclosing[1].?.left_poly = right;
                    }
                }
                const join = try tessellator.allocateEdge(left.lastVertex(), vertex, 1, .inner);
                left_poly = try tessellator.addPolyEdge(left, join, .right);
                right_poly = try tessellator.addPolyEdge(right, join, .left);
            };
            var left_edge = first_below;
            left_edge.left_poly = left_poly;
            assert(active.insertAfter(left_edge, enclosing[0]));
            var next_below = left_edge.next_edge_below;
            while (next_below) |right_edge| : (next_below = right_edge.next_edge_below) {
                try tessellator.charge();
                assert(active.insertAfter(right_edge, left_edge));
                const winding = (if (left_edge.left_poly) |poly| poly.winding else 0) + left_edge.winding;
                if (winding != 0) {
                    const poly = try tessellator.makePoly(&polys, vertex, winding);
                    left_edge.right_poly = poly;
                    right_edge.left_poly = poly;
                }
                left_edge = right_edge;
            }
            vertex.last_edge_below.?.right_poly = right_poly;
        }
        return polys;
    }

    fn makePoly(tessellator: *Tessellator, polys: *?*Poly, vertex: *Vertex, winding: i32) InternalError!*Poly {
        const poly = try tessellator.arena.create(Poly);
        poly.* = .{ .first_vertex = vertex, .winding = winding, .next = polys.* };
        polys.* = poly;
        return poly;
    }

    fn makeMonotonePoly(tessellator: *Tessellator, edge: *Edge, side: Side, winding: i32) InternalError!*MonotonePoly {
        const monotone = try tessellator.arena.create(MonotonePoly);
        monotone.* = .{ .side = side, .winding = winding };
        monotone.addEdge(edge);
        return monotone;
    }

    fn addPolyEdge(tessellator: *Tessellator, poly: *Poly, added_edge: *Edge, side: Side) InternalError!*Poly {
        var edge = added_edge;
        if (switch (side) {
            .right => edge.used_in_right_poly,
            .left => edge.used_in_left_poly,
        }) return poly;
        const partner = poly.partner;
        if (partner) |other| {
            poly.partner = null;
            other.partner = null;
        }
        const tail = poly.tail orelse {
            const monotone = try tessellator.makeMonotonePoly(edge, side, poly.winding);
            poly.head = monotone;
            poly.tail = monotone;
            poly.count += 2;
            return poly;
        };
        const tail_bottom = tail.last_edge.?.bottom.?;
        if (edge.bottom == tail_bottom) return poly;
        if (side == tail.side) {
            tail.addEdge(edge);
            poly.count += 1;
            return poly;
        }
        edge = try tessellator.allocateEdge(tail_bottom, edge.bottom.?, 1, .inner);
        tail.addEdge(edge);
        poly.count += 1;
        if (partner) |other| {
            _ = try tessellator.addPolyEdge(other, edge, side);
            return other;
        }
        const monotone = try tessellator.makeMonotonePoly(edge, side, poly.winding);
        monotone.prev = tail;
        tail.next = monotone;
        poly.tail = monotone;
        return poly;
    }
};

/// Lists in the caller's allocator, handed over without a copy.
const Output = struct {
    allocator: Allocator,
    rotated: bool,
    positions: std.ArrayList(Point) = .empty,
    coverages: std.ArrayList(f32) = .empty,
    indices: std.ArrayList(u32) = .empty,

    fn deinit(output: *Output) void {
        output.positions.deinit(output.allocator);
        output.coverages.deinit(output.allocator);
        output.indices.deinit(output.allocator);
    }

    /// A triangulated planar mesh has fewer vertices and triangles than edges, so the count
    /// of edges plus loose triangles is an estimate for both. Appends past it still grow.
    fn reserve(output: *Output, edge_count: usize) error{OutOfMemory}!void {
        try output.positions.ensureTotalCapacity(output.allocator, edge_count);
        try output.coverages.ensureTotalCapacity(output.allocator, edge_count);
        try output.indices.ensureTotalCapacity(output.allocator, 3 * edge_count);
    }

    fn emitPolys(output: *Output, polys: ?*Poly, fill_rule: FillRule) error{OutOfMemory}!void {
        var poly = polys;
        while (poly) |current| : (poly = current.next) {
            if (current.count < 3 or !arrangement.isFilled(fill_rule, current.winding)) continue;
            var monotone = current.head;
            while (monotone) |monotone_poly| : (monotone = monotone_poly.next) try output.emitMonotonePoly(monotone_poly);
        }
    }

    /// Ear clipping along the monotone chain, the vertices' list links become the chain.
    fn emitMonotonePoly(output: *Output, monotone: *const MonotonePoly) error{OutOfMemory}!void {
        assert(monotone.winding != 0);
        var chain: VertexList = .{};
        var next_edge = monotone.first_edge;
        chain.append(next_edge.?.top.?);
        var count: usize = 1;
        while (next_edge) |edge| : (count += 1) {
            switch (monotone.side) {
                .right => {
                    chain.append(edge.bottom.?);
                    next_edge = edge.right_poly_next;
                },
                .left => {
                    chain.prepend(edge.bottom.?);
                    next_edge = edge.left_poly_next;
                },
            }
        }
        const first = chain.head.?;
        var vertex = first.next.?;
        while (vertex != chain.tail.?) {
            const prev = vertex.prev.?;
            const next = vertex.next.?;
            if (count == 3) return output.emitTriangle(prev, vertex, next);
            const turn = orientation(prev.point, vertex.point, next.point);
            if (turn >= 0) {
                try output.emitTriangle(prev, vertex, next);
                prev.next = next;
                next.prev = prev;
                count -= 1;
                vertex = if (prev == first) next else prev;
            } else vertex = next;
        }
    }

    fn emitTriangle(output: *Output, a: *Vertex, b: *Vertex, c: *Vertex) error{OutOfMemory}!void {
        const area = orientation(a.point, b.point, c.point);
        if (area == 0) return;
        const corners: [3]*Vertex = if (area > 0) .{ a, b, c } else .{ a, c, b };
        for (corners) |corner| try output.indices.append(output.allocator, try output.indexOf(corner));
    }

    fn indexOf(output: *Output, vertex: *Vertex) error{OutOfMemory}!u32 {
        if (vertex.output_index) |index| return index;
        const index: u32 = @intCast(output.positions.items.len);
        const point: GridPoint = if (output.rotated) .{ vertex.point[1], -vertex.point[0] } else vertex.point;
        try output.positions.append(output.allocator, .{ gridToPixels(point[0]), gridToPixels(point[1]) });
        try output.coverages.append(output.allocator, std.math.clamp(vertex.coverage, 0, 1));
        vertex.output_index = index;
        return index;
    }
};

/// Skia's rule, with interpolation along inner edges whose ends lost full coverage: crossings on
/// an inner edge take its coverage, crossings of outer edges stay uncovered, others take the
/// larger coverage interpolated along both edges.
fn crossingCoverage(edge: *const Edge, other: *const Edge, point: GridPoint) f32 {
    const along = coverageAlong(edge.top.?, edge.bottom.?, point);
    const other_along = coverageAlong(other.top.?, other.bottom.?, point);
    if (edge.kind == .inner and other.kind == .inner) return @max(along, other_along);
    if (edge.kind == .inner) return along;
    if (other.kind == .inner) return other_along;
    if (edge.kind == .outer and other.kind == .outer) return 0;
    return @max(along, other_along);
}

/// Coverage interpolated from `from` to `to` at the projection of `point`.
fn coverageAlong(from: *const Vertex, to: *const Vertex, point: GridPoint) f32 {
    const direction = [2]f64{ @floatFromInt(@as(i64, to.point[0]) - from.point[0]), @floatFromInt(@as(i64, to.point[1]) - from.point[1]) };
    const offset = [2]f64{ @floatFromInt(@as(i64, point[0]) - from.point[0]), @floatFromInt(@as(i64, point[1]) - from.point[1]) };
    const length_squared = direction[0] * direction[0] + direction[1] * direction[1];
    if (length_squared == 0) return from.coverage;
    const t = std.math.clamp((offset[0] * direction[0] + offset[1] * direction[1]) / length_squared, 0, 1);
    return @floatCast(@as(f64, from.coverage) + t * (@as(f64, to.coverage) - from.coverage));
}

fn gridToPixels(coordinate: i32) f32 {
    assert(@abs(coordinate) <= 1 << 24);
    return @as(f32, @floatFromInt(coordinate)) / grid_per_pixel;
}

/// Without repeated points and without points on the line through their neighbors.
fn sanitizedContour(arena: Allocator, points: []const GridPoint) error{OutOfMemory}![]const GridPoint {
    var unique: std.ArrayList(GridPoint) = .empty;
    for (points) |point| {
        if (unique.items.len == 0 or !pointsEqual(unique.getLast(), point)) try unique.append(arena, point);
    }
    while (unique.items.len > 1 and pointsEqual(unique.getLast(), unique.items[0])) _ = unique.pop();
    var kept: std.ArrayList(GridPoint) = .empty;
    for (unique.items, 0..) |point, index| {
        const prev = if (kept.items.len > 0) kept.getLast() else unique.getLast();
        const next = if (index + 1 < unique.items.len) unique.items[index + 1] else if (kept.items.len > 0) kept.items[0] else point;
        if (orientation(prev, next, point) != 0) try kept.append(arena, point);
    }
    return kept.items;
}

fn kill(edge: *Edge, sweep: ?*Sweep) void {
    edge.disconnect();
    if (sweep) |active_sweep| _ = active_sweep.active.remove(edge);
    edge.top = null;
    edge.bottom = null;
}

fn removeEdgeAbove(edge: *Edge) void {
    const bottom = edge.bottom.?;
    listRemove(Edge, "prev_edge_above", "next_edge_above", edge, &bottom.first_edge_above, &bottom.last_edge_above);
}

fn removeEdgeBelow(edge: *Edge) void {
    const top = edge.top.?;
    listRemove(Edge, "prev_edge_below", "next_edge_below", edge, &top.first_edge_below, &top.last_edge_below);
}

fn topCollinear(left_or_null: ?*Edge, right_or_null: ?*Edge) bool {
    const left = left_or_null orelse return false;
    const right = right_or_null orelse return false;
    return pointsEqual(left.top.?.point, right.top.?.point) or !left.isLeftOf(right.top.?.point) or !right.isRightOf(left.top.?.point);
}

fn bottomCollinear(left_or_null: ?*Edge, right_or_null: ?*Edge) bool {
    const left = left_or_null orelse return false;
    const right = right_or_null orelse return false;
    return pointsEqual(left.bottom.?.point, right.bottom.?.point) or !left.isLeftOf(right.bottom.?.point) or !right.isRightOf(left.bottom.?.point);
}

/// The crossing of two segments that share no vertex, snapped to the grid, or null if they are
/// parallel or apart. Touching counts.
fn segmentCrossing(edge: *const Edge, other: *const Edge) ?GridPoint {
    const top = edge.top.?;
    const bottom = edge.bottom.?;
    const other_top = other.top.?;
    const other_bottom = other.bottom.?;
    if (top == other_top or bottom == other_bottom or top == other_bottom or bottom == other_top) return null;
    const top_side = other.side(top.point);
    const bottom_side = other.side(bottom.point);
    if (top_side == bottom_side) return null;
    if (sameStrictSign(top_side, bottom_side)) return null;
    if (sameStrictSign(edge.side(other_top.point), edge.side(other_bottom.point))) return null;
    const crossing = lineCrossing(edge.line, other.line) orelse return arrangement.snappedCrossing(top.point, bottom.point, top_side, bottom_side);
    const clamped = clampToBounds(clampToBounds(crossing, edge), other);
    return .{ @intCast(clamped[0]), @intCast(clamped[1]) };
}

/// The crossing of two lines rounded to the grid, or null if they are parallel.
pub fn lineCrossing(line: [2]GridPoint, other: [2]GridPoint) ?[2]i128 {
    const from_side: i128 = orientation(other[0], other[1], line[0]);
    const to_side: i128 = orientation(other[0], other[1], line[1]);
    if (from_side == to_side) return null;
    const sign: i128 = if (from_side > to_side) 1 else -1;
    const denominator = (from_side - to_side) * sign;
    var crossing: [2]i128 = undefined;
    for (&crossing, line[0], line[1]) |*coordinate, from, to| {
        const numerator = @as(i128, from) * denominator + (@as(i128, to) - from) * from_side * sign;
        coordinate.* = @divFloor(2 * numerator + denominator, 2 * denominator);
    }
    return crossing;
}

/// Skia's clamp: each coordinate into the bounding box of the edge.
fn clampToBounds(point: [2]i128, edge: *const Edge) [2]i128 {
    var clamped: [2]i128 = undefined;
    for (&clamped, point, edge.top.?.point, edge.bottom.?.point) |*coordinate, value, top, bottom| {
        coordinate.* = std.math.clamp(value, @min(top, bottom), @max(top, bottom));
    }
    return clamped;
}

pub fn sameStrictSign(a: i64, b: i64) bool {
    return (a > 0 and b > 0) or (a < 0 and b < 0);
}

fn validateActiveEdges(active: EdgeList) void {
    var left = active.head orelse return;
    while (left.right) |right| : (left = right) {
        const left_top = left.top.?;
        const left_bottom = left.bottom.?;
        const right_top = right.top.?;
        const right_bottom = right.bottom.?;
        if (left_top == right_top) {
            assert(left.isLeftOf(right_bottom.point));
            assert(right.isRightOf(left_bottom.point));
        } else if (sweepLessThan(left_top.point, right_top.point)) {
            assert(left.isLeftOf(right_top.point));
        } else assert(right.isRightOf(left_top.point));
        if (left_bottom == right_bottom) {
            assert(left.isLeftOf(right_top.point));
            assert(right.isRightOf(left_top.point));
        } else if (sweepLessThan(right_bottom.point, left_bottom.point)) {
            assert(left.isLeftOf(right_bottom.point));
        } else assert(right.isRightOf(left_bottom.point));
    }
}

pub fn orientation(from: GridPoint, to: GridPoint, point: GridPoint) i64 {
    return (@as(i64, to[0]) - from[0]) * (@as(i64, point[1]) - from[1]) - (@as(i64, to[1]) - from[1]) * (@as(i64, point[0]) - from[0]);
}

pub fn pointsEqual(a: GridPoint, b: GridPoint) bool {
    return a[0] == b[0] and a[1] == b[1];
}

/// Increasing y, then increasing x.
pub fn sweepLessThan(a: GridPoint, b: GridPoint) bool {
    return a[1] < b[1] or (a[1] == b[1] and a[0] < b[0]);
}

fn vertexLessThan(_: void, a: *Vertex, b: *Vertex) bool {
    return sweepLessThan(a.point, b.point);
}

pub fn listInsert(comptime T: type, comptime prev_field: []const u8, comptime next_field: []const u8, item: *T, prev: ?*T, next: ?*T, head: *?*T, tail: *?*T) void {
    @field(item, prev_field) = prev;
    @field(item, next_field) = next;
    if (prev) |before| @field(before, next_field) = item else head.* = item;
    if (next) |after| @field(after, prev_field) = item else tail.* = item;
}

pub fn listRemove(comptime T: type, comptime prev_field: []const u8, comptime next_field: []const u8, item: *T, head: *?*T, tail: *?*T) void {
    const prev = @field(item, prev_field);
    const next = @field(item, next_field);
    if (prev) |before| @field(before, next_field) = next else head.* = next;
    if (next) |after| @field(after, prev_field) = prev else tail.* = prev;
    @field(item, prev_field) = null;
    @field(item, next_field) = null;
}
