//! Fan triangulation of a convex polygon with a fade to transparent on its edge, written into
//! buffers of the caller. The geometry is dvui's `Path.fillConvexTriangles`.

const std = @import("std");
const assert = std.debug.assert;
const tessellation = @import("tessellation.zig");
const Point = tessellation.Point;
const Vector = @Vector(2, f32);

pub const ConvexOptions = struct {
    /// Width in pixels of the fade to transparent, centered on the edge. Above 1 it starts half
    /// a pixel inside and the rest lies outside.
    fade: f32 = 0,
    /// Pivot of the fan with full coverage, for example the center of a radial gradient.
    center: ?Point = null,
};

pub const ConvexCounts = struct {
    vertices: usize,
    indices: usize,
};

pub fn convexCounts(point_count: usize, options: ConvexOptions) ConvexCounts {
    assert(point_count >= 3);
    var counts: ConvexCounts = .{ .vertices = point_count, .indices = (point_count - 2) * 3 };
    if (options.fade > 0) {
        counts.vertices *= 2;
        counts.indices += point_count * 6;
    }
    if (options.center != null) {
        counts.vertices += 1;
        counts.indices += 6;
    }
    return counts;
}

/// Fills `positions`, `coverages` and `indices`, each exactly as long as `convexCounts` says.
/// `polygon` must be convex, either orientation. Triangles wind like `tessellation.Mesh`.
pub fn fillConvex(polygon: []const Point, options: ConvexOptions, positions: []Point, coverages: []f32, indices: []u32) void {
    const counts = convexCounts(polygon.len, options);
    assert(positions.len == counts.vertices and coverages.len == counts.vertices and indices.len == counts.indices);
    assert(counts.vertices <= std.math.maxInt(u32));
    assert(options.fade >= 0);

    var area: f32 = 0;
    for (polygon, 0..) |point, index| {
        const next = polygon[(index + 1) % polygon.len];
        area += point[0] * next[1] - next[0] * point[1];
    }
    const count = polygon.len;
    // Normals and fan winding assume a counterclockwise walk on screen.
    const reversed = area > 0;
    const has_fade = options.fade > 0;
    const vertices_per_point: u32 = if (has_fade) 2 else 1;
    const center_index: u32 = @intCast(counts.vertices - 1);

    var vertex_count: usize = 0;
    var index_count: usize = 0;
    for (0..count) |index| {
        const previous_index: u32 = @intCast((index + count - 1) % count);
        const current_index: u32 = @intCast(index);
        const previous: Vector = polygon[if (reversed) count - 1 - previous_index else previous_index];
        const current: Vector = polygon[if (reversed) count - 1 - current_index else current_index];
        const next: Vector = polygon[if (reversed) count - 1 - (index + 1) % count else (index + 1) % count];

        const incoming = normalize(previous - current);
        const outgoing = normalize(current - next);
        var normal: Vector = .{ (incoming[1] + outgoing[1]) / 2, (-incoming[0] - outgoing[0]) / 2 };

        const inside_distance = @min(0.5, options.fade / 2);
        positions[vertex_count] = current - normal * @as(Vector, @splat(inside_distance));
        coverages[vertex_count] = 1;
        vertex_count += 1;

        const previous_vertex = previous_index * vertices_per_point;
        const current_vertex = current_index * vertices_per_point;
        if (options.center != null) {
            indices[index_count..][0..3].* = .{ center_index, previous_vertex, current_vertex };
            index_count += 3;
        } else if (index > 1) {
            indices[index_count..][0..3].* = .{ 0, previous_vertex, current_vertex };
            index_count += 3;
        }

        if (has_fade) {
            // Dividing the averaged normal by its squared length scales it to the miter.
            const length_squared = @reduce(.Add, normal * normal);
            if (length_squared > 0.000001) normal *= @splat(1.0 / length_squared);
            // At most 2 px, so sharp corners keep a small fringe.
            const normal_length = @sqrt(@reduce(.Add, normal * normal));
            if (normal_length > 2.0) normal *= @splat(2.0 / normal_length);

            const outside_distance = if (options.fade <= 1) options.fade / 2 else options.fade - 0.5;
            positions[vertex_count] = current + normal * @as(Vector, @splat(outside_distance));
            coverages[vertex_count] = 0;
            vertex_count += 1;

            indices[index_count..][0..6].* = .{
                previous_vertex,     previous_vertex + 1, current_vertex,
                previous_vertex + 1, current_vertex + 1,  current_vertex,
            };
            index_count += 6;
        }
    }

    if (options.center) |center| {
        positions[vertex_count] = center;
        coverages[vertex_count] = 1;
        vertex_count += 1;
    }
    assert(vertex_count == counts.vertices);
    assert(index_count == counts.indices);
}

fn normalize(vector: Vector) Vector {
    const length_squared = @reduce(.Add, vector * vector);
    if (length_squared == 0) return .{ 1, 0 };
    return vector * @as(Vector, @splat(1.0 / @sqrt(length_squared)));
}
