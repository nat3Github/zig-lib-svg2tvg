//! Fan triangulation of a convex polygon with a fade to transparent on its edge, written into
//! buffers of the caller. The geometry is dvui's `Path.fillConvexTriangles`, except that the inner
//! fade vertex sits on the miter like the outer one, at most 2 px from the corner.

const std = @import("std");
const assert = std.debug.assert;
const mesh = @import("mesh.zig");
const contour_points = @import("contour.zig");
const Point = mesh.Point;
const Vector = @Vector(2, f32);

/// At least the size `fillConvex` writes, nothing for an invalid contour.
pub fn convexSize(contour: mesh.Contour, options: mesh.ConvexOptions) mesh.MeshSize {
    if (!contour_points.isValid(contour)) return .{ .vertices = 0, .indices = 0 };
    return sizeForPoints(contour_points.maxPointCount(contour, contour_points.convex_tolerance), options);
}

pub fn sizeForPoints(point_count: usize, options: mesh.ConvexOptions) mesh.MeshSize {
    if (point_count < 3) return .{ .vertices = 0, .indices = 0 };
    var size: mesh.MeshSize = .{ .vertices = point_count, .indices = (point_count - 2) * 3 };
    if (options.fade > 0) {
        size.vertices *= 2;
        size.indices += point_count * 6;
    }
    if (options.center != null) {
        size.vertices += 1;
        size.indices += 6;
    }
    return size;
}

/// Flattens the contour once into the buffers, sized by `convexSize`, and returns the size written.
pub fn fillConvex(contour: mesh.Contour, options: mesh.ConvexOptions, buffers: mesh.MeshBuffers) mesh.MeshSize {
    const nothing: mesh.MeshSize = .{ .vertices = 0, .indices = 0 };
    if (buffers.positions.len == 0 or !contour_points.isValid(contour)) return nothing;
    const center_vertices: usize = if (options.center != null) 1 else 0;
    const max_count = (buffers.positions.len - center_vertices) / @as(usize, if (options.fade > 0) 2 else 1);
    const slot = prepare(max_count, options, buffers) orelse return nothing;
    const Writer = struct {
        polygon: []Point,
        written: usize = 0,

        fn emit(writer: *@This(), point: Point) contour_points.Error!void {
            assert(writer.written < writer.polygon.len);
            writer.polygon[writer.written] = point;
            writer.written += 1;
        }
    };
    var writer: Writer = .{ .polygon = slot };
    contour_points.forEachPoint(contour, .{ .tolerance = contour_points.convex_tolerance }, &writer, Writer.emit) catch unreachable;
    if (writer.written < 3) return nothing;
    walk(slot[0..writer.written], options, buffers);
    return sizeForPoints(writer.written, options);
}

/// `fillConvex` of a flattened polygon, sized by `sizeForPoints`.
pub fn fillPolygon(polygon: []const Point, options: mesh.ConvexOptions, buffers: mesh.MeshBuffers) void {
    const slot = prepare(polygon.len, options, buffers) orelse return;
    @memcpy(slot, polygon);
    walk(slot, options, buffers);
}

/// The polygon waits behind the vertices that the walk writes, which never catch up with it.
fn prepare(count: usize, options: mesh.ConvexOptions, buffers: mesh.MeshBuffers) ?[]Point {
    const size = sizeForPoints(count, options);
    assert(buffers.positions.len == size.vertices and buffers.coverages.len == size.vertices and buffers.indices.len == size.indices);
    assert(size.vertices <= std.math.maxInt(u32));
    assert(options.fade >= 0);
    if (count < 3) return null;
    return buffers.positions[if (options.fade > 0) count else 0..][0..count];
}

/// True when the closed polygon turns one way exactly once around.
pub fn isConvex(polygon: []const Point) bool {
    if (polygon.len < 3) return false;
    var turning: f32 = 0;
    var sign: f32 = 0;
    for (polygon, 0..) |point, index| {
        const incoming = @as(Vector, point) - @as(Vector, polygon[(index + polygon.len - 1) % polygon.len]);
        const outgoing = @as(Vector, polygon[(index + 1) % polygon.len]) - @as(Vector, point);
        const cross = incoming[0] * outgoing[1] - incoming[1] * outgoing[0];
        const dot = @reduce(.Add, incoming * outgoing);
        if (cross == 0 and !(dot > 0)) return false;
        if (cross != 0) {
            if (sign != 0 and std.math.sign(cross) != sign) return false;
            sign = std.math.sign(cross);
        }
        turning += std.math.atan2(cross, dot);
    }
    return sign != 0 and @abs(@abs(turning) - 2 * std.math.pi) < 0.1;
}

fn walk(polygon: []Point, options: mesh.ConvexOptions, buffers: mesh.MeshBuffers) void {
    const count = polygon.len;
    const size = sizeForPoints(count, options);
    var area: f32 = 0;
    var area_previous = polygon[count - 1];
    for (polygon) |point| {
        area += area_previous[0] * point[1] - point[0] * area_previous[1];
        area_previous = point;
    }
    // Normals and fan winding assume a counterclockwise walk on screen.
    if (area > 0) std.mem.reverse(Point, polygon);

    const has_fade = options.fade > 0;
    const vertices_per_point: u32 = if (has_fade) 2 else 1;
    const center_index: u32 = @intCast(size.vertices - 1);
    const first: Vector = polygon[0];
    var current: Vector = first;
    var incoming = normalize(@as(Vector, polygon[count - 1]) - first);
    var previous_vertex: u32 = @intCast((count - 1) * vertices_per_point);
    var vertex_count: usize = 0;
    var index_count: usize = 0;
    for (0..count) |index| {
        const next: Vector = if (index + 1 < count) polygon[index + 1] else first;
        const current_vertex: u32 = @intCast(index * vertices_per_point);
        const outgoing = normalize(current - next);
        var normal: Vector = .{ (incoming[1] + outgoing[1]) / 2, (-incoming[0] - outgoing[0]) / 2 };

        const inside_distance = @min(0.5, options.fade / 2);
        var inner_normal = normal;
        const inner_length_squared = @reduce(.Add, normal * normal);
        if (inner_length_squared > 0.000001) inner_normal *= @splat(1.0 / inner_length_squared);
        const inner_length = @sqrt(@reduce(.Add, inner_normal * inner_normal));
        if (inner_length > 2.0) inner_normal *= @splat(2.0 / inner_length);
        buffers.positions[vertex_count] = current - inner_normal * @as(Vector, @splat(inside_distance));
        buffers.coverages[vertex_count] = 1;
        vertex_count += 1;

        if (options.center != null) {
            buffers.indices[index_count..][0..3].* = .{ center_index, previous_vertex, current_vertex };
            index_count += 3;
        } else if (index > 1) {
            buffers.indices[index_count..][0..3].* = .{ 0, previous_vertex, current_vertex };
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
            buffers.positions[vertex_count] = current + normal * @as(Vector, @splat(outside_distance));
            buffers.coverages[vertex_count] = 0;
            vertex_count += 1;

            buffers.indices[index_count..][0..6].* = .{
                previous_vertex,     previous_vertex + 1, current_vertex,
                previous_vertex + 1, current_vertex + 1,  current_vertex,
            };
            index_count += 6;
        }
        current = next;
        incoming = outgoing;
        previous_vertex = current_vertex;
    }

    if (options.center) |center| {
        buffers.positions[vertex_count] = center;
        buffers.coverages[vertex_count] = 1;
        vertex_count += 1;
    }
    assert(vertex_count == size.vertices);
    assert(index_count == size.indices);
}

fn normalize(vector: Vector) Vector {
    const length_squared = @reduce(.Add, vector * vector);
    if (length_squared == 0) return .{ 1, 0 };
    return vector * @as(Vector, @splat(1.0 / @sqrt(length_squared)));
}
