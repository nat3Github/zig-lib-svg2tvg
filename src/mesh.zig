//! Triangle meshes with a coverage per vertex for fills, strokes and TinyVG documents, in pixel
//! coordinates with y down. The caller colors the mesh: vertex color = coverage × paint.
//!
//! Input is contours of segments, so curves reach the mesher as curves: it picks the flattening,
//! sees convexity and smooth joins, and takes the fastest mesh that looks the same.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// TinyVG format: parser, builder, reference renderer.
pub const tvg = @import("tinyvg/tinyvg.zig");

/// Implementation, used by the SVG converter and the tests. Not part of the stable API.
pub const internal = struct {
    pub const tessellation = @import("tessellation.zig");
    pub const arrangement = @import("arrangement.zig");
    pub const contour = @import("contour.zig");
};
const convex = @import("convex.zig");

pub const Point = [2]f32;
pub const Error = error{ OutOfMemory, InvalidInput, TooComplex };
/// Coordinates and widths beyond this are invalid input.
pub const max_coordinate = 1 << 15;

pub const Limits = internal.tessellation.Limits;

/// A work budget shared by several calls, for example all paths of one document.
pub const Work = internal.arrangement.Work;

/// Owned by the allocator of the call. Triangles run counterclockwise on screen.
pub const Mesh = struct {
    positions: []const Point,
    coverages: []const f32,
    indices: []const u32,

    pub fn deinit(mesh: Mesh, allocator: Allocator) void {
        allocator.free(mesh.positions);
        allocator.free(mesh.coverages);
        allocator.free(mesh.indices);
    }
};

/// Slices of the caller, exactly as long as the matching size function says.
pub const MeshBuffers = struct {
    positions: []Point,
    coverages: []f32,
    indices: []u32,
};

pub const MeshSize = struct {
    vertices: usize,
    indices: usize,
};

// Contours

/// Every segment starts where the previous one ends.
pub const Segment = union(enum) {
    line: Point,
    quadratic: struct { control: Point, end: Point },
    cubic: struct { control_0: Point, control_1: Point, end: Point },
    /// Elliptical arc as in SVG and TinyVG, rotation in radians.
    arc: struct { radius: [2]f32, rotation: f32, large_arc: bool, sweep: bool, end: Point },
    /// A line to the start of the arc, then the circular arc from `start_angle` down to
    /// `end_angle`, radians clockwise on screen from the positive x axis. Flattened on a fixed
    /// angle grid, so an arc that moves or turns keeps its points.
    circle_arc: struct { center: Point, radius: f32, start_angle: f32, end_angle: f32 },
};

pub const Contour = struct {
    start: Point,
    segments: []const Segment,
    /// Fills close every contour, strokes join the end back to the start.
    closed: bool = false,
};

// Fill

pub const FillRule = internal.arrangement.FillRule;

pub const FillOptions = struct {
    fill_rule: FillRule = .nonzero,
    /// Width in pixels of the fade to transparent, centered on the edge. 0 or 1 for now.
    fade: f32 = 1,
    /// Largest distance of the flattened contours from the curves.
    tolerance: f32 = 0.25,
    limits: Limits = .{},
    work: ?*Work = null,
};

/// Any contours: concave, with holes, self-intersecting. Convex single contours take the
/// fan of `fillConvex`.
pub fn fill(allocator: Allocator, contours: []const Contour, options: FillOptions) Error!Mesh {
    const anti_alias = if (options.fade == 0) false else if (options.fade == 1) true else return error.InvalidInput;
    if (!(options.tolerance > 0)) return error.InvalidInput;
    for (contours) |contour| if (!internal.contour.isValid(contour)) return error.InvalidInput;

    var points: std.ArrayList(Point) = .empty;
    defer points.deinit(allocator);
    const ends = try allocator.alloc(usize, contours.len);
    defer allocator.free(ends);
    for (contours, ends) |contour, *end| {
        try internal.contour.append(allocator, contour, options.tolerance, options.limits.max_points, &points);
        end.* = points.items.len;
    }
    if (contours.len == 1 and convex.isConvex(points.items)) {
        const convex_options: ConvexOptions = .{ .fade = options.fade };
        const size = convex.sizeForPoints(points.items.len, convex_options);
        const positions = try allocator.alloc(Point, size.vertices);
        errdefer allocator.free(positions);
        const coverages = try allocator.alloc(f32, size.vertices);
        errdefer allocator.free(coverages);
        const indices = try allocator.alloc(u32, size.indices);
        convex.fillPolygon(points.items, convex_options, .{ .positions = positions, .coverages = coverages, .indices = indices });
        return .{ .positions = positions, .coverages = coverages, .indices = indices };
    }
    const polygons = try allocator.alloc([]const Point, contours.len);
    defer allocator.free(polygons);
    for (polygons, ends, 0..) |*polygon, end, index| polygon.* = points.items[if (index == 0) 0 else ends[index - 1]..end];
    return internal.tessellation.fill(allocator, polygons, .{
        .fill_rule = options.fill_rule,
        .anti_alias = anti_alias,
        .limits = options.limits,
        .work = options.work,
    });
}

pub const ConvexOptions = struct {
    /// Width in pixels of the fade to transparent, centered on the edge. Above 1 it starts half
    /// a pixel inside and the rest lies outside.
    fade: f32 = 0,
    /// Pivot of the fan with full coverage, for example the center of a radial gradient.
    center: ?Point = null,
};

pub const convexSize = convex.convexSize;

/// Allocation free, for the hot path of widgets. `contour` must be convex, either orientation;
/// curves flatten like `circle_arc` within half a pixel.
pub const fillConvex = convex.fillConvex;

// Stroke

pub const Join = enum { round, miter, bevel };
pub const Cap = enum { butt, square, round };

pub const StrokeOptions = struct {
    width: f32,
    join: Join = .miter,
    /// Miter length over width beyond which a miter becomes a bevel, as SVG stroke-miterlimit.
    miter_limit: f32 = 4,
    cap: Cap = .butt,
    fade: f32 = 1,
    /// Overlapping triangles would show, so every pixel must be covered once.
    translucent: bool = false,
    tolerance: f32 = 0.25,
    limits: Limits = .{},
    work: ?*Work = null,
};

/// All contours in one mesh: a direct strip where nothing overlaps visibly, else the outline
/// filled nonzero.
pub fn stroke(allocator: Allocator, contours: []const Contour, options: StrokeOptions) Error!Mesh {
    _ = .{ allocator, contours, options };
    @compileError("mesh.stroke: implementation follows (dvui #1056)");
}

// TinyVG

/// RGBA from the document's color table.
pub const Color = [4]f32;

/// Gradient points are in output pixels.
pub const Paint = union(enum) {
    flat: Color,
    linear: struct { color_0: Color, color_1: Color, point_0: Point, point_1: Point },
    radial: struct { color_0: Color, color_1: Color, center: Point, edge: Point },
};

pub const TvgPart = struct {
    kind: enum { fill, stroke },
    paint: Paint,
    mesh: Mesh,
};

pub const TvgOptions = struct {
    /// Pixel rectangle the document maps to.
    origin: Point = .{ 0, 0 },
    size: [2]f32,
    /// Scales evenly and centers, else stretches.
    keep_aspect: bool = true,
    /// Replaces every line width, in document units.
    line_width: ?f32 = null,
    /// False draws only the strokes.
    fills: bool = true,
    fade: f32 = 1,
    tolerance: f32 = 0.25,
    /// Shared by the whole document.
    limits: Limits = .{},
};

/// Owned by the allocator given to `tvgMeshes`.
pub const TvgMeshes = struct {
    /// In drawing order.
    parts: []const TvgPart,

    pub fn deinit(meshes: TvgMeshes, allocator: Allocator) void {
        for (meshes.parts) |part| part.mesh.deinit(allocator);
        allocator.free(meshes.parts);
    }
};

/// One part per fill and stroke of every command. Malformed documents are invalid input.
pub fn tvgMeshes(allocator: Allocator, tvg_bytes: []const u8, options: TvgOptions) Error!TvgMeshes {
    _ = .{ allocator, tvg_bytes, options };
    @compileError("mesh.tvgMeshes: implementation follows (dvui #1056)");
}
