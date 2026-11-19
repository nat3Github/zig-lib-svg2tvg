//! Triangle meshes with a coverage per vertex for fills, strokes and TinyVG documents, in pixel
//! coordinates with y down. The caller colors the mesh: vertex color = coverage × paint.
//!
//! Input is contours of segments, so curves reach the mesher as curves: it picks the flattening,
//! sees convexity and smooth joins, and takes the fastest mesh that looks the same.

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

/// TinyVG format: parser, builder, reference renderer.
pub const tvg = @import("tinyvg/tinyvg.zig");

/// Implementation, used by the SVG converter and the tests. Not part of the stable API.
pub const internal = struct {
    pub const tessellation = @import("tessellation.zig");
    pub const arrangement = @import("arrangement.zig");
    pub const contour = @import("contour.zig");
    pub const stroke = @import("stroke.zig");
    pub const earcut = @import("earcut.zig");
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

/// Slices of the caller, as long as the matching size function says.
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
    /// Width in pixels of the fade to transparent, 0 for hard edges. Up to 1 it is centered on the
    /// edge, above 1 it starts half a pixel inside and the rest lies outside. A convex single
    /// contour fades exactly that wide, other fills fade 1 px for any `fade` up to 1.
    ///
    /// Fills above 1 that are no convex single contour take earcut: each contour must be simple,
    /// nested contours alternate between fill and hole whatever `fill_rule` says, and at concave
    /// corners the fade overlaps itself, which shows with translucent paint.
    fade: f32 = 1,
    /// Largest distance of the flattened contours from the curves.
    tolerance: f32 = 0.25,
    limits: Limits = .{},
    work: ?*Work = null,
};

/// Any contours: concave, with holes, self-intersecting. Convex single contours take the
/// fan of `fillConvex`.
pub fn fill(allocator: Allocator, contours: []const Contour, options: FillOptions) Error!Mesh {
    assert(options.fade >= 0 and options.fade <= max_coordinate);
    if (!(options.tolerance > 0)) return error.InvalidInput;
    for (contours) |contour| if (!internal.contour.isValid(contour)) return error.InvalidInput;

    var points: std.ArrayList(Point) = .empty;
    defer points.deinit(allocator);
    const ends = try allocator.alloc(usize, contours.len);
    defer allocator.free(ends);
    for (contours, ends) |contour, *end| {
        try internal.contour.append(allocator, contour, true, options.tolerance, options.limits.max_points, &points);
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
        const fan: Mesh = .{ .positions = positions, .coverages = coverages, .indices = indices };
        // The fade of thin shapes folds over, the sweep has no folds.
        if (!hasClockwiseTriangle(fan)) return fan;
        fan.deinit(allocator);
    }
    const polygons = try allocator.alloc([]const Point, contours.len);
    defer allocator.free(polygons);
    for (polygons, ends, 0..) |*polygon, end, index| polygon.* = points.items[if (index == 0) 0 else ends[index - 1]..end];
    if (options.fade > 1) return internal.earcut.fill(allocator, polygons, options.fade);
    return internal.tessellation.fill(allocator, polygons, .{
        .fill_rule = options.fill_rule,
        .anti_alias = options.fade > 0,
        .limits = options.limits,
        .work = options.work,
    });
}

fn hasClockwiseTriangle(built: Mesh) bool {
    var triangle: usize = 0;
    while (triangle < built.indices.len) : (triangle += 3) {
        const corners = built.indices[triangle..][0..3];
        const a = built.positions[corners[0]];
        const b = built.positions[corners[1]];
        const c = built.positions[corners[2]];
        const cross = (@as(f64, b[0]) - a[0]) * (@as(f64, c[1]) - a[1]) - (@as(f64, b[1]) - a[1]) * (@as(f64, c[0]) - a[0]);
        if (cross > 0) return true;
    }
    return false;
}

pub const ConvexOptions = struct {
    /// Width in pixels of the fade to transparent, centered on the edge. Above 1 it starts half
    /// a pixel inside and the rest lies outside.
    fade: f32 = 0,
    /// Pivot of the fan with full coverage, for example the center of a radial gradient.
    center: ?Point = null,
};

/// Buffer sizes for `fillConvex`, at least what it writes. Nothing for invalid contours.
pub const convexSize = convex.convexSize;

/// Allocation free, for the hot path of widgets. `contour` must be convex, either orientation;
/// curves flatten like `circle_arc` within half a pixel. Returns the size written to the front of
/// the buffers.
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
    /// Width in pixels of the fade to transparent on both sides, 0 for hard edges. Up to 1 it is
    /// centered on the edge, above 1 it starts half a pixel inside and the rest lies outside.
    /// Strokes thinner than 1 px keep their ink and lower their coverage instead. Translucent
    /// strokes that overlap themselves fade 1 px for any `fade` up to 1.
    ///
    /// Above 1 every stroke takes the direct strip, `translucent` or not: joins, crossings and the
    /// fade at sharp corners overlap, which shows with translucent paint.
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
    assert(options.fade >= 0 and options.fade <= max_coordinate);
    try internal.stroke.validateOptions(options);
    for (contours) |contour| if (!internal.contour.isValid(contour)) return error.InvalidInput;

    const polylines = try allocator.alloc(internal.stroke.Polyline, contours.len);
    defer allocator.free(polylines);
    var points: std.ArrayList(Point) = .empty;
    defer points.deinit(allocator);
    for (contours, polylines) |contour, *polyline| {
        const first_point = points.items.len;
        try internal.contour.append(allocator, contour, contour.closed, options.tolerance, options.limits.max_points, &points);
        polyline.* = .{ .points = points.items[first_point..], .closed = contour.closed };
    }
    // The list may have moved while it grew, only the lengths still hold.
    var start: usize = 0;
    for (polylines) |*polyline| {
        polyline.points = points.items[start..][0..polyline.points.len];
        start += polyline.points.len;
    }
    assert(start == points.items.len);
    var strip_options = options;
    if (options.fade > 1) strip_options.translucent = false;
    if (try internal.stroke.strip(allocator, polylines, strip_options)) |direct| return direct;
    assert(strip_options.translucent);

    var outline_points: std.ArrayList(Point) = .empty;
    defer outline_points.deinit(allocator);
    var outline_ends: std.ArrayList(usize) = .empty;
    defer outline_ends.deinit(allocator);
    for (polylines) |polyline| {
        try internal.stroke.strokeOutline(allocator, polyline.points, polyline.closed, options, options.limits.max_points, &outline_points, &outline_ends);
    }
    const outlines = try allocator.alloc([]const Point, outline_ends.items.len);
    defer allocator.free(outlines);
    for (outlines, outline_ends.items, 0..) |*outline, end, index| outline.* = outline_points.items[if (index == 0) 0 else outline_ends.items[index - 1]..end];
    return internal.tessellation.fill(allocator, outlines, .{
        .fill_rule = .nonzero,
        .anti_alias = options.fade > 0,
        .limits = options.limits,
        .work = options.work,
    });
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
    anti_alias: bool = true,
    tolerance: f32 = 0.125,
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
    return @import("tvg_meshes.zig").tvgMeshes(allocator, tvg_bytes, options);
}
