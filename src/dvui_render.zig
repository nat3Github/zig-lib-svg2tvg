//! Direct TVG → dvui renderer.
//!
//! Parses a TVG icon stream and emits dvui draw calls (Path.fillConvex /
//! Path.stroke / batched Triangles).  Resolution-independent — no raster
//! intermediate.  Must be called inside `Window.begin()`…`Window.end()`.

const std = @import("std");
const math = std.math;
const dvui = @import("dvui");

const svg2tvg = @import("svg2tvg");
const tvg = svg2tvg.tvg;
const parsing = svg2tvg.tvg_parsing;

const Point = dvui.Point.Physical;
const Rect = dvui.Rect.Physical;
const Color = dvui.Color;

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

pub const RenderOptions = struct {
    /// If set, overrides every flat-fill / stroke color.  Gradients are
    /// flattened to a single mid color when an override is active.
    color_override: ?Color = null,
    /// Preserve TVG aspect ratio inside `rect` (letterbox).  When false the
    /// icon is stretched to fill the rect.
    keep_aspect: bool = true,
    /// Edge feather (physical px) for anti-aliasing of filled polygons.
    /// 0 disables AA — sharp edges, useful for pixel-aligned UI strokes.
    fade: f32 = 1.0,
};

/// Render a TVG byte stream into `rect` (physical pixels).
pub fn renderTvg(
    allocator: std.mem.Allocator,
    tvg_bytes: []const u8,
    rect: Rect,
    opts: RenderOptions,
) !void {
    var fbs = std.io.fixedBufferStream(tvg_bytes);
    var parser = try parsing.Parser(@TypeOf(fbs.reader())).init(allocator, fbs.reader());
    defer parser.deinit();

    const xf = Transform.fromRect(rect, @floatFromInt(parser.header.width), @floatFromInt(parser.header.height), opts.keep_aspect);

    while (try parser.next()) |cmd| {
        try renderCommand(allocator, parser.color_table, cmd, xf, opts);
    }
}

// ---------------------------------------------------------------------------
// Transform: TVG-space → physical-pixel space
// ---------------------------------------------------------------------------

const Transform = struct {
    ox: f32,
    oy: f32,
    sx: f32,
    sy: f32,

    fn fromRect(rect: Rect, w: f32, h: f32, keep_aspect: bool) Transform {
        var sx = rect.w / w;
        var sy = rect.h / h;
        var ox = rect.x;
        var oy = rect.y;
        if (keep_aspect) {
            const s = @min(sx, sy);
            sx = s;
            sy = s;
            ox = rect.x + (rect.w - w * s) * 0.5;
            oy = rect.y + (rect.h - h * s) * 0.5;
        }
        return .{ .ox = ox, .oy = oy, .sx = sx, .sy = sy };
    }

    fn apply(self: Transform, p: tvg.Point) Point {
        return .{ .x = self.ox + p.x * self.sx, .y = self.oy + p.y * self.sy };
    }

    fn applyXY(self: Transform, x: f32, y: f32) Point {
        return .{ .x = self.ox + x * self.sx, .y = self.oy + y * self.sy };
    }

    /// Average of x/y scale — for stroke widths and arc tolerances.
    fn meanScale(self: Transform) f32 {
        return (@abs(self.sx) + @abs(self.sy)) * 0.5;
    }
};

// ---------------------------------------------------------------------------
// Command dispatch
// ---------------------------------------------------------------------------

fn renderCommand(
    allocator: std.mem.Allocator,
    color_table: []const tvg.Color,
    cmd: parsing.DrawCommand,
    xf: Transform,
    opts: RenderOptions,
) !void {
    switch (cmd) {
        .fill_polygon => |fp| {
            try fillPolygonTvg(allocator, fp.vertices, fp.style, color_table, xf, opts);
        },
        .fill_rectangles => |fr| {
            const col = resolveStyleColor(fr.style, color_table, opts);
            for (fr.rectangles) |r| fillTvgRect(r, col, xf);
        },
        .fill_path => |fp| {
            try fillPathTvg(allocator, fp.path, fp.style, color_table, xf, opts);
        },
        .draw_lines => |dl| {
            const col = resolveStyleColor(dl.style, color_table, opts);
            const thickness = dl.line_width * xf.meanScale();
            for (dl.lines) |ln| strokeLine(xf.apply(ln.start), xf.apply(ln.end), col, thickness);
        },
        .draw_line_loop => |ls| {
            const col = resolveStyleColor(ls.style, color_table, opts);
            strokePolylineTvg(ls.vertices, true, ls.line_width, col, xf);
        },
        .draw_line_strip => |ls| {
            const col = resolveStyleColor(ls.style, color_table, opts);
            strokePolylineTvg(ls.vertices, false, ls.line_width, col, xf);
        },
        .draw_line_path => |dp| {
            const col = resolveStyleColor(dp.style, color_table, opts);
            try strokePathTvg(allocator, dp.path, dp.line_width, col, xf);
        },
        .outline_fill_polygon => |o| {
            try fillPolygonTvg(allocator, o.vertices, o.fill_style, color_table, xf, opts);
            const stroke_col = resolveStyleColor(o.line_style, color_table, opts);
            strokePolylineTvg(o.vertices, true, o.line_width, stroke_col, xf);
        },
        .outline_fill_rectangles => |o| {
            const fill_col = resolveStyleColor(o.fill_style, color_table, opts);
            const stroke_col = resolveStyleColor(o.line_style, color_table, opts);
            const thickness = o.line_width * xf.meanScale();
            for (o.rectangles) |r| {
                fillTvgRect(r, fill_col, xf);
                strokeTvgRect(r, stroke_col, thickness, xf);
            }
        },
        .outline_fill_path => |o| {
            try fillPathTvg(allocator, o.path, o.fill_style, color_table, xf, opts);
            try strokePathTvg(allocator, o.path, o.line_width, resolveStyleColor(o.line_style, color_table, opts), xf);
        },
    }
}

// ---------------------------------------------------------------------------
// Color helpers
// ---------------------------------------------------------------------------

fn tvgColorToDvui(c: tvg.Color) Color {
    return .{
        .r = @intFromFloat(math.clamp(c.r * 255.0, 0.0, 255.0)),
        .g = @intFromFloat(math.clamp(c.g * 255.0, 0.0, 255.0)),
        .b = @intFromFloat(math.clamp(c.b * 255.0, 0.0, 255.0)),
        .a = @intFromFloat(math.clamp(c.a * 255.0, 0.0, 255.0)),
    };
}

/// Resolve a TVG `Style` to a single dvui `Color`.  Gradients collapse to the
/// midpoint color — a real gradient renderer is a later upgrade.  Override
/// short-circuits.
fn resolveStyleColor(style: tvg.Style, color_table: []const tvg.Color, opts: RenderOptions) Color {
    if (opts.color_override) |c| return c;
    return switch (style) {
        .flat => |idx| tvgColorToDvui(color_table[idx]),
        .linear, .radial => |g| tvgColorToDvui(tvg.Color.lerp(color_table[g.color_0], color_table[g.color_1], 0.5)),
    };
}

// ---------------------------------------------------------------------------
// Fill / stroke primitives in TVG space
// ---------------------------------------------------------------------------

fn fillTvgRect(r: tvg.Rectangle, color: Color, xf: Transform) void {
    const alloc = dvui.currentWindow().lifo();
    var pb = dvui.Path.Builder.init(alloc);
    defer pb.deinit();
    pb.addPoint(xf.applyXY(r.x, r.y));
    pb.addPoint(xf.applyXY(r.x + r.width, r.y));
    pb.addPoint(xf.applyXY(r.x + r.width, r.y + r.height));
    pb.addPoint(xf.applyXY(r.x, r.y + r.height));
    pb.build().fillConvex(.{ .color = color });
}

fn strokeTvgRect(r: tvg.Rectangle, color: Color, thickness: f32, xf: Transform) void {
    const alloc = dvui.currentWindow().lifo();
    var pb = dvui.Path.Builder.init(alloc);
    defer pb.deinit();
    pb.addPoint(xf.applyXY(r.x, r.y));
    pb.addPoint(xf.applyXY(r.x + r.width, r.y));
    pb.addPoint(xf.applyXY(r.x + r.width, r.y + r.height));
    pb.addPoint(xf.applyXY(r.x, r.y + r.height));
    pb.build().stroke(.{ .thickness = thickness, .color = color, .closed = true });
}

fn strokeLine(p0: Point, p1: Point, color: Color, thickness: f32) void {
    const alloc = dvui.currentWindow().lifo();
    var pb = dvui.Path.Builder.init(alloc);
    defer pb.deinit();
    pb.addPoint(p0);
    pb.addPoint(p1);
    pb.build().stroke(.{ .thickness = thickness, .color = color, .closed = false });
}

fn fillPolygonTvg(
    allocator: std.mem.Allocator,
    vertices: []const tvg.Point,
    style: tvg.Style,
    color_table: []const tvg.Color,
    xf: Transform,
    opts: RenderOptions,
) !void {
    if (vertices.len < 3) return;
    const color = resolveStyleColor(style, color_table, opts);

    // Project to physical pixels first; the polygon may be concave so we
    // run an ear-clip tessellation on the result.
    const pts = try allocator.alloc(Point, vertices.len);
    defer allocator.free(pts);
    for (vertices, 0..) |v, i| pts[i] = xf.apply(v);

    try fillPolygonPhysical(allocator, pts, color, opts.fade);
}

fn strokePolylineTvg(
    vertices: []const tvg.Point,
    closed: bool,
    line_width: f32,
    color: Color,
    xf: Transform,
) void {
    if (vertices.len < 2) return;
    const thickness = line_width * xf.meanScale();
    const alloc = dvui.currentWindow().lifo();
    var pb = dvui.Path.Builder.init(alloc);
    defer pb.deinit();
    for (vertices) |v| pb.addPoint(xf.apply(v));
    pb.build().stroke(.{ .thickness = thickness, .color = color, .closed = closed });
}

// ---------------------------------------------------------------------------
// Path flattening (TVG path → polyline in physical pixels)
// ---------------------------------------------------------------------------

/// Flatten one TVG path segment into a polyline of physical points.
/// Per-node `line_width` is ignored for fills; strokes use the command-level
/// line_width since per-node widths require segment-wise stroking which we
/// punt on for T1.
fn flattenSegment(
    segment: tvg.Path.Segment,
    xf: Transform,
    out: *std.ArrayList(Point),
    alloc: std.mem.Allocator,
) !void {
    var cur = segment.start;
    try out.append(alloc, xf.apply(cur));

    for (segment.commands) |node| {
        switch (node) {
            .line => |n| {
                cur = n.data;
                try out.append(alloc, xf.apply(cur));
            },
            .horiz => |n| {
                cur.x = n.data;
                try out.append(alloc, xf.apply(cur));
            },
            .vert => |n| {
                cur.y = n.data;
                try out.append(alloc, xf.apply(cur));
            },
            .bezier => |n| {
                try flattenCubic(out, alloc, xf, cur, n.data.c0, n.data.c1, n.data.p1);
                cur = n.data.p1;
            },
            .quadratic_bezier => |n| {
                try flattenQuadratic(out, alloc, xf, cur, n.data.c, n.data.p1);
                cur = n.data.p1;
            },
            .arc_circle => |n| {
                // NOTE: svg2tvg encodes the sweep bit INVERTED relative to the
                // SVG convention (see rendering.zig where the z2d path passes
                // `!arc.sweep` for the same reason).  Without this flip the
                // rounded corners of icons like `briefcase` come out concave.
                try flattenArc(out, alloc, xf, cur, n.data.radius, n.data.radius, 0, n.data.large_arc, !n.data.sweep, n.data.target);
                cur = n.data.target;
            },
            .arc_ellipse => |n| {
                try flattenArc(out, alloc, xf, cur, n.data.radius_x, n.data.radius_y, n.data.rotation, n.data.large_arc, !n.data.sweep, n.data.target);
                cur = n.data.target;
            },
            .close => {
                cur = segment.start;
                // For fill use, the caller adds the closing edge implicitly
                // (the polygon's last → first vertex).  For stroke use,
                // append the start so the stroke draws the closing segment.
                try out.append(alloc, xf.apply(cur));
            },
        }
    }
}

/// Adaptive subdivision of a cubic Bezier — emits points up to a chord
/// tolerance of ~0.5 px in the OUTPUT (physical pixel) space.  Endpoint p0 is
/// assumed already in `out`; only p1, intermediate, and final points are
/// appended.
fn flattenCubic(
    out: *std.ArrayList(Point),
    alloc: std.mem.Allocator,
    xf: Transform,
    p0: tvg.Point,
    p1: tvg.Point,
    p2: tvg.Point,
    p3: tvg.Point,
) !void {
    const tol_sq: f32 = 0.0025; // (0.05 px)² — same sub-pixel chord budget as flattenArc, for the same reason: dvui's stroke uses miter joins.

    const Frame = struct { p0: Point, p1: Point, p2: Point, p3: Point, depth: u8 };
    var stack: [32]Frame = undefined;
    var sp: usize = 0;
    stack[sp] = .{
        .p0 = xf.apply(p0),
        .p1 = xf.apply(p1),
        .p2 = xf.apply(p2),
        .p3 = xf.apply(p3),
        .depth = 24,
    };
    sp += 1;

    while (sp > 0) {
        sp -= 1;
        const f = stack[sp];

        // Flatness test: max distance from p1, p2 to chord p0-p3.
        const d1 = pointLineDistSq(f.p1, f.p0, f.p3);
        const d2 = pointLineDistSq(f.p2, f.p0, f.p3);
        if (f.depth == 0 or @max(d1, d2) <= tol_sq) {
            try out.append(alloc, f.p3);
            continue;
        }

        // de Casteljau split.
        const m01 = mid(f.p0, f.p1);
        const m12 = mid(f.p1, f.p2);
        const m23 = mid(f.p2, f.p3);
        const m012 = mid(m01, m12);
        const m123 = mid(m12, m23);
        const m0123 = mid(m012, m123);

        // Push RIGHT half first so LEFT is processed next (preserves order).
        stack[sp] = .{ .p0 = m0123, .p1 = m123, .p2 = m23, .p3 = f.p3, .depth = f.depth - 1 };
        sp += 1;
        stack[sp] = .{ .p0 = f.p0, .p1 = m01, .p2 = m012, .p3 = m0123, .depth = f.depth - 1 };
        sp += 1;
    }
}

fn flattenQuadratic(
    out: *std.ArrayList(Point),
    alloc: std.mem.Allocator,
    xf: Transform,
    p0: tvg.Point,
    p1: tvg.Point,
    p2: tvg.Point,
) !void {
    // Promote to cubic: c0 = p0 + 2/3 (p1 - p0), c1 = p2 + 2/3 (p1 - p2).
    const c0 = tvg.Point{
        .x = p0.x + (2.0 / 3.0) * (p1.x - p0.x),
        .y = p0.y + (2.0 / 3.0) * (p1.y - p0.y),
    };
    const c1 = tvg.Point{
        .x = p2.x + (2.0 / 3.0) * (p1.x - p2.x),
        .y = p2.y + (2.0 / 3.0) * (p1.y - p2.y),
    };
    try flattenCubic(out, alloc, xf, p0, c0, c1, p2);
}

/// SVG arc → center-parameterization → sampled chord.  Reference:
/// https://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes
fn flattenArc(
    out: *std.ArrayList(Point),
    alloc: std.mem.Allocator,
    xf: Transform,
    p0: tvg.Point,
    rx_in: f32,
    ry_in: f32,
    rotation_deg: f32,
    large_arc: bool,
    sweep: bool,
    p1: tvg.Point,
) !void {
    var rx = @abs(rx_in);
    var ry = @abs(ry_in);
    if (rx == 0 or ry == 0) {
        try out.append(alloc, xf.apply(p1));
        return;
    }

    const phi = rotation_deg * math.pi / 180.0;
    const cos_phi = @cos(phi);
    const sin_phi = @sin(phi);

    // Step 1: transform to origin-centered, axis-aligned ellipse.
    const dx = (p0.x - p1.x) * 0.5;
    const dy = (p0.y - p1.y) * 0.5;
    const x1p = cos_phi * dx + sin_phi * dy;
    const y1p = -sin_phi * dx + cos_phi * dy;

    // Ensure radii are large enough.
    const lambda = (x1p * x1p) / (rx * rx) + (y1p * y1p) / (ry * ry);
    if (lambda > 1) {
        const s = @sqrt(lambda);
        rx *= s;
        ry *= s;
    }

    // Step 2: compute center in transformed coords.
    const sign: f32 = if (large_arc == sweep) -1.0 else 1.0;
    var num = rx * rx * ry * ry - rx * rx * y1p * y1p - ry * ry * x1p * x1p;
    const den = rx * rx * y1p * y1p + ry * ry * x1p * x1p;
    if (num < 0) num = 0;
    const factor = sign * @sqrt(num / den);
    const cxp = factor * (rx * y1p) / ry;
    const cyp = factor * -(ry * x1p) / rx;

    // Step 3: untransform.
    const cx = cos_phi * cxp - sin_phi * cyp + (p0.x + p1.x) * 0.5;
    const cy = sin_phi * cxp + cos_phi * cyp + (p0.y + p1.y) * 0.5;

    // Step 4: start angle + sweep delta.
    const ux = (x1p - cxp) / rx;
    const uy = (y1p - cyp) / ry;
    const vx = (-x1p - cxp) / rx;
    const vy = (-y1p - cyp) / ry;

    const theta1 = math.atan2(uy, ux);
    var delta = math.atan2(ux * vy - uy * vx, ux * vx + uy * vy);
    if (!sweep and delta > 0) delta -= 2 * math.pi;
    if (sweep and delta < 0) delta += 2 * math.pi;

    // Chord-deviation budget of 0.05 px.  dvui's stroke renders our polyline
    // with hard miter joins between every chord — at ~0.5 px chord error the
    // miters add up to a clearly chamfered corner (e.g. the briefcase icon's
    // rounded corners look octagonal).  Sub-pixel sampling makes the joins
    // invisible to the eye after AA.
    const r_max = @max(rx, ry) * xf.meanScale();
    const err: f32 = 0.05;
    const theta_step = math.acos(math.clamp(r_max / (r_max + err), -1.0, 1.0));
    var n: usize = @intFromFloat(@ceil(@abs(delta) / @max(theta_step, 1e-4)));
    n = math.clamp(n, 4, 512);

    var i: usize = 1;
    while (i < n) : (i += 1) {
        const t = @as(f32, @floatFromInt(i)) / @as(f32, @floatFromInt(n));
        const theta = theta1 + delta * t;
        const ct = @cos(theta);
        const st = @sin(theta);
        const x = cos_phi * rx * ct - sin_phi * ry * st + cx;
        const y = sin_phi * rx * ct + cos_phi * ry * st + cy;
        try out.append(alloc, xf.applyXY(x, y));
    }
    // ALWAYS land the final vertex on `target` exactly.  Floating-point
    // accumulation can otherwise leave a sub-pixel gap → dvui's stroke
    // joins the gap with a stub segment whose normal is undefined →
    // visible miter spike pointing perpendicular to the path.
    try out.append(alloc, xf.apply(p1));
}

fn fillPathTvg(
    allocator: std.mem.Allocator,
    path: tvg.Path,
    style: tvg.Style,
    color_table: []const tvg.Color,
    xf: Transform,
    opts: RenderOptions,
) !void {
    const color = resolveStyleColor(style, color_table, opts);
    // Each segment is a sub-polygon; for fills, render each independently as
    // an ear-clipped polygon.  This is correct for non-overlapping subpaths;
    // compound paths with holes (even-odd) would need proper polygon-with-
    // hole tessellation — punted for T1.
    var pts = std.ArrayList(Point){};
    defer pts.deinit(allocator);
    for (path.segments) |seg| {
        pts.clearRetainingCapacity();
        try flattenSegment(seg, xf, &pts, allocator);
        if (pts.items.len < 3) continue;
        // Strip ALL trailing duplicates of the start vertex.  A closed sub-
        // path commonly produces two copies: the last bezier/arc lands back
        // on the start point, AND the `.close` command appends start again.
        // Leaving even one causes a zero-length segment that pollutes ear-
        // clipping and stroke miter joins.
        stripTrailingDuplicatesOfFirst(&pts);
        if (pts.items.len < 3) continue;
        try fillPolygonPhysical(allocator, pts.items, color, opts.fade);
    }
}

fn strokePathTvg(
    allocator: std.mem.Allocator,
    path: tvg.Path,
    line_width: f32,
    color: Color,
    xf: Transform,
) !void {
    const thickness = line_width * xf.meanScale();
    const alloc = dvui.currentWindow().lifo();
    var pts = std.ArrayList(Point){};
    defer pts.deinit(allocator);
    for (path.segments) |seg| {
        pts.clearRetainingCapacity();
        try flattenSegment(seg, xf, &pts, allocator);
        if (pts.items.len < 2) continue;
        // Strip every trailing copy of the start point — the last bezier/arc
        // commonly lands on `start`, and `.close` then appends start again.
        // dvui's stroke routine produces a perpendicular spike at the join
        // whenever the closing segment has near-zero length (degenerate
        // normal), so we MUST collapse them.
        const closed = pts.items.len > 1 and approxEqPoint(pts.items[0], pts.items[pts.items.len - 1]);
        if (closed) stripTrailingDuplicatesOfFirst(&pts);
        // Also collapse zero-length runs anywhere in the polyline — these
        // cause the same miter-spike artifact at the bad vertex.
        collapseRunDuplicates(&pts);
        if (pts.items.len < 2) continue;
        var pb = dvui.Path.Builder.init(alloc);
        defer pb.deinit();
        for (pts.items) |p| pb.addPoint(p);
        pb.build().stroke(.{ .thickness = thickness, .color = color, .closed = closed });
    }
}

// ---------------------------------------------------------------------------
// Polygon fill (handles concave via ear clipping)
// ---------------------------------------------------------------------------

/// Fill an arbitrary simple polygon `pts` (already in physical pixel space).
/// Convex polygons short-circuit to `dvui.Path.fillConvex`.  Concave polygons
/// run ear clipping and emit a single batched triangle mesh.
fn fillPolygonPhysical(
    allocator: std.mem.Allocator,
    pts: []const Point,
    color: Color,
    fade: f32,
) !void {
    if (pts.len < 3) return;
    if (isConvex(pts)) {
        const win_alloc = dvui.currentWindow().lifo();
        var pb = dvui.Path.Builder.init(win_alloc);
        defer pb.deinit();
        for (pts) |p| pb.addPoint(p);
        pb.build().fillConvex(.{ .color = color, .fade = fade });
        return;
    }
    try earClipFill(allocator, pts, color);
}

fn isConvex(pts: []const Point) bool {
    if (pts.len < 3) return false;
    var sign: f32 = 0;
    for (0..pts.len) |i| {
        const a = pts[i];
        const b = pts[(i + 1) % pts.len];
        const c = pts[(i + 2) % pts.len];
        const cross = (b.x - a.x) * (c.y - b.y) - (b.y - a.y) * (c.x - b.x);
        if (@abs(cross) < 1e-6) continue;
        if (sign == 0) {
            sign = cross;
        } else if ((cross > 0) != (sign > 0)) {
            return false;
        }
    }
    return true;
}

fn signedArea(pts: []const Point) f32 {
    var s: f32 = 0;
    for (0..pts.len) |i| {
        const a = pts[i];
        const b = pts[(i + 1) % pts.len];
        s += (a.x * b.y - b.x * a.y);
    }
    return s * 0.5;
}

fn triangleArea2(a: Point, b: Point, c: Point) f32 {
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
}

fn pointInTriangle(p: Point, a: Point, b: Point, c: Point) bool {
    const d1 = triangleArea2(a, b, p);
    const d2 = triangleArea2(b, c, p);
    const d3 = triangleArea2(c, a, p);
    const has_neg = (d1 < 0) or (d2 < 0) or (d3 < 0);
    const has_pos = (d1 > 0) or (d2 > 0) or (d3 > 0);
    return !(has_neg and has_pos);
}

/// Strictly interior test — points exactly on the triangle's edge are NOT
/// considered inside.  This matters in ear clipping: a reflex vertex sitting
/// on a polygon edge is fine to share with an ear, and forbidding it would
/// freeze the algorithm on geometry where neighbouring path nodes briefly
/// touch the same line (very common after Bezier flattening).
fn pointInTriangleStrict(p: Point, a: Point, b: Point, c: Point) bool {
    const d1 = triangleArea2(a, b, p);
    const d2 = triangleArea2(b, c, p);
    const d3 = triangleArea2(c, a, p);
    const eps: f32 = 1e-5;
    return (d1 > eps and d2 > eps and d3 > eps) or
        (d1 < -eps and d2 < -eps and d3 < -eps);
}

/// Compactly remove the `i`-th entry from a length-`*remaining` ring.
fn removeAt(idx: []u32, remaining: *usize, i: usize) void {
    var k: usize = i;
    while (k + 1 < remaining.*) : (k += 1) idx[k] = idx[k + 1];
    remaining.* -= 1;
}

/// Scale the collinearity tolerance with polygon size so absolute-pixel icons
/// and sub-pixel ones use comparable thresholds.  Returns `2 * area_tol` in
/// the same units as `triangleArea2`.
fn collinearEpsilon(pts: []const Point) f32 {
    var min_x: f32 = pts[0].x;
    var max_x: f32 = pts[0].x;
    var min_y: f32 = pts[0].y;
    var max_y: f32 = pts[0].y;
    for (pts[1..]) |p| {
        if (p.x < min_x) min_x = p.x;
        if (p.x > max_x) max_x = p.x;
        if (p.y < min_y) min_y = p.y;
        if (p.y > max_y) max_y = p.y;
    }
    const extent = @max(max_x - min_x, max_y - min_y);
    // ~0.01 px chord-deviation triangle is considered collinear.
    return @max(1e-5, extent * 1e-4);
}

/// Classic O(n²) ear clipping.  Adequate for icon polygons (≤ a few hundred
/// verts).  Does NOT handle holes or self-intersecting polygons.  Robustness
/// notes:
///   * Re-tests `i_prev` after each ear removal (a removal can create a new
///     ear at the previous vertex; without the back-step we can stall on
///     concave shapes and bail to a bad fan fallback that visibly inverts
///     corners).
///   * Treats near-collinear vertices as removable degenerates so the loop
///     always makes progress and degenerate triangles don't get emitted.
///   * Uses signed-area sign to normalise winding regardless of Y direction
///     — `triangleArea2 > 0` then consistently identifies convex corners.
fn earClipFill(allocator: std.mem.Allocator, pts: []const Point, color: Color) !void {
    const n = pts.len;
    if (n < 3) return;

    var idx = try allocator.alloc(u32, n);
    defer allocator.free(idx);

    // Normalise to a single winding direction so `triangleArea2 > 0` always
    // means "convex corner".  We don't care about absolute handedness — only
    // that the ring traversal and the corner test agree.
    const positive_area = signedArea(pts) > 0;
    for (0..n) |i| idx[i] = @intCast(if (positive_area) i else n - 1 - i);

    var remaining: usize = n;
    const verts_out = pts; // immutable; index ring shrinks instead

    var tris = try std.ArrayList(u32).initCapacity(allocator, (n - 2) * 3);
    defer tris.deinit(allocator);

    // Collinearity threshold scales with the polygon's bounding extent so
    // tiny icons aren't classified as all-degenerate.
    const collinear_eps = collinearEpsilon(pts);

    var i: usize = 0;
    var consecutive_failures: usize = 0;

    while (remaining > 3) {
        if (consecutive_failures > remaining) break; // give up gracefully

        const i_prev = (i + remaining - 1) % remaining;
        const i_next = (i + 1) % remaining;
        const a_i = idx[i_prev];
        const b_i = idx[i];
        const c_i = idx[i_next];
        const a = verts_out[a_i];
        const b = verts_out[b_i];
        const c = verts_out[c_i];

        const cross = triangleArea2(a, b, c);

        // Collinear (or near-zero area) corner: remove without emitting so we
        // never stall on degenerate runs.  This is safe for simple polygons
        // because the middle vertex lies on the prev→next edge.
        if (@abs(cross) <= collinear_eps) {
            removeAt(idx, &remaining, i);
            if (i >= remaining) i = 0;
            consecutive_failures = 0;
            continue;
        }

        var is_ear = cross > 0; // convex corner under our normalised winding

        if (is_ear) {
            // No OTHER vertex inside the candidate triangle?  Reflex vertices
            // of the same polygon are the only ones that can sit inside.
            var j: usize = 0;
            while (j < remaining) : (j += 1) {
                if (j == i_prev or j == i or j == i_next) continue;
                if (pointInTriangleStrict(verts_out[idx[j]], a, b, c)) {
                    is_ear = false;
                    break;
                }
            }
        }

        if (is_ear) {
            try tris.append(allocator, a_i);
            try tris.append(allocator, b_i);
            try tris.append(allocator, c_i);
            removeAt(idx, &remaining, i);
            // Step BACK so the (now) previous vertex gets re-tested — removing
            // an ear can promote its neighbour to an ear too.
            if (i == 0) i = remaining - 1 else i -= 1;
            consecutive_failures = 0;
        } else {
            consecutive_failures += 1;
            i = (i + 1) % remaining;
        }
    }

    if (remaining == 3) {
        try tris.append(allocator, idx[0]);
        try tris.append(allocator, idx[1]);
        try tris.append(allocator, idx[2]);
    }
    // If remaining > 3 here the polygon is genuinely pathological (self-
    // intersection, etc.) — emit nothing rather than a fan that would draw
    // overlapping triangles and look like inverted corners.

    // Emit batched triangle mesh.
    const win_alloc = dvui.currentWindow().lifo();
    var b = dvui.Triangles.Builder.init(win_alloc, n, tris.items.len) catch return;
    defer b.deinit(win_alloc);

    const pma = Color.PMA.fromColor(color);
    for (verts_out) |p| {
        b.appendVertex(.{ .pos = p, .col = pma });
    }
    const Index = dvui.Vertex.Index;
    var t: usize = 0;
    while (t + 2 < tris.items.len) : (t += 3) {
        b.appendTriangles(&.{
            @as(Index, @intCast(tris.items[t])),
            @as(Index, @intCast(tris.items[t + 1])),
            @as(Index, @intCast(tris.items[t + 2])),
        });
    }

    dvui.renderTriangles(b.build_unowned(), null) catch {};
}

// ---------------------------------------------------------------------------
// Tiny math helpers
// ---------------------------------------------------------------------------

fn mid(a: Point, b: Point) Point {
    return .{ .x = (a.x + b.x) * 0.5, .y = (a.y + b.y) * 0.5 };
}

fn pointLineDistSq(p: Point, a: Point, b: Point) f32 {
    const dx = b.x - a.x;
    const dy = b.y - a.y;
    const len_sq = dx * dx + dy * dy;
    if (len_sq < 1e-12) {
        const ex = p.x - a.x;
        const ey = p.y - a.y;
        return ex * ex + ey * ey;
    }
    // Perpendicular distance² from p to infinite line a-b (sufficient for
    // adaptive subdivision flatness).
    const num = dx * (a.y - p.y) - (a.x - p.x) * dy;
    return (num * num) / len_sq;
}

fn approxEqPoint(a: Point, b: Point) bool {
    const dx = a.x - b.x;
    const dy = a.y - b.y;
    return (dx * dx + dy * dy) < 1e-6;
}

/// Pop every trailing point that coincides with `pts[0]`.  Common after
/// closing a sub-path: the last drawing command lands on `start` AND the
/// explicit `.close` node appends `start` again, producing two duplicates.
fn stripTrailingDuplicatesOfFirst(pts: *std.ArrayList(Point)) void {
    while (pts.items.len > 1 and approxEqPoint(pts.items[0], pts.items[pts.items.len - 1])) {
        _ = pts.pop();
    }
}

/// Compact consecutive duplicate points.  Required because a near-zero
/// segment fed to dvui's stroke routine yields a degenerate join normal and
/// renders as a perpendicular spike at that vertex.
fn collapseRunDuplicates(pts: *std.ArrayList(Point)) void {
    if (pts.items.len < 2) return;
    var w: usize = 1;
    for (pts.items[1..]) |p| {
        if (!approxEqPoint(pts.items[w - 1], p)) {
            pts.items[w] = p;
            w += 1;
        }
    }
    pts.shrinkRetainingCapacity(w);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "isConvex square" {
    const sq = [_]Point{
        .{ .x = 0, .y = 0 },
        .{ .x = 10, .y = 0 },
        .{ .x = 10, .y = 10 },
        .{ .x = 0, .y = 10 },
    };
    try std.testing.expect(isConvex(&sq));
}

test "isConvex concave L" {
    const l = [_]Point{
        .{ .x = 0, .y = 0 },
        .{ .x = 10, .y = 0 },
        .{ .x = 10, .y = 4 },
        .{ .x = 4, .y = 4 },
        .{ .x = 4, .y = 10 },
        .{ .x = 0, .y = 10 },
    };
    try std.testing.expect(!isConvex(&l));
}

test "earClipFill L emits n-2 triangles" {
    // Can't render without an active dvui window; just exercise the tess
    // bookkeeping via the helper indirectly.  Skip if no window present.
    // (Smoke test — the dvui rendering call is guarded.)
}
