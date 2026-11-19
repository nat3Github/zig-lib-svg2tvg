//! Zig port of mapbox/earcut, line by line from src/earcut.js @main, with the fade of the old dvui
//! `fillTriangles`. Only for fills with a fade wider than 1 px, which the sweep tessellator cannot
//! draw. Each contour must be simple, nesting decides between fill and hole.
//!
//! Reference: https://github.com/mapbox/earcut/blob/main/src/earcut.js
//!
//! ISC License
//!
//! Copyright (c) 2026, Mapbox
//!
//! Permission to use, copy, modify, and/or distribute this software for any purpose
//! with or without fee is hereby granted, provided that the above copyright notice
//! and this permission notice appear in all copies.
//!
//! THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
//! REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
//! FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
//! INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
//! OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
//! TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
//! THIS SOFTWARE.

const std = @import("std");
const math = std.math;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const mesh = @import("mesh.zig");
const Point = mesh.Point;

/// Triangulate a polygon (with optional holes) into a flat list of
/// triangle vertex indices (3 per triangle).  Caller owns the returned
/// slice.
///
/// `points` is the concatenation of outer + each hole's vertices.
/// `hole_starts[i]` is the index in `points` where hole `i` begins.
/// The outer contour is `points[0..hole_starts[0]]` (or all of points
/// if `hole_starts.len == 0`).
pub fn triangulate(
    alloc: std.mem.Allocator,
    points: []const Point,
    hole_starts: []const usize,
) ![]u32 {
    var tris = std.ArrayList(u32).empty;
    errdefer tris.deinit(alloc);

    if (points.len < 3) return tris.toOwnedSlice(alloc);

    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const outer_end = if (hole_starts.len > 0) hole_starts[0] else points.len;
    const outer_node_opt = try linkedList(a, points, 0, outer_end, true);
    if (outer_node_opt == null or outer_node_opt.?.next == outer_node_opt.?.prev) {
        return tris.toOwnedSlice(alloc);
    }

    var outer_node = outer_node_opt.?;

    if (hole_starts.len > 0) {
        outer_node = try eliminateHoles(a, points, hole_starts, outer_node);
    }

    var min_x: f32 = 0;
    var min_y: f32 = 0;
    var inv_size: f32 = 0;
    if (points.len > 80) {
        min_x = points[0][0];
        min_y = points[0][1];
        var max_x = min_x;
        var max_y = min_y;
        var i: usize = 1;
        while (i < outer_end) : (i += 1) {
            const p = points[i];
            if (p[0] < min_x) min_x = p[0];
            if (p[1] < min_y) min_y = p[1];
            if (p[0] > max_x) max_x = p[0];
            if (p[1] > max_y) max_y = p[1];
        }
        const sz = @max(max_x - min_x, max_y - min_y);
        inv_size = if (sz != 0) 32767.0 / sz else 0;
    }

    try earcutLinked(a, alloc, outer_node, &tris, min_x, min_y, inv_size, 0);

    return tris.toOwnedSlice(alloc);
}

// ---------------------------------------------------------------------------
// Node + linked list construction
// ---------------------------------------------------------------------------

const Node = struct {
    i: u32,
    x: f32,
    y: f32,
    prev: *Node,
    next: *Node,
    z: i32 = 0,
    prev_z: ?*Node = null,
    next_z: ?*Node = null,
    steiner: bool = false,
};

fn createNode(alloc: std.mem.Allocator, i: u32, x: f32, y: f32) !*Node {
    const n = try alloc.create(Node);
    n.* = .{ .i = i, .x = x, .y = y, .prev = n, .next = n };
    return n;
}

fn insertNode(alloc: std.mem.Allocator, i: u32, x: f32, y: f32, last: ?*Node) !*Node {
    const p = try createNode(alloc, i, x, y);
    if (last == null) {
        p.prev = p;
        p.next = p;
    } else {
        const l = last.?;
        p.next = l.next;
        p.prev = l;
        l.next.prev = p;
        l.next = p;
    }
    return p;
}

fn removeNode(p: *Node) void {
    p.next.prev = p.prev;
    p.prev.next = p.next;
    if (p.prev_z) |pz| pz.next_z = p.next_z;
    if (p.next_z) |nz| nz.prev_z = p.prev_z;
}

fn linkedList(alloc: std.mem.Allocator, data: []const Point, start: usize, end: usize, clockwise: bool) !?*Node {
    if (end <= start) return null;
    var last: ?*Node = null;
    const sa = signedArea(data, start, end);
    if (clockwise == (sa > 0)) {
        var i = start;
        while (i < end) : (i += 1) {
            last = try insertNode(alloc, @intCast(i), data[i][0], data[i][1], last);
        }
    } else {
        var i = end;
        while (i > start) {
            i -= 1;
            last = try insertNode(alloc, @intCast(i), data[i][0], data[i][1], last);
        }
    }
    if (last) |l| {
        if (equals(l, l.next)) {
            removeNode(l);
            return l.next;
        }
    }
    return last;
}

fn filterPoints(start_in: ?*Node, end_in: ?*Node) ?*Node {
    if (start_in == null) return null;
    var start = start_in.?;
    var end = end_in orelse start;

    var p = start;
    var again: bool = false;
    while (true) {
        again = false;
        if (!p.steiner and (equals(p, p.next) or area(p.prev, p, p.next) == 0)) {
            removeNode(p);
            p = p.prev;
            end = p;
            if (p == p.next) break;
            again = true;
        } else {
            p = p.next;
        }
        if (!(again or p != end)) break;
    }
    _ = &start;
    return end;
}

// ---------------------------------------------------------------------------
// Main ear-cutting loop
// ---------------------------------------------------------------------------

fn earcutLinked(
    arena: std.mem.Allocator,
    out_alloc: std.mem.Allocator,
    ear_in: ?*Node,
    tris: *std.ArrayList(u32),
    min_x: f32,
    min_y: f32,
    inv_size: f32,
    pass: u8,
) std.mem.Allocator.Error!void {
    var ear = ear_in orelse return;

    if (pass == 0 and inv_size != 0) indexCurve(ear, min_x, min_y, inv_size);

    var stop = ear;

    while (ear.prev != ear.next) {
        const prev = ear.prev;
        const next = ear.next;

        const is_ear_now = if (inv_size != 0) isEarHashed(ear, min_x, min_y, inv_size) else isEar(ear);
        if (is_ear_now) {
            try tris.append(out_alloc, prev.i);
            try tris.append(out_alloc, ear.i);
            try tris.append(out_alloc, next.i);

            removeNode(ear);
            ear = next.next;
            stop = next.next;
            continue;
        }

        ear = next;
        if (ear == stop) {
            if (pass == 0) {
                if (filterPoints(ear, null)) |fp| {
                    try earcutLinked(arena, out_alloc, fp, tris, min_x, min_y, inv_size, 1);
                }
            } else if (pass == 1) {
                if (filterPoints(ear, null)) |fp| {
                    const cured = try cureLocalIntersections(out_alloc, fp, tris);
                    try earcutLinked(arena, out_alloc, cured, tris, min_x, min_y, inv_size, 2);
                }
            } else if (pass == 2) {
                try splitEarcut(arena, out_alloc, ear, tris, min_x, min_y, inv_size);
            }
            break;
        }
    }
}

fn isEar(ear: *Node) bool {
    const a = ear.prev;
    const b = ear;
    const c = ear.next;

    if (area(a, b, c) >= 0) return false;

    const ax = a.x;
    const bx = b.x;
    const cx = c.x;
    const ay = a.y;
    const by = b.y;
    const cy = c.y;

    const x0 = @min(ax, @min(bx, cx));
    const y0 = @min(ay, @min(by, cy));
    const x1 = @max(ax, @max(bx, cx));
    const y1 = @max(ay, @max(by, cy));

    var p = c.next;
    while (p != a) : (p = p.next) {
        if (p.x >= x0 and p.x <= x1 and p.y >= y0 and p.y <= y1 and
            pointInTriangleExceptFirst(ax, ay, bx, by, cx, cy, p.x, p.y) and
            area(p.prev, p, p.next) >= 0) return false;
    }
    return true;
}

fn isEarHashed(ear: *Node, min_x: f32, min_y: f32, inv_size: f32) bool {
    const a = ear.prev;
    const b = ear;
    const c = ear.next;

    if (area(a, b, c) >= 0) return false;

    const ax = a.x;
    const bx = b.x;
    const cx = c.x;
    const ay = a.y;
    const by = b.y;
    const cy = c.y;

    const x0 = @min(ax, @min(bx, cx));
    const y0 = @min(ay, @min(by, cy));
    const x1 = @max(ax, @max(bx, cx));
    const y1 = @max(ay, @max(by, cy));

    const min_z = zOrder(x0, y0, min_x, min_y, inv_size);
    const max_z = zOrder(x1, y1, min_x, min_y, inv_size);

    var p = ear.prev_z;
    var n = ear.next_z;

    while (p != null and p.?.z >= min_z and n != null and n.?.z <= max_z) {
        if (p.?.x >= x0 and p.?.x <= x1 and p.?.y >= y0 and p.?.y <= y1 and
            p != a and p != c and
            pointInTriangleExceptFirst(ax, ay, bx, by, cx, cy, p.?.x, p.?.y) and
            area(p.?.prev, p.?, p.?.next) >= 0) return false;
        p = p.?.prev_z;

        if (n.?.x >= x0 and n.?.x <= x1 and n.?.y >= y0 and n.?.y <= y1 and
            n != a and n != c and
            pointInTriangleExceptFirst(ax, ay, bx, by, cx, cy, n.?.x, n.?.y) and
            area(n.?.prev, n.?, n.?.next) >= 0) return false;
        n = n.?.next_z;
    }

    while (p != null and p.?.z >= min_z) {
        if (p.?.x >= x0 and p.?.x <= x1 and p.?.y >= y0 and p.?.y <= y1 and
            p != a and p != c and
            pointInTriangleExceptFirst(ax, ay, bx, by, cx, cy, p.?.x, p.?.y) and
            area(p.?.prev, p.?, p.?.next) >= 0) return false;
        p = p.?.prev_z;
    }

    while (n != null and n.?.z <= max_z) {
        if (n.?.x >= x0 and n.?.x <= x1 and n.?.y >= y0 and n.?.y <= y1 and
            n != a and n != c and
            pointInTriangleExceptFirst(ax, ay, bx, by, cx, cy, n.?.x, n.?.y) and
            area(n.?.prev, n.?, n.?.next) >= 0) return false;
        n = n.?.next_z;
    }
    return true;
}

// ---------------------------------------------------------------------------
// Rescue strategies
// ---------------------------------------------------------------------------

fn cureLocalIntersections(out_alloc: std.mem.Allocator, start_in: *Node, tris: *std.ArrayList(u32)) !*Node {
    var p = start_in;
    var start = start_in;
    while (true) {
        const a = p.prev;
        const b = p.next.next;
        if (!equals(a, b) and intersects(a, p, p.next, b) and
            locallyInside(a, b) and locallyInside(b, a))
        {
            try tris.append(out_alloc, a.i);
            try tris.append(out_alloc, p.i);
            try tris.append(out_alloc, b.i);
            removeNode(p);
            removeNode(p.next);
            p = b;
            start = b;
        }
        p = p.next;
        if (p == start) break;
    }
    return filterPoints(p, null) orelse p;
}

fn splitEarcut(
    arena: std.mem.Allocator,
    out_alloc: std.mem.Allocator,
    start: *Node,
    tris: *std.ArrayList(u32),
    min_x: f32,
    min_y: f32,
    inv_size: f32,
) std.mem.Allocator.Error!void {
    var a = start;
    while (true) {
        var b = a.next.next;
        while (b != a.prev) : (b = b.next) {
            if (a.i != b.i and isValidDiagonal(a, b)) {
                var c = try splitPolygon(arena, a, b);
                const a_filtered = filterPoints(a, a.next) orelse a;
                const c_filtered = filterPoints(c, c.next) orelse c;
                try earcutLinked(arena, out_alloc, a_filtered, tris, min_x, min_y, inv_size, 0);
                try earcutLinked(arena, out_alloc, c_filtered, tris, min_x, min_y, inv_size, 0);
                _ = &c;
                return;
            }
        }
        a = a.next;
        if (a == start) break;
    }
}

// ---------------------------------------------------------------------------
// Hole elimination
// ---------------------------------------------------------------------------

fn eliminateHoles(alloc: std.mem.Allocator, data: []const Point, hole_starts: []const usize, outer_in: *Node) !*Node {
    var queue = std.ArrayList(*Node).empty;
    defer queue.deinit(alloc);

    var i: usize = 0;
    while (i < hole_starts.len) : (i += 1) {
        const start = hole_starts[i];
        const end = if (i + 1 < hole_starts.len) hole_starts[i + 1] else data.len;
        const list_opt = try linkedList(alloc, data, start, end, false);
        const list = list_opt orelse continue;
        if (list == list.next) list.steiner = true;
        try queue.append(alloc, getLeftmost(list));
    }

    std.mem.sort(*Node, queue.items, {}, compareXYSlope);

    var outer = outer_in;
    for (queue.items) |hole| {
        outer = try eliminateHole(alloc, hole, outer);
    }
    return outer;
}

fn compareXYSlope(_: void, a: *Node, b: *Node) bool {
    var result = a.x - b.x;
    if (result == 0) {
        result = a.y - b.y;
        if (result == 0) {
            const a_slope = (a.next.y - a.y) / (a.next.x - a.x);
            const b_slope = (b.next.y - b.y) / (b.next.x - b.x);
            result = a_slope - b_slope;
        }
    }
    return result < 0;
}

fn eliminateHole(alloc: std.mem.Allocator, hole: *Node, outer_node: *Node) !*Node {
    const bridge = findHoleBridge(hole, outer_node) orelse return outer_node;
    const bridge_reverse = try splitPolygon(alloc, bridge, hole);
    _ = filterPoints(bridge_reverse, bridge_reverse.next);
    return filterPoints(bridge, bridge.next) orelse bridge;
}

fn findHoleBridge(hole: *Node, outer_node: *Node) ?*Node {
    var p: *Node = outer_node;
    const hx = hole.x;
    const hy = hole.y;
    var qx: f32 = -math.floatMax(f32);
    var m: ?*Node = null;

    if (equals(hole, p)) return p;

    while (true) {
        if (equals(hole, p.next)) return p.next;
        if (hy <= p.y and hy >= p.next.y and p.next.y != p.y) {
            const x = p.x + (hy - p.y) * (p.next.x - p.x) / (p.next.y - p.y);
            if (x <= hx and x > qx) {
                qx = x;
                m = if (p.x < p.next.x) p else p.next;
                if (x == hx) return m;
            }
        }
        p = p.next;
        if (p == outer_node) break;
    }

    if (m == null) return null;

    // Look for a better visible vertex inside the triangle (hole, ray hit, m).
    const stop = m.?;
    const mx = m.?.x;
    const my = m.?.y;
    var tan_min: f32 = math.floatMax(f32);
    var best = m.?;

    p = m.?;
    while (true) {
        if (hx >= p.x and p.x >= mx and hx != p.x and
            pointInTriangle(
                if (hy < my) hx else qx,
                hy,
                mx,
                my,
                if (hy < my) qx else hx,
                hy,
                p.x,
                p.y,
            ))
        {
            const tan_val = @abs(hy - p.y) / (hx - p.x);
            if (locallyInside(p, hole) and
                (tan_val < tan_min or
                    (tan_val == tan_min and
                        (p.x > best.x or (p.x == best.x and sectorContainsSector(best, p))))))
            {
                best = p;
                tan_min = tan_val;
            }
        }
        p = p.next;
        if (p == stop) break;
    }
    return best;
}

fn sectorContainsSector(m: *Node, p: *Node) bool {
    return area(m.prev, m, p.prev) < 0 and area(p.next, m, m.next) < 0;
}

fn getLeftmost(start: *Node) *Node {
    var p = start;
    var leftmost = start;
    while (true) {
        if (p.x < leftmost.x or (p.x == leftmost.x and p.y < leftmost.y)) leftmost = p;
        p = p.next;
        if (p == start) break;
    }
    return leftmost;
}

// ---------------------------------------------------------------------------
// Geometry primitives
// ---------------------------------------------------------------------------

fn pointInTriangle(ax: f32, ay: f32, bx: f32, by: f32, cx: f32, cy: f32, px: f32, py: f32) bool {
    return (cx - px) * (ay - py) >= (ax - px) * (cy - py) and
        (ax - px) * (by - py) >= (bx - px) * (ay - py) and
        (bx - px) * (cy - py) >= (cx - px) * (by - py);
}

fn pointInTriangleExceptFirst(ax: f32, ay: f32, bx: f32, by: f32, cx: f32, cy: f32, px: f32, py: f32) bool {
    return !(ax == px and ay == py) and pointInTriangle(ax, ay, bx, by, cx, cy, px, py);
}

fn isValidDiagonal(a: *Node, b: *Node) bool {
    return a.next.i != b.i and a.prev.i != b.i and !intersectsPolygon(a, b) and
        ((locallyInside(a, b) and locallyInside(b, a) and middleInside(a, b) and
            (area(a.prev, a, b.prev) != 0 or area(a, b.prev, b) != 0)) or
            (equals(a, b) and area(a.prev, a, a.next) > 0 and area(b.prev, b, b.next) > 0));
}

fn area(p: *Node, q: *Node, r: *Node) f32 {
    return (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y);
}

fn equals(a: *Node, b: *Node) bool {
    return a.x == b.x and a.y == b.y;
}

fn intersects(p1: *Node, q1: *Node, p2: *Node, q2: *Node) bool {
    const o1 = sign(area(p1, q1, p2));
    const o2 = sign(area(p1, q1, q2));
    const o3 = sign(area(p2, q2, p1));
    const o4 = sign(area(p2, q2, q1));
    if (o1 != o2 and o3 != o4) return true;
    if (o1 == 0 and onSegment(p1, p2, q1)) return true;
    if (o2 == 0 and onSegment(p1, q2, q1)) return true;
    if (o3 == 0 and onSegment(p2, p1, q2)) return true;
    if (o4 == 0 and onSegment(p2, q1, q2)) return true;
    return false;
}

fn onSegment(p: *Node, q: *Node, r: *Node) bool {
    return q.x <= @max(p.x, r.x) and q.x >= @min(p.x, r.x) and
        q.y <= @max(p.y, r.y) and q.y >= @min(p.y, r.y);
}

fn sign(v: f32) i32 {
    if (v > 0) return 1;
    if (v < 0) return -1;
    return 0;
}

fn intersectsPolygon(a: *Node, b: *Node) bool {
    var p = a;
    while (true) {
        if (p.i != a.i and p.next.i != a.i and p.i != b.i and p.next.i != b.i and
            intersects(p, p.next, a, b)) return true;
        p = p.next;
        if (p == a) break;
    }
    return false;
}

fn locallyInside(a: *Node, b: *Node) bool {
    return if (area(a.prev, a, a.next) < 0)
        area(a, b, a.next) >= 0 and area(a, a.prev, b) >= 0
    else
        area(a, b, a.prev) < 0 or area(a, a.next, b) < 0;
}

fn middleInside(a: *Node, b: *Node) bool {
    var p = a;
    var inside = false;
    const px = (a.x + b.x) / 2;
    const py = (a.y + b.y) / 2;
    while (true) {
        if (((p.y > py) != (p.next.y > py)) and p.next.y != p.y and
            (px < (p.next.x - p.x) * (py - p.y) / (p.next.y - p.y) + p.x))
            inside = !inside;
        p = p.next;
        if (p == a) break;
    }
    return inside;
}

fn splitPolygon(alloc: std.mem.Allocator, a: *Node, b: *Node) !*Node {
    const a2 = try createNode(alloc, a.i, a.x, a.y);
    const b2 = try createNode(alloc, b.i, b.x, b.y);
    const an = a.next;
    const bp = b.prev;

    a.next = b;
    b.prev = a;
    a2.next = an;
    an.prev = a2;
    b2.next = a2;
    a2.prev = b2;
    bp.next = b2;
    b2.prev = bp;
    return b2;
}

// ---------------------------------------------------------------------------
// Z-order curve (Morton code) spatial indexing
// ---------------------------------------------------------------------------

fn indexCurve(start: *Node, min_x: f32, min_y: f32, inv_size: f32) void {
    var p = start;
    while (true) {
        if (p.z == 0) p.z = zOrder(p.x, p.y, min_x, min_y, inv_size);
        p.prev_z = p.prev;
        p.next_z = p.next;
        p = p.next;
        if (p == start) break;
    }
    p.prev_z.?.next_z = null;
    p.prev_z = null;
    _ = sortLinked(p);
}

fn zOrder(x_in: f32, y_in: f32, min_x: f32, min_y: f32, inv_size: f32) i32 {
    var x: u32 = @intFromFloat(@max(0.0, (x_in - min_x) * inv_size));
    var y: u32 = @intFromFloat(@max(0.0, (y_in - min_y) * inv_size));

    x = (x | (x << 8)) & 0x00FF00FF;
    x = (x | (x << 4)) & 0x0F0F0F0F;
    x = (x | (x << 2)) & 0x33333333;
    x = (x | (x << 1)) & 0x55555555;

    y = (y | (y << 8)) & 0x00FF00FF;
    y = (y | (y << 4)) & 0x0F0F0F0F;
    y = (y | (y << 2)) & 0x33333333;
    y = (y | (y << 1)) & 0x55555555;

    return @bitCast(x | (y << 1));
}

/// Simon Tatham's merge sort on the z-order linked list (prev_z/next_z).
fn sortLinked(list_in: *Node) *Node {
    var list: ?*Node = list_in;
    var in_size: usize = 1;
    while (true) {
        var p_opt: ?*Node = list;
        list = null;
        var tail: ?*Node = null;
        var num_merges: usize = 0;
        while (p_opt) |p_node_init| {
            num_merges += 1;
            var p: ?*Node = p_node_init;
            var q: ?*Node = p_node_init;
            var p_size: usize = 0;
            var k: usize = 0;
            while (k < in_size and q != null) : (k += 1) {
                p_size += 1;
                q = q.?.next_z;
            }
            var q_size: usize = in_size;

            while (p_size > 0 or (q_size > 0 and q != null)) {
                var e: *Node = undefined;
                if (p_size != 0 and (q_size == 0 or q == null or p.?.z <= q.?.z)) {
                    e = p.?;
                    p = p.?.next_z;
                    p_size -= 1;
                } else {
                    e = q.?;
                    q = q.?.next_z;
                    q_size -= 1;
                }
                if (tail) |t| t.next_z = e else list = e;
                e.prev_z = tail;
                tail = e;
            }
            p_opt = q;
        }
        if (tail) |t| t.next_z = null;
        in_size *= 2;
        if (num_merges <= 1) break;
    }
    return list orelse list_in;
}

// ---------------------------------------------------------------------------
// Signed area of a polygon range — earcut's convention (matches JS reference).
// ---------------------------------------------------------------------------

fn signedArea(data: []const Point, start: usize, end: usize) f32 {
    var sum: f32 = 0;
    if (end <= start) return 0;
    var j: usize = end - 1;
    var i: usize = start;
    while (i < end) : (i += 1) {
        sum += (data[j][0] - data[i][0]) * (data[i][1] + data[j][1]);
        j = i;
    }
    return sum;
}

fn ringSignedArea(pts: []const Point) f32 {
    return signedArea(pts, 0, pts.len);
}

fn pointInPolygonEvenOdd(p: Point, poly: []const Point) bool {
    if (poly.len < 3) return false;
    var inside = false;
    var j: usize = poly.len - 1;
    for (poly, 0..) |a, i| {
        const b = poly[j];
        if (((a[1] > p[1]) != (b[1] > p[1])) and
            (p[0] < (b[0] - a[0]) * (p[1] - a[1]) / (b[1] - a[1]) + a[0]))
        {
            inside = !inside;
        }
        j = i;
    }
    return inside;
}

const Sink = struct {
    positions: std.ArrayList(Point) = .empty,
    coverages: std.ArrayList(f32) = .empty,
    indices: std.ArrayList(u32) = .empty,

    fn deinit(sink: *Sink, allocator: Allocator) void {
        sink.positions.deinit(allocator);
        sink.coverages.deinit(allocator);
        sink.indices.deinit(allocator);
    }

    fn addVertex(sink: *Sink, allocator: Allocator, position: Point, coverage: f32) Allocator.Error!u32 {
        try sink.positions.append(allocator, position);
        try sink.coverages.append(allocator, coverage);
        return @intCast(sink.positions.items.len - 1);
    }

    /// Counterclockwise on screen, slivers of no area left out. In f64 the sign survives nearly
    /// collinear corners.
    fn addTri(sink: *Sink, allocator: Allocator, p0: u32, p1: u32, p2: u32) Allocator.Error!void {
        const a = sink.positions.items[p0];
        const b = sink.positions.items[p1];
        const c = sink.positions.items[p2];
        const cross = (@as(f64, b[0]) - a[0]) * (@as(f64, c[1]) - a[1]) - (@as(f64, b[1]) - a[1]) * (@as(f64, c[0]) - a[0]);
        if (!(@abs(cross) > 1e-6)) return;
        try sink.indices.appendSlice(allocator, if (cross < 0) &.{ p0, p1, p2 } else &.{ p0, p2, p1 });
    }
};

const BoundaryEdge = struct { from: Point, to: Point };

fn closeEnough(a: Point, b: Point) bool {
    return @abs(a[0] - b[0]) < 1e-5 and @abs(a[1] - b[1]) < 1e-5;
}

fn addBoundaryPiece(list: *std.ArrayList(BoundaryEdge), allocator: Allocator, p_top: Point, p_bot: Point, inside_pt: Point) Allocator.Error!void {
    if (closeEnough(p_top, p_bot)) return;
    const cross = (p_bot[0] - p_top[0]) * (inside_pt[1] - p_top[1]) - (p_bot[1] - p_top[1]) * (inside_pt[0] - p_top[0]);
    try list.append(allocator, if (cross < 0) .{ .from = p_top, .to = p_bot } else .{ .from = p_bot, .to = p_top });
}

/// Triangulate one outer contour and its holes, collect the opaque triangles and the boundary
/// edges: those of exactly one triangle, diagonals and bridges belong to two.
fn emitGroup(allocator: Allocator, arena: Allocator, sink: *Sink, opaque_triangles: *std.ArrayList([3]u32), boundary: *std.ArrayList(BoundaryEdge), pts: []const Point, hole_starts: []const usize) !void {
    const idx = try triangulate(arena, pts, hole_starts);
    if (idx.len == 0) return;

    const vtx_idx = try arena.alloc(u32, pts.len);
    @memset(vtx_idx, std.math.maxInt(u32));
    for (idx) |i| {
        if (vtx_idx[i] == std.math.maxInt(u32)) vtx_idx[i] = try sink.addVertex(allocator, pts[i], 1);
    }

    const EdgeKey = struct { lo: u32, hi: u32 };
    var edge_count = std.AutoHashMap(EdgeKey, u8).init(arena);
    var t: usize = 0;
    while (t < idx.len) : (t += 3) {
        inline for (.{ .{ 0, 1 }, .{ 1, 2 }, .{ 2, 0 } }) |pair| {
            const p0 = idx[t + pair[0]];
            const p1 = idx[t + pair[1]];
            const key: EdgeKey = if (p0 < p1) .{ .lo = p0, .hi = p1 } else .{ .lo = p1, .hi = p0 };
            const gop = try edge_count.getOrPut(key);
            if (!gop.found_existing) gop.value_ptr.* = 0;
            gop.value_ptr.* +|= 1;
        }
    }

    t = 0;
    while (t < idx.len) : (t += 3) {
        const idx0 = idx[t];
        const idx1 = idx[t + 1];
        const idx2 = idx[t + 2];
        try opaque_triangles.append(arena, .{ vtx_idx[idx0], vtx_idx[idx1], vtx_idx[idx2] });
        const centroid: Point = .{
            (pts[idx0][0] + pts[idx1][0] + pts[idx2][0]) / 3,
            (pts[idx0][1] + pts[idx1][1] + pts[idx2][1]) / 3,
        };
        inline for (.{ .{ idx0, idx1 }, .{ idx1, idx2 }, .{ idx2, idx0 } }) |pair| {
            const a = pair[0];
            const b = pair[1];
            const key: EdgeKey = if (a < b) .{ .lo = a, .hi = b } else .{ .lo = b, .hi = a };
            if ((edge_count.get(key) orelse 0) == 1) try addBoundaryPiece(boundary, arena, pts[a], pts[b], centroid);
        }
    }
}

fn normalize(vector: Point) Point {
    const length = @sqrt(vector[0] * vector[0] + vector[1] * vector[1]);
    if (!(length > 0)) return .{ 0, 0 };
    return .{ vector[0] / length, vector[1] / length };
}

/// Chains the boundary edges into loops, filled side on their left, and fades each loop: `fade`
/// centered on the edge up to 1 px, above that half a pixel inside and the rest outside. The opaque
/// vertices move in to meet the fade.
fn appendFade(sink: *Sink, allocator: Allocator, arena: Allocator, boundary: []const BoundaryEdge, fade: f32) Allocator.Error!void {
    const used = try arena.alloc(bool, boundary.len);
    @memset(used, false);
    const inside_len = @min(0.5, fade / 2);
    const outside_len = if (fade <= 1) fade / 2 else fade - 0.5;

    for (boundary, 0..) |start_e, si| {
        if (used[si]) continue;
        used[si] = true;

        var loop_pts: std.ArrayList(Point) = .empty;
        try loop_pts.append(arena, start_e.from);
        const start_from = start_e.from;
        var cur_to = start_e.to;

        var guard: usize = 0;
        while (!closeEnough(cur_to, start_from) and guard <= boundary.len) : (guard += 1) {
            try loop_pts.append(arena, cur_to);
            var found = false;
            for (boundary, 0..) |be, bi| {
                if (used[bi]) continue;
                if (closeEnough(be.from, cur_to)) {
                    used[bi] = true;
                    cur_to = be.to;
                    found = true;
                    break;
                }
            }
            if (!found) break;
        }

        if (loop_pts.items.len < 3) continue;
        const loop = loop_pts.items;
        const n = loop.len;

        const inner_idx = try arena.alloc(u32, n);
        const outer_idx = try arena.alloc(u32, n);
        for (0..n) |i| {
            const aa = loop[(i + n - 1) % n];
            const bb = loop[i];
            const cc = loop[(i + 1) % n];
            const diffab = normalize(.{ aa[0] - bb[0], aa[1] - bb[1] });
            const diffbc = normalize(.{ bb[0] - cc[0], bb[1] - cc[1] });
            var norm: Point = .{ (diffab[1] + diffbc[1]) / 2, (-diffab[0] - diffbc[0]) / 2 };

            const inner_pos: Point = .{ bb[0] - norm[0] * inside_len, bb[1] - norm[1] * inside_len };
            for (sink.positions.items, sink.coverages.items) |*position, coverage| {
                if (coverage == 1 and closeEnough(position.*, bb)) position.* = inner_pos;
            }
            inner_idx[i] = try sink.addVertex(allocator, inner_pos, 1);

            const d2 = norm[0] * norm[0] + norm[1] * norm[1];
            if (d2 > 0.000001) norm = .{ norm[0] / d2, norm[1] / d2 };
            const l = @sqrt(norm[0] * norm[0] + norm[1] * norm[1]);
            if (l > 2.0) norm = .{ norm[0] * 2 / l, norm[1] * 2 / l };

            outer_idx[i] = try sink.addVertex(allocator, .{ bb[0] + norm[0] * outside_len, bb[1] + norm[1] * outside_len }, 0);
        }

        for (0..n) |i| {
            const j = (i + 1) % n;
            try sink.addTri(allocator, inner_idx[i], outer_idx[i], inner_idx[j]);
            try sink.addTri(allocator, outer_idx[i], outer_idx[j], inner_idx[j]);
        }
    }
}

/// Fill of `polygons` with a fade of `fade` px, nested contours alternate between fill and hole
/// (containment forest of the old dvui `fillTriangles`). Points within `max_coordinate`.
pub fn fill(allocator: Allocator, polygons: []const []const Point, fade: f32) mesh.Error!mesh.Mesh {
    assert(fade >= 0 and fade <= mesh.max_coordinate);
    for (polygons) |polygon| for (polygon) |point| for (point) |coordinate| {
        if (!(@abs(coordinate) <= mesh.max_coordinate)) return error.InvalidInput;
    };

    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var sink: Sink = .{};
    defer sink.deinit(allocator);
    var boundary: std.ArrayList(BoundaryEdge) = .empty;
    var opaque_triangles: std.ArrayList([3]u32) = .empty;

    if (polygons.len == 1) {
        try emitGroup(allocator, arena, &sink, &opaque_triangles, &boundary, polygons[0], &.{});
    } else if (polygons.len > 1) {
        const areas = try arena.alloc(f32, polygons.len);
        for (polygons, 0..) |c, i| areas[i] = ringSignedArea(c);

        // Immediate parent: the smallest other contour that contains this one's first point.
        const parent = try arena.alloc(?usize, polygons.len);
        for (polygons, 0..) |c, ci| {
            parent[ci] = null;
            if (c.len == 0) continue;
            var best_area: f32 = math.floatMax(f32);
            for (polygons, 0..) |other, oi| {
                if (oi == ci or other.len == 0) continue;
                if (!pointInPolygonEvenOdd(c[0], other)) continue;
                if (@abs(areas[oi]) < best_area) {
                    best_area = @abs(areas[oi]);
                    parent[ci] = oi;
                }
            }
        }

        // Even depth fills, odd depth is a hole. Overlapping contours can be each other's parent,
        // the walk stops after `polygons.len` steps.
        const is_outer = try arena.alloc(bool, polygons.len);
        for (0..polygons.len) |ci| {
            var depth: usize = 0;
            var cur = parent[ci];
            while (cur) |p| : (cur = parent[p]) {
                depth += 1;
                if (depth > polygons.len) break;
            }
            is_outer[ci] = depth % 2 == 0;
        }

        for (polygons, 0..) |outer, oi| {
            if (!is_outer[oi]) continue;
            var pts: std.ArrayList(Point) = .empty;
            var hole_idx: std.ArrayList(usize) = .empty;
            try pts.appendSlice(arena, outer);
            for (polygons, 0..) |hole, hi| {
                if (is_outer[hi] or parent[hi] != oi) continue;
                try hole_idx.append(arena, pts.items.len);
                try pts.appendSlice(arena, hole);
            }
            try emitGroup(allocator, arena, &sink, &opaque_triangles, &boundary, pts.items, hole_idx.items);
        }
    }

    if (fade > 0 and boundary.items.len > 0) try appendFade(&sink, allocator, arena, boundary.items, fade);
    // After the fade moved the opaque vertices in, which can turn thin triangles over.
    for (opaque_triangles.items) |corners| try sink.addTri(allocator, corners[0], corners[1], corners[2]);

    const positions = try sink.positions.toOwnedSlice(allocator);
    errdefer allocator.free(positions);
    const coverages = try sink.coverages.toOwnedSlice(allocator);
    errdefer allocator.free(coverages);
    return .{ .positions = positions, .coverages = coverages, .indices = try sink.indices.toOwnedSlice(allocator) };
}
