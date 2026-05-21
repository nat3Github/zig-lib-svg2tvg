//! Zig port of mapbox/earcut, line-by-line from src/earcut.js @main.
//!
//! Reference: https://github.com/mapbox/earcut/blob/main/src/earcut.js
//!
//! Robust polygon triangulator with hole support.  Algorithm:
//!   1. Build doubly-linked rings (outer + holes).
//!   2. Link holes into outer via bridge edges (Eberly's algorithm).
//!   3. Ear-clipping loop with z-order spatial hash for fast point-in-
//!      triangle.
//!   4. Three rescue passes for non-simple input: collinear filter,
//!      cure local self-intersections, then split-into-halves.

const std = @import("std");
const math = std.math;

pub const Point = struct { x: f32, y: f32 };

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
        min_x = points[0].x;
        min_y = points[0].y;
        var max_x = min_x;
        var max_y = min_y;
        var i: usize = 1;
        while (i < outer_end) : (i += 1) {
            const p = points[i];
            if (p.x < min_x) min_x = p.x;
            if (p.y < min_y) min_y = p.y;
            if (p.x > max_x) max_x = p.x;
            if (p.y > max_y) max_y = p.y;
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
            last = try insertNode(alloc, @intCast(i), data[i].x, data[i].y, last);
        }
    } else {
        var i = end;
        while (i > start) {
            i -= 1;
            last = try insertNode(alloc, @intCast(i), data[i].x, data[i].y, last);
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
                var c = splitPolygon(arena, a, b) catch return;
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
        )) {
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
        sum += (data[j].x - data[i].x) * (data[i].y + data[j].y);
        j = i;
    }
    return sum;
}

// ---------------------------------------------------------------------------
// Self-tests on small known polygons
// ---------------------------------------------------------------------------

test "triangulate unit square" {
    const pts = [_]Point{
        .{ .x = 0, .y = 0 },
        .{ .x = 1, .y = 0 },
        .{ .x = 1, .y = 1 },
        .{ .x = 0, .y = 1 },
    };
    const tri = try triangulate(std.testing.allocator, &pts, &.{});
    defer std.testing.allocator.free(tri);
    try std.testing.expectEqual(@as(usize, 6), tri.len);
}

test "triangulate square with square hole" {
    // Outer CCW, hole CW.
    const pts = [_]Point{
        // outer
        .{ .x = 0, .y = 0 },
        .{ .x = 10, .y = 0 },
        .{ .x = 10, .y = 10 },
        .{ .x = 0, .y = 10 },
        // hole
        .{ .x = 3, .y = 3 },
        .{ .x = 3, .y = 7 },
        .{ .x = 7, .y = 7 },
        .{ .x = 7, .y = 3 },
    };
    const holes = [_]usize{4};
    const tri = try triangulate(std.testing.allocator, &pts, &holes);
    defer std.testing.allocator.free(tri);
    // Donut has 8 vertices, expected 6 triangles → 18 indices.
    try std.testing.expectEqual(@as(usize, 24), tri.len);
}

test "triangulate L-shape" {
    const pts = [_]Point{
        .{ .x = 0, .y = 0 },
        .{ .x = 10, .y = 0 },
        .{ .x = 10, .y = 4 },
        .{ .x = 4, .y = 4 },
        .{ .x = 4, .y = 10 },
        .{ .x = 0, .y = 10 },
    };
    const tri = try triangulate(std.testing.allocator, &pts, &.{});
    defer std.testing.allocator.free(tri);
    // L has 6 vertices → 4 triangles → 12 indices.
    try std.testing.expectEqual(@as(usize, 12), tri.len);
}
