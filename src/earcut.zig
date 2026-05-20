//! Zig port of mapbox/earcut — robust polygon triangulator with hole
//! support, used in Mapbox GL JS, Tippecanoe, etc.
//!
//! Algorithm:
//!   1. Build a doubly-linked vertex ring per contour.
//!   2. Link holes into the outer contour via bridge edges.
//!   3. Run ear-cutting with a z-order spatial index that turns the
//!      "is any other vertex inside the candidate ear?" test from O(N)
//!      per ear into ~O(1) on well-behaved polygons.
//!   4. Three rescue passes for pathological input: trivial collinear
//!      filter, local self-intersection cure, then split-into-halves.
//!
//! Reference: https://github.com/mapbox/earcut
//!
//! Public entry point: `triangulate(allocator, points, hole_starts)`.
//! `points` is a flat list of all vertices for outer + holes laid out
//! contiguously.  `hole_starts` lists the index in `points` where each
//! hole begins; the outer contour is `points[0..hole_starts[0]]`.
//! Returns a flat list of triangle vertex indices into `points`.

const std = @import("std");
const math = std.math;

pub const Point = struct { x: f32, y: f32 };

/// Triangulate a polygon (with optional holes) into a list of triangle
/// vertex indices (3 per triangle).  Caller owns the returned slice.
pub fn triangulate(
    alloc: std.mem.Allocator,
    points: []const Point,
    hole_starts: []const usize,
) ![]u32 {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    var tris = std.ArrayList(u32){};
    errdefer tris.deinit(alloc);

    if (points.len < 3) return tris.toOwnedSlice(alloc);

    const outer_end = if (hole_starts.len > 0) hole_starts[0] else points.len;
    var outer = (try linkedList(a, points, 0, outer_end, true)) orelse return tris.toOwnedSlice(alloc);

    if (hole_starts.len > 0) {
        outer = try eliminateHoles(a, points, hole_starts, outer);
    }

    var min_x: f32 = points[0].x;
    var min_y: f32 = points[0].y;
    var max_x: f32 = points[0].x;
    var max_y: f32 = points[0].y;
    if (points.len > 80) {
        for (points[1..outer_end]) |p| {
            if (p.x < min_x) min_x = p.x;
            if (p.y < min_y) min_y = p.y;
            if (p.x > max_x) max_x = p.x;
            if (p.y > max_y) max_y = p.y;
        }
    }
    const sz = @max(max_x - min_x, max_y - min_y);
    const inv_size: f32 = if (sz > 0) 32767.0 / sz else 0;

    try earcutLinked(a, alloc, outer, &tris, min_x, min_y, inv_size, 0);

    return tris.toOwnedSlice(alloc);
}

// ---------------------------------------------------------------------------
// Node / linked-list
// ---------------------------------------------------------------------------

const Node = struct {
    /// Index of this vertex in the original `points` array.
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

fn newNode(alloc: std.mem.Allocator, i: u32, p: Point) !*Node {
    const n = try alloc.create(Node);
    n.* = .{
        .i = i,
        .x = p.x,
        .y = p.y,
        .prev = n,
        .next = n,
    };
    return n;
}

fn insertNode(alloc: std.mem.Allocator, i: u32, p: Point, last: ?*Node) !*Node {
    const n = try newNode(alloc, i, p);
    if (last) |l| {
        n.next = l.next;
        n.prev = l;
        l.next.prev = n;
        l.next = n;
    }
    return n;
}

fn removeNode(p: *Node) void {
    p.next.prev = p.prev;
    p.prev.next = p.next;
    if (p.prev_z) |pz| pz.next_z = p.next_z;
    if (p.next_z) |nz| nz.prev_z = p.prev_z;
}

/// Build a circular doubly-linked list from `points[start..end]`,
/// oriented to match `outer_ccw` (true = CCW i.e. positive signed area
/// in standard Y-up; in Y-down screen coords this same formula corresponds
/// to visually clockwise but the algorithm is sign-agnostic as long as
/// outer and holes have opposite orientations).
fn linkedList(
    alloc: std.mem.Allocator,
    points: []const Point,
    start: usize,
    end: usize,
    outer_ccw: bool,
) !?*Node {
    if (end <= start) return null;
    var last: ?*Node = null;

    const sa = signedArea(points, start, end);
    if ((sa > 0) == outer_ccw) {
        var i = start;
        while (i < end) : (i += 1) {
            last = try insertNode(alloc, @intCast(i), points[i], last);
        }
    } else {
        var i = end;
        while (i > start) {
            i -= 1;
            last = try insertNode(alloc, @intCast(i), points[i], last);
        }
    }

    if (last != null and equals(last.?, last.?.next)) {
        const ln = last.?;
        removeNode(ln);
        last = ln.next;
    }
    return last;
}

/// Eliminate trivial collinear / coincident vertices from the ring.
fn filterPoints(start_in: ?*Node, end_in_opt: ?*Node) ?*Node {
    const start = start_in orelse return null;
    var end = end_in_opt orelse start;

    var p = start;
    var again = true;
    while (again or p != end) {
        again = false;
        if (!p.steiner and (equals(p, p.next) or area(p.prev, p, p.next) == 0)) {
            removeNode(p);
            p = p.prev;
            end = p;
            if (p == p.next) return null;
            again = true;
        } else {
            p = p.next;
        }
    }
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
    if (pass == 0 and inv_size > 0) indexCurve(ear, min_x, min_y, inv_size);

    var stop = ear;
    var prev: *Node = undefined;
    var next: *Node = undefined;

    while (ear.prev != ear.next) {
        prev = ear.prev;
        next = ear.next;

        const is_ear_now = if (inv_size > 0) isEarHashed(ear, min_x, min_y, inv_size) else isEar(ear);
        if (is_ear_now) {
            try tris.append(out_alloc, prev.i);
            try tris.append(out_alloc, ear.i);
            try tris.append(out_alloc, next.i);
            removeNode(ear);
            // skip next vertex (already covered); we'll come back to it
            ear = next.next;
            stop = next.next;
            continue;
        }

        ear = next;
        if (ear == stop) {
            // No ear found — try a rescue.
            if (pass == 0) {
                const fp = filterPoints(ear, null) orelse break;
                try earcutLinked(arena, out_alloc, fp, tris, min_x, min_y, inv_size, 1);
            } else if (pass == 1) {
                const cured = try cureLocalIntersections(out_alloc, ear, tris);
                try earcutLinked(arena, out_alloc, cured, tris, min_x, min_y, inv_size, 2);
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
    if (area(a, b, c) >= 0) return false; // reflex

    // Check none of the other polygon vertices lie inside abc.
    var p = ear.next.next;
    while (p != ear.prev) : (p = p.next) {
        if (pointInTriangle(a.x, a.y, b.x, b.y, c.x, c.y, p.x, p.y) and
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

    // Walk z-curve in both directions from ear.
    var p = ear.prev_z;
    var n = ear.next_z;

    while (p != null and p.?.z >= min_z and n != null and n.?.z <= max_z) {
        if (p.?.x >= x0 and p.?.x <= x1 and p.?.y >= y0 and p.?.y <= y1 and
            p != a and p != c and
            pointInTriangle(ax, ay, bx, by, cx, cy, p.?.x, p.?.y) and
            area(p.?.prev, p.?, p.?.next) >= 0) return false;
        p = p.?.prev_z;

        if (n.?.x >= x0 and n.?.x <= x1 and n.?.y >= y0 and n.?.y <= y1 and
            n != a and n != c and
            pointInTriangle(ax, ay, bx, by, cx, cy, n.?.x, n.?.y) and
            area(n.?.prev, n.?, n.?.next) >= 0) return false;
        n = n.?.next_z;
    }
    while (p != null and p.?.z >= min_z) {
        if (p.?.x >= x0 and p.?.x <= x1 and p.?.y >= y0 and p.?.y <= y1 and
            p != a and p != c and
            pointInTriangle(ax, ay, bx, by, cx, cy, p.?.x, p.?.y) and
            area(p.?.prev, p.?, p.?.next) >= 0) return false;
        p = p.?.prev_z;
    }
    while (n != null and n.?.z <= max_z) {
        if (n.?.x >= x0 and n.?.x <= x1 and n.?.y >= y0 and n.?.y <= y1 and
            n != a and n != c and
            pointInTriangle(ax, ay, bx, by, cx, cy, n.?.x, n.?.y) and
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
                a = filterPoints(a, a.next) orelse a;
                c = filterPoints(c, c.next) orelse c;
                try earcutLinked(arena, out_alloc, a, tris, min_x, min_y, inv_size, 0);
                try earcutLinked(arena, out_alloc, c, tris, min_x, min_y, inv_size, 0);
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

fn eliminateHoles(
    alloc: std.mem.Allocator,
    points: []const Point,
    hole_starts: []const usize,
    outer_in: *Node,
) !*Node {
    var queue = std.ArrayList(*Node){};
    defer queue.deinit(alloc);

    var i: usize = 0;
    while (i < hole_starts.len) : (i += 1) {
        const start = hole_starts[i];
        const end = if (i + 1 < hole_starts.len) hole_starts[i + 1] else points.len;
        const list_opt = try linkedList(alloc, points, start, end, false);
        const list = list_opt orelse continue;
        if (list == list.next) list.steiner = true;
        try queue.append(alloc, getLeftmost(list));
    }

    // Sort by x ascending so we process leftmost holes first.
    std.mem.sort(*Node, queue.items, {}, struct {
        fn lt(_: void, a: *Node, b: *Node) bool {
            if (a.x != b.x) return a.x < b.x;
            return a.y < b.y;
        }
    }.lt);

    var outer = outer_in;
    for (queue.items) |hole| {
        outer = try eliminateHole(alloc, hole, outer);
    }
    return outer;
}

fn eliminateHole(alloc: std.mem.Allocator, hole: *Node, outer_in: *Node) !*Node {
    const outer = outer_in;
    const bridge = findHoleBridge(hole, outer) orelse return outer;
    const bridge_reverse = try splitPolygon(alloc, bridge, hole);
    _ = filterPoints(bridge_reverse, bridge_reverse.next);
    return filterPoints(outer, outer.next) orelse outer;
}

fn findHoleBridge(hole: *Node, outer: *Node) ?*Node {
    var p = outer;
    const hx = hole.x;
    const hy = hole.y;
    var qx: f32 = -math.floatMax(f32);
    var bridge: ?*Node = null;

    // Find an edge whose horizontal ray from hole hits, take its closer
    // endpoint as the candidate.
    while (true) {
        if (hy <= p.y and hy >= p.next.y and p.next.y != p.y) {
            const x = p.x + (hy - p.y) * (p.next.x - p.x) / (p.next.y - p.y);
            if (x <= hx and x > qx) {
                qx = x;
                bridge = if (p.x < p.next.x) p else p.next;
                if (x == hx) return bridge;
            }
        }
        p = p.next;
        if (p == outer) break;
    }

    if (bridge == null) return null;
    // Look for a better visible vertex inside the (hole, ray_hit, bridge)
    // triangle.
    const stop = bridge.?;
    const mx = bridge.?.x;
    const my = bridge.?.y;
    var tan_min: f32 = math.floatMax(f32);
    var best = bridge.?;

    p = bridge.?;
    while (true) {
        if (hx >= p.x and p.x >= mx and hx != p.x and
            pointInTriangle(if (hy < my) hx else qx, hy, mx, my, if (hy < my) qx else hx, hy, p.x, p.y))
        {
            const dx = @abs(hy - p.y);
            const tan_val = dx / (hx - p.x);
            if (locallyInside(p, hole) and
                (tan_val < tan_min or (tan_val == tan_min and (p.x > best.x or (p.x == best.x and sectorContainsSector(best, p))))))
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

fn isValidDiagonal(a: *Node, b: *Node) bool {
    return a.next.i != b.i and a.prev.i != b.i and
        !intersectsPolygon(a, b) and
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

/// Insert a diagonal a-b, splitting the polygon ring into two rings.
fn splitPolygon(alloc: std.mem.Allocator, a: *Node, b: *Node) !*Node {
    const a2 = try newNode(alloc, a.i, .{ .x = a.x, .y = a.y });
    const b2 = try newNode(alloc, b.i, .{ .x = b.x, .y = b.y });
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
// Z-order curve hashing (Morton code) for spatial indexing
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

/// Merge-sort the z-order linked list (`prev_z`/`next_z`).
fn sortLinked(list_in: *Node) *Node {
    var in_size: usize = 1;
    var list: ?*Node = list_in;
    while (true) {
        var p_opt = list;
        list = null;
        var tail: ?*Node = null;
        var num_merges: usize = 0;

        while (p_opt) |p_node| {
            num_merges += 1;
            var q_opt: ?*Node = p_node;
            var p_size: usize = 0;
            var i: usize = 0;
            while (i < in_size and q_opt != null) : (i += 1) {
                p_size += 1;
                q_opt = q_opt.?.next_z;
            }
            var q_size: usize = in_size;
            var pn: ?*Node = p_node;
            while (p_size > 0 or (q_size > 0 and q_opt != null)) {
                var e: *Node = undefined;
                if (p_size != 0 and (q_size == 0 or q_opt == null or pn.?.z <= q_opt.?.z)) {
                    e = pn.?;
                    pn = e.next_z;
                    p_size -= 1;
                } else {
                    e = q_opt.?;
                    q_opt = q_opt.?.next_z;
                    q_size -= 1;
                }
                if (tail) |t| t.next_z = e else list = e;
                e.prev_z = tail;
                tail = e;
            }
            p_opt = q_opt;
        }
        if (tail) |t| t.next_z = null;
        if (num_merges <= 1) return list orelse list_in;
        in_size *= 2;
    }
}

// ---------------------------------------------------------------------------
// Signed area of a range of `points`.
// ---------------------------------------------------------------------------

fn signedArea(points: []const Point, start: usize, end: usize) f32 {
    var sum: f32 = 0;
    var j: usize = end - 1;
    var i: usize = start;
    while (i < end) : (i += 1) {
        const a = points[j];
        const b = points[i];
        sum += (b.x - a.x) * (a.y + b.y);
        j = i;
    }
    return sum;
}
