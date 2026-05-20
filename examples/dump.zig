const std = @import("std");
const svg2tvg = @import("svg2tvg");
const icons = @import("icons");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();

    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();
    _ = args.next();
    const icon_name = args.next() orelse "at-sign";

    @setEvalBranchQuota(200_000);
    const bytes_opt: ?[]const u8 = blk: {
        const set_name = args.next() orelse "feather";
        if (std.mem.eql(u8, set_name, "lucide")) {
            inline for (@typeInfo(icons.tvg.lucide).@"struct".decls) |d| {
                if (std.mem.eql(u8, d.name, icon_name)) {
                    break :blk @field(icons.tvg.lucide, d.name);
                }
            }
        } else if (std.mem.eql(u8, set_name, "entypo")) {
            inline for (@typeInfo(icons.tvg.entypo).@"struct".decls) |d| {
                if (std.mem.eql(u8, d.name, icon_name)) {
                    break :blk @field(icons.tvg.entypo, d.name);
                }
            }
        } else {
            inline for (@typeInfo(icons.tvg.feather).@"struct".decls) |d| {
                if (std.mem.eql(u8, d.name, icon_name)) {
                    break :blk @field(icons.tvg.feather, d.name);
                }
            }
        }
        break :blk null;
    };
    const bytes = bytes_opt orelse {
        std.debug.print("unknown icon: {s}\n", .{icon_name});
        return;
    };

    var fbs = std.io.fixedBufferStream(bytes);
    var parser = try svg2tvg.tvg_parsing.Parser(@TypeOf(fbs.reader())).init(alloc, fbs.reader());
    defer parser.deinit();
    std.debug.print("icon {s}: {d}x{d}, {d} colors, {d} bytes\n", .{ icon_name, parser.header.width, parser.header.height, parser.color_table.len, bytes.len });

    var cmd_i: usize = 0;
    while (try parser.next()) |cmd| : (cmd_i += 1) {
        std.debug.print("  [{d}] {s}\n", .{ cmd_i, @tagName(cmd) });
        switch (cmd) {
            .fill_path => |fp| dumpPath("fill", fp.path),
            .draw_line_path => |dp| dumpPath("draw_line", dp.path),
            .outline_fill_path => |o| dumpPath("outline_fill", o.path),
            else => {},
        }
    }
}

fn dumpPath(kind: []const u8, path: anytype) void {
    std.debug.print("    {s} path: {d} segments\n", .{ kind, path.segments.len });
    for (path.segments, 0..) |seg, si| {
        std.debug.print("      seg[{d}] start=({d:.2},{d:.2}) cmds={d}\n", .{ si, seg.start.x, seg.start.y, seg.commands.len });
        for (seg.commands, 0..) |node, ni| {
            std.debug.print("        n[{d}] {s}", .{ ni, @tagName(node) });
            switch (node) {
                .line => |n| std.debug.print(" -> ({d:.2},{d:.2})", .{ n.data.x, n.data.y }),
                .horiz => |n| std.debug.print(" x={d:.2}", .{n.data}),
                .vert => |n| std.debug.print(" y={d:.2}", .{n.data}),
                .bezier => |n| std.debug.print(" c0=({d:.2},{d:.2}) c1=({d:.2},{d:.2}) p1=({d:.2},{d:.2})", .{ n.data.c0.x, n.data.c0.y, n.data.c1.x, n.data.c1.y, n.data.p1.x, n.data.p1.y }),
                .arc_circle => |n| std.debug.print(" r={d:.2} large={} sweep={} -> ({d:.2},{d:.2})", .{ n.data.radius, n.data.large_arc, n.data.sweep, n.data.target.x, n.data.target.y }),
                .arc_ellipse => |n| std.debug.print(" rx={d:.2} ry={d:.2} rot={d:.2} large={} sweep={} -> ({d:.2},{d:.2})", .{ n.data.radius_x, n.data.radius_y, n.data.rotation, n.data.large_arc, n.data.sweep, n.data.target.x, n.data.target.y }),
                .close => std.debug.print(" close", .{}),
                else => {},
            }
            std.debug.print("\n", .{});
        }
    }
}
