const std = @import("std");

pub const NodeType = enum {
    move_to,
    close_path,
    line_to,
    horizontal_line_to,
    vertical_line_to,
    curve_to,
    smooth_curve_to,
    quadratic_bezier_curve_to,
    smooth_quadratic_bezier_curve_to,
    elliptical_arc,
};

pub const Command = struct {
    node_type: NodeType,
    rel: bool,
    values: []const f32,
};

pub fn parse_path_data(allocator: std.mem.Allocator, d: []const u8) ![]Command {
    var commands = std.ArrayList(Command).init(allocator);
    var it = std.mem.tokenizeAny(u8, d, " ,\t\n\r");

    var tokens = std.ArrayList([]const u8).init(allocator);
    while (it.next()) |token| {
        std.log.warn("token: {s}", .{token});
        var start: usize = 0;
        var dot: bool = false;
        var i: usize = 0;
        while (i < token.len) {
            const c = token[i];
            if (c == '.') {
                if (dot) {
                    if (start != i) {
                        try tokens.append(token[start..i]);
                        start = i;
                    }
                } else dot = true;
            }
            if (c == '-' or c == '+') {
                if (start != i) {
                    try tokens.append(token[start..i]);
                    dot = false;
                    start = i;
                }
            }
            if (std.ascii.isAlphabetic(c)) {
                if (start != i) {
                    try tokens.append(token[start..i]);
                }
                try tokens.append(token[i .. i + 1]);
                dot = false;
                start = i + 1;
            }
            i = i + 1;
        }
        if (start != token.len)
            try tokens.append(token[start..token.len]);
    }
    for (tokens.items) |tk| std.log.warn("{s}", .{tk});

    var current_cmd: ?u8 = null;
    var is_relative: bool = false;

    var values = std.ArrayList(f32).init(allocator);
    var grabbing = false;
    for (tokens.items) |token| {
        if (std.ascii.isAlphabetic(token[0])) {
            if (!grabbing) {
                current_cmd = token[0];
                is_relative = std.ascii.isLower(current_cmd.?);
                grabbing = true;
            } else {
                try commands.append(Command{
                    .node_type = try map_command_to_node(current_cmd.?),
                    .rel = is_relative,
                    .values = try allocator.dupe(f32, values.items),
                });
                values = std.ArrayList(f32).init(allocator);
                current_cmd = token[0];
                is_relative = std.ascii.isLower(current_cmd.?);
            }
        } else if (current_cmd == null) {
            return error.InvalidPathData;
        } else {
            std.log.warn("{s}", .{token});
            const val = try std.fmt.parseFloat(f32, token);
            std.debug.assert(std.math.isNormal(val) or val == 0);
            try values.append(val);
        }
    }
    try commands.append(Command{
        .node_type = try map_command_to_node(current_cmd.?),
        .rel = is_relative,
        .values = try allocator.dupe(f32, values.items),
    });
    return commands.items;
}
fn map_command_to_node(c: u8) !NodeType {
    return switch (std.ascii.toLower(c)) {
        'm' => NodeType.move_to,
        'z' => NodeType.close_path,
        'l' => NodeType.line_to,
        'h' => NodeType.horizontal_line_to,
        'v' => NodeType.vertical_line_to,
        'c' => NodeType.curve_to,
        's' => NodeType.smooth_curve_to,
        'q' => NodeType.quadratic_bezier_curve_to,
        't' => NodeType.smooth_quadratic_bezier_curve_to,
        'a' => NodeType.elliptical_arc,
        else => error.UnknownCommand,
    };
}
