const root = @import("root.zig");
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

/// Number grammar of SVG path data: "1e1.5" is 1e1 followed by .5, "1-2" is 1 followed by -2.
pub fn parse_path_data(allocator: std.mem.Allocator, d: []const u8) ![]Command {
    var commands = std.array_list.Managed(Command).init(allocator);
    var values = std.array_list.Managed(f32).init(allocator);
    var current_cmd: ?u8 = null;
    var position: usize = 0;
    while (position < d.len) {
        const c = d[position];
        if (std.ascii.isWhitespace(c) or c == ',') {
            position += 1;
        } else if (std.ascii.isAlphabetic(c)) {
            if (current_cmd) |cmd| try commands.append(.{
                .node_type = try map_command_to_node(cmd),
                .rel = std.ascii.isLower(cmd),
                .values = try values.toOwnedSlice(),
            });
            current_cmd = c;
            position += 1;
        } else {
            if (current_cmd == null) return error.InvalidPathData;
            var number_end = position;
            if (d[number_end] == '+' or d[number_end] == '-') number_end += 1;
            const integer_start = number_end;
            while (number_end < d.len and std.ascii.isDigit(d[number_end])) number_end += 1;
            var digit_count = number_end - integer_start;
            if (number_end < d.len and d[number_end] == '.') {
                number_end += 1;
                const fraction_start = number_end;
                while (number_end < d.len and std.ascii.isDigit(d[number_end])) number_end += 1;
                digit_count += number_end - fraction_start;
            }
            if (digit_count == 0) return error.InvalidPathData;
            if (number_end < d.len and (d[number_end] == 'e' or d[number_end] == 'E')) {
                var exponent_end = number_end + 1;
                if (exponent_end < d.len and (d[exponent_end] == '+' or d[exponent_end] == '-')) exponent_end += 1;
                const exponent_digits_start = exponent_end;
                while (exponent_end < d.len and std.ascii.isDigit(d[exponent_end])) exponent_end += 1;
                if (exponent_end > exponent_digits_start) number_end = exponent_end;
            }
            std.debug.assert(number_end > position);
            const value = try std.fmt.parseFloat(f32, d[position..number_end]);
            if (!std.math.isFinite(value)) return error.InvalidPathData;
            try values.append(value);
            position = number_end;
        }
    }
    const last_cmd = current_cmd orelse return commands.items;
    try commands.append(.{
        .node_type = try map_command_to_node(last_cmd),
        .rel = std.ascii.isLower(last_cmd),
        .values = try values.toOwnedSlice(),
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

pub const SvgColorAttribute = enum {
    none,
    inherit,
    currentColor,
};
pub const svg_parsing = @import("svg-vancluever.zig");
pub const Color = root.tvg.Color;
pub const SvgColorTag = enum {
    att,
    col,
};
/// TODO: Svg Color inheritance handling of overriding values inside containers
/// (Stack) -> Color Resolving
pub const SvgColor = union(SvgColorTag) {
    att: SvgColorAttribute,
    col: Color,
    pub const none: SvgColor = .{ .att = .none };
    pub const inherit: SvgColor = .{ .att = .inherit };
    pub const currentColor: SvgColor = .{ .att = .currentColor };

    pub fn parseColor(val: []const u8) SvgColor {
        const trimmed = std.mem.trim(u8, val, " ");
        if (std.mem.eql(u8, trimmed, "none")) {
            return none;
        }
        if (std.mem.eql(u8, trimmed, "inherit")) {
            return inherit;
        }
        if (std.mem.eql(u8, trimmed, "currentColor")) {
            return currentColor;
        }
        const parsed = svg_parsing.Color.parse(val).color;
        const hex_col = SvgColor{
            .col = Color_from(parsed, 1.0),
        };
        return hex_col;
    }
    fn normalize_u8(u: u8) f32 {
        const x: f32 = @floatFromInt(u);
        return x / 255.0;
    }
    fn Color_from(col: svg_parsing.Color, alpha_normalized: f32) Color {
        return Color{
            .r = normalize_u8(col.r),
            .g = normalize_u8(col.g),
            .b = normalize_u8(col.b),
            .a = std.math.clamp(alpha_normalized, 0, 1),
        };
    }
};
