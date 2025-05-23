const std = @import("std");
pub const ut = @import("util.zig");
pub const z2d = @import("z2d");
pub const svg_ut = @import("svg-util.zig");
const tinyvg2 = @import("tinyvg/tinyvg.zig");
pub const tvg = tinyvg2;
pub const rendering = @import("rendering.zig");
pub const conversion = @import("conversion.zig");
pub const renderStream = rendering.renderStream;
pub const RenderOptions = rendering.Options;
pub const tvg_from_svg = conversion.tvg_from_svg;

pub const Color = tvg.Color;

test "coverage" {
    _ = .{ tvg_from_svg, renderStream, rendering, conversion };
    std.log.warn("ok", .{});
}
