pub const ut = @import("util.zig");
pub const svg_ut = @import("svg-util.zig");
const tinyvg2 = @import("tinyvg/tinyvg.zig");
pub const tvg = tinyvg2;
pub const tvg_parsing = @import("tinyvg/parsing.zig");
pub const conversion = @import("conversion.zig");
pub const tvg_from_svg = conversion.tvg_from_svg;

pub const Color = tvg.Color;

test "Typecheck everything" {
    // Force the compiler to typecheck these modules
    _ = .{ tvg_from_svg, conversion };
}
