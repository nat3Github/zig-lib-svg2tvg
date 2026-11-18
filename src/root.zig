pub const ut = @import("util.zig");
pub const svg_ut = @import("svg-util.zig");
pub const mesh = @import("mesh");
pub const tvg = mesh.tvg;
pub const tvg_parsing = tvg.parsing;
pub const conversion = @import("conversion.zig");
pub const tvg_from_svg = conversion.tvg_from_svg;

pub const Color = tvg.Color;

test "Typecheck everything" {
    // Force the compiler to typecheck these modules
    _ = .{ tvg_from_svg, conversion };
}
