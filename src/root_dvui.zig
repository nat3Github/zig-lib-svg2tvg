//! svg2tvg — dvui flavor.
//!
//! Same SVG → TVG conversion as the main module, plus a direct TVG → dvui
//! renderer.  Does NOT pull in z2d (no raster path).  Downstream consumers
//! must inject their own `dvui` module:
//!
//!     const svg2tvg_dep = b.dependency("svg2tvg", .{});
//!     const svg2tvg_dvui = svg2tvg_dep.module("svg2tvg_dvui");
//!     svg2tvg_dvui.addImport("dvui", my_dvui_module);

const std = @import("std");
pub const ut = @import("util.zig");
pub const svg_ut = @import("svg-util.zig");
const tinyvg2 = @import("tinyvg/tinyvg.zig");
pub const tvg = tinyvg2;
pub const conversion = @import("conversion.zig");
pub const dvui_render = @import("dvui_render.zig");

pub const tvg_from_svg = conversion.tvg_from_svg;
pub const renderTvg = dvui_render.renderTvg;
pub const RenderOptions = dvui_render.RenderOptions;
pub const Color = tvg.Color;
