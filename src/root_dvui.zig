//! svg2tvg — dvui flavor.
//!
//! Thin wrapper exposing `dvui_render` on top of the core `svg2tvg` module.
//! Re-exports everything `svg2tvg` exposes, plus the dvui triangle renderer.
//! Downstream consumers must inject their own `dvui` module:
//!
//!     const svg2tvg_dep = b.dependency("svg2tvg", .{});
//!     const svg2tvg_dvui = svg2tvg_dep.module("svg2tvg_dvui");
//!     svg2tvg_dvui.addImport("dvui", my_dvui_module);

pub const svg2tvg = @import("svg2tvg");
pub const dvui_render = @import("dvui_render.zig");

// re-exports for convenience
pub const tvg = svg2tvg.tvg;
pub const Color = svg2tvg.Color;
pub const tvg_from_svg = svg2tvg.tvg_from_svg;
pub const conversion = svg2tvg.conversion;

pub const renderTvg = dvui_render.renderTvg;
pub const appendTvg = dvui_render.appendTvg;
pub const MeshBuilder = dvui_render.MeshBuilder;
pub const RenderOptions = dvui_render.RenderOptions;
