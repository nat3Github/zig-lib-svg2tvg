const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const module_xml = b.dependency("xml", .{
        .target = target,
        .optimize = optimize,
    }).module("xml");

    // --- Full module: SVG→TVG + z2d raster renderer ---------------------------
    const this_module = b.addModule("svg2tvg", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    this_module.addImport("xml", module_xml);
    if (b.lazyDependency("z2d", .{
        .target = target,
        .optimize = optimize,
    })) |z2d_dep| {
        this_module.addImport("z2d", z2d_dep.module("z2d"));
    }

    // --- dvui module: SVG→TVG + direct dvui triangle renderer (no z2d) -------
    // Downstream MUST inject their own dvui module:
    //
    //     const svg2tvg_dep = b.dependency("svg2tvg", .{});
    //     const svg2tvg_dvui = svg2tvg_dep.module("svg2tvg_dvui");
    //     svg2tvg_dvui.addImport("dvui", my_dvui_module);
    const dvui_module = b.addModule("svg2tvg_dvui", .{
        .root_source_file = b.path("src/root_dvui.zig"),
        .target = target,
        .optimize = optimize,
    });
    dvui_module.addImport("svg2tvg", this_module);

    const tests = b.addRunArtifact(b.addTest(.{
        .root_module = this_module,
    }));
    b.step("test", "Run unit tests").dependOn(&tests.step);

    // Demo + dump examples live in examples/ as a separate build that imports
    // svg2tvg via local path. See examples/build.zig.
}
