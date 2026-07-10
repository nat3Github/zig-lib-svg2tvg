const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const module_xml = b.dependency("xml", .{
        .target = target,
        .optimize = optimize,
    }).module("xml");

    // --- Core module: SVG→TVG conversion only, no renderer -------------------
    const this_module = b.addModule("svg2tvg", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    this_module.addImport("xml", module_xml);

    const tests = b.addRunArtifact(b.addTest(.{
        .root_module = this_module,
    }));
    b.step("test", "Run unit tests").dependOn(&tests.step);

    // Demo + dump examples live in examples/ as a separate build that imports
    // svg2tvg via local path. See examples/build.zig.
}
