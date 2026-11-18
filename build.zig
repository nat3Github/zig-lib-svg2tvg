const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const test_step = b.step("test", "Run unit tests");

    // --- Meshes and TVG parsing, no xml ---------------------------------------
    const mesh_module = b.addModule("mesh", .{
        .root_source_file = b.path("src/mesh.zig"),
        .target = target,
        .optimize = optimize,
    });
    test_step.dependOn(&b.addRunArtifact(b.addTest(.{ .root_module = mesh_module })).step);

    // --- Core module: SVG→TVG conversion only, no renderer -------------------
    const xml_dependency = b.dependency("xml", .{ .target = target, .optimize = optimize });
    const this_module = b.addModule("svg2tvg", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    this_module.addImport("xml", xml_dependency.module("xml"));
    this_module.addImport("mesh", mesh_module);
    test_step.dependOn(&b.addRunArtifact(b.addTest(.{ .root_module = this_module })).step);

    // Demo + dump examples live in examples/ as a separate build that imports
    // svg2tvg via local path. See examples/build.zig.
}
