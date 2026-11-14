const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const test_step = b.step("test", "Run unit tests");

    // --- Tessellation and TVG parsing, no xml ---------------------------------
    const tessellation_module = b.addModule("tessellation", .{
        .root_source_file = b.path("src/tessellation.zig"),
        .target = target,
        .optimize = optimize,
    });
    test_step.dependOn(&b.addRunArtifact(b.addTest(.{ .root_module = tessellation_module })).step);

    // --- Core module: SVG→TVG conversion only, no renderer -------------------
    if (b.option(bool, "converter", "Module svg2tvg with the SVG converter, fetches xml") orelse true) {
        if (b.lazyDependency("xml", .{ .target = target, .optimize = optimize })) |xml_dependency| {
            const this_module = b.addModule("svg2tvg", .{
                .root_source_file = b.path("src/root.zig"),
                .target = target,
                .optimize = optimize,
            });
            this_module.addImport("xml", xml_dependency.module("xml"));
            this_module.addImport("tessellation", tessellation_module);
            test_step.dependOn(&b.addRunArtifact(b.addTest(.{ .root_module = this_module })).step);
        }
    }

    // Demo + dump examples live in examples/ as a separate build that imports
    // svg2tvg via local path. See examples/build.zig.
}
