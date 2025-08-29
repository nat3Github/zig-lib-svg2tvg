const std = @import("std");
const update = @import("update.zig");
const GitDependency = update.GitDependency;
fn update_step(step: *std.Build.Step, _: std.Build.Step.MakeOptions) !void {
    const deps = &.{
        // GitDependency{
        //     // z2d vancluever
        //     .url = "https://github.com/vancluever/z2d",
        //     .branch = "main",
        // },
        GitDependency{
            // z2d fixed fork
            .url = "https://github.com/nat3Github/zig-lib-z2d-dev-fork",
            .branch = "main",
        },
        GitDependency{
            // zig-xml
            .url = "https://github.com/ianprime0509/zig-xml",
            .branch = "main",
        },
    };
    try update.update_dependency(step.owner.allocator, deps);
}

pub fn build(b: *std.Build) void {
    const step = b.step("update", "update git dependencies");
    step.makeFn = update_step;

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const this_module = b.addModule("svg2tvg", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    const module_xml = b.dependency("xml", .{
        .target = target,
        .optimize = optimize,
    }).module("xml");

    const module_z2d = b.dependency("z2d", .{
        .target = target,
        .optimize = optimize,
    }).module("z2d");

    this_module.addImport("xml", module_xml);
    this_module.addImport("z2d", module_z2d);

    const tests = b.addRunArtifact(b.addTest(.{
        .root_module = this_module,
    }));
    b.step("test", "Run unit tests").dependOn(&tests.step);
}
