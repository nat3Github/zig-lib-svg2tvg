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
    dvui_module.addImport("xml", module_xml);

    const tests = b.addRunArtifact(b.addTest(.{
        .root_module = this_module,
    }));
    b.step("test", "Run unit tests").dependOn(&tests.step);
}
