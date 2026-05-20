const std = @import("std");
const update = @import("update.zig");
const GitDependency = update.GitDependency;
fn update_step(step: *std.Build.Step, _: std.Build.Step.MakeOptions) !void {
    const deps = &.{
        GitDependency{
            .url = "https://github.com/nat3Github/zig-lib-z2d-dev-fork",
            .branch = "main",
        },
        GitDependency{
            .url = "https://github.com/ianprime0509/zig-xml",
            .branch = "main",
        },
        GitDependency{
            .url = "https://github.com/nat3Github/zig-lib-icons",
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
    const maybe_z2d = b.lazyDependency("z2d", .{
        .target = target,
        .optimize = optimize,
    });
    if (maybe_z2d) |z2d_dep| {
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

    // ------------------------------------------------------------------------
    // Demo app — renders feather icons via dvui_render vs z2d→texture and
    // shows per-method ms / frame.  Pulls icons + dvui (sdl3) as LAZY deps so
    // library consumers never pay for these unless they invoke `zig build demo`.
    // ------------------------------------------------------------------------
    const maybe_icons = b.lazyDependency("icons", .{
        .target = target,
        .optimize = optimize,
    });
    const maybe_dvui = b.lazyDependency("dvui", .{
        .target = target,
        .optimize = optimize,
        .backend = @as([]const u8, "sdl3"),
    });

    if (maybe_icons != null and maybe_dvui != null and maybe_z2d != null) {
        const icons_mod = maybe_icons.?.module("icons");
        const dvui_mod = maybe_dvui.?.module("dvui_sdl3");
        const sdl_backend_mod = maybe_dvui.?.module("sdl3");

        // Inject dvui into svg2tvg_dvui so demo can use both renderers.
        dvui_module.addImport("dvui", dvui_mod);

        const demo_mod = b.createModule(.{
            .root_source_file = b.path("examples/demo.zig"),
            .target = target,
            .optimize = optimize,
        });
        demo_mod.addImport("dvui", dvui_mod);
        demo_mod.addImport("sdl-backend", sdl_backend_mod);
        demo_mod.addImport("svg2tvg", this_module);
        demo_mod.addImport("svg2tvg_dvui", dvui_module);
        demo_mod.addImport("icons", icons_mod);

        const demo_exe = b.addExecutable(.{
            .name = "svg2tvg-demo",
            .root_module = demo_mod,
        });

        b.installArtifact(demo_exe);
        const run_demo = b.addRunArtifact(demo_exe);
        run_demo.step.dependOn(b.getInstallStep());
        b.step("demo", "Run the dvui_render vs z2d demo").dependOn(&run_demo.step);

        // ---- dump: print TVG command structure of one icon (debug helper) ----
        const dump_mod = b.createModule(.{
            .root_source_file = b.path("examples/dump.zig"),
            .target = target,
            .optimize = optimize,
        });
        dump_mod.addImport("svg2tvg", this_module);
        dump_mod.addImport("icons", icons_mod);
        const dump_exe = b.addExecutable(.{ .name = "svg2tvg-dump", .root_module = dump_mod });
        const run_dump = b.addRunArtifact(dump_exe);
        if (b.args) |a| run_dump.addArgs(a);
        b.step("dump", "Dump TVG commands for an icon (zig build dump -- <name>)").dependOn(&run_dump.step);
    }
}
