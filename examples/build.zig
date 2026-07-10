const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const svg2tvg_dep = b.dependency("svg2tvg", .{
        .target = target,
        .optimize = optimize,
    });
    const svg2tvg_mod = svg2tvg_dep.module("svg2tvg");

    const icons_mod = b.dependency("icons", .{
        .target = target,
        .optimize = optimize,
    }).module("icons");

    const dvui_dep = b.dependency("dvui", .{
        .target = target,
        .optimize = optimize,
        .backend = @as([]const u8, "sdl3"),
    });
    const dvui_mod = dvui_dep.module("dvui_sdl3");
    const sdl_backend_mod = dvui_dep.module("sdl3");

    const z2d_mod = b.dependency("z2d", .{
        .target = target,
        .optimize = optimize,
    }).module("z2d");

    // ---- demo: z2d raster -> dvui texture ----
    const demo_mod = b.createModule(.{
        .root_source_file = b.path("demo.zig"),
        .target = target,
        .optimize = optimize,
    });
    demo_mod.addImport("dvui", dvui_mod);
    demo_mod.addImport("sdl-backend", sdl_backend_mod);
    demo_mod.addImport("svg2tvg", svg2tvg_mod);
    demo_mod.addImport("z2d", z2d_mod);
    demo_mod.addImport("icons", icons_mod);

    const demo_exe = b.addExecutable(.{
        .name = "svg2tvg-demo",
        .root_module = demo_mod,
    });
    b.installArtifact(demo_exe);
    const run_demo = b.addRunArtifact(demo_exe);
    run_demo.step.dependOn(b.getInstallStep());
    b.step("demo", "Run the z2d raster demo").dependOn(&run_demo.step);

    // ---- dump: print TVG command structure of one icon ----
    const dump_mod = b.createModule(.{
        .root_source_file = b.path("dump.zig"),
        .target = target,
        .optimize = optimize,
    });
    dump_mod.addImport("svg2tvg", svg2tvg_mod);
    dump_mod.addImport("icons", icons_mod);
    const dump_exe = b.addExecutable(.{ .name = "svg2tvg-dump", .root_module = dump_mod });
    b.installArtifact(dump_exe);
    const run_dump = b.addRunArtifact(dump_exe);
    if (b.args) |a| run_dump.addArgs(a);
    b.step("dump", "Dump TVG commands for an icon (zig build dump -- <name>)").dependOn(&run_dump.step);
}
