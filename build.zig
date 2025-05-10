// SPDX-License-Identifier: MPL-2.0
//   Copyright © 2024 Chris Marchesi
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const this_module = b.addModule("svg", .{
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
        .target = target,
        .optimize = .Debug,
    }));
    b.step("test", "Run unit tests").dependOn(&tests.step);
}
