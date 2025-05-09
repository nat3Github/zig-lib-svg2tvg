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
    const module_icons = b.dependency("icons", .{
        .target = target,
        .optimize = optimize,
    }).module("icons");
    const module_tvg_sdk = b.dependency("tvg_sdk", .{
        .target = target,
        .optimize = optimize,
    }).module("tvg");

    const module_image = b.dependency("image", .{
        .target = target,
        .optimize = optimize,
    }).module("image");

    this_module.addImport("xml", module_xml);
    this_module.addImport("icons", module_icons);
    this_module.addImport("tvg", module_tvg_sdk);
    this_module.addImport("image", module_image);

    const tests = b.addRunArtifact(b.addTest(.{
        .root_module = this_module,
        .target = target,
        .optimize = .Debug,
    }));
    b.step("test", "Run unit tests").dependOn(&tests.step);
}
