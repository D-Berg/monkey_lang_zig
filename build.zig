const std = @import("std");
const builtin = @import("builtin");

pub fn build(b: *std.Build) !void {
    const log_level = b.option(std.log.Level, "log", "Set log level") orelse std.log.Level.info;
    const strip = b.option(bool, "strip", "strip debug info from binary");
    const enable_tracy = b.option(bool, "trace", "Enables tracy") orelse false;

    var build_options = b.addOptions();
    build_options.addOption(@TypeOf(log_level), "log_level", log_level);

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // only ran when doing zig build web
    buildWeb(b);

    const runtime_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .wasi,
        // .os_version_min = .{
        //     // change in order to target different wasi versions
        //     // for now I think zig only support wasi preview 1
        //     .semver = try std.SemanticVersion.parse("0.1.0"),
        // },
    });
    const monkey_runtime_mod = b.addModule("monkey_runtime", .{
        .root_source_file = b.path("src/wasm/runtime.zig"),
        .target = runtime_target,
        .optimize = .ReleaseSmall,
        .strip = true,
    });

    monkey_runtime_mod.export_symbol_names = &.{
        "__allocate_string",
        "__print_object",
    };

    const monkey_runtime_exe = b.addExecutable(.{
        .name = "monkey_runtime",
        .root_module = monkey_runtime_mod,
    });

    const embed_wasm = try createEmbedWasmStep(b, monkey_runtime_exe);

    const monkey_mod = b.addModule("monkey", .{
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("src/main.zig"),
        .strip = strip,
    });
    monkey_mod.addOptions("build_options", build_options);
    monkey_mod.addOptions("runtime", embed_wasm.options);

    const exe = b.addExecutable(.{
        .name = "monkey",
        .root_module = monkey_mod,
    });

    const tracy_dep = b.dependency("tracy", .{});
    const tracy_lib = b.addLibrary(.{
        .name = "tracy",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libcpp = true,
        }),
        .linkage = .static,
    });
    tracy_lib.root_module.addIncludePath(tracy_dep.path("public"));
    tracy_lib.root_module.addCSourceFile(.{
        .file = tracy_dep.path("public/TracyClient.cpp"),
    });

    if (enable_tracy) {
        exe.root_module.linkLibrary(tracy_lib);
        tracy_lib.root_module.addCMacro("TRACY_ENABLE", "1");
    }

    build_options.addOption(bool, "enable_tracy", enable_tracy);
    b.installArtifact(exe);

    // makes it possible to do `zig build run -Dtarget="wasm32-wasi`
    b.enable_wasmtime = true;

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_module = monkey_mod,
        .test_runner = .{
            .path = b.path("test_runner.zig"),
            .mode = .simple,
        },
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    if (b.args) |args| {
        run_cmd.addArgs(args);
        run_exe_unit_tests.addArgs(args);
    }

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    buildRelease(b, build_options, embed_wasm);

    const clean_step = b.step("clean", "clean artififacts");
    const rm = b.addSystemCommand(&.{ "rm", "-r", "zig-out", ".zig-cache" });
    clean_step.dependOn(&rm.step);
}

/// Makes a release step
fn buildRelease(
    b: *std.Build,
    build_options: *std.Build.Step.Options,
    embed_wasm: *EmbedWasm,
) void {
    const release_step = b.step("release", "make a release build for all supported targets");
    const release_target = [_]std.Target.Query{
        .{ .os_tag = .macos, .cpu_arch = .aarch64 },
        .{ .os_tag = .macos, .cpu_arch = .x86_64 },
        .{ .os_tag = .linux, .cpu_arch = .aarch64, .abi = .musl },
        .{ .os_tag = .linux, .cpu_arch = .x86_64, .abi = .musl },
        .{ .os_tag = .windows, .cpu_arch = .x86_64 },
        .{ .os_tag = .wasi, .cpu_arch = .wasm32 },
    };

    for (release_target) |target_query| {
        const resolved_target = b.resolveTargetQuery(target_query);
        const t = resolved_target.result;

        const release_mod = b.addModule("monkey", .{
            .target = resolved_target,
            .optimize = .ReleaseFast,
            .root_source_file = b.path("src/main.zig"),
            .strip = true,
        });
        release_mod.addOptions("build_options", build_options);
        release_mod.addOptions("runtime", embed_wasm.options);

        const rel_exe = b.addExecutable(.{
            .name = "monkey",
            .root_module = release_mod,
        });

        const install_release = b.addInstallArtifact(rel_exe, .{
            .dest_dir = .{ .override = .{ .custom = "release" } },
            .dest_sub_path = b.fmt("{s}-{s}-{s}", .{
                @tagName(t.cpu.arch), @tagName(t.os.tag), rel_exe.name,
            }),
        });

        install_release.step.dependOn(&rel_exe.step);
        release_step.dependOn(&install_release.step);
    }
}

fn buildWeb(b: *std.Build) void {
    const web = b.step("web", "build monkey for the web");

    // for running monkey interpreter in web
    const monkey_web_mod = b.addModule("monkey_web", .{
        .root_source_file = b.path("src/main_web.zig"),
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
        }),
        .optimize = .ReleaseSmall,
        .strip = true,
    });
    monkey_web_mod.export_symbol_names = &.{ "alloc", "free", "wasm_evaluate" };

    const wasm_exe = b.addExecutable(.{
        .root_module = monkey_web_mod,
        .name = "monkey_web",
    });

    wasm_exe.entry = .disabled;

    // copy web
    const intall_dir = b.addInstallDirectory(.{
        .source_dir = b.path("web"),
        .install_dir = .{ .custom = "web" },
        .install_subdir = "",
    });

    const install_web = b.addInstallArtifact(wasm_exe, .{});
    install_web.dest_dir = .{ .custom = "web/bin" };

    web.dependOn(&install_web.step);
    web.dependOn(&intall_dir.step);
}

const EmbedWasm = struct {
    step: std.Build.Step,
    options: *std.Build.Step.Options,
    lazy_path: std.Build.LazyPath,

    /// build runner calls this
    fn make(step: *std.Build.Step, _: std.Build.Step.MakeOptions) !void {
        const read_wasm: *EmbedWasm = @fieldParentPtr("step", step);

        const file_path = read_wasm.lazy_path.getPath(step.owner);

        const file = try std.fs.cwd().openFile(file_path, .{});
        defer file.close();

        const stat = try file.stat();

        const content = try file.readToEndAlloc(step.owner.allocator, stat.size);

        read_wasm.options.addOption([]const u8, "bytes", content);
    }
};

/// Create ReadWasm step to read and embed monkey_runtime.wasm
fn createEmbedWasmStep(
    owner: *std.Build,
    bin: *std.Build.Step.Compile,
    // lazy_path: std.Build.LazyPath,
) !*EmbedWasm {
    const install_runtime = owner.addInstallArtifact(bin, .{});
    const path_to_wasm = bin.getEmittedBin();

    const read_wasm = try owner.allocator.create(EmbedWasm);

    const step = std.Build.Step.init(.{
        .name = "embed wasm",
        .id = .custom,
        .owner = owner,
        .makeFn = EmbedWasm.make,
    });

    const options = owner.addOptions();
    options.step.name = "embed wasm options";

    read_wasm.* = EmbedWasm{
        .step = step,
        .options = options,
        .lazy_path = path_to_wasm,
    };

    read_wasm.step.dependOn(&install_runtime.step);
    //makes sure that ReadWasm.make run before
    options.step.dependOn(&read_wasm.step);

    return read_wasm;
}
