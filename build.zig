const std = @import("std");
const builtin = @import("builtin");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    //
    const log_level = b.option(std.log.Level, "log", "Set log level") orelse std.log.Level.info;
    const strip = b.option(bool, "strip", "strip debug info from binary");

    var options = b.addOptions();
    options.addOption(@TypeOf(log_level), "log_level", log_level);

    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    // This declares intent for the library to be installed into the standard
    // location when the user invokes the "install" step (the default step when
    // running `zig build`).

    const monkey_runtime_mod = b.addModule("monkey_runtime", .{
        .root_source_file = b.path("src/wasm_runtime.zig"),
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .wasi,
        }),
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

    const install_runtime = b.addInstallArtifact(monkey_runtime_exe, .{});
    const path_to_wasm = monkey_runtime_exe.getEmittedBin();

    b.default_step.dependOn(&install_runtime.step);

    const read_wasm = createReadWasm(b, options, path_to_wasm);
    read_wasm.step.dependOn(&install_runtime.step);

    // const files = b.addWriteFiles();
    // const wasm_bytes_path = files.addCopyFile(b.path("zig-out/bin/monkey_runtime.wasm"), "monkey_runtime.wasm");
    //
    // files.step.dependOn(&install.step);

    if (target.result.cpu.arch == .wasm32 and target.result.os.tag == .freestanding) {
        // for running monkey interpreter in web
        const monkey_web_mod = b.addModule("monkey_web", .{
            .root_source_file = b.path("src/main_web.zig"),
            .target = target,
            .optimize = optimize,
            .strip = true,
        });
        monkey_web_mod.export_symbol_names = &.{ "alloc", "free", "wasm_evaluate" };

        const wasm_exe = b.addExecutable(.{
            .root_module = monkey_web_mod,
            .name = "monkey_web",
        });

        wasm_exe.entry = .disabled;
        // wasm_exe.entry = .{ .symbol_name = "web_main" };
        b.installArtifact(wasm_exe);
    } else {
        const monkey_mod = b.addModule("monkey", .{
            .target = target,
            .optimize = optimize,
            .root_source_file = b.path("src/main.zig"),
            .strip = strip,
        });

        const wasm_mod = b.addModule("wasm", .{
            .target = target,
            .optimize = optimize,
            .root_source_file = b.path("src/wasm/wasm.zig"),
        });

        // monkey_mod.addAnonymousImport("monkey_runtime", .{ .root_source_file = wasm_bytes_path });

        monkey_mod.addOptions("build_options", options);
        wasm_mod.addOptions("build_options", options);

        const exe = b.addExecutable(.{
            .name = "monkey",
            .root_module = monkey_mod,
        });
        exe.step.dependOn(&monkey_runtime_exe.step);

        b.installArtifact(exe);

        const run_cmd = b.addRunArtifact(exe);

        // By making the run step depend on the install step, it will be run from the
        // installation directory rather than directly from within the cache directory.
        // This is not necessary, however, if the application depends on other installed
        // files, this ensures they will be present and in the expected location.
        run_cmd.step.dependOn(b.getInstallStep());

        // This allows the user to pass arguments to the application in the build
        // command itself, like this: `zig build run -- arg1 arg2 etc`
        if (b.args) |args| {
            run_cmd.addArgs(args);
        }

        // This creates a build step. It will be visible in the `zig build --help` menu,
        // and can be selected like this: `zig build run`
        // This will evaluate the `run` step rather than the default, which is "install".
        const run_step = b.step("run", "Run the app");
        run_step.dependOn(&run_cmd.step);

        const exe_unit_tests = b.addTest(.{
            .root_module = monkey_mod,
            .test_runner = .{ .path = b.path("test_runner.zig"), .mode = .simple },
        });

        const exe_wasm_unit_tests = b.addTest(.{
            .root_module = wasm_mod,
        });

        const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
        const run_exe_wasm_unit_tests = b.addRunArtifact(exe_wasm_unit_tests);

        // Similar to creating the run step earlier, this exposes a `test` step to
        // the `zig build --help` menu, providing a way for the user to request
        // running the unit tests.
        const test_step = b.step("test", "Run unit tests");
        const wasm_test_step = b.step("test-wasm", "Run unit tests for wasm mod");
        test_step.dependOn(&run_exe_unit_tests.step);
        wasm_test_step.dependOn(&run_exe_wasm_unit_tests.step);
    }
}

const ReadWasm = struct {
    step: std.Build.Step,
    options: *std.Build.Step.Options,
    lazy_path: std.Build.LazyPath,

    fn make(step: *std.Build.Step, make_options: std.Build.Step.MakeOptions) !void {
        _ = make_options;
        const read_wasm: *ReadWasm = @fieldParentPtr("step", step);

        const file = std.fs.cwd().openFile(read_wasm.lazy_path.getPath(step.owner), .{}) catch return step.fail(
            "couldnt open file",
            .{},
        );
        defer file.close();

        const stat = try file.stat();

        const content = try file.readToEndAlloc(step.owner.allocator, stat.size);

        read_wasm.options.addOption([]const u8, "runtime", content);
    }
};

fn createReadWasm(owner: *std.Build, options: *std.Build.Step.Options, lazy_path: std.Build.LazyPath) *ReadWasm {
    const read_wasm = owner.allocator.create(ReadWasm) catch @panic("OOM");

    const step = std.Build.Step.init(.{
        .name = "READ WAAASM",
        .id = .custom,
        .owner = owner,
        .makeFn = ReadWasm.make,
    });

    read_wasm.* = ReadWasm{
        .step = step,
        .options = options,
        .lazy_path = lazy_path,
    };

    // step2 runs before step1.
    // step1.dependOn(&step2.step);
    options.step.dependOn(&read_wasm.step);

    return read_wasm;
}
