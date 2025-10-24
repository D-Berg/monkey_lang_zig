const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");

const print = std.debug.print;
const log = std.log;
const expect = std.testing.expect;
const Allocator = std.mem.Allocator;
const cli_args = @import("cli_args.zig");
const compile = @import("compiler.zig").compile;
const Environment = @import("Environment.zig");
const evaluator = @import("evaluator.zig");
const Lexer = @import("Lexer.zig");
const object = @import("object.zig");
const Parser = @import("Parser.zig");
const repl = @import("repl.zig");

const trace = @import("tracy.zig").trace;

pub const std_options: std.Options = .{
    .log_level = @enumFromInt(@intFromEnum(build_options.log_level)),
};

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

const monkey =
    \\ .--.  .-"   "-.  .--.
    \\/..  \/ .-. .-. \/..  \ 
    \\| | '| /   Y   \ |'  | |
    \\| \ \  \ 0 | 0 / /   / |
    \\\ '-,\.-"""""""-./,-' /
    \\ ''-'/ _  ^ ^  _ \'-''
    \\    |   \._ _./   |
    \\    \    \'~'/    /
    \\     '._ '-=-' _.'
    \\        '-----'
;

pub fn main() !void {
    if (build_options.enable_tracy) std.Thread.sleep(1 * std.time.ns_per_s);
    defer if (build_options.enable_tracy) std.Thread.sleep(1 * std.time.ns_per_s);
    const tracy = trace(@src());
    defer tracy.end();

    const gpa, const is_debug = gpa: {
        break :gpa switch (builtin.mode) {
            // debug_allocator uses wasm_allocator under the hood
            .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
            .ReleaseFast, .ReleaseSmall => .{
                if (builtin.os.tag == .wasi)
                    std.heap.wasm_allocator
                else
                    std.heap.smp_allocator,
                false,
            },
        };
    };
    defer if (is_debug) {
        const check = debug_allocator.deinit();

        switch (check) {
            .ok => log.debug("no leaks", .{}),
            .leak => log.err("leaked", .{}),
        }
    };

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    var stdin_buffer: [1024]u8 = undefined;
    var stdout_buffer: [1024]u8 = undefined;
    var stderr_buffer: [1024]u8 = undefined;

    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);

    const stdin = &stdin_reader.interface;
    const stdout = &stdout_writer.interface;
    const stderr = &stderr_writer.interface;

    const parsed_args = cli_args.parse(gpa, args[1..]);
    switch (parsed_args) {
        .repl => try monkeyRepl(gpa, stdout, stdin, stderr),
        .run => |run_args| try monkeyRun(gpa, run_args, stdout, stderr),
        .build => |build_args| try monkeyBuild(gpa, build_args, stdout, stderr),
        .problem => |err_msg| {
            try stderr.print("Error: {s}\n", .{err_msg});
        },
        else => {},
    }
}

fn monkeyRepl(
    gpa: Allocator,
    stdout: *std.Io.Writer,
    stdin: *std.Io.Reader,
    stderr: *std.Io.Writer,
) !void {
    try stdout.print("Hello! This is the monkey programming language!\n", .{});
    try stdout.print("{s}\n", .{monkey});
    try stdout.print("Feel free to type in commands\n", .{});
    try stdout.print("You can exit any time by CTRL-C or typing typing in command exit\n", .{});
    try stdout.flush();

    try repl.start(gpa, stdin, stdout, stderr);
}

fn monkeyRun(
    gpa: Allocator,
    run_args: cli_args.RunArgs,
    out: *std.Io.Writer,
    out_err: *std.Io.Writer,
) !void {
    _ = out_err;
    const tracy = trace(@src());
    defer tracy.end();
    const input = input: {
        const file = try std.fs.cwd().openFile(run_args.path, .{});
        defer file.close();

        var in_buf: [1024]u8 = undefined;
        var in = file.reader(&in_buf);

        break :input try in.interface.allocRemaining(gpa, .limited(1024));
    };
    defer gpa.free(input);

    var parser = Parser.init(input);
    defer parser.deinit(gpa);

    var program = try parser.Program(gpa);
    defer program.deinit(gpa);

    var env: Environment = .empty;
    defer env.deinit(gpa);

    const evaluated = try evaluator.eval(gpa, &program, &env);
    defer evaluated.deinit(gpa);

    try out.print("{f}\n", .{evaluated});
    try out.flush();
}

fn monkeyBuild(
    gpa: Allocator,
    build_args: cli_args.BuildArgs,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !void {
    _ = stdout;
    const input = input: {
        const file = std.fs.cwd().openFile(build_args.path, .{}) catch |err| {
            try stderr.print("error: Couldnt open file: '{s}'\n", .{build_args.path});
            return err;
        };
        defer file.close();
        break :input try file.readToEndAlloc(gpa, 1024);
    };
    defer gpa.free(input);

    var parser = Parser.init(input);
    defer parser.deinit(gpa);

    var program = try parser.Program(gpa);
    defer program.deinit(gpa);

    // TODO: name out file after input file
    // TODO: put it in a dir monkey-out
    //
    const out_file = try std.fs.cwd().createFile(
        build_args.out_name,
        std.fs.File.CreateFlags{ .truncate = true },
    );
    defer out_file.close();

    var file_writer_buffer: [1024]u8 = undefined;
    var file_writer = out_file.writer(&file_writer_buffer);
    const fw = &file_writer.interface;

    try compile(gpa, &program, fw);

    try fw.flush();
}

test "all" {
    // https://ziggit.dev/t/how-do-i-get-zig-build-to-run-all-the-tests/4434
    std.testing.refAllDecls(@This());
}
