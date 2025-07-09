const std = @import("std");
const print = std.debug.print;
const log = std.log;
const expect = std.testing.expect;
const builtin = @import("builtin");
const build_options = @import("build_options");

const Environment = @import("Environment.zig");
const evaluator = @import("evaluator.zig");
const Lexer = @import("Lexer.zig");
const object = @import("object.zig");
const Parser = @import("Parser.zig");
const repl = @import("repl.zig");
const compile = @import("compiler.zig").compile;

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

    for (args, 0..) |arg, i| {
        log.debug("arg {} = {s}\n", .{ i, arg });
    }

    const stdout = std.io.getStdOut().writer();

    if (args.len == 1) {
        try stdout.print("Hello! This is the monkey programming language!\n", .{});
        try stdout.print("{s}\n", .{monkey});
        try stdout.print("Feel free to type in commands\n", .{});
        try stdout.print("You can exit any time by CTRL-C or typing typing in command exit\n", .{});

        const stdin = std.io.getStdIn().reader();
        const stderr = std.io.getStdErr().writer();

        try repl.start(gpa, stdin.any(), stdout.any(), stderr.any());
    } else if (args.len == 3) {
        const path = args[2];

        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();

        const input = try file.readToEndAlloc(gpa, 1024);
        defer gpa.free(input);

        const action = args[1];

        var parser = Parser.init(gpa, input);
        defer parser.deinit(gpa);

        var program = try parser.Program(gpa);
        defer program.deinit(gpa);

        if (std.mem.eql(u8, action, "run")) {
            var env: Environment = .empty;
            defer env.deinit(gpa);

            const maybe_evaluated = try evaluator.eval(gpa, &program, &env);

            if (maybe_evaluated) |evaluated| {
                defer evaluated.deinit(gpa);
                const eval_str = try evaluated.inspect(gpa);
                defer gpa.free(eval_str);
                try stdout.print("{s}\n", .{eval_str});
            }
        } else if (std.mem.eql(u8, action, "build")) {

            // TODO: name out file after input file
            // TODO: put it in a dir monkey-out
            const out_file = try std.fs.cwd().createFile(
                "main.wasm",
                std.fs.File.CreateFlags{ .truncate = true },
            );
            defer out_file.close();

            const file_writer = out_file.writer();

            var buffered_writer = std.io.bufferedWriter(file_writer);
            const bw = buffered_writer.writer();

            try compile(gpa, &program, bw.any());

            try buffered_writer.flush();
        }
        // print("file = {s}\n", .{input});

    } else {
        std.debug.print("n_args={}", .{args.len});
        return error.UnsupportedNumberOfArgs;
    }
}

test "all" {
    // https://ziggit.dev/t/how-do-i-get-zig-build-to-run-all-the-tests/4434
    std.testing.refAllDecls(@This());
}
