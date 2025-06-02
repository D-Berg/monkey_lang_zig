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
    const allocator, const is_debug = gpa: {
        if (builtin.os.tag == .wasi) break :gpa .{ std.heap.wasm_allocator, false };
        break :gpa switch (builtin.mode) {
            .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
            .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
        };
    };
    defer if (is_debug) {
        const check = debug_allocator.deinit();

        switch (check) {
            .ok => log.debug("no leaks", .{}),
            .leak => log.err("leaked", .{}),
        }
    };

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

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

        try repl.start(allocator, stdin.any(), stdout.any(), stderr.any());
    } else {
        if (args.len == 2) {
            const path = args[1];

            const file = try std.fs.cwd().openFile(path, .{});
            defer file.close();

            const input = try file.readToEndAlloc(allocator, 1024);
            defer allocator.free(input);

            // print("file = {s}\n", .{input});

            var env: Environment = .empty;
            defer env.deinit(allocator);

            var parser = Parser.init(allocator, input);
            defer parser.deinit(allocator);

            var program = try parser.Program(allocator);
            defer program.deinit(allocator);

            const maybe_evaluated = try evaluator.eval(allocator, &program, &env);

            if (maybe_evaluated) |evaluated| {
                defer evaluated.deinit(allocator);
                const eval_str = try evaluated.inspect(allocator);
                defer allocator.free(eval_str);
                try stdout.print("{s}\n", .{eval_str});
            }
        } else {
            return error.UnsupportedNumberOfArgs;
        }
    }
}

test "all" {
    // https://ziggit.dev/t/how-do-i-get-zig-build-to-run-all-the-tests/4434
    std.testing.refAllDecls(@This());
}
