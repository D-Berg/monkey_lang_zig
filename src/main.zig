const std = @import("std");
const build_options = @import("build_options");
const repl = @import("repl.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const object = @import("object.zig");
const evaluator = @import("evaluator.zig");
const Environment = @import("Environment.zig");


const print = std.debug.print;
const log = std.log;

pub const std_options: std.Options = .{
    .log_level = @enumFromInt(@intFromEnum(build_options.log_level)),
};

const expect = std.testing.expect;

const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();

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
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    for (args, 0..) |arg, i| {
        log.debug("arg {} = {s}\n", .{ i, arg });
    }

    if (args.len == 1) {
        try stdout.print("Hello! This is the monkey programming language!\n", .{});
        try stdout.print("{s}\n", .{monkey});
        try stdout.print("Feel free to type in commands\n", .{});
        try stdout.print("You can exit any time by CTRL-C or typing typing in command exit\n", .{});

        try repl.start(allocator);
    } else {
        if (args.len == 2) {
            const path = args[1];

            const file = try std.fs.cwd().openFile(path, .{});
            defer file.close();

            const input = try file.readToEndAlloc(allocator, 1024);
            defer allocator.free(input);

            // print("file = {s}\n", .{input});

            var env = try Environment.init(allocator);
            defer env.deinit();

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
