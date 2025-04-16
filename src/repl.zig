const std = @import("std");
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");
const Parser = @import("Parser.zig");
const evaluator = @import("evaluator.zig");
const Environment = @import("Environment.zig");

const Allocator = std.mem.Allocator;
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const print = std.debug.print;

const prompt = ">> ";
const buffer_size = 256;

pub fn start(allocator: Allocator) !void {
    var buffer: [buffer_size]u8 = undefined;

    var env = try Environment.init(allocator);
    defer env.deinit();

    while (true) {
        try stdout.print("{s}", .{prompt});

        const line = try stdin.readUntilDelimiter(&buffer, '\n'); // TODO: handle error better

        if (isExit(line)) break;


        var parser = Parser.init(allocator, line);
        defer parser.deinit(allocator);

        var program = try parser.Program(allocator);
        defer program.deinit(allocator);

        const prog_str = try program.String(allocator);
        defer allocator.free(prog_str);

        // for (parser.errors.items) |err| {
        //     std.debug.print("monkey_parse_err: {s}\n", .{err});
        // }

        std.debug.print("Program: {s}\n", .{prog_str});

        const maybe_evaluated = evaluator.eval(allocator, &program, &env) catch |err| switch (err) {
            evaluator.EvalError.EvalIdentNonExistent => {
                try stdout.print("Error: Couldnt find variable or function\n", .{});
                continue;
            },
            else => return err,
        };

        if (maybe_evaluated) |evaluated| {
            defer evaluated.deinit(allocator);
            const eval_str = try evaluated.inspect(allocator);
            defer allocator.free(eval_str);
            try stdout.print("evaluated: {s}\n", .{eval_str});
        }
        //
        // var tok = lex.NextToken();
        // while (tok.kind != Token.Kind.Eof) : (tok = lex.NextToken()) {
        //     std.debug.print("Token: {any}, {s}\n", .{tok.kind, tok.tokenLiteral()});
        // }
    }
}

fn isExit(line: []const u8) bool {
    const exit = "exit";
    // std.debug.print("{s}", .{line});
    return std.mem.eql(u8, line, exit);
}

// TODO: test repl by passing a writer and reader
