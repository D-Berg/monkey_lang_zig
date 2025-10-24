const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");
const Parser = @import("Parser.zig");
const evaluator = @import("evaluator.zig");
const Environment = @import("Environment.zig");

const trace = @import("tracy.zig").trace;

const ParseError = Parser.ParseError;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const print = std.debug.print;

const AnyReader = std.io.AnyReader;
const AnyWriter = std.io.AnyWriter;

const prompt = ">> ";
const buffer_size = 256;

// TODO: save lines in a list
pub fn start(
    gpa: Allocator,
    in: *std.Io.Reader,
    out: *std.Io.Writer,
    err_out: *std.Io.Writer,
) !void {
    var env: Environment = .empty;
    defer env.deinit(gpa);

    while (true) {
        const tracy = trace(@src());
        defer tracy.end();
        try out.print("{s}", .{prompt});
        try out.flush();

        var buffer: [258]u8 = undefined;
        var buf_writer = std.Io.Writer.fixed(&buffer);
        const line_len = try in.streamDelimiter(&buf_writer, '\n');
        in.toss(1); // remove '\n'

        const line = buffer[0..line_len];

        if (std.mem.eql(u8, "exit", line)) break;

        var parser = Parser.init(line);
        defer parser.deinit(gpa);

        var program = parser.Program(gpa) catch |err| switch (err) {
            ParseError.NoSpaceLeft, ParseError.OutOfMemory => |e| {
                return e;
            },
            else => {
                for (parser.errors.items) |monkey_err| {
                    try err_out.print("Parse Error: {s}\n", .{monkey_err.msg});
                }
                try err_out.flush();
                continue;
            },
        };
        defer program.deinit(gpa);

        const prog_str = try program.String(gpa);
        defer gpa.free(prog_str);

        // std.debug.print("Program: {s}\n", .{prog_str});

        switch (try evaluator.eval(gpa, &program, &env)) {
            .nullable => if (builtin.mode == .Debug) std.debug.print("null\n", .{}),
            else => |evaluated| {
                defer evaluated.deinit(gpa);
                try out.print("{f}\n", .{evaluated});
                try out.flush();
            },
        }

        if (std.options.log_level == .debug) env.print();
        //
        // var tok = lex.NextToken();
        // while (tok.kind != Token.Kind.Eof) : (tok = lex.NextToken()) {
        //     std.debug.print("Token: {any}, {s}\n", .{tok.kind, tok.tokenLiteral()});
        // }

        try out.flush();
    }
}

fn testRepl(gpa: Allocator, input: []const u8) !void {
    var out_buffer: [300]u8 = undefined;
    var in_buffer: [300]u8 = undefined;
    var err_buffer: [300]u8 = undefined;

    var stdout = std.Io.Writer.fixed(out_buffer[0..]);
    var stdin = std.Io.Reader.fixed(in_buffer[0..]);
    var stderr = std.Io.Writer.fixed(err_buffer[0..]);

    @memcpy(in_buffer[0..input.len], input);

    try start(gpa, &stdin, &stdout, &stderr);
}

// TODO: test repl by passing a writer and reader
test "exit" {
    const gpa = std.testing.allocator;

    const input = "let x = 4;\nx\nexit\n";

    try testRepl(gpa, input);
}

test "failing allocator" {
    const gpa = std.testing.failing_allocator;
    const input = "let x = 4;\nx\nexit\n";

    const repl_res = testRepl(gpa, input);
    try std.testing.expectError(Allocator.Error.OutOfMemory, repl_res);
}
