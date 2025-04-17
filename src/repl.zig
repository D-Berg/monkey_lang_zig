const std = @import("std");
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");
const Parser = @import("Parser.zig");
const evaluator = @import("evaluator.zig");
const Environment = @import("Environment.zig");

const ParseError = Parser.ParseError;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const print = std.debug.print;

const AnyReader = std.io.AnyReader;
const AnyWriter = std.io.AnyWriter;

const prompt = ">> ";
const buffer_size = 256;


pub fn start(allocator: Allocator, reader: AnyReader, writer: AnyWriter, err_writer: AnyWriter) !void {

    var env = try Environment.init(allocator);
    defer env.deinit();

    var buffer: [2048]u8 = undefined;
    var fixed_buf_allocator: std.heap.FixedBufferAllocator = .init(buffer[0..]);
    
    const fba = fixed_buf_allocator.allocator();

    while (true) {
        try writer.print("{s}", .{prompt});

        var line_array = ArrayList(u8).init(fba);
        try reader.streamUntilDelimiter(line_array.writer(), '\n', null);

        const line = try line_array.toOwnedSlice();  // TODO: handle error better
        defer fba.free(line);

        if (isExit(line)) break;


        var parser = Parser.init(allocator, line);
        defer parser.deinit(allocator);

        var program = parser.Program(allocator) catch |err| switch (err) {
            ParseError.NoSpaceLeft,
            ParseError.OutOfMemory => |e| {
                return e;
            },
            else => {
                for (parser.errors.items) |monkey_err| {
                    try err_writer.print("Parse Error: {s}\n", .{monkey_err.msg});
                }
                continue;
            }
        };
        defer program.deinit(allocator);

        const prog_str = try program.String(allocator);
        defer allocator.free(prog_str);


        // std.debug.print("Program: {s}\n", .{prog_str});

        const maybe_evaluated = evaluator.eval(allocator, &program, &env) catch |err| switch (err) {
            evaluator.EvalError.EvalIdentNonExistent => {
                try writer.print("Error: Couldnt find variable or function\n", .{});
                continue;
            },
            else => return err,
        };

        if (maybe_evaluated) |evaluated| {
            defer evaluated.deinit(allocator);
            const eval_str = try evaluated.inspect(allocator);
            defer allocator.free(eval_str);
            try writer.print("{s}\n", .{eval_str});
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
test "exit" {
    const allocator = std.testing.allocator;

    var out_buffer: [300]u8 = undefined;
    var in_buffer: [300]u8 = undefined;
    var err_buffer: [300]u8 = undefined;

    var stdout = std.io.fixedBufferStream(out_buffer[0..]);
    var stdin = std.io.fixedBufferStream(in_buffer[0..]);
    var stderr = std.io.fixedBufferStream(err_buffer[0..]);

    _ = try stdin.write("let x = 4;\nx\nexit\n");

    stdin.reset();

    try start(
        allocator, 
        stdin.reader().any(), 
        stdout.writer().any(), 
        stderr.writer().any()
    );

}


test "failing allocator" {
    const allocator = std.testing.failing_allocator;

    var out_buffer: [300]u8 = undefined;
    var in_buffer: [300]u8 = undefined;
    var err_buffer: [300]u8 = undefined;

    var stdout = std.io.fixedBufferStream(out_buffer[0..]);
    var stdin = std.io.fixedBufferStream(in_buffer[0..]);
    var stderr = std.io.fixedBufferStream(err_buffer[0..]);

    _ = try stdin.write("let x = 4;\nx\nexit\n");

    stdin.reset();

    const err =  start(
        allocator, 
        stdin.reader().any(), 
        stdout.writer().any(), 
        stderr.writer().any()
    );

    try std.testing.expectError(Allocator.Error.OutOfMemory, err);

}
