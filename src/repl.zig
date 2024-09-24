const std = @import("std");
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");
const Parser = @import("Parser.zig");
const evaluator = @import("evaluator.zig");
const Environment = @import("object.zig").Environment;

const Allocator = std.mem.Allocator;
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const print = std.debug.print;

const prompt = ">> ";
const buffer_size = 256;

pub fn start(allocator: Allocator) !void {


    var buffer: [buffer_size]u8 = undefined;

    var env = Environment.init(allocator);
    defer env.deinit();

    while (true) {

        try stdout.print("{s}", .{prompt});

        const line = try stdin.readUntilDelimiter(&buffer,'\n'); // TODO: handle error better
        
        if (isExit(line)) break;

        var lex = Lexer.init(allocator, line);
        // defer lex.deinit();

        var parser = try Parser.init(&lex, allocator);
        defer parser.deinit();

        var program = try parser.ParseProgram(allocator);
        defer program.deinit();

        // const prog_str = try program.String();
        // defer allocator.free(prog_str);

        // for (parser.errors.items) |err| {
        //     std.debug.print("monkey_parse_err: {s}\n", .{err});
        // }
        //
        // std.debug.print("Program: {s}\n", .{prog_str});
        //
        const maybe_evaluated = try evaluator.Eval(&program, &env);


        if (maybe_evaluated) |evaluated| {
            defer evaluated.deinit();
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
