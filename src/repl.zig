const std = @import("std");
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");

const Allocator = std.mem.Allocator;
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();

const prompt = ">> ";
const buffer_size = 256;

pub fn start(allocator: Allocator) !void {


    var buffer: [buffer_size]u8 = undefined;


    while (true) {

        try stdout.print("{s}", .{prompt});

        const line = try stdin.readUntilDelimiter(&buffer,'\n'); // TODO: handle error better
        
        if (isExit(line)) break;

        var lex = try Lexer.init(allocator, line);
        defer lex.deinit();

        var tok = try lex.NextToken();
        
        while (tok.kind != Token.Kind.Eof) : (tok = try lex.NextToken()) {
            std.debug.print("Token: {any}, {s}\n", .{tok.kind, tok.literal});
        }
    }

}


fn isExit(line: []const u8) bool {

    const exit = "exit";
    // std.debug.print("{s}", .{line});
    return std.mem.eql(u8, line, exit);

}
