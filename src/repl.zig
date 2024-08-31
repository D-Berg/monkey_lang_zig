const std = @import("std");
const Allocator = std.mem.Allocator;
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const Lexer = @import("lexer.zig").Lexer;

const prompt = ">> ";
const buffer_size = 256;

pub fn start() !void {


    var buffer: [buffer_size]u8 = undefined;

    try stdout.print("{s}", .{prompt});
    var line  = try stdin.readUntilDelimiter(&buffer,'\n'); // TODO: handle error better
    std.debug.print("{s}\n", .{line});

    while (!isExit(line)) {


        // var lex = Lexer.new(&line);
        // var tok = l.NextToken(allocator: Allocator, key_words: *const Keywords)
        


        try stdout.print("{s}", .{prompt});
        line = try stdin.readUntilDelimiter(&buffer,'\n'); // TODO: handle error better
        std.debug.print("{s}\n", .{line});

    }

}


fn isExit(line: []const u8) bool {

    const exit = "exit";
    // std.debug.print("{s}", .{line});
    return std.mem.eql(u8, line, exit);

}
