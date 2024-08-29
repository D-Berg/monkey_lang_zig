const std = @import("std");
const Allocator = std.mem.Allocator;
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();

const prompt = ">> ";

pub fn start() !void {


    var buffer: [256]u8 = undefined;
    try stdout.print("{s}", .{prompt});
    var line  = try stdin.readUntilDelimiter(&buffer,'\n');
    std.debug.print("{s}\n", .{line});


    while (!isExit(line)) {

        try stdout.print("{s}", .{prompt});
        line = try stdin.readUntilDelimiter(&buffer,'\n');
        std.debug.print("{s}\n", .{line});

    }

}

fn isExit(line: []const u8) bool {

    const exit = "exit";
    // std.debug.print("{s}", .{line});
    return std.mem.eql(u8, line, exit);

}
