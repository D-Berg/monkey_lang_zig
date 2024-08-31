const std = @import("std");
const Token = @import("token.zig").Token;
const lexer = @import("lexer.zig");
const repl = @import("repl.zig");

const print = std.debug.print;
const expect = std.testing.expect;

const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();

pub fn main() !void {

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    // const allocator = gpa.allocator();

    try stdout.print("Hello! This is the monkey programming language!\n", .{});
    try stdout.print("Feel free to type in commands\n", .{});
    try stdout.print("You can exit any time by CTRL-C or typing typing in command exit\n", .{});

    try repl.start();


}




