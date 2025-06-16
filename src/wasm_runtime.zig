const std = @import("std");

var wasm_allocator = std.heap.wasm_allocator;

extern fn monkey_main() void;

pub fn main() !void {
    std.debug.print("hello world", .{});
}
