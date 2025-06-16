const std = @import("std");

var wasm_allocator = std.heap.wasm_allocator;

extern fn monkey_main() i32;

export fn __allocate() void {
    std.debug.print("successfully called __allocate\n", .{});
}

pub fn main() !void {
    std.debug.print("hello world\n", .{});
    const rc = monkey_main();

    std.debug.print("got {} back from monkey_main", .{rc});
}
