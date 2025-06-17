const std = @import("std");

var wasm_allocator = std.heap.wasm_allocator;

extern fn __monkey_main() i32;

// export fn __allocate() void {
//     std.debug.print("successfully called __allocate\n", .{});
// }

pub fn main() !void {
    std.debug.print("hello world\n", .{});
    const rc = __monkey_main();

    std.debug.print("got {} back from monkey_main", .{rc});
}
