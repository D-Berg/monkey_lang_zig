const std = @import("std");
const builtin = @import("builtin");
// https://www.openmymind.net/Using-A-Custom-Test-Runner-In-Zig/
//
pub fn main() !void {
    const out = std.io.getStdOut().writer();

    for (builtin.test_functions) |t| {
        const name = extractName(t);

        t.func() catch |err| {
            try std.fmt.format(out, "{s} fail: {}\n", .{t.name, err});
            continue;
        };
        try std.fmt.format(out, "{s:<20} passed\n", .{name});
    }
}

fn extractName(t: std.builtin.TestFn) []const u8 {
    const marker = std.mem.lastIndexOf(u8, t.name, ".test.") orelse return t.name;

    return t.name[marker+6..];
}
