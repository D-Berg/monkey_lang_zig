const std = @import("std");
const builtin = @import("builtin");
// https://www.openmymind.net/Using-A-Custom-Test-Runner-In-Zig/

pub const log_level: std.log.Level = .err;

pub fn main() !void {
    const out = std.io.getStdOut().writer();

    
    var n_tests: u32= 0;
    var passed_tests: u32 = 0;
    var n_leaks: u32 = 0;
    const passed = "\u{001b}[32mpassed\u{001b}[0m";
    const failed = "\u{001b}[31mfailed\u{001b}[0m";
    // TODO: take arg if leaks should be checked
    // const leaked = true;

    for (builtin.test_functions, 0..) |t, i| {
        if (i == 0) continue;

        const name = extractName(t);
        const file = extractFile(t);

        std.testing.allocator_instance = .{};
        const result = t.func();
        const leaked = std.testing.allocator_instance.detectLeaks();

        if (leaked) n_leaks += 1;
        
        if (result) {
            passed_tests += 1;
            try std.fmt.format(out, "{s:<10} | {s:<20} | {s} | leaked: {}\n", .{
                file, name, passed, leaked
            });
        } else |err| {
            try std.fmt.format(out, "{s:<10} | {s:<20} | {s} | leaked: {} | error: {}\n", .{
                file, name, failed, leaked, err
            });
        }

        n_tests += 1;
    }

    try std.fmt.format(out, "passing: {}/{}, leaks: {}/{}\n", .{
        passed_tests, n_tests, n_leaks, n_tests
    });

}

fn extractFile(t: std.builtin.TestFn) []const u8 {
    const marker = std.mem.lastIndexOf(u8, t.name, ".test.") orelse return t.name;
    return t.name[0..marker];

}

fn extractName(t: std.builtin.TestFn) []const u8 {
    const marker = std.mem.lastIndexOf(u8, t.name, ".test.") orelse return t.name;
    return t.name[marker+6..];
}
