const std = @import("std");
const builtin = @import("builtin");
// https://www.openmymind.net/Using-A-Custom-Test-Runner-In-Zig/

// set log level for tests
pub const std_options: std.Options = .{
    .log_level = .warn,
};

pub fn main() !void {
    var arg_it = std.process.ArgIterator.init();
    defer arg_it.deinit();

    var arena_alloc = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_alloc.deinit();

    const arena = arena_alloc.allocator();

    const args = try std.process.argsAlloc(arena);

    var hm: std.StringHashMapUnmanaged([]const u8) = .empty;

    for (args, 0..) |arg, i| {
        if (i == 0) continue;
        try hm.put(arena, arg, arg);
    }

    const stdout = std.io.getStdOut().writer();

    var n_tests: u32 = 0;
    var passed_tests: u32 = 0;
    var n_leaks: u32 = 0;
    const passed = "\u{001b}[32mpassed\u{001b}[0m";
    const failed = "\u{001b}[31mfailed\u{001b}[0m";

    var did_fail = false; // some test failed

    for (builtin.test_functions, 0..) |t, i| {
        if (i == 0) continue;

        const name = extractName(t);
        const file = extractFile(t);

        // filter tests
        if (args.len > 1) {
            const root = extractRoot(file);
            if (hm.get(root) == null) continue;
        }

        std.testing.allocator_instance = .{};
        const result = t.func();
        const leaked = std.testing.allocator_instance.detectLeaks();

        if (leaked) n_leaks += 1;

        try stdout.print("[{d:>3}/{d:<3}] ", .{ n_tests + 1, builtin.test_functions.len - 1 });
        if (result) {
            passed_tests += 1;
            try std.fmt.format(stdout, "{s:<20} | {s:<20} | {s} | leaked: {}\n", .{
                file,
                name,
                passed,
                leaked,
            });
        } else |err| {
            did_fail = true;
            try std.fmt.format(stdout, "{s:<20} | {s:<20} | {s} | leaked: {} | error: {}\n", .{
                file,
                name,
                failed,
                leaked,
                err,
            });
        }

        n_tests += 1;
    }

    try std.fmt.format(stdout, "passing: {}/{}, leaks: {}/{}\n", .{
        passed_tests,
        n_tests,
        n_leaks,
        n_tests,
    });

    if (did_fail) return error.TestingFailure;
}

fn extractFile(t: std.builtin.TestFn) []const u8 {
    const marker = std.mem.lastIndexOf(u8, t.name, ".test.") orelse return t.name;
    return t.name[0..marker];
}

fn extractName(t: std.builtin.TestFn) []const u8 {
    const marker = std.mem.lastIndexOf(u8, t.name, ".test.") orelse return t.name;
    return t.name[marker + 6 ..];
}

fn extractRoot(file_name: []const u8) []const u8 {
    const marker = std.mem.indexOfScalar(u8, file_name, '.') orelse return file_name;
    return file_name[0..marker];
}
