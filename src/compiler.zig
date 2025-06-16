//! Compiles Monkey code to wasm

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayListUnmanaged;
// const runtime: []const u8 = @import("monkey_runtime");
const Program = @import("ast.zig").Program;
const runtime = @import("build_options").runtime;

// https://webassembly.github.io/spec/core/appendix/index-instructions.html
const OpCode = enum(u8) {
    end = 0x0b,
    @"i64.add" = 0x7c,
    @"i64.const" = 0x42,
};

// https://webassembly.github.io/spec/core/binary/modules.html#binary-typeidx
const SectionID = enum(u8) {
    custom = 0,
    type = 1,
    import = 2,
    function = 3,
    table = 4,
    memory = 5,
    global = 6,
    @"export" = 7,
    start = 8,
    element = 9,
    code = 10,
    data = 11,
    data_cound = 12,
};

const Type = enum(u8) {
    i64 = 0x7e,
    function = 0x60,
};
const MAGIC_MODULE_HEADER = [4]u8{ 0x00, 0x61, 0x73, 0x6d };
const MODULE_VERSION = [4]u8{ 0x01, 0x00, 0x00, 0x00 };

pub fn compile(gpa: Allocator, program: *Program) !void {
    _ = program;
    var wasm: ArrayList(u8) = .empty;
    defer wasm.deinit(gpa);
    //
    // try wasm.appendSlice(gpa, MAGIC_MODULE_HEADER[0..]);
    // try wasm.appendSlice(gpa, MODULE_VERSION[0..]);

    try wasm.appendSlice(gpa, runtime[0..]);

    const out_file = try std.fs.cwd().createFile("main.wasm", std.fs.File.CreateFlags{ .truncate = false });
    defer out_file.close();

    try out_file.chmod(0o0755);

    try out_file.writeAll(wasm.items);
}

test "print magic" {
    std.debug.print("{s}\n", .{MAGIC_MODULE_HEADER[0..]});
}

// test "write simple wasm_file" {
//     const allocator = std.testing.allocator;
//
//     try compile(allocator, "3 + 4;");
// }
