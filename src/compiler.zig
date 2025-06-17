//! Compiles Monkey code to wasm

const std = @import("std");
const wasm = @import("wasm/wasm.zig");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayListUnmanaged;
const EnumArray = std.enums.EnumArray;
// const runtime: []const u8 = @import("monkey_runtime");
const Program = @import("ast.zig").Program;
const runtime = @import("build_options").runtime;
const assert = std.debug.assert;
// https://webassembly.github.io/spec/core/appendix/index-instructions.html

const MAX_SECTIONS = @typeInfo(wasm.Section.ID).@"enum".fields.len;

// TODO: make it work for signed

// std.mem.Allocator()

const TypeSection = struct {};

pub fn compile(gpa: Allocator, program: *Program) !void {
    _ = program;

    var module: wasm.Module = .init;
    defer module.deinit(gpa);

    module.addSection(.type);
    module.addSection(.function);
    module.addSection(.code);
    module.addSection(.@"export");

    // __monkey_main
    try module.addFunction(gpa, .{
        .param_types = &.{},
        .return_types = &.{.i32},
        .body = &.{
            .@"i32.const",
            @enumFromInt(10),
            .@"local.set",
            @enumFromInt(0),
            .@"local.get",
            @enumFromInt(0),
            .@"return",
            .end,
        },
        .@"export" = true,
        .name = "__monkey_main",
    });

    const out_file = try std.fs.cwd().createFile(
        "main.wasm",
        std.fs.File.CreateFlags{ .truncate = true },
    );
    defer out_file.close();

    try out_file.chmod(0o0755);

    const file_writer = out_file.writer();

    var buffered_writer = std.io.bufferedWriter(file_writer);
    const bw = buffered_writer.writer();

    try module.write(gpa, bw.any());

    try buffered_writer.flush();
}

test "print magic" {
    std.debug.print("{s}\n", .{wasm.MAGIC_MODULE_HEADER[0..]});
}

// test "write simple wasm_file" {
//     const allocator = std.testing.allocator;
//
//     try compile(allocator, "3 + 4;");
// }
