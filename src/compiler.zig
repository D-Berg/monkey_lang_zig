//! Compiles Monkey code to wasm

const std = @import("std");
const wasm = @import("wasm.zig");
const compiler = @This();
const Parser = @import("Parser.zig");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayListUnmanaged;
const EnumArray = std.enums.EnumArray;
const ast = @import("ast.zig");
const Program = ast.Program;
const runtime = @import("runtime").bytes;
const assert = std.debug.assert;
// https://webassembly.github.io/spec/core/appendix/index-instructions.html

const MAX_SECTIONS = @typeInfo(wasm.Section.ID).@"enum".fields.len;

// TODO: make it work for signed

// std.mem.Allocator()

const TypeSection = struct {};

pub const Error = error{
    InvalidInfixOperator,
    InvalidPrefixOperator,
} || Allocator.Error;

pub fn compile(gpa: Allocator, program: *Program, writer: *std.Io.Writer) !void {
    var module: wasm.Module = .init;
    defer module.deinit(gpa);

    module.addSection(.type);
    module.addSection(.function);
    module.addSection(.code);
    module.addSection(.@"export");

    var current_func: wasm.Function = .init;
    defer current_func.deinit(gpa);

    current_func.name = "__monkey_main";
    current_func.param_types = &.{};
    current_func.return_types = &.{.i32};
    current_func.@"export" = true;

    // wasm.Function{
    //
    //
    //     .param_types = &.{},
    //     .return_types = &.{.i32},
    //     .body = &.{
    //         .@"i32.const",
    //         @enumFromInt(5), // TODO: need to encode as i32
    //         .@"return",
    //         .end,
    //     },
    //     .@"export" = true,
    //     .name = "__monkey_main",
    // };
    module.current_function = &current_func;

    try compileStatements(gpa, program.statements, &module);

    try current_func.body.append(gpa, .@"return");
    try current_func.body.append(gpa, .end);

    // __monkey_main
    try module.addFunction(
        gpa,
        current_func,
    );
    try module.write(gpa, writer);
}

fn compileStatements(
    gpa: Allocator,
    statements: []const ast.Statement,
    module: *wasm.Module,
) !void {
    for (statements) |stmt| {
        switch (stmt) {
            .expr_stmt => |ex| {
                try compileExpression(gpa, ex.expression, module);
                // try current_func.body.append(gpa, .drop);
            },
            .let_stmt => {},
            else => {},
        }
    }
}

fn compileLetStatement(
    gpa: Allocator,
    let_stmt: *const ast.LetStatement,
    module: *wasm.Module,
) !void {
    _ = gpa;
    _ = let_stmt;
    _ = module;
}

fn compileExpression(
    gpa: Allocator,
    expr: *const ast.Expression,
    module: *wasm.Module,
) compiler.Error!void {
    switch (expr.*) {
        .integer_literal => |*il| try compileIntegerLiteralExpression(gpa, il, module),
        .prefix_expression => |*pe| try compilePrefixExpression(gpa, pe, module),
        .infix_expression => |*ie| try compileInfixExpression(gpa, ie, module),
        .boolean_literal => |*bl| try compileBooleanLiteralExpression(gpa, bl, module),
        .if_expression => |*ie| try compileIfExpression(gpa, ie, module),
        .string_expression => |*se| try compileStringExpression(gpa, se, module),
        else => {},
    }
}

fn compileIntegerLiteralExpression(
    gpa: Allocator,
    int_lit: *const ast.IntegerLiteralExpression,
    module: *wasm.Module,
) !void {
    var encoder = wasm.LEB128Encoder(i32).init;
    try module.current_function.body.append(gpa, .@"i32.const");
    try module.current_function.body.appendSlice(
        gpa,
        @ptrCast(encoder.encode(@intCast(int_lit.value))),
    );
}

fn compileBooleanLiteralExpression(
    gpa: Allocator,
    bool_lit: *const ast.BooleanLiteralExpression,
    module: *wasm.Module,
) !void {
    try module.current_function.body.append(gpa, .@"i32.const");

    var encoder = wasm.LEB128Encoder(i32).init;
    if (bool_lit.value) {
        try module.current_function.body.appendSlice(gpa, @ptrCast(encoder.encode(1)));
    } else {
        try module.current_function.body.appendSlice(gpa, @ptrCast(encoder.encode(0)));
    }
}

fn compilePrefixExpression(
    gpa: Allocator,
    prefix_expr: *const ast.PrefixExpression,
    module: *wasm.Module,
) !void {
    switch (prefix_expr.token.kind) {
        .Bang => {
            try compileExpression(gpa, prefix_expr.right, module);
            try module.current_function.body.append(gpa, .@"i32.eqz");
        },
        .Minus => {
            try module.current_function.body.appendSlice(gpa, &.{
                .@"i32.const",
                @enumFromInt(0),
            });
            try compileExpression(gpa, prefix_expr.right, module);
            try module.current_function.body.append(gpa, .@"i32.sub");
        },
        else => return Error.InvalidPrefixOperator,
    }
}

fn compileInfixExpression(
    gpa: Allocator,
    infix_expr: *const ast.InfixExpression,
    module: *wasm.Module,
) !void {
    try compileExpression(gpa, infix_expr.left, module);
    try compileExpression(gpa, infix_expr.right, module);

    switch (infix_expr.token.kind) {
        .Plus => try module.current_function.body.append(gpa, .@"i32.add"),
        .Minus => try module.current_function.body.append(gpa, .@"i32.sub"),
        .Asterisk => try module.current_function.body.append(gpa, .@"i32.mul"),
        .Slash => try module.current_function.body.append(gpa, .@"i32.div_s"),
        .Eq => try module.current_function.body.append(gpa, .@"i32.eq"),
        .Gt => try module.current_function.body.append(gpa, .@"i32.gt_s"),
        .Lt => try module.current_function.body.append(gpa, .@"i32.lt_s"),
        else => return Error.InvalidInfixOperator,
    }
}

fn compileIfExpression(
    gpa: Allocator,
    if_expr: *const ast.IfExpression,
    module: *wasm.Module,
) !void {
    try compileExpression(gpa, if_expr.condition, module);

    try module.current_function.body.appendSlice(gpa, &.{ .@"if", wasm.ValType.i32.opcode() });

    try compileStatements(gpa, if_expr.consequence.statements, module);

    if (if_expr.alternative) |alt| {
        try module.current_function.body.append(gpa, .@"else");

        try compileStatements(gpa, alt.statements, module);
    }

    try module.current_function.body.append(gpa, .end);
}

fn compileStringExpression(
    gpa: Allocator,
    string_expr: *const ast.StringExpression,
    module: *wasm.Module,
) !void {
    _ = gpa;
    _ = string_expr;
    _ = module;
}
//
// fn testCompiler(
//     gpa: Allocator,
//     test_name: []const u8,
//     input: []const u8,
// ) ![]const u8 {
//     var parser = Parser.init(gpa, input);
//     defer parser.deinit(gpa);
//
//     var program = try parser.Program(gpa);
//     defer program.deinit(gpa);
//
//     const cwd = try std.process.getCwdAlloc(gpa);
//     defer gpa.free(cwd);
//
//     if (std.fs.cwd().openDir("tests", .{})) |dir| {
//         var opened_dir = dir;
//         defer opened_dir.close();
//     } else |_| {
//         try std.fs.cwd().makeDir("tests");
//     }
//     const file_path = try std.fmt.allocPrint(gpa, "tests/{s}.wasm", .{test_name});
//     defer gpa.free(file_path);
//
//     {
//         const file = try std.fs.cwd().createFile(file_path, .{});
//         defer file.close();
//
//         try compile(gpa, &program, file.writer().any());
//     }
//
//     var wasm2wat = std.process.Child.init(&.{ "wasm2wat", file_path }, gpa);
//     wasm2wat.stdout_behavior = .Pipe;
//
//     if (wasm2wat.spawnAndWait()) |term| {
//         try std.testing.expect(term.Exited == 0);
//     } else |err| return err;
//
//     var wasmtime = std.process.Child.init(&.{ "wasmtime", "--invoke", "__monkey_main", file_path }, gpa);
//
//     wasmtime.stdout_behavior = .Pipe;
//     wasmtime.stderr_behavior = .Pipe;
//
//     try wasmtime.spawn();
//
//     var result_str = ArrayList(u8).empty;
//     errdefer result_str.deinit(gpa);
//
//     const wasmtime_stdout = wasmtime.stdout.?.reader();
//
//     try wasmtime_stdout.streamUntilDelimiter(result_str.writer(gpa).any(), '\n', 100);
//
//     if (wasmtime.wait()) |term| {
//         try std.testing.expect(term.Exited == 0);
//         return result_str.toOwnedSlice(gpa);
//     } else |_| return error.CompilationFailed;
// }
//
// test "integer_literal" {
//     const gpa = std.testing.allocator;
//
//     const result = try testCompiler(gpa, "integer_literal", "5;3;");
//     defer gpa.free(result);
//
//     try std.testing.expectEqualStrings("3", result);
// }
//
// test "infix_expr" {
//     const gpa = std.testing.allocator;
//
//     const result = try testCompiler(gpa, "infix_expr", "4 * 5;");
//     defer gpa.free(result);
//
//     try std.testing.expectEqualStrings("20", result);
// }
//
// test "bool" {
//     const gpa = std.testing.allocator;
//
//     const result = try testCompiler(gpa, "bool", "3 == 3");
//     defer gpa.free(result);
//
//     try std.testing.expectEqualStrings("1", result);
// }
//
// test "if" {
//     const gpa = std.testing.allocator;
//
//     const input =
//         \\if (4 > 2) {
//         \\  10
//         \\} else {
//         \\  20
//         \\}
//     ;
//
//     const result = try testCompiler(gpa, "if", input);
//     defer gpa.free(result);
//
//     try std.testing.expectEqualStrings("10", result);
// }
//
// test "prefix" {
//     const gpa = std.testing.allocator;
//
//     const result = try testCompiler(gpa, "prefix", "-(10 - 20)");
//     defer gpa.free(result);
//
//     try std.testing.expectEqualStrings("10", result);
// }
