const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const object = @import("object.zig");
const Program = @import("ast.zig").Program;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const expect = std.testing.expect;


pub fn Eval(program: *Program) error{FailedEvaluation}!object.Object {

    var maybe_result: ?object.Object = null;

    for (program.statement_indexes.items) |stmt_idx| {

        maybe_result = EvalNode(program, stmt_idx);

    }

    if (maybe_result) |result| {
        return result;
    } else {
        return error.FailedEvaluation;
    }

}

fn EvalNode(program: *Program, node_idx: usize) ?object.Object {

    const node = program.nodes.items[node_idx];

    
    switch (node) {
        .statement => |stmt| {
            switch (stmt) {

                .expr_stmt => |es| {
                    return EvalNode(program, es.expression.?);
                },

                else => {
                    unreachable;
                }

            }
        },

        .expression => |expr| {

            switch (expr) {

                .integer_literal => |int_lit| {

                    return object.Object {
                        .integer = .{ 
                            .value = int_lit.value,
                        }
                    };

                }, 
                .boolean_literal => |bool_lit| {

                    return object.Object {
                        .boolean = .{ 
                            .value = bool_lit.value,
                        }
                    };

                }, 

                else => {
                    unreachable;
                }

            }

        }

    }

}


fn testEval(allocator: Allocator, input: []const u8) !object.Object {
    
    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    return try Eval(&program);

}

test "Eval Int expr" {
    const allocator = std.testing.allocator;

    const inputs = [_][]const u8{ "5", "10" };
    const answers = [_]u32{ 5, 10 };

    for (inputs, answers) |inp, ans| {

        const evaluated = try testEval(allocator, inp);

        try expect(evaluated.integer.value == ans);
    }

}

test "Eval bool expr" {
    const allocator = std.testing.allocator;

    const inputs = [_][]const u8{ "true", "false" };
    const answers = [_]bool{ true, false };

    for (inputs, answers) |inp, ans| {

        const evaluated = try testEval(allocator, inp);

        try expect(evaluated.boolean.value == ans);
    }

}



