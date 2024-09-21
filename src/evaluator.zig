const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const Token = @import("Token.zig");

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
                            .value = @intCast(int_lit.value),
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
                
                .prefix_expression => |pe| {
                    const right = EvalNode(program, pe.right).?;
                    return evalPrefixExpression(pe.token.kind, &right);
                },

                else => {
                    unreachable;
                }

            }

        }

    }

}


fn evalPrefixExpression(operator: Token.Kind, right: *const object.Object) ?object.Object {
    switch (operator) {
        .Bang => {

            // same as evalBangOperatorExpression()
            switch (right.*) {

                .boolean => |b| {
                    if (b.value) {
                        return object.Object { 
                            .boolean = .{ 
                                .value = false 
                            }
                        };
                    } else {
                        return object.Object {
                            .boolean = .{ 
                                .value = true 
                            }
                        };
                    }
                },

                .nullable => {
                    return object.Object {
                        .boolean = .{
                            .value = true
                        }
                    };
                },
                else => {
                    return object.Object {
                        .boolean = .{ 
                            .value = false 
                        }
                    };
                }

            }

        },

        .Minus => {
            if (right.* != .integer) {
                return null;
            }

            const val = right.integer.value;

            return object.Object {
                .integer = .{ 
                    .value = -val
                }
            };
        },

        else => {
            return null;
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

    const inputs = [_][]const u8{ "5", "10", "-5", "-10" };
    const answers = [_]i32{ 5, 10, -5, -10 };

    for (inputs, answers) |inp, ans| {

        const evaluated = try testEval(allocator, inp);

        expect(evaluated.integer.value == ans) catch |err| {
            print("Expected {}, got {}\n", .{ans, evaluated.integer.value});
            return err;
        };
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

test "Bang(!) operator" {

    const allocator = std.testing.allocator;

    const inputs = [_][]const u8{ 
        "!true", "!false", "!5",
        "!!true", "!!false", "!!5",
    };
    const answers = [_]bool{ 
        false, true, false,
        true, false, true
    };

    for (inputs, answers) |inp, ans| {

        const evaluated = try testEval(allocator, inp);

        try expect(evaluated.boolean.value == ans);
    }

}




