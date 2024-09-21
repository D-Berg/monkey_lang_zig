const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const Token = @import("Token.zig");

const object = @import("object.zig");
const Program = @import("ast.zig").Program;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const expect = std.testing.expect;


pub fn Eval(program: *Program) error{FailedEvaluation, OutOfMemory}!object.Object {

    var maybe_result: ?object.Object = null;

    // const prg_str = try program.String();
    // defer program.allocator.free(prg_str);
    // print("prog str: {s}\n", .{prg_str});

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

                .infix_expression => |ie| {
                    const left = EvalNode(program, ie.left).?;
                    const right = EvalNode(program, ie.right).?;
                    return evalInfixExpression(ie.token.kind, &left, &right);
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

fn evalInfixExpression(
    operator: Token.Kind, 
    left: *const object.Object, 
    right: *const object.Object
) ?object.Object {

    if (left.* == .integer and right.* == .integer) {

        const left_val = left.integer.value;
        const right_val = right.integer.value;

        switch (operator) {
            .Plus => {
                return object.Object { 
                    .integer = .{ 
                        .value = left_val + right_val 
                    }
                };
            },
            .Minus => {
                return object.Object { 
                    .integer = .{ 
                        .value = left_val - right_val 
                    }
                };
            },
            .Asterisk => {
                return object.Object { 
                    .integer = .{ 
                        .value = left_val * right_val 
                    }
                };
            },
            .Slash => {
                return object.Object { 
                    .integer = .{ 
                        .value = @divTrunc(left_val, right_val) 
                    }
                };
            },
            else => {
                return null;
            }

        }

    }

    return null;

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

    const inputs = [_][]const u8{ 
        "5", "10", "-5", "-10", 
        "5 + 5 + 5 + 5 - 10",
        "2 * 2 * 2 * 2 * 2",
        "-50 + 100 + -50",
        "5 * 2 + 10",
        "5 + 2 * 10",
        "20 + 2 * -10",
        "50 / 2 * 2 + 10",
        "2 * (5 + 10)",
        "3 * 3 * 3 + 10",
        "3 * (3 * 3) + 10",
        "(5 + 10 * 2 + 15 / 3) * 2 + -10"
    };
    const answers = [_]i32{ 
        5, 10, -5, -10,
        10,
        32,
        0,
        20,
        25,
        0,
        60,
        30,
        37,
        37,
        50
    };

    for (inputs, answers) |inp, ans| {

        const evaluated = try testEval(allocator, inp);

        expect(evaluated.integer.value == ans) catch |err| {
            print("{s}\n", .{inp});
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




