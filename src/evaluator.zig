const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const Token = @import("Token.zig");

const object = @import("object.zig");
const ast = @import("ast.zig");
const Program = ast.Program;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const expect = std.testing.expect;


pub fn Eval(program: *Program) error{FailedEvaluation, OutOfMemory}!object.Object {

    var result: object.Object = undefined;

    const prg_str = try program.String();
    defer program.allocator.free(prg_str);
    print("prog str: {s}\n", .{prg_str});

    for (program.statement_indexes.items) |stmt_idx| {

        result = EvalNode(program, stmt_idx);

    }

    return result;

}

fn EvalNode(program: *Program, node_idx: usize) object.Object {

    const node = program.nodes.items[node_idx];

    
    switch (node) {
        .statement => |stmt| {
            switch (stmt) {

                .expr_stmt => |es| {
                    return EvalNode(program, es.expression.?);
                },

                .blck_stmt => |bs| {

                    return evalBlockStatement(program, &bs);
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
                        .integer = @intCast(int_lit.value),
                    };

                }, 
                .boolean_literal => |bool_lit| {

                    return object.Object {
                        .boolean = bool_lit.value,
                    };

                }, 
                
                .prefix_expression => |pe| {
                    const right = EvalNode(program, pe.right);
                    return evalPrefixExpression(pe.token.kind, &right);
                },

                .infix_expression => |ie| {
                    const left = EvalNode(program, ie.left);
                    const right = EvalNode(program, ie.right);
                    return evalInfixExpression(ie.token.kind, &left, &right);
                },

                .if_expression => |ie| {
                    return evalIfExpression(program, &ie);
                },

                else => {
                    unreachable;
                }

            }

        }

    }

}


fn evalPrefixExpression(operator: Token.Kind, right: *const object.Object) object.Object {
    switch (operator) {
        .Bang => {

            // same as evalBangOperatorExpression()
            switch (right.*) {

                .boolean => |b| {
                    if (b) {
                        return object.Object { 
                            .boolean = false 
                        };
                    } else {
                        return object.Object {
                            .boolean = true 
                        };
                    }
                },

                .nullable => {
                    return object.Object {
                        .boolean = true
                    };
                },
                else => {
                    return object.Object {
                        .boolean = false 
                    };
                }

            }

        },

        .Minus => {
            if (right.* != .integer) {
                return object.Object {
                    .nullable = {}
                };
            }

            const val = right.integer;

            return object.Object {
                .integer =  -val
            };
        },

        else => {
            return object.Object {
                .nullable = {}
            };
        }
    }
}

fn evalInfixExpression(
    operator: Token.Kind, 
    left: *const object.Object, 
    right: *const object.Object
) object.Object {

    if (left.* == .integer and right.* == .integer) {

        const left_val = left.integer;
        const right_val = right.integer;

        switch (operator) {
            .Plus => {
                return object.Object { 
                    .integer = left_val + right_val 
                };
            },
            .Minus => {
                return object.Object { 
                    .integer = left_val - right_val 
                };
            },
            .Asterisk => {
                return object.Object { 
                    .integer = left_val * right_val 
                };
            },
            .Slash => {
                return object.Object { 
                    .integer = @divTrunc(left_val, right_val) 
                };
            },
            .Lt => {
                return object.Object {
                    .boolean = left_val < right_val 
                };
            },

            .Gt => {
                return object.Object {
                    .boolean = left_val > right_val 
                };
            },
            .Eq => {
                return object.Object {
                    .boolean = left_val == right_val 
                };
            },
            .Neq => {
                return object.Object {
                    .boolean = left_val != right_val 
                };
            },
            else => {
                return object.Object {
                    .nullable = {}
                };
            }

        }

    }

    if (left.* == .boolean and right.* == .boolean) {
        const left_val = left.boolean;
        const right_val = right.boolean;

        switch (operator) {
            .Eq => {
                return object.Object {
                    .boolean = left_val == right_val
                };

            },
            .Neq => {
                return object.Object {
                    .boolean = left_val != right_val
                };
            },
            else => {
                return object.Object { .nullable = {} };
            }

        }
    }

    return object.Object { .nullable = {} };

}

fn evalBlockStatement(program: *Program, blck_stmt: *const ast.BlockStatement) object.Object {

    var obj: object.Object = undefined;

    for (blck_stmt.statements.items) |stmt_idx| {
        obj = EvalNode(program, stmt_idx);
    }

    return obj;
    

}
fn evalIfExpression(program: *Program, if_epxr: *const ast.IfExpression) object.Object {

    const condition = EvalNode(program, if_epxr.condition);

    if (isTruthy(&condition)) {

        return evalBlockStatement(program, &if_epxr.consequence);

    } else {

        if (if_epxr.alternative) |alt| {
            return evalBlockStatement(program, &alt);

        } else {
            return object.Object { .nullable = {} };
        }
    }

}

fn isTruthy(obj: *const object.Object) bool {

    switch (obj.*) {
        .boolean => |b| {
            return b;
        },
        .nullable => {
            return false;
        },
        else => {
            return true;
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

        expect(evaluated.integer == ans) catch |err| {
            print("{s}\n", .{inp});
            print("Expected {}, got {}\n", .{ans, evaluated.integer});
            return err;
        };
    }

}

test "Eval bool expr" {
    const allocator = std.testing.allocator;

    const inputs = [_][]const u8{ 
        "true", 
        "false",
        "1 < 2",
        "1 > 2",
        "1 < 1",
        "1 > 1",
        "1 == 1",
        "1 != 1",
        "1 == 2",
        "1 != 2",
        "true == true",
        "false == false",
        "true == false",
        "true != false",
        "false != true",
        "(1 < 2) == true",
        "(1 < 2) == false",
        "(1 > 2) == true",
        "(1 > 2) == false"
    };

    const answers = [_]bool{
        true, 
        false,
        true,
        false,
        false,
        false,
        true,
        false,
        false,
        true,
        true,
        true,
        false,
        true,
        true,
        true,
        false,
        false,
        true
    };

    for (inputs, answers) |inp, ans| {

        const evaluated = try testEval(allocator, inp);

        expect(evaluated.boolean == ans) catch |err| {
            print("{s}\n", .{inp});
            print("Expected {}, got {}\n", .{ans, evaluated.boolean});
            return err;

        };
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

        try expect(evaluated.boolean == ans);
    }

}


test "eval if expr" {

    const allocator = std.testing.allocator;

    const inputs = [_][]const u8{ 
        "if (true) { 10 }",
        "if (false) { 10 }",
        "if (1) { 10 }",
        "if (1 < 2) { 10 }",
        "if (1 > 2) { 10 }",
        "if (1 > 2) { 10 } else { 20 }",
        "if (1 < 2) { 10 } else { 20 }"
    };
    const answers = [_]object.Object { 
        object.Object{ .integer = 10 },
        object.Object{ .nullable = {} },
        object.Object{ .integer = 10 },
        object.Object{ .integer = 10 },
        object.Object{ .nullable = {} },
        object.Object{ .integer = 20 },
        object.Object{ .integer = 10 },

    };

    for (inputs, answers) |inp, ans| {

        const evaluated = try testEval(allocator, inp);


        switch (evaluated) {
            .integer => |int| {
                try expect(int == ans.integer);
            },
            .nullable => {
                try expect(ans == .nullable);
            },
            else => {
                return error.FailedEvaluation;
            }
        }


    }


}


