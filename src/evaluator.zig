const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const Token = @import("Token.zig");

const object = @import("object.zig");
const Object = object.Object;
const Environment = object.Environment;
const ast = @import("ast.zig");
const Program = ast.Program;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const ArrayList = std.ArrayList;
const Identifier = ast.Identifier;

const expect = std.testing.expect;

const EvalError = error {
    // TODO: fill this out
    FailedEvalLet,
    EvalIdentNonExistent,
} || Allocator.Error;


pub fn Eval(program: *Program, env: *Environment) EvalError!?Object {

    var maybe_result: ?object.Object = null;

    // const prg_str = try program.String();
    // defer program.allocator.free(prg_str);
    // print("\nprog str: {s}\n", .{prg_str});

    for (program.statement_indexes.items) |stmt_idx| {
        maybe_result = try EvalNode(program, env, stmt_idx);

        if (maybe_result) |result| {
            if (result == .return_val_obj) {

                print("got a return obj\n", .{});

                defer result.return_val_obj.deinit();
                const val = result.return_val_obj.value.*;

                print("returning obj = {}\n", .{val});

                return val;
            }
        }

    }

    return maybe_result;

}

fn EvalNode(program: *Program, env: *Environment, node_idx: usize) EvalError!?object.Object {

    const node = program.nodes.items[node_idx];

    
    switch (node) {
        .statement => |stmt| {
            switch (stmt) {

                .let_stmt => |*ls| {

                    var ident = ls.name;
                    const name = ident.tokenLiteral();


                    // print("Evaluating let stmt: {s}\n", .{name});

                    const val = try EvalNode(program, env, ls.value.?);


                    // print("putting ident: {s} with val: {}\n", .{name, val.?});
                    try env.store.put(name, val.?);

                    // TODO print env, think key disapear because env lives longer than tokens.

                    // TODO: errors p.137
                    return null;

                },
                .ret_stmt => |rs| {

                    print("evaluating  return stmt\n", .{});
                    const val = try EvalNode(program, env, rs.value.?);
                    const res = try program.allocator.create(Object);
                    res.* = val.?;

                    return object.Object {
                        .return_val_obj = .{
                            .allocator = program.allocator,
                            .value = res
                        } // somehow this works lol
                    };
                },

                .expr_stmt => |es| {
                    return try EvalNode(program, env, es.expression.?);
                },

                .blck_stmt => |bs| {
                    return try evalBlockStatement(program, env, &bs);
                }
            }
        },

        .expression => |expr| {

            switch (expr) {

                .identifier => |ident| {
                    var tok = ident.token;
                    const ident_name = tok.tokenLiteral();
                    
                    // print("Evaluating ident expr: {s}\n", .{ident_name});

                    // print("getting ident name: {s}\n", .{ident_name});

                    const maybe_val = env.store.get(ident_name);

                    if (maybe_val) |val| {
                        return val;
                    } else {
                        print("didnt find: {s}\n", .{ident_name});
                        // TODO: create a eval error
                        // return EvalError.EvalIdentNonExistent;
                        return null;
                    }

                },
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
                    const right = (try EvalNode(program, env, pe.right)).?;
                    return evalPrefixExpression(pe.token.kind, &right);
                },

                .infix_expression => |ie| {
                    const left = (try EvalNode(program, env, ie.left)).?;
                    const right = (try EvalNode(program, env, ie.right)).?;
                    return evalInfixExpression(ie.token.kind, &left, &right);
                },

                .if_expression => |ie| {
                    return try evalIfExpression(program, env, &ie);
                },

                .fn_literal => |fl| {

                    var params: ?ArrayList(Identifier) = null;

                    const maybe_params = fl.parameters;

                    // print("making fn obj\n", .{});
                    // defer print("\n", .{});

                    if (maybe_params) |fl_params| {
                        params = try fl_params.clone();

                        // for (params.?.items) |*p| {
                        //     print("param: {s}\n", .{p.tokenLiteral()});
                        // }
                    }


                    return Object {
                        .function = .{
                            .params = params,
                            .body = try fl.body.clone(),
                            .env = env,
                        }
                    };

                },

                .call_expression => |ce| {
                    const maybe_func = try EvalNode(program, env, ce.function);
                    const func = maybe_func.?;
                    defer func.deinit();

                    var args = ArrayList(Object).init(program.allocator);
                    defer {
                        for (args.items) |arg| {
                            arg.deinit();
                        }
                        args.deinit();
                    }
                        
                    // evalExpressions p.144
                    for (ce.args.items) |arg_idx| {
                        try args.append((try EvalNode(program, env, arg_idx)).?);
                    }


                    return try applyFunction(program, &func, &args);


                },

            }

        }

    }

}

fn applyFunction(program: *Program, func_obj: *const Object, args: *ArrayList(Object)) !?Object {

    const func = func_obj.function;
    var extendedEnv = func.env.initClosedEnv();
    defer extendedEnv.deinit();

    // print("\napplying func {s}\n", .{});

    if (func.params) |params| {

        std.debug.assert(args.items.len == params.items.len);
        // print("n_params = {}, n_args = {}\n", .{args.items.len, params.items.len});

        for (params.items, args.items) |*p, arg| {

            // print("param: {s} = {}\n", .{p.token.literal, arg});

            const name = p.token.tokenLiteral();
            // print("name = {s}\n", .{name});

            try extendedEnv.store.put(name, arg);
        }

    }

    const evaluated = try evalBlockStatement(program, &extendedEnv, &func.body);

    if (evaluated.? == .return_val_obj) {
        return evaluated.?.return_val_obj.value.*;
    } else { 
        return null;
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

fn evalBlockStatement(program: *Program, env: *Environment, blck_stmt: *const ast.BlockStatement) EvalError!?object.Object {

    var maybe_result: ?object.Object = null;

    for (blck_stmt.statements.items) |stmt_idx| {
        maybe_result = try EvalNode(program, env, stmt_idx);
        
        if (maybe_result) |result| {
            if (result != .nullable and result == .return_val_obj) {
                return result; // p.131
            }
        }

    }

    return maybe_result;
    

}
fn evalIfExpression(program: *Program, env: *Environment, if_epxr: *const ast.IfExpression) EvalError!?object.Object {

    const condition = (try EvalNode(program, env, if_epxr.condition)).?;

    if (isTruthy(&condition)) {

        return try evalBlockStatement(program, env, &if_epxr.consequence);

    } else {

        if (if_epxr.alternative) |alt| {
            return try evalBlockStatement(program, env, &alt);

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

fn testEval(allocator: Allocator, input: []const u8) !?object.Object {
    
    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    var env = Environment.init(allocator);
    defer env.deinit();

    return try Eval(&program, &env);

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

        const evaluated = (try testEval(allocator, inp)).?;

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

        const evaluated = (try testEval(allocator, inp)).?;

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

        const evaluated = (try testEval(allocator, inp)).?;

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

        const evaluated = (try testEval(allocator, inp)).?;


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

test "Eval return stmt" {

    const allocator = std.testing.allocator;

    const inputs = [_][]const u8{ 
        "return 10;",
        "return 10; 9;",
        "return 2 * 5; 9;",
        "9; return 2 * 5; 9;",
        "if (10 > 1) { if (10 > 1) { return 10; } 129 return 1; }"
    };
    const answers = [_]i32 { 
        10,
        10,
        10,
        10,
        10,
    };

    for (inputs, answers) |inp, ans| {
        const evaluated = (try testEval(allocator, inp)).?;
        expect(ans == evaluated.integer) catch |err| {
            print("expected {}, got {}\n", .{ans, evaluated.integer});
            return err;
        };
    }

}

test "Eval Let stmt" {
    const allocator = std.testing.allocator;

    const inputs = [_][]const u8{ 
        "let a = 5; a;",
        "let a = 5 * 5; a;",
        "let a = 5; let b = a; b;",
        "let a = 5; let b = a; let c = a + b + 5; c;"
    };
    const answers = [_]i32 { 
        5,
        25,
        5,
        15
    };

    for (inputs, answers) |inp, ans| {
        const evaluated = (try testEval(allocator, inp)).?;
        
        expect(ans == evaluated.integer) catch |err| {
            print("expected {}, got {}\n", .{
                ans, evaluated.integer
            });

            return err;
        };
    }

}

// test "func object" {
//
//     const allocator = std.testing.allocator;
//     const input = "fn(x) { x + 2 };";
//
//     const evaluated = try testEval(allocator, input);
//     defer evaluated.deinit();
//
//     try expect(evaluated == .function);
//
// }


// test "func application" {
//
//     const allocator = std.testing.allocator;
//
//     const inputs = [_][]const u8{ 
//         "let identity = fn(x) { x; }; identity(5);",
//         "let identity = fn(x) { return x; }; identity(5);",
//         "let double = fn(x) { x * 2; }; double(5);",
//         "let add = fn(x, y) { x + y; }; add(5, 5);",
//         "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", // breaks everything
//         "fn(x) { x; }(5)"
//     };
//     const answers = [_]i32 { 
//         5,
//         5,
//         10,
//         10,
//         20,
//         5
//     };
//
//     for (inputs, answers) |inp, ans| {
//         const evaluated = (try testEval(allocator, inp)).?;
//
//         expect(ans == evaluated.integer) catch |err| {
//             print("expected {}, got {}\n", .{
//                 ans, evaluated.integer
//             });
//
//             return err;
//         };
//     }
// }
