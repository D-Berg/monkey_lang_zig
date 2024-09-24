const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const Token = @import("Token.zig");

const object = @import("object.zig");
const Object = object.Object;
const Environment = object.Environment;
const ast = @import("ast.zig");
const Statement = ast.Statement;
const Expression = ast.Expression;
const Program = ast.Program;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const ArrayList = std.ArrayList;
const Identifier = ast.Identifier;
const FuncionObject = object.FunctionObject;

const expect = std.testing.expect;

const EvalError = error {
    // TODO: fill this out
    FailedEvalLet,
    EvalIdentNonExistent,
} || Allocator.Error;


pub fn Eval(program: *Program, env: *Environment) EvalError!?Object {

    var maybe_result: ?object.Object = null;

    const prg_str = try program.String();
    defer program.allocator.free(prg_str);
    print("\nprog str: {s}\n", .{prg_str});

    for (program.statements.items) |stmt| {
        maybe_result = try EvalStmt(program, env, &stmt);

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

fn EvalStmt(program: *Program, env: *Environment, stmt: *const Statement) EvalError!?object.Object {

    switch (stmt.*) {

        .let_stmt => |*ls| {
                
            var ident = ls.name;
            const name = ident.tokenLiteral();

            print("Evaluating let stmt: {s}\n", .{name});

            const val = try EvalExpr(program, env, ls.value);

            // print("putting ident: {s} with val: {}\n", .{name, val.?});
            try env.store.put(name, val.?);

            // TODO print env, think key disapear because env lives longer than tokens.

            // TODO: errors p.137
            return null;

        },
        .ret_stmt => |rs| {

            print("evaluating  return stmt\n", .{});
            const val = try EvalExpr(program, env, rs.value);
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
            print("evaluating expression stmt\n", .{});
            return try EvalExpr(program, env, es.expression);
        },

        .blck_stmt => |bs| {
            return try evalBlockStatement(program, env, &bs);
        }
    }


}

fn EvalExpr(program: *Program, env: *Environment, expr: *const Expression) EvalError!?object.Object {

    switch (expr.*) {

        .identifier => |ident| {
            print("\nEvaluating ident expr\n", .{});
            var tok = ident.token;
            const ident_name = tok.tokenLiteral();
            

            print("Retreiving from env: {*}\n", .{env});
            const maybe_val = try env.get(ident_name);

            print("getting ident name: {s} = {?}\n", .{ident_name, maybe_val});

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

            print("eval int lit expr {s}\n", .{int_lit.token.tokenLiteral()});

            return object.Object {
                .integer = @intCast(int_lit.value),
            };

        }, 
        .boolean_literal => |bool_lit| {

            print("eval boolean lit expr with val {}\n", .{bool_lit.value});
            return object.Object {
                .boolean = bool_lit.value,
            };

        }, 
        
        .prefix_expression => |pe| {
            const right = (try EvalExpr(program, env, pe.right)).?;
            return evalPrefixExpression(pe.token.kind, &right);
        },

        .infix_expression => |ie| {
            const left = (try EvalExpr(program, env, ie.left)).?;
            const right = (try EvalExpr(program, env, ie.right)).?;
            return evalInfixExpression(ie.token.kind, &left, &right);
        },

        .if_expression => |ie| {
            return try evalIfExpression(program, env, &ie);
        },

        .fn_literal => |fl| {

            print("making fn object\n", .{});

            var params: ?ArrayList(Identifier) = null; // TODO: use slice instead

            const maybe_params = fl.parameters;

            // print("making fn obj\n", .{});
            // defer print("\n", .{});

            if (maybe_params) |fl_params| {
                params = try fl_params.clone(); // TODO: memcpy to slice

                // for (params.?.items) |*p| {
                //     print("param: {s}\n", .{p.tokenLiteral()});
                // }
            }

            // TODO: Remove
            for (fl.body.statements.items) |stmt| {

                const stmt_str = try stmt.String(program);
                defer program.allocator.free(stmt_str);
                print("body smt: {s}\n", .{stmt_str});

            }

            const body = try fl.body.clone();

            for (body.statements.items) |stmt| {

                const stmt_str = try stmt.String(program);
                defer program.allocator.free(stmt_str);
                print("body smt: {s}\n", .{stmt_str});

            }

            return Object {
                .function = .{
                    .params = params,
                    .body = body,
                    .env = env,
                }
            };

        },

        .call_expression => |ce| {
            print("calling func\n", .{});
            const maybe_func = try EvalExpr(program, env, ce.function);
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
            for (ce.args.items) |*arg| {
                try args.append((try EvalExpr(program, env, arg)).?);
            }


            return try applyFunction(program, &func.function, &args);


        },

    }
}

fn applyFunction(program: *Program, func: *const FuncionObject, args: *ArrayList(Object)) !?Object {


    print("\napplying func\n", .{});

    // print("function = {}\n", .{func});
    var extendedEnv = func.env.initClosedEnv();
    defer extendedEnv.deinit();

    print("Extended env has address {*}\n", .{&extendedEnv});
    print("outer env has adress {*}\n", .{extendedEnv.outer.?});

    if (func.params) |params| {

        std.debug.assert(args.items.len == params.items.len);
        // print("n_params = {}, n_args = {}\n", .{args.items.len, params.items.len});

        for (params.items, args.items) |*p, arg| {


            const name = p.token.tokenLiteral();

            print("putting param: {s} = {} in env {*}\n", .{name, arg, &extendedEnv});

            try extendedEnv.store.put(name, arg);
        }

    }

    print("printing functions block statements\n", .{});
    for (func.body.statements.items) |stmt| {
        const stmt_str = try stmt.String(program);
        defer program.allocator.free(stmt_str);
        print("body smt: {s}\n", .{stmt_str});

    }

    // Failes on new line because BlockStatement has indices to old program
    const maybe_evaluated = try evalBlockStatement(program, &extendedEnv, &func.body);

    // unwrap
    if (maybe_evaluated) |evaluated| {

        if (evaluated == .return_val_obj) {
            // TODO: do I need to deinit
            defer evaluated.return_val_obj.deinit();
            return evaluated.return_val_obj.value.*;
        }
    }

    return maybe_evaluated;

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

    print("evaluating block stmts\n", .{});
    defer print("finished eval of block\n", .{});

    for (blck_stmt.statements.items) |*stmt| {

        print("\tblck: evaluating {}\n", .{stmt});
        maybe_result = try EvalStmt(program, env, stmt);
        
        if (maybe_result) |result| {
            if (result != .nullable and result == .return_val_obj) {
                return result; // p.131
            }
        }

    }

    return maybe_result;
}
fn evalIfExpression(program: *Program, env: *Environment, if_epxr: *const ast.IfExpression) EvalError!?object.Object {

    const condition = (try EvalExpr(program, env, if_epxr.condition)).?;

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

test "func object" {

    const allocator = std.testing.allocator;
    const input = "fn(x) { x + 2 };";

    const evaluated = (try testEval(allocator, input)).?;
    defer evaluated.deinit();

    try expect(evaluated == .function);

    try expect(evaluated.function.params.?.items.len == 1);
}


test "func application" {

    const allocator = std.testing.allocator;

    const inputs = [_][]const u8{ 
        "let identity = fn(x) { x; }; identity(5);",
        "let identity = fn(x) { return x; }; identity(5);",
        "let double = fn(x) { x * 2; }; double(5);",
        "let add = fn(x, y) { x + y; }; add(5, 5);",
        "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", // breaks everything
        "fn(x) { x; }(5)"
    };
    const answers = [_]i32 { 
        5,
        5,
        10,
        10,
        20,
        5
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


test "multi func application" {

    const allocator = std.testing.allocator;

    const inputs = [_][]const u8{ 
        "let add = fn(x, y) { x + y; };",
        "add(5, 5);", // breaks everything
    };

    var env = Environment.init(allocator);
    defer env.deinit();

    for (inputs, 0..) |inp, idx| {

        var lexer = try Lexer.init(allocator, inp);
        defer lexer.deinit();

        var parser = Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.ParseProgram(allocator);
        defer program.deinit();

        const evaluated = try Eval(&program, &env);

        if (idx == 0) {
            expect(evaluated == null) catch |err| {
                print("expected null, got {?}\n", .{
                    evaluated
                });

                return err;
            };
        }

        if (idx == 1) {
            expect(evaluated.? == .integer) catch |err| {
                print("expected integer, got {?}\n", .{
                    evaluated
                });

                return err;
            };
        }
    }
}
