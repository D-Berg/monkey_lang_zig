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


/// Returns an Object that needs to be deinitiated or null
pub fn Eval(program: *Program, env: *Environment) EvalError!?Object {

    var maybe_result: ?object.Object = null;

    const prg_str = try program.String();
    defer program.allocator.free(prg_str);
    print("\nprog str: {s}\n", .{prg_str});

    print("main env {*}\n", .{env});

    for (program.statements.items) |stmt| {

        maybe_result = try EvalStmt(&stmt, env);

        if (maybe_result) |result| {

            if (result == .return_val_obj) {

                // print("got a return obj\n", .{});

                defer result.return_val_obj.deinit();
                const val = result.return_val_obj.value.*;

                // print("returning obj = {}\n", .{val});

                return val;
            }
        }

    }

    return maybe_result;
}

fn EvalStmt(stmt: *const Statement, env: *Environment) EvalError!?object.Object {

    switch (stmt.*) {

        .let_stmt => |*ls| {
                
            var ident = ls.name;
            const name = ident.tokenLiteral();

            print("Evaluating let stmt: {s}\n", .{name});

            const val = try EvalExpr(ls.value, env);
            defer val.?.deinit(); // deinit because store.put clones val

            // print("putting ident: {s} with val: {}\n", .{name, val.?});
            try env.store.put(name, val.?);

            // TODO print env, think key disapear because env lives longer than tokens.

            // TODO: errors p.137
            return null;

        },
        .ret_stmt => |rs| {

            // print("evaluating  return stmt\n", .{});
            const val = try EvalExpr(rs.value, env);
            const res = try rs.allocator.create(Object);
            res.* = val.?;

            return object.Object {
                .return_val_obj = .{
                    .allocator = rs.allocator,
                    .value = res
                } // somehow this works lol
            };
        },

        .expr_stmt => |es| {
            // print("evaluating expression stmt\n", .{});
            return try EvalExpr(es.expression, env);
        },

        .blck_stmt => |bs| {
            return try evalBlockStatement(&bs, env);
        }
    }


}

fn EvalExpr(expr: *const Expression, env: *Environment) EvalError!?object.Object {

    switch (expr.*) {

        .identifier => |ident| {
            print("\nEvaluating ident expr\n", .{});
            var tok = ident.token;
            const ident_name = tok.tokenLiteral();
            

            print("Retreiving {s} from env: {*}\n", .{ident.tokenLiteral(), env});
            const maybe_val = try env.get(ident_name); // gets a clone of object

            // print("getting ident name: {s} = {?}\n", .{ident_name, maybe_val});

            if (maybe_val) |val| {
                return val;
            } else {
                // print("didnt find: {s}\n", .{ident_name});
                // TODO: create a eval error
                // return EvalError.EvalIdentNonExistent;
                return null;
            }

        },
        .integer_literal => |int_lit| {

            // print("eval int lit expr {s}\n", .{int_lit.token.tokenLiteral()});

            return object.Object {
                .integer = @intCast(int_lit.value),
            };

        }, 
        .boolean_literal => |bool_lit| {

            // print("eval boolean lit expr with val {}\n", .{bool_lit.value});
            return object.Object {
                .boolean = bool_lit.value,
            };

        }, 
        
        .prefix_expression => |pe| {
            const right = (try EvalExpr(pe.right, env)).?;
            return evalPrefixExpression(pe.token.kind, &right);
        },

        .infix_expression => |ie| {
            const ie_str = try ie.String();
            defer ie.allocator.free(ie_str);
            print("infix_expression = {s}\n", .{ie_str});

            const maybe_left = try EvalExpr(ie.left, env);
            const left = maybe_left.?;
            const right = (try EvalExpr(ie.right, env)).?;
            return evalInfixExpression(ie.token.kind, &left, &right);
        },

        .if_expression => |ie| {
            return try evalIfExpression(&ie, env);
        },

        .fn_literal => |fl| {

            // print("outer env = {?}\n", .{env.outer});

            // Clone func expr param identifiers to func obj

            var params = ArrayList(Identifier).init(fl.token.allocator);
            
            for (fl.parameters.items) |p| {
                try params.append(try p.clone());
            }

            var fn_env: *Environment = undefined;

            // clone env if its an enclosed one
            if (env.outer == null)  {
                fn_env = env;
            } else {
                fn_env = try fl.parameters.allocator.create(Environment);
                fn_env.* = try env.clone();
            }

            const fn_obj = Object {
                .function = .{
                    .allocator = fl.token.allocator,
                    .params = params,
                    .body = try fl.body.clone(),
                    .env = fn_env,
                }
            };
            // print("outer env = {?}\n", .{fn_obj.function.env.outer});
            const fn_obj_str = try fn_obj.function.String();
            defer fn_obj.function.allocator.free(fn_obj_str);
            
            print("made {s}\n", .{fn_obj_str});

            return fn_obj;
        },

        .call_expression => |ce| {
            const maybe_func = try EvalExpr(ce.function, env);
            var func = maybe_func.?;
            defer func.deinit();

            const fn_obj_str = try func.function.String();
            defer func.function.allocator.free(fn_obj_str);

            print("\ncalling func {s}\n", .{fn_obj_str});

            var args = ArrayList(Object).init(ce.allocator);
            defer {
                for (args.items) |arg| {
                    arg.deinit();
                }
                args.deinit();
            }
                
            // evalExpressions p.144
            for (ce.args.items) |*arg| {
                try args.append((try EvalExpr(arg, env)).?);
            }


            return try applyFunction(&func.function, &args);


        },

    }
}

fn applyFunction(func: *FuncionObject, args: *ArrayList(Object)) !?Object {


    print("\napplying func\n", .{});

    // print("function = {}\n", .{func});

    const extendedEnv = try func.allocator.create(Environment);
    extendedEnv.* = func.env.initClosedEnv();

    func.env = extendedEnv;

    // defer {
    //     print("closing extendEnv at {*}\n", .{&extendedEnv});
    //     extendedEnv.deinit();
    // }

    print("Creating Extended env, has address {*}\n", .{func.env});
    print("outer env has adress {*}\n", .{func.env.outer.?});


    std.debug.assert(args.items.len == func.params.items.len);
    // print("n_params = {}, n_args = {}\n", .{args.items.len, params.items.len});

    for (func.params.items, args.items) |*p, arg| {

        const name = p.token.tokenLiteral();

        print("putting param: {s} = {} in env {*}\n", .{name, arg, func.env});

        try func.env.store.put(name, arg);
    }


    // print("printing functions block statements\n", .{});
    for (func.body.statements.items) |stmt| {
        const stmt_str = try stmt.String();
        defer func.params.allocator.free(stmt_str);
        // print("body smt: {s}\n", .{stmt_str});

    }

    // Failes on new line because BlockStatement has indices to old program
    print("Evaluating functions blck stmts\n", .{});
    const maybe_evaluated = try evalBlockStatement(&func.body, extendedEnv);

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

fn evalBlockStatement(blck_stmt: *const ast.BlockStatement, env: *Environment) EvalError!?object.Object {

    var maybe_result: ?object.Object = null;

    // print("evaluating block stmts\n", .{});
    // defer print("finished eval of block\n", .{});

    for (blck_stmt.statements.items) |*stmt| {

        // print("\tblck: evaluating {}\n", .{stmt});
        maybe_result = try EvalStmt(stmt, env);
        
        if (maybe_result) |result| {
            if (result != .nullable and result == .return_val_obj) {
                return result; // p.131
            }
        }
    }

    return maybe_result;
}

fn evalIfExpression(if_epxr: *const ast.IfExpression, env: *Environment) EvalError!?object.Object {

    const condition = (try EvalExpr(if_epxr.condition, env)).?;

    if (isTruthy(&condition)) {

        return try evalBlockStatement(&if_epxr.consequence, env);

    } else {

        if (if_epxr.alternative) |alt| {
            return try evalBlockStatement(&alt, env);

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

fn testEval(env: *Environment, input: []const u8) !?object.Object {
    
    const allocator = env.store.allocator;

    var lexer = Lexer.init(allocator, input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    return try Eval(&program, env);

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
        var env = Environment.init(allocator);
        defer env.deinit();

        const evaluated = (try testEval(&env, inp)).?;
        defer evaluated.deinit();

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

        var env = Environment.init(allocator);
        defer env.deinit();

        const evaluated = (try testEval(&env, inp)).?;
        defer evaluated.deinit();

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
        var env = Environment.init(allocator);
        defer env.deinit();

        const evaluated = (try testEval(&env, inp)).?;
        defer evaluated.deinit();

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
        var env = Environment.init(allocator);
        defer env.deinit();


        const evaluated = (try testEval(&env, inp)).?;
        defer evaluated.deinit();


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

        var env = Environment.init(allocator);
        defer env.deinit();

        const evaluated = (try testEval(&env, inp)).?;
        defer evaluated.deinit();

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

        var env = Environment.init(allocator);
        defer env.deinit();

        const evaluated = (try testEval(&env, inp)).?;
        defer evaluated.deinit();

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
    const input = "fn(x) { x + 2; };";

    var env = Environment.init(allocator);
    defer env.deinit();

    const evaluated = (try testEval(&env, input)).?;
    defer evaluated.deinit();

    try expect(evaluated == .function);


    try expect(evaluated.function.params.items.len == 1);
}


test "func application" {

    const allocator = std.testing.allocator;

    const inputs = [_][]const u8{ 
        "let identity = fn(x) { x; }; identity(5);",
        // "let identity = fn(x) { return x; }; identity(5);",
        // "let double = fn(x) { x * 2; }; double(5);",
        // "let add = fn(x, y) { x + y; }; add(5, 5);",
        // "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", // breaks everything
        // "fn(x) { x; }(5)"
    };
    const answers = [_]i32 { 
        5,
        // 5,
        // 10,
        // 10,
        // 20,
        // 5
    };

    for (inputs, answers) |inp, ans| {
        
        var env = Environment.init(allocator);
        defer env.deinit();

        const evaluated = (try testEval(&env, inp)).?;
        defer evaluated.deinit();

        expect(ans == evaluated.integer) catch |err| {
            print("expected {}, got {}\n", .{
                ans, evaluated.integer
            });

            return err;
        };
    }
}

test "multi input fn application" {

    const allocator = std.testing.allocator;

    const inputs = [_][]const u8{ 
        "let add = fn(x, y) { x + y; };",
        "add(5, 5);", // breaks everything
    };

    var env = Environment.init(allocator);
    defer env.deinit();

    for (inputs, 0..) |inp, idx| {

        var lexer = Lexer.init(allocator, inp);

        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.ParseProgram(allocator);
        defer program.deinit();

        const evaluated = try Eval(&program, &env);
        defer {
            if (evaluated != null) evaluated.?.deinit();
        }

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

test "Closures" {
    const allocator = std.testing.allocator;

    const input = 
        \\let newAdder = fn(x) {
        \\ fn(y) { x + y; };
        \\};
        \\
        \\let addTwo = newAdder(2);
        \\addTwo(2);
    ;

    var env = Environment.init(allocator);
    defer env.deinit();

    const maybe_eval = try testEval(&env, input);

    if (maybe_eval) |evaluated| {
        defer evaluated.deinit();
        expect(evaluated.integer == 4) catch |err| {
            print("exptexted 4, got {}\n", .{evaluated.integer});
            return err;
        };
    } else {
        print("got null back\n", .{});
        return error.FailedEvalLet;
    }


}
//
// test "eval counter p.150" {
//     const allocator = std.testing.allocator;
//
//     const input = 
//         \\let counter = fn(x) { 
//         \\  if (x > 100) {
//         \\      return true; 
//         \\  } else {
//         \\      let foobar = 9999;
//         \\      counter(x + 1);
//         \\  }
//         \\};
//         \\counter(0);
//     ;
//
//     const maybe_eval = try testEval(allocator, input);
//
//     if (maybe_eval) |evaluated| {
//         defer evaluated.deinit();
//         try expect(evaluated.boolean);
//     } 
//
//     return error.FailedEvalLet;
//
// }
