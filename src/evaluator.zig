// TODO make evaluator file upercase
const std = @import("std");
const print = std.debug.print;
const log = std.log;

const Allocator = std.mem.Allocator;
const Token = @import("Token.zig");

const object = @import("object.zig");
const ast = @import("ast.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const Object = object.Object;
const FuncionObject = object.FunctionObject;
const StringObject = object.StringObject;
const Environment = @import("Environment.zig");


const Statement = ast.Statement;
const LetStatement = ast.LetStatement;
const ReturnStatement = ast.ReturnStatement;

const Expression = ast.Expression;
const Identifier = ast.Identifier;
const PrefixExpression = ast.PrefixExpression;
const InfixExpression = ast.InfixExpression;
const FnLiteralExpression = ast.FnLiteralExpression;
const CallExpression = ast.CallExpression;
const StringExpression = ast.StringExpression;

const Program = ast.Program;
const ArrayList = std.ArrayList;

const expect = std.testing.expect;
const expectEqualStrings = std.testing.expectEqualStrings;

const EvalError = error {
    // TODO: fill this out
    FailedEvalLet,
    FailedEvalString,
    EvalIdentNonExistent,
} || Allocator.Error || std.fmt.BufPrintError;


/// Returns an Object that needs to be deinitiated or null
pub fn Eval(program: *Program, env: *Environment) EvalError!?Object {

    var maybe_result: ?object.Object = null;

    const prg_str = try program.String();
    defer program.allocator.free(prg_str);
    // print("\nprog str: {s}\n", .{prg_str});

    log.debug("main env {*}\n", .{env});

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
            try EvalLetStmt(ls, env);
            return null;

        },
        .ret_stmt => |*rs| {
            return try EvalRetStmt(rs, env);
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

fn EvalLetStmt(ls: *const LetStatement, env: *Environment) EvalError!void {

    var ident = ls.name;
    const name = ident.tokenLiteral();

    log.debug("Evaluating let stmt: {s}\n", .{name});

    const maybe_val = try EvalExpr(ls.value, env);
    // defer val.?.deinit(); // deinit because store.put clones val

    // print("putting ident: {s} with val: {}\n", .{name, val.?});
    //
    if (maybe_val) |val| {
        var val_var = val;
        try env.put(name, &val_var);
    } else {
        return EvalError.FailedEvalLet;
    }

    // TODO print env, think key disapear because env lives longer than tokens.

    // TODO: errors p.137

}

fn EvalRetStmt(rs: *const ReturnStatement, env: *Environment) EvalError!Object {

    // print("evaluating  return stmt\n", .{});
    const val = try EvalExpr(rs.value, env);
    const res = try rs.allocator.create(Object);
    res.* = val.?;

    return object.Object {
        .return_val_obj = .{
            .allocator = rs.allocator,
            .value = res,
            .owner = null
        } // somehow this works lol
    };
}

fn EvalExpr(expr: *const Expression, env: *Environment) EvalError!?object.Object {

    switch (expr.*) {

        .identifier => |*ident| {

            return try EvalIdentExpr(ident, env);

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
        
        .prefix_expression => |*pe| {
            return try EvalPrefixExpr(pe, env);
        },

        .infix_expression => |*ie| { 
            return try EvalInfixExpr(ie, env);
        },

        .if_expression => |ie| {
            return try evalIfExpression(&ie, env);
        },

        .fn_literal => |*fl| {

            const fn_obj_ptr = try env.store.allocator.create(FuncionObject);
            errdefer env.store.allocator.destroy(fn_obj_ptr);
            fn_obj_ptr.* = try EvalFnExpr(fl, env);

            log.debug("created fn obj {*}\n", .{fn_obj_ptr});
                
            return Object {
                .function = fn_obj_ptr,
            };

        },

        .call_expression => |*ce| {

            return try EvalCallExpr(ce, env);

        },

        .string_expression => |*se|{
            const str_obj_ptr = try env.store.allocator.create(StringObject);
            errdefer str_obj_ptr.deintit();
            str_obj_ptr.* = try EvalStringExpr(se);

            return Object { .string = str_obj_ptr };
        }

    }
}


/// Retrieves a cloned obj from env 
fn EvalIdentExpr(ident: *const Identifier, env: *Environment) EvalError!Object {

    // print("\nEvaluating ident expr\n", .{});
    var tok = ident.token;
    const ident_name = tok.tokenLiteral();

    const maybe_val = env.get(ident_name); // gets a clone of object

    // print("getting ident name: {s} = {?}\n", .{ident_name, maybe_val});

    if (maybe_val) |val| {
        // print("Retreived {s} = {}\n", .{ident_name, val});
        return val;
    } else {
        // print("didnt find: {s}\n", .{ident_name});
        // TODO: create a eval error


        log.err("couldnt find {s} in {*}\n", .{ident_name, env});
        log.debug("env cointains:\n", .{});
        env.printEnv();
        
        @panic("Failed EvalIDentExpr");
        // return EvalError.EvalIdentNonExistent;
    }
}

fn EvalFnExpr(fl: *const FnLiteralExpression, env: *Environment) EvalError!FuncionObject {

    // print("outer env = {?}\n", .{env.outer});

    // Clone func expr param identifiers to func obj

    var params = ArrayList(Identifier).init(fl.token.allocator);
    
    for (fl.parameters.items) |p| {
        try params.append(try p.clone());
    }

    // var fn_env: *Environment = undefined;
    //
    // // clone env if its an enclosed one
    // if (env.outer == null)  {
    //     fn_env = env;
    // } else {
    //     fn_env = try env.clone();
    // }
    
    env.rc += 1;

    return FuncionObject {
        .allocator = fl.token.allocator,
        .params = params,
        .body = try fl.body.clone(),
        .env = env,
    };

}

fn EvalCallExpr(ce: *const CallExpression, env: *Environment) EvalError!?Object {

    const maybe_func = try EvalExpr(ce.function, env);
    const func = maybe_func.?;
    defer func.deinit(); // only deinit if fnc dont have a owner

    // const fn_obj_str = try func.function.String();
    // defer func.function.allocator.free(fn_obj_str);

    // print("\ncalling func {s}\n", .{fn_obj_str});

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


    return try applyFunction(func.function, &args);
}

fn applyFunction(func: *FuncionObject, args: *ArrayList(Object)) EvalError!?Object {


    log.debug("\napplying func {*}\n", .{func});
    defer log.debug("funished applying func {*}\n", .{func});

    // print("function = {}\n", .{func});

    const extendedEnv = try func.allocator.create(Environment);
    extendedEnv.* = try func.env.initClosedEnv();
    log.debug("Creating Extended env, has address {*}\n", .{extendedEnv});

    func.env = extendedEnv;
    func.env.rc += 1;

    if (func.env.outer) |outer| {
        outer.rc -= 1;
    }

    log.debug("func {*} has env: {*}\n", .{func, func.env});

    defer {
        // print("closing extendEnv at {*}\n", .{extendedEnv});

        // func.env = extendedEnv.outer.?;
        // print("set func {*} env to {*}\n", .{func, func.env});
        // extendedEnv.deinit();
        // func.allocator.destroy(extendedEnv);
    }

    // print("outer env has adress {*}\n", .{func.env.outer.?});


    log.debug("n_params = {}, n_args = {}\n", .{func.params.items.len, args.items.len});
    std.debug.assert(args.items.len == func.params.items.len);

    for (func.params.items, args.items) |*p, arg| {

        const name = p.token.tokenLiteral();

        log.debug("putting param: {s} = {} in env {*}\n", .{name, arg, func.env});

        // TODO: Clone arg since its deinited
        
        var cloned_arg = try arg.clone();
        try func.env.put(name, &cloned_arg);
    }


    // print("printing functions block statements\n", .{});
    for (func.body.statements.items) |stmt| {
        const stmt_str = try stmt.String();
        defer func.params.allocator.free(stmt_str);
        // print("body smt: {s}\n", .{stmt_str});

    }

    // Failes on new line because BlockStatement has indices to old program
    log.debug("Evaluating functions blck stmts\n", .{});
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


fn EvalPrefixExpr(pe: *const PrefixExpression, env: *Environment) EvalError!Object {
    const operator = pe.token.kind;

    const right = (try EvalExpr(pe.right, env)).?; // TODO: handle null case
    defer right.deinit();

    switch (operator) {
        .Bang => {
            // same as evalBangOperatorExpression()
            switch (right) {

                .boolean => |b| {
                    if (b) {
                        return Object { 
                            .boolean = false 
                        };
                    } else {
                        return Object {
                            .boolean = true 
                        };
                    }
                },

                .nullable => {
                    return Object {
                        .boolean = true
                    };
                },
                else => {
                    return Object {
                        .boolean = false 
                    };
                }

            }

        },

        .Minus => {
            if (right != .integer) {
                return Object {
                    .nullable = {}
                };
            }

            const val = right.integer;

            return Object {
                .integer =  -val
            };
        },

        else => {
            return Object {
                .nullable = {}
            };
        }
    }
}

fn EvalInfixExpr( ie: *const InfixExpression, env: *Environment) EvalError!object.Object {
    const operator = ie.token.kind;

    const ie_str = try ie.String();
    defer ie.allocator.free(ie_str);
    // print("infix_expression = {s}\n", .{ie_str});

    // TODO: handle null cases
    const maybe_left = try EvalExpr(ie.left, env);
    const left = maybe_left.?;
    defer left.deinit();

    const right = (try EvalExpr(ie.right, env)).?;
    defer right.deinit();

    if (left == .integer and right == .integer) {

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

    if (left == .boolean and right == .boolean) {
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


    if (left == .string and right == .string) {

        const left_str = left.string.value;
        const right_str = right.string.value;


        switch (operator) {

            .Plus => {
                const left_len = left_str.len;
                const right_len = right_str.len;
                const new_len = left_len + right_len;
                log.debug("{} + {} = {}", .{left_len, right_len, new_len});

                const allocator = left.string.allocator;
                
                const str = try allocator.alloc(u8, new_len);
                errdefer allocator.free(str);

                @memcpy(str[0..left_len], left_str);
                @memcpy(str[left_len..new_len], right_str);

                const str_obj = try allocator.create(StringObject);

                str_obj.* = StringObject {
                    .allocator = allocator,
                    .value = str,
                    .rc = 0,
                };

                return Object {
                    .string = str_obj
                };

            },

            .Eq => {

                if (std.mem.eql(u8, left_str, right_str)) {

                    return Object {
                        .boolean = true,
                    };

                } else {
                    return Object {
                        .boolean = false,
                    };
                }
            },

            .Neq => {

                if (std.mem.eql(u8, left_str, right_str)) {

                    return Object {
                        .boolean = false,
                    };

                } else {
                    return Object {
                        .boolean = true,
                    };
                }

            },

            inline else => |op| {

                var buffer: [1024]u8 = undefined; // Stack allocated
                const panic_str = try std.fmt.bufPrint(&buffer, "Operand {} is unsuppered for strings", .{op});

                @panic(panic_str);
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

fn EvalStringExpr(se: *const StringExpression) EvalError!StringObject {

    const allocator = se.token.allocator;

    // TODO: move to StringObject.init(allocator, str: []const u8)
    const str = try allocator.alloc(u8, se.value.len);
    @memcpy(str, se.value);

    return StringObject {
        .allocator = allocator,
        .value = str
    };


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
        var env = try Environment.init(allocator);
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

        var env = try Environment.init(allocator);
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
        var env = try Environment.init(allocator);
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
        var env = try Environment.init(allocator);
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

        var env = try Environment.init(allocator);
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

        var env = try Environment.init(allocator);
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

    var env = try Environment.init(allocator);
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

        var env = try Environment.init(allocator);
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

test "multi input fn appl" {

    const allocator = std.testing.allocator;

    const inputs = [_][]const u8{ 
        "let add = fn(x, y) { x + y; };",
        "add(5, 5);", // breaks everything
    };

    var env = try Environment.init(allocator);
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

    var env = try Environment.init(allocator);
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


test "eval counter p.150" {
    const allocator = std.testing.allocator;

    const input = 
        \\let counter = fn(x) { 
        \\  if (x > 100) {
        \\      return true; 
        \\  } else {
        \\      let foobar = 9999;
        \\      counter(x + 1);
        \\  }
        \\};
        \\counter(0);
    ;

    // let counter = fn(x) { if (x > 997) { return x; } else { let foobar = 9999; counter(x + 1); } };

    var env = try Environment.init(allocator);
    defer env.deinit();
    const maybe_eval = try testEval(&env, input);

    if (maybe_eval) |evaluated| {
        defer evaluated.deinit();
        try expect(evaluated.boolean);
    } else {
        return error.FailedEvalLet;
    }

}

test "String" {
    
    const allocator = std.testing.allocator;

    const input = 
        \\let greeting = "Hello world!";
        \\greeting
    ;

    var env = try Environment.init(allocator);
    defer env.deinit();

    const maybe_eval = try testEval(&env, input);

    if (maybe_eval) |evaluated| {
        defer evaluated.deinit();
        try expect(evaluated == .string);

        const eval_str = try evaluated.inspect(allocator);
        defer allocator.free(eval_str);

        try expectEqualStrings("Hello world!", eval_str);

    } else {
        return error.FailedEvalString;
    }


}


test "String concat" {
    const allocator = std.testing.allocator;

    const input = 
        \\"Hello" + " " + "World!"
    ;


    var env = try Environment.init(allocator);
    defer env.deinit();

    const maybe_eval = try testEval(&env, input);

    if (maybe_eval) |evaluated| {
        defer evaluated.deinit();
        try expect(evaluated == .string);

        const eval_str = try evaluated.inspect(allocator);
        defer allocator.free(eval_str);

        try expectEqualStrings("Hello World!", eval_str);

    } else {
        return error.FailedEvalString;
    }

}

// TODO: Implemtent test for str - str p.158

// TODO add tests for 
//    >> let add = fn(a, b) { a + b };
//    >> let sub = fn(a, b) { a - b };
//    >> let applyFunc = fn(a, b, func) { func(a, b) };
//    >> applyFunc(2, 2, add);
//    4
//    >> applyFunc(10, 2, sub);
//    8
