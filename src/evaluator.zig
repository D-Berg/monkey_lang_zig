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
const Environment = @import("Environment.zig");
const BuiltIn = @import("BuiltIn.zig");

const OPTIMIZE_MODE = @import("builtin").mode;

const Object = object.Object;
const FuncionObject = object.FunctionObject;
const StringObject = object.StringObject;
const ArrayObject = object.ArrayObject;

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
const ArrayLiteralExpression = ast.ArrayLiteralExpression;
const IndexExpression = ast.IndexExpression;

const Program = ast.Program;
const ArrayList = std.ArrayList;

const expect = std.testing.expect;
const expectEqualStrings = std.testing.expectEqualStrings;

pub const EvalError = error{
    // TODO: fill this out
    FailedEvalLet,
    FailedEvalString,
    EvalIdentNonExistent,
    EvalUnsupportedIndexType,
    Unimplemented
} || Allocator.Error || std.fmt.BufPrintError || BuiltIn.BuiltInError;


/// Returns an Object that needs to be deinitiated or null
pub fn eval(allocator: Allocator, program: *Program, env: *Environment) EvalError!?Object {

    var maybe_result: ?object.Object = null;

    const prg_str = try program.String(allocator);
    defer allocator.free(prg_str);
    // print("\nprog str: {s}\n", .{prg_str});

    log.debug("main env {*}\n", .{env});

    for (program.statements) |stmt| {
        maybe_result = try evalStatement(allocator, &stmt, env);

        if (maybe_result) |result| {
            if (result == .return_val_obj) {

                // print("got a return obj\n", .{});

                defer result.return_val_obj.deinit(allocator);
                const val = result.return_val_obj.value.*;

                // print("returning obj = {}\n", .{val});

                return val;
            }
        }
    }

    return maybe_result;
}

fn evalStatement(allocator: Allocator, stmt: *const Statement, env: *Environment) EvalError!?object.Object {
    switch (stmt.*) {
        .let_stmt => |*ls| {
            try evalLetStatement(allocator, ls, env);
            return null;
        },
        .ret_stmt => |*rs| {
            return try evalReturnStatement(allocator, rs, env);
        },

        .expr_stmt => |es| {
            // print("evaluating expression stmt\n", .{});
            return try evalExpression(allocator, es.expression, env);
        },

        .blck_stmt => |bs| {
            return try evalBlockStatement(allocator, &bs, env);
        },
    }
}

fn evalLetStatement(allocator: Allocator, ls: *const LetStatement, env: *Environment) EvalError!void {
    var ident = ls.name;
    const name = ident.tokenLiteral();

    log.debug("Evaluating let stmt: {s}\n", .{name});

    const maybe_val = try evalExpression(allocator, ls.value, env);
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

fn evalReturnStatement(allocator: Allocator, rs: *const ReturnStatement, env: *Environment) EvalError!Object {

    // print("evaluating  return stmt\n", .{});
    const val = try evalExpression(allocator, rs.value, env);
    errdefer if (val) |v| v.deinit(allocator);

    const res = try allocator.create(Object);
    // TODO: handle null
    res.* = val.?;

    return object.Object{
        .return_val_obj = .{ 
            .value = res, 
            .owner = null 
        }, // somehow this works lol
    };
}

fn evalExpression(allocator: Allocator, expr: *const Expression, env: *Environment) EvalError!?object.Object {
    switch (expr.*) {
        .identifier => |*ident| {
            return try EvalIdentExpr(ident, env);
        },
        .integer_literal => |int_lit| {

            // print("eval int lit expr {s}\n", .{int_lit.token.tokenLiteral()});

            return object.Object{
                .integer = @intCast(int_lit.value),
            };
        },
        .boolean_literal => |bool_lit| {

            // print("eval boolean lit expr with val {}\n", .{bool_lit.value});
            return object.Object{
                .boolean = bool_lit.value,
            };
        },

        .prefix_expression => |*pe| {
            return try evalPrefixExpr(allocator, pe, env);
        },

        .infix_expression => |*ie| {
            return try evalInfixExpr(allocator, ie, env);
        },

        .if_expression => |ie| {
            return try evalIfExpression(allocator, &ie, env);
        },

        .fn_literal => |*fl| {
            const fn_obj_ptr = try env.store.allocator.create(FuncionObject);
            errdefer env.store.allocator.destroy(fn_obj_ptr);
            fn_obj_ptr.* = try EvalFnExpr(allocator, fl, env);

            log.debug("created fn obj {*}\n", .{fn_obj_ptr});

            return Object{
                .function = fn_obj_ptr,
            };
        },

        .call_expression => |*ce| {
            return try evalCallExpression(allocator, ce, env);
        },

        .string_expression => |*se| {
            const str_obj_ptr = try allocator.create(StringObject);
            errdefer allocator.destroy(str_obj_ptr);
            str_obj_ptr.* = try evalStringExpression(allocator, se);

            return Object{ .string = str_obj_ptr };
        },
        
        .array_literal_expr => |*ale| {

            const array_ptr = try allocator.create(ArrayObject);
            errdefer allocator.destroy(array_ptr);

            array_ptr.* = try evalArrayExpression(allocator, ale, env);
            
            return Object{
                .array = array_ptr
            };
        },

        .index_expr => |*ie| {

            return try evalIndexExpression(allocator, ie, env);

            
        },

        .dictionary => |*dict| {
            _ = dict;

            return EvalError.Unimplemented;

        }



    }
}

/// Retrieves a cloned obj from env
fn EvalIdentExpr(ident: *const Identifier, env: *Environment) EvalError!Object {

    const ident_name = ident.token.literal;

    const maybe_val = env.get(ident_name);

    // print("getting ident name: {s} = {?}\n", .{ident_name, maybe_val});

    if (maybe_val) |val| {
        // print("Retreived {s} = {}\n", .{ident_name, val});
        return val;
    }

    if (BuiltIn.getBuiltInFn(ident_name)) |built_in| {
        return Object{ .built_in = built_in };
    } else {

        // TODO: create a eval error

        return EvalError.EvalIdentNonExistent;
    }
}

fn EvalFnExpr(allocator: Allocator, fl: *const FnLiteralExpression, env: *Environment) EvalError!FuncionObject {

    // print("outer env = {?}\n", .{env.outer});

    // Clone func expr param identifiers to func obj

    var params = ArrayList(Identifier).init(allocator);
    errdefer params.deinit();
    // TODO: errdefer deinit

    for (fl.parameters) |p| {
        try params.append(p);
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
        .params = try params.toOwnedSlice(),
        .body = try fl.body.clone(allocator),
        .env = env,
    };
}

fn evalCallExpression(allocator: Allocator, ce: *const CallExpression, env: *Environment) EvalError!?Object {
    const maybe_func = try evalExpression(allocator, ce.function, env);

    // TODO: handle null
    const func = maybe_func.?;
    defer func.deinit(allocator); // only deinit if fnc dont have a owner

    // const fn_obj_str = try func.function.String();
    // defer func.function.allocator.free(fn_obj_str);

    // print("\ncalling func {s}\n", .{fn_obj_str});

    var args = ArrayList(Object).init(allocator);
    defer {
        for (args.items) |arg| {
            arg.deinit(allocator);
        }
        args.deinit();
    }

    // evalExpressions p.144
    for (ce.args) |*arg| {
        try args.append((try evalExpression(allocator, arg, env)).?);
    }

    switch (func) {
        .built_in => |kind| {
            return try BuiltIn.Execute(allocator, kind, &args);
        },
        .function => |func_obj| return try applyFunction(allocator, func_obj, &args),
        else => {
            // TODO: return error 
            @panic("call failed because func is not a function");
        },
    }
}

fn applyFunction(allocator: Allocator, func: *FuncionObject, args: *ArrayList(Object)) EvalError!?Object {


    log.debug("\napplying func {*}\n", .{func});
    defer log.debug("funished applying func {*}\n", .{func});

    // print("function = {}\n", .{func});

    const extendedEnv = try allocator.create(Environment);
    errdefer allocator.destroy(extendedEnv);
    extendedEnv.* = try func.env.initClosedEnv();
    errdefer extendedEnv.deinit();
    log.debug("Creating Extended env, has address {*}\n", .{extendedEnv});

    func.env = extendedEnv;
    func.env.rc += 1;

    if (func.env.outer) |outer| {
        outer.rc -= 1;
    }

    log.debug("func {*} has env: {*}\n", .{ func, func.env });

    defer {
        // print("closing extendEnv at {*}\n", .{extendedEnv});

        // func.env = extendedEnv.outer.?;
        // print("set func {*} env to {*}\n", .{func, func.env});
        // extendedEnv.deinit();
        // func.allocator.destroy(extendedEnv);
    }

    // print("outer env has adress {*}\n", .{func.env.outer.?});

    log.debug("n_params = {}, n_args = {}\n", .{ func.params.len, args.items.len });
    std.debug.assert(args.items.len == func.params.len);

    for (func.params, args.items) |*p, arg| {
        const name = p.token.literal;

        log.debug("putting param: {s} = {} in env {*}\n", .{ name, arg, func.env });

        // TODO: Clone arg since its deinited

        var cloned_arg = try arg.clone(allocator);
        try func.env.put(name, &cloned_arg);
    }

    // print("printing functions block statements\n", .{});
    if (OPTIMIZE_MODE == .Debug ) {
        for (func.body.statements) |stmt| {
            const stmt_str = try stmt.String(allocator);
            defer allocator.free(stmt_str);
            // print("body smt: {s}\n", .{stmt_str});
        }
    }

    // Failes on new line because BlockStatement has indices to old program
    log.debug("Evaluating functions blck stmts\n", .{});
    const maybe_evaluated = try evalBlockStatement(allocator, &func.body, extendedEnv);

    // unwrap
    if (maybe_evaluated) |evaluated| {
        if (evaluated == .return_val_obj) {
            // TODO: do I need to deinit
            defer evaluated.return_val_obj.deinit(allocator);
            return evaluated.return_val_obj.value.*;
        }
    }

    return maybe_evaluated;
}

fn evalPrefixExpr(allocator: Allocator, pe: *const PrefixExpression, env: *Environment) EvalError!Object {
    const operator = pe.token.kind;

    const right = (try evalExpression(allocator, pe.right, env)).?; // TODO: handle null case
    defer right.deinit(allocator);

    switch (operator) {
        .Bang => {
            // same as evalBangOperatorExpression()
            switch (right) {
                .boolean => |b| {
                    if (b) {
                        return Object{ .boolean = false };
                    } else {
                        return Object{ .boolean = true };
                    }
                },

                .nullable => {
                    return Object{ .boolean = true };
                },
                else => {
                    return Object{ .boolean = false };
                },
            }
        },

        .Minus => {
            if (right != .integer) {
                return Object{ .nullable = {} };
            }

            const val = right.integer;

            return Object{ .integer = -val };
        },

        else => {
            return Object{ .nullable = {} };
        },
    }
}

fn evalInfixExpr(allocator: Allocator, ie: *const InfixExpression, env: *Environment) EvalError!object.Object {


    const operator = ie.token.kind;

    const ie_str = try ie.String(allocator);
    defer allocator.free(ie_str);
    // print("infix_expression = {s}\n", .{ie_str});

    // TODO: handle null cases
    const maybe_left = try evalExpression(allocator, ie.left, env);
    const left = maybe_left.?;
    defer left.deinit(allocator);

    const right = (try evalExpression(allocator, ie.right, env)).?;
    defer right.deinit(allocator);

    if (left == .integer and right == .integer) {
        const left_val = left.integer;
        const right_val = right.integer;

        switch (operator) {
            .Plus => {
                return object.Object{ .integer = left_val + right_val };
            },
            .Minus => {
                return object.Object{ .integer = left_val - right_val };
            },
            .Asterisk => {
                return object.Object{ .integer = left_val * right_val };
            },
            .Slash => {
                return object.Object{ .integer = @divTrunc(left_val, right_val) };
            },
            .Lt => {
                return object.Object{ .boolean = left_val < right_val };
            },

            .Gt => {
                return object.Object{ .boolean = left_val > right_val };
            },
            .Eq => {
                return object.Object{ .boolean = left_val == right_val };
            },
            .Neq => {
                return object.Object{ .boolean = left_val != right_val };
            },
            else => {
                return object.Object{ .nullable = {} };
            },
        }
    }

    if (left == .boolean and right == .boolean) {
        const left_val = left.boolean;
        const right_val = right.boolean;

        switch (operator) {
            .Eq => {
                return object.Object{ .boolean = left_val == right_val };
            },
            .Neq => {
                return object.Object{ .boolean = left_val != right_val };
            },
            else => {
                return object.Object{ .nullable = {} };
            },
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
                log.debug("{} + {} = {}", .{ left_len, right_len, new_len });

                const str = try allocator.alloc(u8, new_len);
                errdefer allocator.free(str);

                @memcpy(str[0..left_len], left_str);
                @memcpy(str[left_len..new_len], right_str);

                const str_obj = try allocator.create(StringObject);

                str_obj.* = StringObject {
                    .value = str,
                    .rc = 0,
                };

                return Object{ .string = str_obj };
            },

            .Eq => {
                if (std.mem.eql(u8, left_str, right_str)) {
                    return Object{
                        .boolean = true,
                    };
                } else {
                    return Object{
                        .boolean = false,
                    };
                }
            },

            .Neq => {
                if (std.mem.eql(u8, left_str, right_str)) {
                    return Object{
                        .boolean = false,
                    };
                } else {
                    return Object{
                        .boolean = true,
                    };
                }
            },

            inline else => |op| {

                // TODO: make a err for monkey

                var buffer: [1024]u8 = undefined; // Stack allocated
                const panic_str = try std.fmt.bufPrint(&buffer, "Operand {} is unsuppered for strings", .{op});

                @panic(panic_str);
            },
        }
    }

    return object.Object{ .nullable = {} };
}

fn evalBlockStatement(allocator: Allocator, blck_stmt: *const ast.BlockStatement, env: *Environment) EvalError!?object.Object {
    var maybe_result: ?object.Object = null;

    // print("evaluating block stmts\n", .{});
    // defer print("finished eval of block\n", .{});

    for (blck_stmt.statements) |*stmt| {

        // print("\tblck: evaluating {}\n", .{stmt});
        maybe_result = try evalStatement(allocator, stmt, env);

        if (maybe_result) |result| {
            if (result != .nullable and result == .return_val_obj) {
                return result; // p.131
            }
        }
    }

    return maybe_result;
}

fn evalIfExpression(allocator: Allocator, if_epxr: *const ast.IfExpression, env: *Environment) EvalError!?object.Object {

    // TODO: handle null 
    const condition = (try evalExpression(allocator, if_epxr.condition, env)).?;

    if (isTruthy(&condition)) {
        return try evalBlockStatement(allocator, &if_epxr.consequence, env);
    } else {
        if (if_epxr.alternative) |alt| {
            return try evalBlockStatement(allocator, &alt, env);
        } else {
            return object.Object{ .nullable = {} };
        }
    }
}

fn evalArrayExpression(allocator: Allocator, array_expr: *const ArrayLiteralExpression, env: *Environment) EvalError!ArrayObject {

    var elements = ArrayList(Object).init(allocator);
    errdefer {
        for (elements.items) |e| e.deinit(allocator);
        elements.deinit();
    }

    for (array_expr.elements) |*elem| {

        const expr = try evalExpression(allocator, elem, env);
        errdefer if (expr) |e| e.deinit(allocator);
        try elements.append(expr.?); // cant be null because why put let inside an array, right?!?!

    }

    return ArrayObject {
        .elements = try elements.toOwnedSlice(),
    };


}

fn evalIndexExpression(allocator: Allocator, ie: *const IndexExpression, env: *Environment) EvalError!Object {

    const left_obj = (try evalExpression(allocator, ie.left, env)).?;
    defer left_obj.deinit(allocator);

    const index_obj = (try evalExpression(allocator, ie.index, env)).?;
    defer index_obj.deinit(allocator);

    if (index_obj == .integer and left_obj == .array) {

        const idx = index_obj.integer;
        const max = left_obj.array.elements.len;

        if (idx < 0 or idx > max) return Object{ .nullable = {} };
        
        const idx_usize: usize = @intCast(idx);
        return left_obj.array.elements[idx_usize];

    } else {
        return error.EvalUnsupportedIndexType;
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
        },
    }
}

fn evalStringExpression(allocator: Allocator, string_expression: *const StringExpression) EvalError!StringObject {

    // TODO: move to StringObject.init(allocator, str: []const u8)
    const str = try allocator.alloc(u8, string_expression.value.len);
    @memcpy(str, string_expression.value);

    return StringObject{ .value = str };
}

fn testEval(allocator: Allocator, env: *Environment, input: []const u8) !?object.Object {

    var parser = Parser.init(allocator, input);
    defer parser.deinit(allocator);

    var program = try parser.Program(allocator);
    defer program.deinit(allocator);

    return try eval(allocator, &program, env);

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

        const evaluated = (try testEval(allocator, &env, inp)).?;
        defer evaluated.deinit(allocator);

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

        const evaluated = (try testEval(allocator, &env, inp)).?;
        defer evaluated.deinit(allocator);

        expect(evaluated.boolean == ans) catch |err| {
            print("{s}\n", .{inp});
            print("Expected {}, got {}\n", .{ ans, evaluated.boolean });
            return err;
        };
    }
}

test "Bang(!) operator" {
    const allocator = std.testing.allocator;

    const inputs = [_][]const u8{
        "!true",  "!false",  "!5",
        "!!true", "!!false", "!!5",
    };
    const answers = [_]bool{ false, true, false, true, false, true };

    for (inputs, answers) |inp, ans| {
        var env = try Environment.init(allocator);
        defer env.deinit();

        const evaluated = (try testEval(allocator, &env, inp)).?;
        defer evaluated.deinit(allocator);

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


        const evaluated = (try testEval(allocator, &env, inp)).?;
        defer evaluated.deinit(allocator);


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

        const evaluated = (try testEval(allocator, &env, inp)).?;
        defer evaluated.deinit(allocator);

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

        const evaluated = (try testEval(allocator, &env, inp)).?;
        defer evaluated.deinit(allocator);

        expect(ans == evaluated.integer) catch |err| {
            print("expected {}, got {}\n", .{ ans, evaluated.integer });

            return err;
        };
    }
}

test "func object" {
    const allocator = std.testing.allocator;
    const input = "fn(x) { x + 2; };";

    var env = try Environment.init(allocator);
    defer env.deinit();

    const evaluated = (try testEval(allocator, &env, input)).?;
    defer evaluated.deinit(allocator);

    try expect(evaluated == .function);


    try expect(evaluated.function.params.len == 1);
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

        // TODO:, handle nulll
        const evaluated = (try testEval(allocator, &env, inp)).?;
        defer evaluated.deinit(allocator);

        expect(ans == evaluated.integer) catch |err| {
            print("expected {}, got {}\n", .{ ans, evaluated.integer });

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

        var parser = Parser.init(allocator, inp);
        defer parser.deinit(allocator);

        var program = try parser.Program(allocator);
        defer program.deinit(allocator);

        const evaluated = try eval(allocator, &program, &env);
        defer if (evaluated) |e| e.deinit(allocator);

        if (idx == 0) {
            expect(evaluated == null) catch |err| {
                print("expected null, got {?}\n", .{evaluated});

                return err;
            };
        }

        if (idx == 1) {
            expect(evaluated.? == .integer) catch |err| {
                print("expected integer, got {?}\n", .{evaluated});

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

    const maybe_eval = try testEval(allocator, &env, input);

    if (maybe_eval) |evaluated| {
        defer evaluated.deinit(allocator);
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
    const maybe_eval = try testEval(allocator, &env, input);

    if (maybe_eval) |evaluated| {
        defer evaluated.deinit(allocator);
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

    const maybe_eval = try testEval(allocator, &env, input);

    if (maybe_eval) |evaluated| {
        defer evaluated.deinit(allocator);
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

    const maybe_eval = try testEval(allocator, &env, input);

    if (maybe_eval) |evaluated| {
        defer evaluated.deinit(allocator);
        try expect(evaluated == .string);

        const eval_str = try evaluated.inspect(allocator);
        defer allocator.free(eval_str);

        try expectEqualStrings("Hello World!", eval_str);
    } else {
        return error.FailedEvalString;
    }
}

test "Array Lit" {

    const allocator = std.testing.allocator;

    const input = "[1, 2 * 2, 3 + 3]";

    var env = try Environment.init(allocator);
    defer env.deinit();

    const maybe_eval = try testEval(allocator, &env, input);

    if (maybe_eval) |evaluated| {
        defer evaluated.deinit(allocator);
        try expect(evaluated == .array);

        const eval_str = try evaluated.inspect(allocator);
        defer allocator.free(eval_str);

        try expectEqualStrings("[1, 4, 6]", eval_str);
    } else {
        return error.FailedEvalString;
    }
}

test "Array Index" {

    const allocator = std.testing.allocator;

    const inputs = [_][]const u8{
        "[1, 2, 3][0]",
        "[1, 2 * 2, 3 + 3][1]",
        "let i = 0; [1][i];",
        "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]"
    };

    const answers = [_]i32{
        1,
        4, 
        1,
        2
    };
    
    for (inputs, answers) |inp, ans| {

        var env = try Environment.init(allocator);
        defer env.deinit();

        const maybe_eval = try testEval(allocator, &env, inp);

        if (maybe_eval) |evaluated| {
            defer evaluated.deinit(allocator);

            try expect(evaluated == .integer);
            
            try expect(evaluated.integer == ans);

        } else {
            return error.FailedEvalString;
        }
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
