// TODO make evaluator file upercase
const std = @import("std");
const print = std.debug.print;
const log = std.log.scoped(.evaluator);

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
const DictionaryObject = object.DictionayObject;

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
const DictionaryExpression = ast.DictionaryExpression;

const Program = ast.Program;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMapUnmanaged;

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

pub const EvalError = error{
    // TODO: fill this out
    NullPrefix,
    NullLeftInfix,
    NullRightInfix,
    NullIfCondition,
    NullLeftObject,
    NullObject,
    FailedEvalLet,
    FailedEvalString,
    EvalIdentNonExistent,
    EvalUnsupportedIndexType,
    Unimplemented,
} || Allocator.Error || std.fmt.BufPrintError || BuiltIn.BuiltInError;

/// Returns an Object that needs to be deinitiated or null
pub fn eval(allocator: Allocator, program: *Program, env: *Environment) EvalError!?Object {
    if (std.options.log_level == .debug) {
        const prg_str = try program.String(allocator);
        defer allocator.free(prg_str);
        log.debug("executing program: {s}", .{prg_str});
    }

    defer log.debug("finished exucution of program", .{});

    log.debug("main env {*}\n", .{env});

    log.debug("evaluating program statements", .{});

    var maybe_result: ?Object = null;

    for (program.statements) |stmt| {
        if (std.options.log_level == .debug) {
            const stmt_str = try stmt.String(allocator);
            defer allocator.free(stmt_str);
            log.debug("evaluating stmt: {s}", .{stmt_str});
        }

        maybe_result = try evalStatement(allocator, &stmt, env);

        if (maybe_result) |result| {
            log.debug("evaluated stmt to object: {s}", .{@tagName(result)});

            if (result == .return_val_obj) {
                log.debug("got a return obj\n", .{});

                defer result.return_val_obj.deinit(allocator);
                const val = result.return_val_obj.value.*;

                // print("returning obj = {}\n", .{val});

                return val;
            }
        }

        log.debug("stmt evaluated to null", .{});
    }

    log.debug("returning null", .{});

    return maybe_result;
}

fn evalStatement(gpa: Allocator, stmt: *const Statement, env: *Environment) EvalError!?object.Object {
    switch (stmt.*) {
        .let_stmt => |*ls| {
            try evalLetStatement(gpa, ls, env);
            return null;
        },
        .ret_stmt => |*rs| {
            return try evalReturnStatement(gpa, rs, env);
        },

        .expr_stmt => |es| {
            // print("evaluating expression stmt\n", .{});
            return try evalExpression(gpa, es.expression, env);
        },

        .blck_stmt => |bs| {
            return try evalBlockStatement(gpa, &bs, env);
        },
    }
}

fn evalLetStatement(gpa: Allocator, ls: *const LetStatement, env: *Environment) EvalError!void {
    var ident = ls.name;
    const name = ident.tokenLiteral();

    log.debug("Evaluating let stmt: {s}\n", .{name});

    const maybe_val = try evalExpression(gpa, ls.value, env);
    errdefer if (maybe_val) |val| val.deinit(gpa); // deinit because store.put clones val

    // print("putting ident: {s} with val: {}\n", .{name, val.?});
    //
    if (maybe_val) |val| {
        try env.put(gpa, name, val);
    } else {
        return EvalError.FailedEvalLet;
    }

    // TODO: errors p.137

}

fn evalReturnStatement(gpa: Allocator, rs: *const ReturnStatement, env: *Environment) EvalError!Object {

    // print("evaluating  return stmt\n", .{});
    const val = try evalExpression(gpa, rs.value, env);
    errdefer if (val) |v| v.deinit(gpa);

    const res = try gpa.create(Object);
    // TODO: handle null
    res.* = val.?;

    return object.Object{
        .return_val_obj = .{ .value = res, .owner = null }, // somehow this works lol
    };
}

fn evalExpression(gpa: Allocator, expr: *const Expression, env: *Environment) EvalError!?object.Object {
    switch (expr.*) {
        .identifier => |*ident| {
            return EvalIdentExpr(ident, env);
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
            return try evalPrefixExpr(gpa, pe, env);
        },

        .infix_expression => |*ie| {
            return try evalInfixExpr(gpa, ie, env);
        },

        .if_expression => |ie| {
            return try evalIfExpression(gpa, &ie, env);
        },

        .fn_literal => |*fl| {
            const func_ptr = try gpa.create(FuncionObject);
            errdefer gpa.destroy(func_ptr);
            func_ptr.* = try EvalFnExpr(gpa, env, fl);
            return Object{
                .function = func_ptr,
            };
        },

        .call_expression => |*ce| {
            return try evalCallExpression(gpa, ce, env);
        },

        .string_expression => |*se| {
            const str_ptr = try gpa.create(StringObject);
            errdefer gpa.destroy(str_ptr);

            str_ptr.* = try evalStringExpression(gpa, se);

            return Object{
                .string = str_ptr,
            };
        },

        .array_literal_expr => |*ale| {
            const array_ptr = try gpa.create(ArrayObject);
            errdefer gpa.destroy(array_ptr);

            array_ptr.* = try evalArrayExpression(gpa, ale, env);
            return Object{ .array = array_ptr };
        },

        .index_expr => |*ie| {
            return try evalIndexExpression(gpa, ie, env);
        },

        .dictionary => |*dict_expr| {
            const dict_ptr = try gpa.create(DictionaryObject);
            errdefer gpa.destroy(dict_ptr);

            dict_ptr.* = try evalDictionaryExpression(gpa, dict_expr, env);

            return Object{
                .dictionary = dict_ptr,
            };
        },
    }
}

/// Retrieves a cloned obj from env
fn EvalIdentExpr(ident: *const Identifier, env: *Environment) ?Object {
    const ident_name = ident.token.literal;
    if (BuiltIn.getBuiltInFn(ident_name)) |built_in| {
        return Object{ .built_in = built_in };
    }

    const maybe_val = env.get(ident_name);

    // print("getting ident name: {s} = {?}\n", .{ident_name, maybe_val});

    if (maybe_val) |val| {
        // print("Retreived {s} = {}\n", .{ident_name, val});
        return val;
    } else {
        log.debug("couldn't find variable {s}", .{ident_name});
        // TODO: print error message to user to stderr
        return null;
    }
}

fn EvalFnExpr(gpa: Allocator, env: *Environment, fl: *const FnLiteralExpression) EvalError!FuncionObject {
    const params = try gpa.alloc(Identifier, fl.parameters.len);
    errdefer {
        for (params) |p| gpa.free(p.token.literal);
        gpa.free(params);
    }

    for (fl.parameters, 0..) |p, i| {
        params[i] = Identifier{
            .token = .{
                .literal = try gpa.dupe(u8, p.token.literal),
                .loc = p.token.loc,
                .kind = p.token.kind,
            },
        };
    }

    env.rc += 1; // increase rc of env since fn reference it

    return FuncionObject{
        .params = params,
        .body = try fl.body.clone(gpa), // TODO: just reference body
        .env = env,
        .rc = 0,
    };
}

fn evalCallExpression(gpa: Allocator, ce: *const CallExpression, env: *Environment) EvalError!?Object {
    if (std.options.log_level == .debug) {
        const call_str = try ce.String(gpa);
        defer gpa.free(call_str);
        log.debug("calling {s}", .{call_str});
    }

    const maybe_func = try evalExpression(gpa, ce.function, env);

    var func = maybe_func orelse return EvalError.NullObject;
    defer func.deinit(gpa); // only deinit if fnc dont have a owner

    const args = try gpa.alloc(Object, ce.args.len);
    defer {
        for (args) |arg| {
            arg.deinit(gpa);
        }
        gpa.free(args);
    }

    // evalExpressions p.144
    for (ce.args, 0..) |*arg, i| {
        args[i] = try evalExpression(gpa, arg, env) orelse
            return EvalError.NullObject;
    }

    switch (func) {
        .built_in => |kind| {
            return try BuiltIn.Execute(gpa, kind, args);
        },
        .function => |func_obj| return try applyFunction(gpa, func_obj, args),
        else => {
            // TODO: return error
            @panic("call failed because func is not a function");
        },
    }
}

fn applyFunction(gpa: Allocator, func: *FuncionObject, args: []const Object) EvalError!?Object {
    log.debug("executing func {*}\n", .{func});
    defer log.debug("finnished executing func {*}\n", .{func});

    const enclosed_env_ptr = try gpa.create(Environment);
    errdefer {
        enclosed_env_ptr.deinit(gpa);
        // allocator.destroy(enclosed_env_ptr);
    }
    enclosed_env_ptr.* = func.env.enclosed();
    func.env = enclosed_env_ptr;

    log.debug("n_params = {}, n_args = {}\n", .{ func.params.len, args.len });
    std.debug.assert(args.len == func.params.len);

    for (func.params, args) |*p, arg| {
        const name = p.token.literal;

        log.debug("putting param: {s} = {} in env {*}\n", .{ name, arg, func.env });

        // TODO: Clone arg since its deinited

        const cloned_arg = try arg.clone(gpa);
        try func.env.put(gpa, name, cloned_arg);
    }

    // Failes on new line because BlockStatement has indices to old program
    log.debug("Evaluating functions blck stmts\n", .{});
    const maybe_evaluated = try evalBlockStatement(gpa, &func.body, func.env);

    // unwrap
    if (maybe_evaluated) |evaluated| {
        if (evaluated == .return_val_obj) {
            // TODO: do I need to deinit
            defer evaluated.return_val_obj.deinit(gpa);
            return evaluated.return_val_obj.value.*;
        }
    }

    return maybe_evaluated;
}

fn evalPrefixExpr(
    gpa: Allocator,
    pe: *const PrefixExpression,
    env: *Environment,
) EvalError!Object {
    const operator = pe.token.kind;

    const right = try evalExpression(gpa, pe.right, env) orelse
        return EvalError.NullPrefix;
    defer right.deinit(gpa);

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

fn evalInfixExpr(
    allocator: Allocator,
    ie: *const InfixExpression,
    env: *Environment,
) EvalError!object.Object {
    const operator = ie.token.kind;

    const maybe_left = try evalExpression(allocator, ie.left, env);
    const left = maybe_left orelse return EvalError.NullLeftInfix;
    defer left.deinit(allocator);

    const right = try evalExpression(allocator, ie.right, env) orelse
        return EvalError.NullRightInfix;
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

                const new_str_ptr = try allocator.create(StringObject);
                errdefer allocator.destroy(new_str_ptr);

                const str_obj = StringObject{
                    .value = str,
                    .rc = 0,
                };

                new_str_ptr.* = str_obj;

                return Object{ .string = new_str_ptr };
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
                std.debug.panic("Operand {} is unsuppered for strings", .{op});
            },
        }
    }

    return object.Object{ .nullable = {} };
}

fn evalBlockStatement(
    gpa: Allocator,
    block: *const ast.BlockStatement,
    env: *Environment,
) EvalError!?object.Object {
    log.debug("evaluating block stmts", .{});
    defer log.debug("finished evaluating block", .{});

    var maybe_result: ?Object = null;

    for (block.statements) |stmt| {
        maybe_result = try evalStatement(gpa, &stmt, env);

        if (maybe_result) |result| {
            if (result == .return_val_obj) return maybe_result;
        }
    }

    return maybe_result;
}

fn evalIfExpression(
    gpa: Allocator,
    if_epxr: *const ast.IfExpression,
    env: *Environment,
) EvalError!?object.Object {
    log.debug("evaluating if expression", .{});

    if (std.options.log_level == .debug) {
        const if_str = try if_epxr.String(gpa);
        defer gpa.free(if_str);

        log.debug("if_str = '{s}'", .{if_str});
    }

    const condition = try evalExpression(gpa, if_epxr.condition, env) orelse
        return EvalError.NullIfCondition;

    if (isTruthy(&condition)) {
        log.debug("is true", .{});
        return try evalBlockStatement(gpa, &if_epxr.consequence, env);
    } else {
        log.debug("is true", .{});
        if (if_epxr.alternative) |alt| {
            log.debug("evaluating alternative", .{});
            return try evalBlockStatement(gpa, &alt, env);
        } else {
            log.debug("returning null object", .{});
            return object.Object{ .nullable = {} };
        }
    }
}

fn evalArrayExpression(
    gpa: Allocator,
    array_expr: *const ArrayLiteralExpression,
    env: *Environment,
) EvalError!ArrayObject {
    var elements: ArrayList(Object) = .empty;
    errdefer {
        for (elements.items) |e| e.deinit(gpa);
        elements.deinit(gpa);
    }

    for (array_expr.elements) |*elem| {
        const maybe_expr = try evalExpression(gpa, elem, env);
        if (maybe_expr) |expr| {
            errdefer expr.deinit(gpa);
            try elements.append(gpa, expr); // cant be null because why put let inside an array, right?!?!
        }
    }

    return ArrayObject{
        .elements = try elements.toOwnedSlice(gpa),
    };
}

fn evalIndexExpression(
    allocator: Allocator,
    ie: *const IndexExpression,
    env: *Environment,
) EvalError!Object {
    const left_obj = try evalExpression(allocator, ie.left, env) orelse
        return EvalError.NullObject;
    defer left_obj.deinit(allocator);

    const index_obj = try evalExpression(allocator, ie.index, env) orelse
        return EvalError.NullObject;
    defer index_obj.deinit(allocator);

    if (index_obj == .integer and left_obj == .array) {
        const idx = index_obj.integer;
        const max = left_obj.array.elements.len;

        if (idx < 0 or idx > max) return Object{ .nullable = {} };

        const idx_usize: usize = @intCast(idx);
        return left_obj.array.elements[idx_usize];
    } else if (index_obj == .string and left_obj == .dictionary) {
        const key = index_obj.string.value;

        if (left_obj.dictionary.inner.get(key)) |value| {
            return value;
        } else {
            return Object{ .nullable = {} };
        }
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

fn evalStringExpression(
    gpa: Allocator,
    string_expression: *const StringExpression,
) EvalError!StringObject {

    // TODO: move to StringObject.init(allocator, str: []const u8)
    const str = try gpa.alloc(u8, string_expression.value.len);
    @memcpy(str, string_expression.value);

    return StringObject{ .value = str };
}

fn evalDictionaryExpression(
    gpa: Allocator,
    dictionary_expression: *const DictionaryExpression,
    env: *Environment,
) EvalError!DictionaryObject {
    var object_hm: StringHashMap(Object) = .empty;
    errdefer {
        var object_hm_it = object_hm.iterator();
        while (object_hm_it.next()) |entry| {
            gpa.free(entry.key_ptr.*);
            entry.value_ptr.deinit(gpa);
        }
    }

    var expression_hm_it = dictionary_expression.hash_map.iterator();

    while (expression_hm_it.next()) |entry| {
        const key_str = try gpa.dupe(u8, entry.key_ptr.*);
        errdefer gpa.free(key_str);

        const value_obj = try evalExpression(gpa, entry.value_ptr, env) orelse
            return EvalError.NullObject;
        errdefer value_obj.destroy(gpa);

        switch (value_obj) {
            .function => |f| f.rc += 1,
            .string => |s| s.rc += 1,
            .array => |a| a.rc += 1,
            .dictionary => |d| d.rc += 1,
            else => {},
        }

        try object_hm.put(gpa, key_str, value_obj);
    }

    return DictionaryObject{ .inner = object_hm, .rc = 0 };
}

fn testEval(allocator: Allocator, env: *Environment, input: []const u8) !?object.Object {
    var parser = Parser.init(input);
    defer parser.deinit(allocator);

    var program = try parser.Program(allocator);
    defer program.deinit(allocator);

    return try eval(allocator, &program, env);
}

test "Eval Int expr" {
    const allocator = std.testing.allocator;

    const inputs = [_][]const u8{
        "5",
        "10",
        "-5",
        "-10",
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
        "(5 + 10 * 2 + 15 / 3) * 2 + -10",
    };
    const answers = [_]i32{ 5, 10, -5, -10, 10, 32, 0, 20, 25, 0, 60, 30, 37, 37, 50 };

    for (inputs, answers) |inp, ans| {
        var env: Environment = .empty;
        defer env.deinit(allocator);

        const evaluated = try testEval(allocator, &env, inp) orelse
            return error.NullObject;
        defer evaluated.deinit(allocator);

        expect(evaluated.integer == ans) catch |err| {
            print("{s}\n", .{inp});
            print("Expected {}, got {}\n", .{ ans, evaluated.integer });
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
        "(1 > 2) == false",
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
        true,
    };

    for (inputs, answers) |inp, ans| {
        var env: Environment = .empty;
        defer env.deinit(allocator);

        const evaluated = try testEval(allocator, &env, inp) orelse
            return EvalError.NullObject;
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
        var env: Environment = .empty;
        defer env.deinit(allocator);

        const evaluated = try testEval(allocator, &env, inp) orelse
            return EvalError.NullObject;
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
        "if (1 < 2) { 10 } else { 20 }",
    };
    const answers = [_]object.Object{
        object.Object{ .integer = 10 },
        object.Object{ .nullable = {} },
        object.Object{ .integer = 10 },
        object.Object{ .integer = 10 },
        object.Object{ .nullable = {} },
        object.Object{ .integer = 20 },
        object.Object{ .integer = 10 },
    };

    for (inputs, answers) |inp, ans| {
        var env: Environment = .empty;
        defer env.deinit(allocator);

        const evaluated = try testEval(allocator, &env, inp) orelse
            return EvalError.NullObject;
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
            },
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
        "if (10 > 1) { if (10 > 1) { return 10; } 129 return 1; }",
    };

    const answer = 10;

    for (inputs) |inp| {
        var env: Environment = .empty;
        defer env.deinit(allocator);

        const evaluated = try testEval(allocator, &env, inp) orelse
            return EvalError.NullObject;
        defer evaluated.deinit(allocator);

        expect(answer == evaluated.integer) catch |err| {
            print("expected {}, got {}\n", .{ answer, evaluated.integer });
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
        "let a = 5; let b = a; let c = a + b + 5; c;",
    };
    const answers = [_]i32{ 5, 25, 5, 15 };

    for (inputs, answers) |inp, ans| {
        var env: Environment = .empty;
        defer env.deinit(allocator);

        const evaluated = try testEval(allocator, &env, inp) orelse
            return EvalError.NullObject;
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

    var env: Environment = .empty;
    defer env.deinit(allocator);

    const evaluated = try testEval(allocator, &env, input) orelse
        return EvalError.NullObject;
    defer evaluated.deinit(allocator);

    try expect(evaluated == .function);

    try expect(evaluated.function.params.len == 1);
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
    const answers = [_]i32{
        5,
        // 5,
        // 10,
        // 10,
        // 20,
        // 5
    };

    for (inputs, answers) |inp, ans| {
        var env: Environment = .empty;
        defer env.deinit(allocator);

        const evaluated = try testEval(allocator, &env, inp) orelse
            return EvalError.NullObject;
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

    var env: Environment = .empty;
    defer env.deinit(allocator);

    for (inputs, 0..) |inp, idx| {
        var parser = Parser.init(inp);
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
    std.testing.log_level = .debug;
    const allocator = std.testing.allocator;

    const input =
        \\let newAdder = fn(x) {
        \\ fn(y) { x + y; };
        \\};
        \\
        \\let addTwo = newAdder(2);
        \\addTwo(2);
    ;
    // let newAdder = fn(x) { fn(y) { x + y; }; }; let addTwo = newAdder(2); addTwo(2);
    var env: Environment = .empty;
    defer env.deinit(allocator);

    const maybe_eval = try testEval(allocator, &env, input);

    if (maybe_eval) |evaluated| {
        defer evaluated.deinit(allocator);
        try expect(evaluated == .integer);
        try expectEqual(4, evaluated.integer);
    } else {
        print("got null back\n", .{});
        return error.FailedEvalLet;
    }
}

test "eval counter p.150" {
    std.testing.log_level = .debug;
    const allocator = std.testing.allocator;

    const input =
        \\let counter = fn(x) { 
        \\  if (x > 2) {
        \\      return true; 
        \\  } else {
        \\      let foobar = 9999;
        \\      counter(x + 1);
        \\  }
        \\};
        \\counter(0);
    ;

    var env: Environment = .empty;
    defer env.deinit(allocator);
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

    var env: Environment = .empty;
    defer env.deinit(allocator);

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

    var env: Environment = .empty;
    defer env.deinit(allocator);

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

    var env: Environment = .empty;
    defer env.deinit(allocator);

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
        "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
    };

    const answers = [_]i32{ 1, 4, 1, 2 };

    for (inputs, answers) |inp, ans| {
        var env: Environment = .empty;
        defer env.deinit(allocator);

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
