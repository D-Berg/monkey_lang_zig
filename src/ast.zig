const Token = @import("Token.zig");
const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const log = std.log.scoped(.@"ast");

const ExprIdx = usize;
const StateIdx = usize;

// TODO: remove
// pub const Node = union(enum) { statement: Statement, expression: Expression };


// Statements ---------------------------------------------------------------------
pub const Statement = union(enum) {
    let_stmt: LetStatement,
    ret_stmt: ReturnStatement,
    // need deallocations
    expr_stmt: ExpressionStatement,
    blck_stmt: BlockStatement, // hold arraylist to idxs to statements

    pub fn deinit(stmt: *const Statement, allocator: Allocator) void {
        switch (stmt.*) {
            inline else => |case| case.deinit(allocator),
        }
    }

    pub fn clone(stmt: *const Statement) Allocator.Error!Statement {
        switch (stmt.*) {
            .blck_stmt => {
                @panic("blck_stmt.clone() should maybe not be called");
            },
            inline else => |sub_stmt| {
                return try sub_stmt.clone();
            },
        }
    }

    pub fn tokenLiteral(stmt: *const Statement) []const u8 {
        switch (stmt.*) {
            inline else => |*case| return case.token.literal,
        }
    }

    /// Calls sub_stmt.String()
    pub fn String(stmt: *const Statement, allocator: Allocator) Allocator.Error![]const u8 {
        switch (stmt.*) {
            inline else => |case| return try case.String(allocator),
        }
    }
};

// Statements
pub const LetStatement = struct {
    token: Token, // the Let TokenLiteral
    name: Identifier,
    value: *const Expression, //TODO: remove null

    pub fn deinit(ls: *const LetStatement, allocator: Allocator) void {
        // print("deinits let stmt\n", .{});
        ls.value.deinit(allocator);
        allocator.destroy(ls.value);
    }

    pub fn clone(ls: *const LetStatement) Allocator.Error!Statement {
        const value_ptr = try ls.allocator.create(Expression);
        value_ptr.* = try ls.value.clone();

        return Statement{ .let_stmt = .{
            .allocator = ls.allocator,
            .token = ls.token,
            .name = try ls.name.clone(),
            .value = value_ptr,
        } };
    }

    pub fn String(ls: *const LetStatement, allocator: Allocator) Allocator.Error![]const u8 {
        const value = ls.value;
        const value_str = try value.String(allocator);
        defer allocator.free(value_str);

        const str = try std.fmt.allocPrint(allocator, "let {s} = {s};", .{ ls.name.tokenLiteral(), value_str });

        return str;
    }
};

pub const ReturnStatement = struct {
    token: Token, // A Return token
    value: *const Expression,

    pub fn deinit(rs: *const ReturnStatement, allocator: Allocator) void {
        rs.value.deinit(allocator);
        allocator.destroy(rs.value);
    }
    // TODO: deinit
    pub fn clone(rs: *const ReturnStatement) Allocator.Error!Statement {
        const value_ptr = try rs.allocator.create(Expression);
        value_ptr.* = try rs.value.clone();

        return Statement{ .ret_stmt = .{
            .allocator = rs.allocator,
            .token = rs.token,
            .value = value_ptr,
        } };
    }

    pub fn String(rs: *const ReturnStatement, allocator: Allocator) Allocator.Error![]const u8 {
        const val_str = try rs.value.String(allocator);
        defer allocator.free(val_str);

        const str = try std.fmt.allocPrint(allocator, "return {s};", .{val_str});
        return str;
    }
};

pub const ExpressionStatement = struct {
    token: Token, // the first token of the expression
    expression: *const Expression,

    pub fn deinit(es: *const ExpressionStatement, allocator: Allocator) void {
        es.expression.deinit(allocator);
        allocator.destroy(es.expression);
    }

    pub fn clone(es: *const ExpressionStatement) Allocator.Error!Statement {
        const expr_ptr = try es.allocator.create(Expression);
        expr_ptr.* = try es.expression.clone();

        return Statement{ .expr_stmt = .{ .allocator = es.allocator, .token = try es.token, .expression = expr_ptr } };
    }

    pub fn String(es: *const ExpressionStatement, allocator: Allocator) Allocator.Error![]const u8 {
        return try es.expression.String(allocator);
    }
};

pub const BlockStatement = struct {
    token: Token,
    statements: []const Statement,

    pub fn deinit(bs: *const BlockStatement, allocator: Allocator) void {

        for (bs.statements) |stmt| {
            stmt.deinit(allocator);
        }
        allocator.free(bs.statements);

    }

    pub fn clone(self: *const BlockStatement) !BlockStatement {
        var statements = ArrayList(Statement).init(self.statements.allocator);

        for (self.statements.items) |stmt| {
            try statements.append(try stmt.clone());
        }

        return BlockStatement{
            .token = self.token,
            .statements = statements,
        };
    }

    pub fn String(bs: *const BlockStatement, allocator: Allocator) Allocator.Error![]const u8 {

        var bs_str: ArrayList(u8) = .init(allocator);
        errdefer bs_str.deinit();

        for (bs.statements) |stmt| {
            const stmt_str = try stmt.String(allocator);
            defer allocator.free(stmt_str);

            try bs_str.appendSlice(stmt_str);
        }

        return try bs_str.toOwnedSlice();
    }
};

// Expressions ----------------------------------------------------------------------
pub const Expression = union(enum) {
    identifier: Identifier,
    integer_literal: IntegerLiteralExpression,
    boolean_literal: BooleanLiteralExpression,
    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,
    if_expression: IfExpression, // deinit
    fn_literal: FnLiteralExpression, // deinit
    call_expression: CallExpression,
    string_expression: StringExpression,
    array_literal_expr: ArrayLiteralExpression,
    index_expr: IndexExpression,
    dictionary: DictionaryExpression,

    pub fn deinit(expr: *const Expression, allocator: Allocator) void {
        switch (expr.*) {
            .identifier,
            .integer_literal, 
            .boolean_literal, 
            .string_expression => {},
            inline else => |case| case.deinit(allocator),
        }
    }

    pub fn clone(expr: *const Expression) Allocator.Error!Expression {
        switch (expr.*) {
            .identifier => |ident| {
                return Expression{ .identifier = try ident.clone() };
            },
            inline else => |case| return try case.clone(),
        }
    }

    /// Calls respective sub_expr.String method and returns a string.
    /// String need to be deallocated by caller.
    pub fn String(expr: *const Expression, allocator: Allocator) Allocator.Error![]const u8 {
        switch (expr.*) {
            inline else => |case| {
                return try case.String(allocator);
            },
        }
    }
};

// TODO: check if need to be public
pub const IntegerLiteralExpression = struct {
    token: Token,
    value: u32,

    pub fn clone(int_lit_expr: *const IntegerLiteralExpression) !Expression {
        return Expression{ .integer_literal = .{ .token = try int_lit_expr.token, .value = int_lit_expr.value } };
    }

    pub fn String(ile: *const IntegerLiteralExpression, allocator: Allocator) Allocator.Error![]const u8 {
        const str = try std.fmt.allocPrint(allocator, "{}", .{ile.value});
        return str;
    }
};

pub const BooleanLiteralExpression = struct {
    token: Token,
    value: bool,

    pub fn clone(bool_lit_expr: *const BooleanLiteralExpression) !Expression {
        return Expression {
            .boolean_literal = .{
                .token = bool_lit_expr.token,
                .value = bool_lit_expr.value
            }
        };
    }

    pub fn String(ble: *const BooleanLiteralExpression, allocator: Allocator) Allocator.Error![]const u8 {
        const str = try std.fmt.allocPrint(allocator, "{}", .{ble.value});
        return str;
    }
};

pub const PrefixExpression = struct {
    token: Token,
    right: *const Expression,

    pub fn deinit(pe: *const PrefixExpression, allocator: Allocator) void {
        pe.right.deinit(allocator);
        allocator.destroy(pe.right);
    }

    pub fn clone(pe: *const PrefixExpression) Allocator.Error!Expression {
        const right_ptr = try pe.allocator.create(Expression);
        right_ptr.* = try pe.right.clone();

        return Expression {
            .prefix_expression = .{
                .allocator = pe.allocator,
                .token = pe.token,
                .right = right_ptr
            }
        };
        
    }

    /// Returns a String for PrefixExpression.
    /// Need to be freed by caller.
    pub fn String(pe: *const PrefixExpression, allocator: Allocator) Allocator.Error![]const u8 {

        const right_str = try pe.right.String(allocator);
        defer allocator.free(right_str);

        return try std.fmt.allocPrint(allocator, "({s}{s})", .{
            pe.token.literal, right_str
        });

    }

};

pub const InfixExpression = struct {
    token: Token,
    left: *const Expression,
    right: *const Expression,

    pub fn deinit(ie: *const InfixExpression, allocator: Allocator) void {
        ie.left.deinit(allocator);
        ie.right.deinit(allocator);
        allocator.destroy(ie.left);
        allocator.destroy(ie.right);
    }

    pub fn clone(ie: *const InfixExpression) Allocator.Error!Expression {
        const left_ptr = try ie.allocator.create(Expression);
        left_ptr.* = try ie.left.clone();
        const right_ptr = try ie.allocator.create(Expression);
        right_ptr.* = try ie.right.clone();
        //TODO add err defer when allocating

        return Expression {
            .infix_expression = .{
                .allocator = ie.allocator,
                .token = ie.token,
                .left = left_ptr,
                .right = right_ptr
            }

        };

    }

    pub fn String(ie: *const InfixExpression, allocator: Allocator) Allocator.Error![]const u8 {
        const left_str = try ie.left.String(allocator);
        defer allocator.free(left_str);

        const right_str = try ie.right.String(allocator);
        defer allocator.free(right_str);

        return try std.fmt.allocPrint(allocator, "({s} {s} {s})", .{
            left_str, ie.token.literal, right_str
        });

    }
    // TODO add string method

};


// Need dealloc
pub const IfExpression = struct {
    token: Token, 
    condition: *const Expression,
    consequence: BlockStatement,
    alternative: ?BlockStatement,


    pub fn deinit(ie: *const IfExpression, allocator: Allocator) void {
        
        ie.condition.deinit(allocator);
        allocator.destroy(ie.condition);

        ie.consequence.deinit(allocator);
        
        if (ie.alternative) |alt| alt.deinit(allocator);
    }

    pub fn clone(ie: *const IfExpression) Allocator.Error!Expression {
        const condition_ptr = try ie.allocator.create(Expression);
        condition_ptr.* = try ie.condition.clone();
        
        var new_alt: ?BlockStatement = null;
        
        if (ie.alternative) |alt| {
            new_alt = try alt.clone();
        } 

        return Expression {
            .if_expression = .{
                .allocator = ie.allocator,
                .token = ie.token,
                .condition = condition_ptr,
                .consequence = try ie.consequence.clone(),
                .alternative = new_alt,
            }
        };

    }

    pub fn String(if_expr: *const IfExpression, allocator: Allocator) Allocator.Error![]const u8 {
                const condition_str = try if_expr.condition.String(allocator);
                defer allocator.free(condition_str);
                
                const consequence_str = try if_expr.consequence.String(allocator);
                defer allocator.free(consequence_str);
                const l_b = "{";
                const r_b = "}";

                // TODO: get string for BlockStatement
                if (if_expr.alternative) |alt| {
                    const alt_str = try alt.String(allocator);
                    defer allocator.free(alt_str);

                    const format = "if ({s}) {s} {s} {s} else {s} {s} {s}";
                    
                    return try std.fmt.allocPrint(allocator, format, .{
                        condition_str, 
                        l_b,consequence_str, r_b,
                        l_b, alt_str, r_b
                    });

                } else {
                    const format = "if ({s}) {s} {s} {s}";

                    return try std.fmt.allocPrint(allocator, format, .{
                        condition_str, 
                        l_b, consequence_str, r_b
                    });
                }
    }

};

// need dealloc
pub const FnLiteralExpression = struct {
    token: Token,
    parameters: []const Identifier,
    body: BlockStatement,

    pub fn deinit(fe: *const FnLiteralExpression, allocator: Allocator) void {

        allocator.free(fe.parameters);

        fe.body.deinit(allocator);
    }

    pub fn clone(fe: *const FnLiteralExpression) Allocator.Error!Expression {
        var parameters = ArrayList(Identifier).init(fe.parameters.allocator);

        for (fe.parameters.items) |p| {
            try parameters.append(try p.clone());
        }

        return Expression{ .fn_literal = .{
            .token = fe.token,
            .body = try fe.body.clone(),
            .parameters = parameters,
        } };
    }

    // TODO: use ArrayList(u8)
    pub fn String(fle: *const FnLiteralExpression, allocator: Allocator) Allocator.Error![]const u8 {

        var str_len: usize = 0;
        var params_str = try allocator.alloc(u8, 0);
        defer allocator.free(params_str);

        const n_params = fle.parameters.len;

        for (fle.parameters, 0..) |p, i| {
            const p_str = try p.String(allocator);
            defer allocator.free(p_str);

            if (i == n_params - 1) {
                params_str = try allocator.realloc(params_str, str_len + p_str.len);
                @memcpy(params_str[(str_len)..(str_len + p_str.len)], p_str);
                str_len += p_str.len;
            } else {
                params_str = try allocator.realloc(params_str, str_len + p_str.len + 2);
                params_str[str_len + p_str.len] = ',';
                params_str[str_len + p_str.len + 1] = ' ';

                @memcpy(params_str[(str_len)..(str_len + p_str.len)], p_str);
                str_len += p_str.len + 2;
            }
        }
        
        const body_str = try fle.body.String(allocator);
        defer allocator.free(body_str);

        const l_b = "{";
        const r_b = "}";

        return try std.fmt.allocPrint(allocator, "fn({s}) {s} {s} {s}", .{
            params_str, l_b, body_str, r_b
        });
        
    }
    
};


pub const CallExpression = struct {
    token: Token, // the '('
    function: *const Expression, // fnlit or ident expr
    args: [] const Expression, // TODO: use slice

    pub fn deinit(ce: *const CallExpression, allocator: Allocator) void {
        ce.function.deinit(allocator);
        allocator.destroy(ce.function);

        for (ce.args) |arg| {
            arg.deinit(allocator);
        }
        
        allocator.free(ce.args);
    }

    pub fn clone(ce: *const CallExpression) Allocator.Error!Expression {
        
        const func_ptr = try ce.allocator.create(Expression);
        func_ptr.* = try ce.function.clone();
        
        var args = ArrayList(Expression).init(ce.allocator);
        
        for (ce.args.items) |arg| {
            try args.append(try arg.clone());

        }

        return Expression {
            .call_expression = .{
                .allocator = ce.allocator,
                .token = ce.token,
                .function = func_ptr,
                .args = args

            }
            
        };

    }

    pub fn String(ce: *const CallExpression, allocator: Allocator) Allocator.Error![]const u8 {
        
        const fn_str = try ce.function.String(allocator);
        defer allocator.free(fn_str);

        var result_str: ArrayList(u8) = .init(allocator);
        errdefer result_str.deinit();

        try result_str.appendSlice(fn_str);
        try result_str.append('(');

        const n_args = ce.args.len;

        for (ce.args, 0..) |arg_expr, i| {

            const arg_str = try arg_expr.String(allocator);
            defer allocator.free(arg_str);

            try result_str.appendSlice(arg_str);

            if (i != n_args - 1) try result_str.appendSlice(", ");

        }

        try result_str.append(')');

        return result_str.toOwnedSlice();
    }

};

pub const StringExpression = struct {
    token: Token,
    value: []const u8,

    pub fn clone(se: *const StringExpression) Allocator.Error!Expression {
        const cloned_token = se.token;
        
        return Expression {
            .string_expression = .{
                .token = cloned_token,
                .value = cloned_token.literal
            }
        };

    }

    pub fn String(se: *const StringExpression, allocator: Allocator) Allocator.Error![]const u8 {

        const str = try allocator.alloc(u8, se.token.literal.len);
        @memcpy(str, se.value);
        return str;

    }

};


pub const ArrayLiteralExpression = struct {
    token: Token, // [
    elements: []const Expression,

    pub fn deinit(array_lit: *const ArrayLiteralExpression, allocator: Allocator) void {

        for (array_lit.elements) |expr| expr.deinit(allocator);

        allocator.free(array_lit.elements);

    }

    pub fn clone(array_lit: *const ArrayLiteralExpression) Allocator.Error!Expression {
        _ = array_lit;
        @panic("clone for array_literal_expr not implemented");

    }

    pub fn String(array_lit: *const ArrayLiteralExpression, allocator: Allocator) Allocator.Error![]const u8 {

        var elem_str: ArrayList(u8) = .init(allocator);
        errdefer elem_str.deinit();
    
        try elem_str.append('[');

        const n_elem = array_lit.elements.len;

        for (array_lit.elements, 0..) |elem_expr, i| {

            const e_str = try elem_expr.String(allocator);
            defer allocator.free(e_str);
            
            try elem_str.appendSlice(e_str);
            
            if (i != n_elem - 1) try elem_str.appendSlice(", ");

        }

        try elem_str.append(']');

        return try elem_str.toOwnedSlice();
    }
};

pub const DictionaryExpression = struct {
    token: Token,
    keys: []const Expression,
    values: []const Expression,

    fn deinit(dictionary: *const DictionaryExpression, allocator: Allocator) void {

    
        for (dictionary.keys, dictionary.values) |key, value| {
            key.deinit(allocator);
            value.deinit(allocator);
        }
        
        allocator.free(dictionary.keys);
        allocator.free(dictionary.values);
    }
    
    fn String(dictionary: *const DictionaryExpression, allocator: Allocator) Allocator.Error![]const u8 {

        var str: ArrayList(u8) = .init(allocator);

        const n_items = dictionary.keys.len;

        const writer = str.writer();

        try str.appendSlice("{ ");

        for (dictionary.keys, dictionary.values, 0..) |key, val, i| {
            const key_str = try key.String(allocator);
            defer allocator.free(key_str);

            const val_str = try val.String(allocator);
            defer allocator.free(val_str);
            
            try writer.print("{s}: {s}", .{key_str, val_str});

            if ((i + 1) != n_items) {
                try writer.print(", ", .{});
            }

        }

        try str.appendSlice(" }");
        
        return try str.toOwnedSlice();

    }

    fn clone(dictionary: *const DictionaryExpression) Allocator.Error!Expression {

        _ = dictionary;

        @panic("unimplemented clone for DictionaryExpression");

    }

};

pub const IndexExpression = struct {
    token: Token,
    left: *Expression,
    index: *Expression,

    pub fn deinit(ie: *const IndexExpression, allocator: Allocator) void {

        ie.left.deinit(allocator);
        allocator.destroy(ie.left);
        
        ie.index.deinit(allocator);
        allocator.destroy(ie.index);
    }

    pub fn clone(ie: *const IndexExpression) Allocator.Error!Expression {
        _ = ie;

        @panic("clone for IndexExpression not implemented");

    }

    /// Returns a String that needs to be deallocated by caller.
    pub fn String(ie: *const IndexExpression, allocator: Allocator) Allocator.Error![]const u8 {
        
        const left_str = try ie.left.String(allocator);
        defer allocator.free(left_str);

        const index_str = try ie.index.String(allocator);
        defer allocator.free(index_str);

        return try std.fmt.allocPrint(allocator, "({s}[{s}])", .{left_str, index_str});
    }

};


// Identifier --------------------------------------------------------------
pub const Identifier = struct {
    token: Token,

    pub fn clone(ident: *const Identifier) Allocator.Error!Identifier {
        return Identifier{
            .token = ident.token,
        };
    }

    pub fn tokenLiteral(ident: *const Identifier) []const u8 {
        return ident.token.literal;
    }

    pub fn String(ident: *const Identifier, allocator: Allocator) Allocator.Error![]const u8 {
        return try std.fmt.allocPrint(allocator, "{s}", .{ident.token.literal});
    }
};

// TODO: Move to its own file
pub const Program = struct {
    statements: []const Statement = &[_]Statement{}, //

    pub fn deinit(program: *Program, allocator: Allocator) void {

        for (program.statements) |stmt| {
            stmt.deinit(allocator);
        }

        allocator.free(program.statements);
    }

    /// String need to be deallocated by caller
    pub fn String(program: *Program, allocator: Allocator) Allocator.Error![]const u8 { // TODO: take allocator as arg

        var prog_str: ArrayList(u8) = .init(allocator);
        errdefer prog_str.deinit();

        for (program.statements) |stmt| {
            const stmt_str = try stmt.String(allocator);
            defer allocator.free(stmt_str);

            try prog_str.appendSlice(stmt_str);
        }

        return try prog_str.toOwnedSlice();
    }

    // TODO: check if needeed
    fn TokenLiteral(program: *Program) []const u8 {
        if (program.statements.len > 0) {
            return program.statements[0].TokenLiteral();
        } else {
            return "";
        }
    }
};
