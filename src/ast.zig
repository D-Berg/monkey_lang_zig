const Token = @import("Token.zig");
const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const print = std.debug.print;

const ExprIdx = usize;
const StateIdx = usize;

pub const Node = union(enum) {
    statement: Statement,
    expression: Expression
};

pub const Statement = union(enum) {
    let_stmt: LetStatement,
    ret_stmt: ReturnStatement,
    // need deallocations
    expr_stmt: ExpressionStatement,
    blck_stmt: BlockStatement, // hold arraylist to idxs to statements
    
    pub fn deinit(stmt: *const Statement) void {
        switch (stmt.*) {
            inline else => |case| case.deinit(),
        }
    }

    pub fn clone(stmt: *const Statement) Allocator.Error!Statement {
        switch (stmt.*) {
            .blck_stmt => {@panic("blck_stmt.clone() should maybe not be called");},
            inline else => |sub_stmt| {
                return try sub_stmt.clone();
            }
        }
    }

    pub fn tokenLiteral(stmt: *const Statement) []const u8 {
        switch (stmt.*) {
            inline else => |*case| return case.token.tokenLiteral(),
        }
    }

    /// Calls sub_stmt.String()
    pub fn String(stmt: *const Statement) Allocator.Error![]const u8 {
        switch (stmt.*) {
            inline else => |case| return try case.String(),
        }
    }
};

// Statements
pub const LetStatement = struct {
    allocator: Allocator,
    token: Token, // the Let TokenLiteral
    name: Identifier,
    value: *const Expression, //TODO: remove null

    pub fn deinit(ls: *const LetStatement) void {
        // print("deinits let stmt\n", .{});
        ls.token.deinit();
        ls.name.deinit();
        ls.value.deinit();
        ls.allocator.destroy(ls.value);
    }

    pub fn clone(ls: *const LetStatement) Allocator.Error!Statement {
        
        const value_ptr = try ls.allocator.create(Expression);
        value_ptr.* = try ls.value.clone();

        return Statement{
            .let_stmt = .{
                .allocator = ls.allocator,
                .token = try ls.token.clone(),
                .name = try ls.name.clone(),
                .value = value_ptr,
            }
        };

    }

    pub fn String(ls: *const LetStatement) Allocator.Error![]const u8 {
        const value = ls.value;
        const value_str = try value.String();
        defer ls.allocator.free(value_str);

        const str = try std.fmt.allocPrint(ls.allocator, "let {s} = {s};", .{
            ls.name.tokenLiteral(),
            value_str
        });

        return str;
    }
};

pub const ReturnStatement = struct {
    allocator: Allocator,
    token: Token, // A Return token
    value: *const Expression,
    
    pub fn deinit(rs: *const ReturnStatement) void {
        rs.token.deinit();
        rs.value.deinit();
        rs.allocator.destroy(rs.value);
    }
    // TODO: deinit
    pub fn clone(rs: *const ReturnStatement) Allocator.Error!Statement {

        const value_ptr = try rs.allocator.create(Expression);
        value_ptr.* = try rs.value.clone();

        return Statement {
            .ret_stmt = .{
                .allocator = rs.allocator,
                .token = try rs.token.clone(),
                .value = value_ptr,
            }
        };
    }

    pub fn String(rs: *const ReturnStatement) Allocator.Error![]const u8 {

        const val_str = try rs.value.String();
        defer rs.allocator.free(val_str);

        const str = try std.fmt.allocPrint(rs.allocator, "return {s};", .{val_str});
        return str;

    }
};

pub const ExpressionStatement = struct {
    allocator: Allocator,
    token: Token, // the first token of the expression
    expression: *const Expression,

    pub fn deinit(es: *const ExpressionStatement) void {
        es.token.deinit();
        es.expression.deinit();
        es.allocator.destroy(es.expression);
    }

    pub fn clone(es: *const ExpressionStatement) Allocator.Error!Statement {
        
        const expr_ptr = try es.allocator.create(Expression);
        expr_ptr.* = try es.expression.clone();
        
        return Statement {
            .expr_stmt = .{
                .allocator =  es.allocator,
                .token = try es.token.clone(),
                .expression = expr_ptr
            }
        };

    }

    pub fn String(es: *const ExpressionStatement) Allocator.Error![]const u8 {
        return try es.expression.String();
    }
};

pub const BlockStatement = struct {
    token: Token,
    statements: ArrayList(Statement), 

    pub fn deinit(bs: *const BlockStatement) void {
        bs.token.deinit();

        for (bs.statements.items) |stmt| {
            stmt.deinit();
        }
        
        bs.statements.deinit();

    }
    
    pub fn clone(self: *const BlockStatement) !BlockStatement {

        var statements = ArrayList(Statement).init(self.statements.allocator);

        for (self.statements.items) |stmt| {

            try statements.append(try stmt.clone());

        }

        return BlockStatement {
            .token = try self.token.clone(),
            .statements = statements,
        };

    }
    
    pub fn String(bs: *const BlockStatement) Allocator.Error![]const u8 {
        var str_len: usize = 0;
        const allocator = bs.token.allocator;
        var bs_str = try allocator.alloc(u8, 0);

        for (bs.statements.items) |stmt| {

            const stmt_str = try stmt.String();
            defer allocator.free(stmt_str);

            bs_str = try allocator.realloc(bs_str, str_len + stmt_str.len);

            @memcpy(bs_str[str_len..(str_len + stmt_str.len)], stmt_str);

            str_len += stmt_str.len;
        }

        return bs_str;

    }
};


// Expressions
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

    pub fn deinit(expr: *const Expression) void {
        
        switch (expr.*) {
            inline else => |case| case.deinit(),
        }

    }

    pub fn clone(expr: *const Expression) Allocator.Error!Expression {

        switch (expr.*) {
            .identifier => |ident| {
                return Expression {
                    .identifier = try ident.clone()
                };
            },
            inline else => |case| return try case.clone(),
        }

    }

    /// Calls respective sub_expr.String method and returns a string.
    /// String need to be deallocated by caller.
    pub fn String(expr: *const Expression) Allocator.Error![]const u8 {
        switch (expr.*) {
            inline else => |case| {
                return try case.String();
            }
        }
    }
    
};

// TODO: check if need to be public
pub const IntegerLiteralExpression = struct {
    token: Token,
    value: u32,

    pub fn deinit(int_lit: *const IntegerLiteralExpression) void {
        // print("deinits int lit expr\n", .{});
        int_lit.token.deinit();
    }

    pub fn clone(int_lit_expr: *const IntegerLiteralExpression) !Expression {
        return Expression {
            .integer_literal = .{
                .token = try int_lit_expr.token.clone(),
                .value = int_lit_expr.value
            }
        };
    }

    pub fn String(ile: *const IntegerLiteralExpression) Allocator.Error![]const u8 {
        const str = try std.fmt.allocPrint(ile.token.allocator, "{}", .{ile.value});
        return str;
    }
};

pub const BooleanLiteralExpression = struct {
    token: Token,
    value: bool,

    pub fn deinit(bool_lit: *const BooleanLiteralExpression) void {
        bool_lit.token.deinit();
    }

    pub fn clone(bool_lit_expr: *const BooleanLiteralExpression) !Expression {
        return Expression {
            .boolean_literal = .{
                .token = try bool_lit_expr.token.clone(),
                .value = bool_lit_expr.value
            }
        };
    }

    pub fn String(ble: *const BooleanLiteralExpression) Allocator.Error![]const u8 {
        const str = try std.fmt.allocPrint(ble.token.allocator, "{}", .{ble.value});
        return str;
    }
};

pub const PrefixExpression = struct {
    allocator: Allocator,
    token: Token,
    right: *const Expression,

    pub fn deinit(pe: *const PrefixExpression) void {
        pe.token.deinit();
        pe.right.deinit();
        pe.allocator.destroy(pe.right);
    }

    pub fn clone(pe: *const PrefixExpression) Allocator.Error!Expression {
        const right_ptr = try pe.allocator.create(Expression);
        right_ptr.* = try pe.right.clone();

        return Expression {
            .prefix_expression = .{
                .allocator = pe.allocator,
                .token = try pe.token.clone(),
                .right = right_ptr
            }
        };
        
    }

    /// Returns a String for PrefixExpression.
    /// Need to be freed by caller.
    pub fn String(pe: *const PrefixExpression) Allocator.Error![]const u8 {

        const right_str = try pe.right.String();
        defer pe.allocator.free(right_str);

        return try std.fmt.allocPrint(pe.allocator, "({s}{s})", .{
            pe.token.tokenLiteral(), right_str
        });

    }

};

pub const InfixExpression = struct {
    allocator: Allocator,
    token: Token,
    left: *const Expression,
    right: *const Expression,

    pub fn deinit(ie: *const InfixExpression) void {
        ie.token.deinit();
        ie.left.deinit();
        ie.right.deinit();
        ie.allocator.destroy(ie.left);
        ie.allocator.destroy(ie.right);
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
                .token = try ie.token.clone(),
                .left = left_ptr,
                .right = right_ptr
            }

        };

    }

    pub fn String(ie: *const InfixExpression) Allocator.Error![]const u8 {
        const left_str = try ie.left.String();
        defer ie.allocator.free(left_str);

        const right_str = try ie.right.String();
        defer ie.allocator.free(right_str);

        return try std.fmt.allocPrint(ie.allocator, "({s} {s} {s})", .{
            left_str, ie.token.tokenLiteral(), right_str
        });

    }
    // TODO add string method

};


// Need dealloc
pub const IfExpression = struct {
    allocator: Allocator,
    token: Token, 
    condition: *const Expression,
    consequence: BlockStatement,
    alternative: ?BlockStatement,


    pub fn deinit(ie: *const IfExpression) void {
        
        ie.token.deinit();
        ie.condition.deinit();
        ie.allocator.destroy(ie.condition);

        ie.consequence.deinit();
        
        if (ie.alternative) |alt| alt.deinit();
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
                .token = try ie.token.clone(),
                .condition = condition_ptr,
                .consequence = try ie.consequence.clone(),
                .alternative = new_alt,
            }
        };

    }

    pub fn String(if_expr: *const IfExpression) Allocator.Error![]const u8 {
                const condition_str = try if_expr.condition.String();
                defer if_expr.allocator.free(condition_str);
                
                const consequence_str = try if_expr.consequence.String();
                defer if_expr.allocator.free(consequence_str);
                const l_b = "{";
                const r_b = "}";

                // TODO: get string for BlockStatement
                if (if_expr.alternative) |alt| {
                    const alt_str = try alt.String();
                    defer if_expr.allocator.free(alt_str);

                    const format = "if ({s}) {s} {s} {s} else {s} {s} {s}";
                    
                    return try std.fmt.allocPrint(if_expr.allocator, format, .{
                        condition_str, 
                        l_b,consequence_str, r_b,
                        l_b, alt_str, r_b
                    });

                } else {
                    const format = "if ({s}) {s} {s} {s}";

                    return try std.fmt.allocPrint(if_expr.allocator, format, .{
                        condition_str, 
                        l_b, consequence_str, r_b
                    });
                }
    }

};

// need dealloc
pub const FnLiteralExpression = struct {
    token: Token,
    parameters: ArrayList(Identifier), // might be empty
    body: BlockStatement,

    pub fn deinit(fe: *const FnLiteralExpression) void {
        fe.token.deinit();

        for (fe.parameters.items) |p| {
            p.deinit();
        }
        fe.parameters.deinit();
        
        fe.body.deinit();
    }

    pub fn clone(fe: *const FnLiteralExpression) Allocator.Error!Expression {

        
        var parameters = ArrayList(Identifier).init(fe.parameters.allocator);
        
        for (fe.parameters.items) |p| {
            try parameters.append(try p.clone());
        }

        return Expression {
            .fn_literal = .{
                .token = try fe.token.clone(),
                .body = try fe.body.clone(),
                .parameters = parameters,
            }
        };

    }

    pub fn String(fle: *const FnLiteralExpression) Allocator.Error![]const u8 {
        const allocator = fle.token.allocator;
        
        var str_len: usize = 0;
        var params_str = try allocator.alloc(u8, 0);
        defer allocator.free(params_str);

        const n_params = fle.parameters.items.len;
        
        for (fle.parameters.items, 0..) |p, i| {

            const p_str = try p.String();
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
        
        const body_str = try fle.body.String();
        defer allocator.free(body_str);

        const l_b = "{";
        const r_b = "}";

        return try std.fmt.allocPrint(allocator, "fn({s}) {s} {s} {s}", .{
            params_str, l_b, body_str, r_b
        });
        
    }
    
};


pub const CallExpression = struct {
    allocator: Allocator,
    token: Token, // the '('
    function: *const Expression, // fnlit or ident expr
    args: ArrayList(Expression),

    pub fn deinit(ce: *const CallExpression) void {
        ce.token.deinit();
        ce.function.deinit();
        ce.allocator.destroy(ce.function);

        for (ce.args.items) |arg| {
            arg.deinit();
        }
        
        ce.args.deinit();
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
                .token = try ce.token.clone(),
                .function = func_ptr,
                .args = args

            }
            
        };

    }

    pub fn String(ce: *const CallExpression) Allocator.Error![]const u8 {
        
        const fn_str = try ce.function.String();
        defer ce.allocator.free(fn_str);

        var str_len: usize = 0;
        var args_str = try ce.allocator.alloc(u8, 0);
        defer ce.allocator.free(args_str);

        const n_args = ce.args.items.len;
        

        for (ce.args.items, 0..) |arg_expr, i| {

            const arg_str = try arg_expr.String();
            defer ce.allocator.free(arg_str);
            
            
            if (i == n_args - 1) {
                args_str = try ce.allocator.realloc(args_str, str_len + arg_str.len);
                @memcpy(args_str[(str_len)..(str_len + arg_str.len)], arg_str);
                str_len += arg_str.len;

            } else {
                
                args_str = try ce.allocator.realloc(args_str, str_len + arg_str.len + 2);
                args_str[str_len + arg_str.len] = ',';
                args_str[str_len + arg_str.len + 1] = ' ';

                @memcpy(args_str[(str_len)..(str_len + arg_str.len)], arg_str);
                str_len += arg_str.len + 2;

        
            }

        }

        return try std.fmt.allocPrint(ce.allocator, "{s}({s})", .{
            fn_str, args_str
        });

    }

};

pub const StringExpression = struct {
    token: Token,
    value: []const u8, // deinited by token.deinit

    pub fn deinit(se: *const StringExpression) void {

        se.token.deinit();

    }   

    pub fn clone(se: *const StringExpression) Allocator.Error!Expression {
        const cloned_token = try se.token.clone();
        
        return Expression {
            .string_expression = .{
                .token = cloned_token,
                .value = cloned_token.literal
            }
        };

    }

    pub fn String(se: *const StringExpression) Allocator.Error![]const u8 {
        _ = se;
        
        @panic("String for StringExpression not implemented");

    }

};


// Identifier
pub const Identifier = struct {
    token: Token,

    pub fn deinit(ident: *const Identifier) void {
        // print("deinits ident expr\n", .{});
        ident.token.deinit();
    }

    pub fn clone(ident: *const Identifier) Allocator.Error!Identifier {

        return Identifier {
            .token = try ident.token.clone(),
        };

    }

    pub fn tokenLiteral(ident: *const Identifier) []const u8 {
        return ident.token.tokenLiteral();
    }

    pub fn String(ident: *const Identifier) Allocator.Error![]const u8 {
        return try std.fmt.allocPrint(ident.token.allocator, "{s}", .{ident.tokenLiteral()});
    }
};

pub const Program = struct {
    allocator: Allocator,
    statements: ArrayList(Statement), // 
    
    // statements: ArrayList(Statement),
    // expressions: ArrayList(Expression),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .statements = ArrayList(Statement).init(allocator),
        };

    }

    pub fn deinit(program: *Program) void {
        
        for (program.statements.items) |stmt| {
            stmt.deinit();
        }

        program.statements.deinit();

        
    }
    
    pub fn String(program: *Program) Allocator.Error![]const u8 {

        var str_len: usize = 0;
        var prog_str = try program.allocator.alloc(u8, 0);

        for (program.statements.items) |stmt| {

            const stmt_str = try stmt.String();
            defer program.allocator.free(stmt_str);

            prog_str = try program.allocator.realloc(prog_str, str_len + stmt_str.len);

            @memcpy(prog_str[str_len..(str_len + stmt_str.len)], stmt_str);

            str_len += stmt_str.len;
        }

        return prog_str;

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

