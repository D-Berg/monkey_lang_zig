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

    pub fn tokenLiteral(stmt: *const Statement) []const u8 {
        switch (stmt.*) {
            inline else => |*case| return case.token.tokenLiteral(),
        }
    }

    pub fn String(stmt: *const Statement, program: *Program) ![]const u8 {

        const allocator = program.allocator;

        switch (stmt.*) {

            .let_stmt => |*ls| {
                const value = ls.value;
                const value_str = try value.String(program);
                defer program.allocator.free(value_str);

                const str = try std.fmt.allocPrint(allocator, "let {s} = {s};", .{
                    ls.name.token.tokenLiteral(),
                    value_str
                });

                return str;

            },

            .expr_stmt => |*es| {
                return try es.expression.String(program);

            },

            // TODO .block_stmt
        
            .ret_stmt => |*rs| {

                const val_str = try rs.value.String(program);
                defer allocator.free(val_str);

                const str = try std.fmt.allocPrint(allocator, "return {s};", .{val_str});
                return str;

            },

            else => {
                unreachable;
            }

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
        ls.value.deinit();
        ls.allocator.destroy(ls.value);
    }
};

pub const ReturnStatement = struct {
    allocator: Allocator,
    token: Token, // A Return token
    value: *const Expression,
    
    pub fn deinit(rs: *const ReturnStatement) void {
        rs.value.deinit();
        rs.allocator.destroy(rs.value);
    }
    // TODO: deinit
};

pub const ExpressionStatement = struct {
    allocator: Allocator,
    token: Token, // the first token of the expression
    expression: *const Expression,

    pub fn deinit(es: *const ExpressionStatement) void {
        es.expression.deinit();
        es.allocator.destroy(es.expression);
    }
    
    // TODO: deinit
};

pub const BlockStatement = struct {
    token: Token,
    statements: ArrayList(Statement), 

    fn deinit(bs: *const BlockStatement) void {
        for (bs.statements.items) |stmt| {
            stmt.deinit();
        }
        
        bs.statements.deinit();

    }
    
    pub fn clone(self: *const BlockStatement) !BlockStatement {
        
        return BlockStatement {
            .token =  self.token,
            .statements = try self.statements.clone(),
        };

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

    pub fn deinit(expr: *const Expression) void {
        
        switch (expr.*) {
            .identifier, .integer_literal, .boolean_literal=> {},
            inline else => |case| case.deinit(),
        }

    }

    fn String(expr: *const Expression, program: *Program) ![]const u8 {
        
        const allocator = program.allocator;
        
        switch (expr.*) {
            .prefix_expression => |*pe| {
                const right_str = try pe.right.String(program);
                defer allocator.free(right_str);

                return try std.fmt.allocPrint(allocator, "({s}{s})", .{
                    pe.token.tokenLiteral(), right_str
                });
            },

            .infix_expression => |*ie| {
                const left_str = try ie.left.String(program);
                defer allocator.free(left_str);

                const right_str = try ie.right.String(program);
                defer allocator.free(right_str);

                return try std.fmt.allocPrint(allocator, "({s} {s} {s})", .{
                    left_str, ie.token.tokenLiteral(), right_str
                });
            },

            // TODO: fix if_expr.string()
            .if_expression => |*if_expr| {
                const condition_str = try if_expr.condition.String(program);
                defer allocator.free(condition_str);

                // TODO: get string for BlockStatement
                if (if_expr.alternative) |alt| {
                    _ = alt;
                    return try std.fmt.allocPrint(allocator, "if {s} cons else alt", .{
                        condition_str
                    });
                } else {
                    return try std.fmt.allocPrint(allocator, "if {s} conse", .{
                        condition_str
                    });
                }

            },

            .fn_literal => |*fn_lit| {
                _ = fn_lit;
                // TODO impl string() for fn_lit
                return try std.fmt.allocPrint(allocator, "not yet impl for fn", .{});

            },

            .call_expression => |*call_expr| {

                // TODO impl string() for fn_lit
                const fn_str = try call_expr.function.String(program);
                defer allocator.free(fn_str);

                var str_len: usize = 0;
                var args_str = try program.allocator.alloc(u8, 0);
                defer allocator.free(args_str);

                const n_args = call_expr.args.items.len;
                

                for (call_expr.args.items, 0..) |arg_expr, i| {

                    const arg_str = try arg_expr.String(program);
                    defer allocator.free(arg_str);
                    
                    
                    if (i == n_args - 1) {
                        args_str = try allocator.realloc(args_str, str_len + arg_str.len);
                        @memcpy(args_str[(str_len)..(str_len + arg_str.len)], arg_str);
                        str_len += arg_str.len;

                    } else {
                        
                        args_str = try allocator.realloc(args_str, str_len + arg_str.len + 2);
                        args_str[str_len + arg_str.len] = ',';
                        args_str[str_len + arg_str.len + 1] = ' ';

                        @memcpy(args_str[(str_len)..(str_len + arg_str.len)], arg_str);
                        str_len += arg_str.len + 2;

                
                    }

                }

                return try std.fmt.allocPrint(allocator, "{s}({s})", .{
                    fn_str, args_str
                });

            },
            inline else => |*case| {
                const str = try std.fmt.allocPrint(allocator, "{s}", .{case.token.tokenLiteral()});
                return str;
            }

        }

    }
    
};

// TODO: check if need to be public
pub const IntegerLiteralExpression = struct {
    token: Token,
    value: u32
};

pub const BooleanLiteralExpression = struct {
    token: Token,
    value: bool
};

pub const PrefixExpression = struct {
    allocator: Allocator,
    token: Token,
    right: *const Expression,

    pub fn deinit(pe: *const PrefixExpression) void {
        pe.right.deinit();
        pe.allocator.destroy(pe.right);
    }
};

pub const InfixExpression = struct {
    allocator: Allocator,
    token: Token,
    left: *const Expression,
    right: *const Expression,

    pub fn deinit(ie: *const InfixExpression) void {
        ie.left.deinit();
        ie.right.deinit();
        ie.allocator.destroy(ie.left);
        ie.allocator.destroy(ie.right);
    }
};


// Need dealloc
pub const IfExpression = struct {
    allocator: Allocator,
    token: Token, 
    condition: *const Expression,
    consequence: BlockStatement,
    alternative: ?BlockStatement,


    pub fn deinit(ie: *const IfExpression) void {
        
        ie.condition.deinit();
        ie.allocator.destroy(ie.condition);

        ie.consequence.deinit();
        
        if (ie.alternative) |alt| alt.deinit();

    }
};

// need dealloc
pub const FnLiteralExpression = struct {
    token: Token,
    parameters: ?ArrayList(Identifier),
    body: BlockStatement,

    pub fn deinit(fe: *const FnLiteralExpression) void {
        if (fe.parameters) |params| {
            params.deinit();
        }
        
        fe.body.deinit();

    }
};


pub const CallExpression = struct {
    allocator: Allocator,
    token: Token, // the '('
    function: *const Expression, // fnlit or ident expr
    args: ArrayList(Expression),

    pub fn deinit(ce: *const CallExpression) void {
        ce.function.deinit();
        ce.allocator.destroy(ce.function);

        for (ce.args.items) |arg| {
            arg.deinit();
        }
        
        ce.args.deinit();
    }

};


// Identifier
pub const Identifier = struct {
    token: Token,

    pub fn tokenLiteral(ident: *const Identifier) []const u8 {
        return ident.token.tokenLiteral();
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
    
    pub fn String(program: *Program) ![]const u8 {

        var str_len: usize = 0;
        var buffer = try program.allocator.alloc(u8, 0);

        for (program.statements.items) |stmt| {

            const stmt_str = try stmt.String(program);
            defer program.allocator.free(stmt_str);

            buffer = try program.allocator.realloc(buffer, str_len + stmt_str.len);

            @memcpy(buffer[str_len..(str_len + stmt_str.len)], stmt_str);

            str_len += stmt_str.len;

        }

        return buffer;


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

