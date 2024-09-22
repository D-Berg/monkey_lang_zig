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

    pub fn tokenLiteral(stmt: *Statement) []const u8 {
        switch (stmt.*) {
            inline else => |*case| return case.token.tokenLiteral(),
        }
    }

    pub fn String(stmt: *Statement, program: *Program) ![]const u8 {

        const allocator = program.allocator;

        switch (stmt.*) {

            .let_stmt => |*ls| {

                if (ls.value != null) {

                    const expr_idx = ls.value.?;
                    var value = program.nodes.items[expr_idx].expression;
                    const value_str = try value.String(program);
                    defer program.allocator.free(value_str);

                    const str = try std.fmt.allocPrint(allocator, "let {s} = {s};", .{
                        ls.name.token.tokenLiteral(),
                        value_str
                    });

                    return str;
                } else {
                    const str = try std.fmt.allocPrint(allocator, "let {s} = null;", .{
                        ls.name.token.tokenLiteral(),
                    });

                    return str;

                }


            },

            .expr_stmt => |*es| {

                if (es.expression) |expr_idx| {
                    var expr = program.nodes.items[expr_idx].expression;
                    return try expr.String(program);
                } else {

                    return try std.fmt.allocPrint(allocator, "", .{});

                }


            },

            // TODO .block_stmt
        
            .ret_stmt => |*rs| {

                if (rs.value) |val_idx| {
                    var val_expr = program.nodes.items[val_idx].expression;
                    const val_str = try val_expr.String(program);
                    defer allocator.free(val_str);

                    const str = try std.fmt.allocPrint(allocator, "return {s};", .{val_str});
                    return str;

                } else {
                    const str = try std.fmt.allocPrint(allocator, "return;", .{});
                    return str;
                }

                // TODO: implement
            },

            else => {
                unreachable;
            }

        }


    }
};

// Statements
pub const LetStatement = struct {
    token: Token, // the Let TokenLiteral
    name: Identifier,
    value: ?ExprIdx, //TODO: remove null
};

pub const ReturnStatement = struct {
    token: Token, // A Return token
    value: ?ExprIdx
};

pub const ExpressionStatement = struct {
    token: Token, // the first token of the expression
    expression: ?ExprIdx
};

pub const BlockStatement = struct {
    token: Token,
    statements: ArrayList(usize), 
    // fn deinit(allocator: Allocator) {
    //
    // }
    
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

    pub fn deinit(expr: *Expression) void {
        
        switch (expr.*) {

            .if_expression => |*ie| {
                ie.consequence.statements.deinit();
                if (ie.alternative) |*alt| {
                    alt.statements.deinit();
                }
            },
            
            .fn_literal => |fl| {
                fl.body.statements.deinit();
                if (fl.parameters) |params| {
                    params.deinit();
                }

            },
            .call_expression => |ce| {
                ce.args.deinit();
            },
            
            inline else => {}

        }

    }

    fn String(expr: *Expression, program: *Program) ![]const u8 {
        
        const allocator = program.allocator;
        
        switch (expr.*) {
            .prefix_expression => |*pe| {
                var right_expr = program.nodes.items[pe.right].expression;
                const right_str = try right_expr.String(program);
                defer allocator.free(right_str);

                return try std.fmt.allocPrint(allocator, "({s}{s})", .{
                    pe.token.tokenLiteral(), right_str
                });
            },

            .infix_expression => |*ie| {
                var left_expr = program.nodes.items[ie.left].expression;
                const left_str = try left_expr.String(program);
                defer allocator.free(left_str);

                var right_expr = program.nodes.items[ie.right].expression;
                const right_str = try right_expr.String(program);
                defer allocator.free(right_str);

                return try std.fmt.allocPrint(allocator, "({s} {s} {s})", .{
                    left_str, ie.token.tokenLiteral(), right_str
                });
            },

            // TODO: fix if_expr.string()
            .if_expression => |*if_expr| {
                var condition_expr = program.nodes.items[if_expr.condition].expression;
                const condition_str = try condition_expr.String(program);
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
                var fn_expr = program.nodes.items[call_expr.function].expression;
                const fn_str = try fn_expr.String(program);
                defer allocator.free(fn_str);

                var str_len: usize = 0;
                var args_str = try program.allocator.alloc(u8, 0);
                defer allocator.free(args_str);

                const n_args = call_expr.args.items.len;
                

                for (call_expr.args.items, 0..) |arg_idx, i| {

                    var arg_expr = program.nodes.items[arg_idx].expression;
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
    token: Token,
    right: ExprIdx,
};

pub const InfixExpression = struct {
    token: Token,
    left: ExprIdx,
    right: ExprIdx,
};


// Need dealloc
pub const IfExpression = struct {
    token: Token, 
    condition: ExprIdx,
    consequence: BlockStatement,
    alternative: ?BlockStatement,
};

// need dealloc
pub const FnLiteralExpression = struct {
    token: Token,
    parameters: ?ArrayList(Identifier),
    body: BlockStatement
};


pub const CallExpression = struct {
    token: Token, // the '('
    function: ExprIdx, // fnlit or ident expr
    args: ArrayList(usize),
};


// Identifier
pub const Identifier = struct {
    token: Token,

    pub fn tokenLiteral(ident: *Identifier) []const u8 {
        return ident.token.tokenLiteral();
    }
};

pub const Program = struct {
    allocator: Allocator,
    statement_indexes: ArrayList(usize), // 
    nodes: ArrayList(Node),
    
    // statements: ArrayList(Statement),
    // expressions: ArrayList(Expression),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .statement_indexes = ArrayList(StateIdx).init(allocator),
            .nodes = ArrayList(Node).init(allocator),
        };

    }

    pub fn deinit(program: *Program) void {
        program.statement_indexes.deinit();
        
        for (program.nodes.items) |*node| {
            switch (node.*) {
                .statement => {

                },
                .expression => |*expr| {
                    expr.deinit();
                }, 
            }
        }

        program.nodes.deinit();

        
    }
    
    pub fn String(program: *Program) ![]const u8 {

        var str_len: usize = 0;
        var buffer = try program.allocator.alloc(u8, 0);

        for (program.statement_indexes.items) |stmt_idx| {

            var stmt = program.nodes.items[stmt_idx].statement;

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

