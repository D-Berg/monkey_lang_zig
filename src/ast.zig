const Token = @import("Token.zig");
const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const print = std.debug.print;

const ExprIdx = usize;
const StateIdx = usize;

const Node = struct {

};

pub const Statement = union(enum) {
    let_stmt: LetStatement,
    ret_stmt: ReturnStatement,
    // need deallocations
    expr_stmt: ExpressionStatement,
    blck_stmt: BlockStatement,

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
                    var value = program.expressions.items[expr_idx];
                    const value_str = try value.String(allocator);
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
            //
            // .expr_stmt => |*es| {
            //
            //     if (es.expression != null) {
            //         return try es.expression.?.String(allocator);
            //     } else {
            //
            //         return try std.fmt.allocPrint(allocator, "", .{});
            //
            //     }
            //
            //
            // },

            // TODO .block_stmt
        
            .ret_stmt => {
                const str = try std.fmt.allocPrint(allocator, "return <expr>;", .{});
                return str;
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
    statements: []StateIdx, // TODO: use arraylist for better perf

    // fn deinit(allocator: Allocator) {
    //
    // }
};


// Expressions
pub const Expression = union(enum) {
    identifier: Identifier,
    integer_literal: IntegerLiteralExpression,
    boolean_literal: BooleanLiteralExpression,
    // Recursive: holds pointers to other Expression, need to be deallocated
    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,
    if_expression: IfExpression,
    fn_literal: FnLiteralExpression,
    call_expression: CallExpression,

    fn String(expr: *Expression, allocator: Allocator) ![]const u8 {
        _ = expr;
        _ = allocator;
        unreachable;
        
        // switch (expr.*) {
        //     .prefix_expression => |*pe| {
        //         const right_str = try pe.right.String(allocator);
        //         defer allocator.free(right_str);
        //
        //         return try std.fmt.allocPrint(allocator, "({s}{s})", .{
        //             pe.token.tokenLiteral(), right_str
        //         });
        //     },
        //
        //     .infix_expression => |*ie| {
        //         const left_str = try ie.left.String(allocator);
        //         defer allocator.free(left_str);
        //         const right_str = try ie.right.String(allocator);
        //         defer allocator.free(right_str);
        //
        //         return try std.fmt.allocPrint(allocator, "({s} {s} {s})", .{
        //             left_str, ie.token.tokenLiteral(), right_str
        //         });
        //     },
        //
        //     // TODO: fix if_expr.string()
        //     .if_expression => |*if_expr| {
        //         const condition_str = try if_expr.condition.String(allocator);
        //         defer allocator.free(condition_str);
        //
        //         // TODO: get string for BlockStatement
        //         if (if_expr.alternative) |alt| {
        //             _ = alt;
        //             return try std.fmt.allocPrint(allocator, "if {s} cons else alt", .{
        //                 condition_str
        //             });
        //         } else {
        //             return try std.fmt.allocPrint(allocator, "if {s} conse", .{
        //                 condition_str
        //             });
        //         }
        //
        //     },
        //
        //     .fn_literal => |*fn_lit| {
        //         _ = fn_lit;
        //         // TODO impl string() for fn_lit
        //         return try std.fmt.allocPrint(allocator, "not yet impl for fn", .{});
        //
        //     },
        //
        //     inline else => |*case| {
        //         const str = try std.fmt.allocPrint(allocator, "{s}", .{case.token.tokenLiteral()});
        //         return str;
        //     }
        //
        // }

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


pub const IfExpression = struct {
    token: Token, 
    condition: ExprIdx,
    consequence: BlockStatement,
    alternative: ?BlockStatement,
};

pub const FnLiteralExpression = struct {
    token: Token,
    parameters: ?ArrayList(Identifier),
    body: BlockStatement
};


pub const CallExpression = struct {
    token: Token, // the '('
    function: ExprIdx, // fnlit or ident expr
    args: []ExprIdx,
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
    statements: ArrayList(Statement),
    expressions: ArrayList(Expression),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .statements = ArrayList(Statement).init(allocator),
            .expressions = ArrayList(Expression).init(allocator)
        };

    }

    pub fn deinit(program: *Program) void {
        program.statements.deinit();
        program.expressions.deinit();
    }
    
    pub fn String(program: *Program) ![]const u8 {

        var str_len: usize = 0;
        var buffer = try program.allocator.alloc(u8, 0);

        for (program.statements.items) |*stmt| {

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

