const Token = @import("Token.zig");
const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;



const Node = struct {

};

pub const Statement = union(enum) {
    let_stmt: LetStatement,
    ret_stmt: ReturnStatement,
    expr_stmt: ExpressionStatement,
    
    pub fn tokenLiteral(stmt: *Statement) []const u8 {
        switch (stmt.*) {
            .let_stmt => |*ls| return ls.token.tokenLiteral(),
            .ret_stmt => |*rs| return rs.token.tokenLiteral(),
            .expr_stmt => |*es| return es.token.tokenLiteral(),
        }
    }

    pub fn String(stmt: *Statement, allocator: Allocator) ![]const u8 {

        switch (stmt.*) {

            .let_stmt => |*ls| {
                
                if (ls.value != null) {
                    const value_str = try ls.value.?.String(allocator);
                    defer allocator.free(value_str);

                    const str = try std.fmt.allocPrint(allocator, "let {s} = {s};", .{
                        ls.name.token.tokenLiteral(),
                        value_str
                    });

                    return str;
                } else {
                    unreachable;

                }


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
    value: ?Expression, //TODO: remove null
};

pub const ReturnStatement = struct {
    token: Token, // A Return token
    value: ?Expression
};

pub const ExpressionStatement = struct {
    token: Token, // the first token of the expression
    expression: ?Expression
};


// Expressions
pub const Expression = union(enum) {
    identifier: Identifier,
    integer_literal: IntegerLiteralExpression,
    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,

    fn String(expr: *Expression, allocator: Allocator) ![]const u8 {
        
        switch (expr.*) {

            .identifier => |ie| {

                var token = ie.token;
                const str = try std.fmt.allocPrint(allocator, "{s}", .{Token.tokenLiteral(&token)});
                return str;
            
            },

            else => unreachable
        }

    }
    
};

pub const IntegerLiteralExpression = struct {
    token: Token,
    value: u32
};

pub const PrefixExpression = struct {
    token: Token,
    right: *Expression,
};

pub const InfixExpression = struct {
    token: Token,
    left: *Expression,
    right: *Expression,
};


// Identifier
pub const Identifier = struct {
    token: Token,
};

pub const Program = struct {
    allocator: Allocator,
    statements: ArrayList(Statement),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .statements = ArrayList(Statement).init(allocator),
        };

    }

    pub fn deinit(program: *Program) void {

        for (program.statements.items) |stmt| {

            switch (stmt) {

                .expr_stmt => |es| {
                    if (es.expression) |es_expr|{
                        switch (es_expr) {
                            .prefix_expression => |pe| program.allocator.destroy(pe.right),
                            .infix_expression => |ie| {
                                program.allocator.destroy(ie.left);
                                program.allocator.destroy(ie.right);
                            },
                            else => continue
                        }
                    }
                },
                else => continue,
            }


        }

        program.statements.deinit();

    }
    
    pub fn String(program: *Program) ![]const u8 {
        
        var str_len: usize = 0;
        var buffer = try program.allocator.alloc(u8, 0);

        for (program.statements.items) |*stmt| {


            const stmt_str = try stmt.String(program.allocator);
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


