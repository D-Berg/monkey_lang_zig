const Token = @import("Token.zig");
const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;



const Node = struct {

};

pub const Statement = struct {
    token: Token,
    kind: Kind,
    expression: ?Expression = null,
    identifier: ?Identifier = null,
    
    // fn statementNode(ls: *LetStatement) void {},
    
    pub fn tokenLiteral(stmt: *Statement) []const u8 {
        return stmt.token.tokenLiteral();
    }
    
    const Kind = enum {
        Let,
        Return,
        Expression,
    };
};

pub const Expression = union(enum) {
    identifier: Identifier,
    integer_literal: IntegerLiteralExpression,
    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,
    
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

            if (stmt.kind == .Expression) {

                if (stmt.expression) |expr| {
                    switch (expr) {
                        .prefix_expression => |pe| program.allocator.destroy(pe.right),
                        .infix_expression => |ie| {
                            program.allocator.destroy(ie.left);
                            program.allocator.destroy(ie.right);
                        },
                        else => continue
                    }
                }

            }
                

        }

        program.statements.deinit();

    }
    
    fn TokenLiteral(program: *Program) []const u8 {
        
        if (program.statements.len > 0) {
            return program.statements[0].TokenLiteral();
        } else {
            return "";
        }

    }
};


