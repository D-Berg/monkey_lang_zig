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

pub const Expression = struct {
    token: Token,
    kind: Kind,
    value: ?Value = null,
    
    const Kind = enum {
        Identifier,
        IntegerLiteral
    };
    
    pub const Value = union(enum) {
        int_val: u32,
    };
};

pub const Identifier = struct {
    token: Token,
};

pub const Program = struct {
    statements: ArrayList(Statement),

    pub fn init(allocator: Allocator) Program {

        return .{
            .statements = ArrayList(Statement).init(allocator),
        };

    }

    pub fn deinit(program: *Program) void {
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


