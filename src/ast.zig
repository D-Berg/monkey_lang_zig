const Token = @import("Token.zig");
const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;



const Node = struct {

};

pub const Statement = struct {
    token: Token,
    name: ?Identifier = null,
    value: ?Expression = null,
    
    // fn statementNode(ls: *LetStatement) void {},
    
    pub fn TokenLiteral(ls: *Statement) []const u8 {
        return ls.token.literal;
    }
    
    const Kind = enum {
        Let,
        Return,
    };
    
};

const Expression = struct {
};

pub const Identifier = struct {
    token: Token, // token.Ident
    value: []const u8,
    
    // fn expressionNode(ident: *Identifier) void {},
    
    fn TokenLiteral(ident: *Identifier) []const u8 {
        return ident.token.literal;
    }
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


