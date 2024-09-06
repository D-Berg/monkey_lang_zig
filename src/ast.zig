const Token = @import("Token.zig");
const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const Node = struct {

};

pub const Statement = struct {
    
};

const Expression = struct {
};


pub const Program = struct {
    statements: ArrayList(Statement),

    pub fn init(allocator: Allocator) Program {

        return .{
            .statements = ArrayList(Statement).init(allocator),
        };

    }
    
    fn TokenLiteral(program: *Program) []const u8 {
        
        if (program.statements.len > 0) {
            return program.statements[0].TokenLiteral();
        } else {
            return "";
        }

    }
};

pub const LetStatement = struct {
    token: Token,
    name: *Identifier,
    value: Expression,
    
    // fn statementNode(ls: *LetStatement) void {},
    
    fn TokenLiteral(ls: *LetStatement) []const u8 {
        return ls.token.literal;
    }
};

const Identifier = struct {
    token: Token, // token.Ident
    value: []const u8,
    
    // fn expressionNode(ident: *Identifier) void {},
    
    fn TokenLiteral(ident: *Identifier) []const u8 {
        return ident.token.literal;
    }
};

