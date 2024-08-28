const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Token = struct {
    kind: Kind,
    literal: []const u8,

    pub fn new(allocator: Allocator, kind: Token.Kind, ch: u8) !Token {

        const str = try allocator.alloc(u8, 1);
        str[0] = ch;

        return .{
            .kind = kind, 
            .literal = str
        };
    }

    pub const Kind = enum {
        Illegal,
        Eof,
        
        Ident,
        Int,

        Assign,
        Plus,

        Comma,
        Semicolon,

        Lparen,
        Rparen,
        Lbrace,
        Rbrace,

        Function,
        Let,
    };
};
