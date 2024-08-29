const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Token = struct {
    allocator: Allocator,
    kind: Kind,
    literal: []const u8, 
    
    pub fn init(allocator: Allocator, kind: Token.Kind, chars: []const u8) !Token {

        const str = try allocator.dupe(u8, chars);

        return .{
            .allocator = allocator,
            .kind = kind, 
            .literal = str
        };
    }

    pub fn deinit(t: *const Token) void {
        t.allocator.free(t.literal);
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


pub const Keywords = struct {
    
    words: std.StringHashMap(Token.Kind),

    pub fn init(allocator: Allocator) !Keywords {

        var words = std.StringHashMap(Token.Kind).init(allocator);
        try words.put("let", Token.Kind.Let);
        try words.put("fn", Token.Kind.Function);

        return .{
            .words = words,
        };
        
    }

    pub fn deinit(keywords: *Keywords) void {

        keywords.words.deinit();

    }



    
};
