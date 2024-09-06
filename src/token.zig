// TODO: remove token struct 
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
        
        // Identifiers + literals
        Ident, // add, foobar, x, y
        Int, // 12221378

        // operators
        Assign, // = 
        Plus, // +
        Minus,
        Bang, // !
        Asterisk, // *
        Slash, // /

        Lt, // <
        Gt, // >

        Eq, // ==
        Neq, // !=

        // Delimiters
        Comma,
        Semicolon,

        Lparen,
        Rparen,
        Lbrace,
        Rbrace,

        // Keywords
        Function,
        Let,
        True,
        False,
        If,
        Else,
        Return,
    };
};


pub const Keywords = struct {
    
    words: std.StringHashMap(Token.Kind),

    pub fn init(allocator: Allocator) !Keywords {

        var words = std.StringHashMap(Token.Kind).init(allocator);
        try words.put("let", Token.Kind.Let);
        try words.put("fn", Token.Kind.Function);
        try words.put("true", Token.Kind.True);
        try words.put("false", Token.Kind.False);
        try words.put("if", Token.Kind.If);
        try words.put("else", Token.Kind.Else);
        try words.put("return", Token.Kind.Return);

        return .{
            .words = words,
        };
        
    }

    pub fn deinit(keywords: *Keywords) void {

        keywords.words.deinit();

    }



    
};
