const std = @import("std");
const Allocator = std.mem.Allocator;

const Token = @This();

// allocator: Allocator,
kind: Kind,
// TODO: allocate string on heap
literal: [32:0]u8, // literal can be at most 32 bytes 
literal_len: usize,
    
pub fn init(kind: Token.Kind, chars: []const u8) Token {
    var literal: [32:0]u8 = undefined;

    for (0..literal.len) |i| {
        literal[i] = 0;
    }

    for (0..chars.len) |i| {
        literal[i] = chars[i];
    }
    
    return .{
        .kind = kind, 
        .literal = literal,
        .literal_len = chars.len
    };
}


pub fn tokenLiteral(tok: *Token) []const u8 {
    return tok.literal[0..tok.literal_len];
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


// TODO: remove and replace with a switch
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

