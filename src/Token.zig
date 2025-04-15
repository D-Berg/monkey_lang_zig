const std = @import("std");
const Allocator = std.mem.Allocator;

const Token = @This();

allocator: Allocator,
kind: Kind,
// TODO: allocate string on heap
literal: []const u8, // literal can be at most 32 bytes
loc: ?Location = null,

// store where in src token starts and ends
const Location = struct {
    start: usize,
    end: usize
};

pub fn init(allocator: Allocator, kind: Token.Kind, chars: []const u8) Allocator.Error!Token {
    const literal = try allocator.alloc(u8, chars.len);

    @memcpy(literal, chars);

    return .{
        .allocator = allocator,
        .kind = kind,
        .literal = literal,
    };
}

pub fn deinit(tok: *const Token) void {
    // std.debug.print("freeing tok '{s}' literal at addr: {*}\n", .{tok.literal, tok.literal});
    tok.allocator.free(tok.literal);
}

pub fn clone(tok: *const Token) Allocator.Error!Token {
    const literal = try tok.allocator.alloc(u8, tok.literal.len);
    std.mem.copyForwards(u8, literal, tok.literal);

    return Token{ .allocator = tok.allocator, .kind = tok.kind, .literal = literal };
}

pub fn tokenLiteral(tok: *const Token) []const u8 {
    // std.debug.print("tk_len = {}\n", .{tok.literal_len});
    return tok.literal;
}

pub const Kind = enum {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident, // add, foobar, x, y
    Int, // 12221378
    String,

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
    Semicolon, // ;
    Colon, // :

    Lparen, // (
    Rparen, // )
    Lbrace, // {
    Rbrace, // }
    Lbracket, // [
    Rbracket, // ]

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
};

// pub fn GetKindFromStr(str)

// TODO: remove and replace with a switch
// pub const Keywords = struct {
//
//     words: std.StringHashMap(Token.Kind),
//
//     pub fn init(allocator: Allocator) !Keywords {
//
//         var words = std.StringHashMap(Token.Kind).init(allocator);
//         try words.put("let", Token.Kind.Let);
//         try words.put("fn", Token.Kind.Function);
//         try words.put("true", Token.Kind.True);
//         try words.put("false", Token.Kind.False);
//         try words.put("if", Token.Kind.If);
//         try words.put("else", Token.Kind.Else);
//         try words.put("return", Token.Kind.Return);
//
//         return .{
//             .words = words,
//         };
//
//     }
//
//     pub fn deinit(keywords: *Keywords) void {
//
//         keywords.words.deinit();
//
//     }
// };
