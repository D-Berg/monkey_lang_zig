const std = @import("std");


kind: Kind,
literal: []const u8,
loc: ?Location = null,
        
// store where in src token starts and ends
const Location = struct {
    start: usize,
    end: usize,
    line: usize
};

        
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


