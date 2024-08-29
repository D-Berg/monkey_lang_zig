const std = @import("std");
const token_mod = @import("token.zig");

const Allocator = std.mem.Allocator;
const Token = token_mod.Token;
const Keywords = token_mod.Keywords;

const print = std.debug.print;
const log = std.log;
const expect = std.testing.expect;
const expectEqualSlices = std.testing.expectEqualSlices;



const TokenError = error {
    unsupported_char,
    no_next_token
};

pub const Lexer = struct {
    input: []const u8,
    position: usize = 0,
    readPosition: usize = 0,
    ch: u8 = 0,

    pub fn new(input: []const u8) Lexer {
        var l = Lexer {
            .input = input
        };

        l.readChar();


        return l;
    }

    fn readChar(l: *Lexer) void {

        if (l.readPosition >= l.input.len) {
            l.ch = 0;
        } else {
            l.ch = l.input[l.readPosition];
        }

        l.position = l.readPosition;
        l.readPosition += 1;
    }

    pub fn NextToken(l: *Lexer, allocator: Allocator, key_words: *const Keywords) !Token {

        var read_next_char = true; 

        defer {
            if (read_next_char) l.readChar();
        }

        l.skipWhiteSpace();

        const chars = [1]u8{ l.ch };
        switch (l.ch) {
            '=' => { return try Token.init(allocator, Token.Kind.Assign, &chars); },
            '+' => { return try Token.init(allocator, Token.Kind.Plus, &chars); },
            '-' => { return try Token.init(allocator, Token.Kind.Minus, &chars); },
            '!' => { return try Token.init(allocator, Token.Kind.Bang, &chars); },
            '/' => { return try Token.init(allocator, Token.Kind.Slash, &chars); },
            '*' => { return try Token.init(allocator, Token.Kind.Asterisk, &chars); },
            '<' => { return try Token.init(allocator, Token.Kind.Lt, &chars); },
            '>' => { return try Token.init(allocator, Token.Kind.Gt, &chars); },
            ';' => { return try Token.init(allocator, Token.Kind.Semicolon, &chars); },
            ',' => { return try Token.init(allocator, Token.Kind.Comma, &chars); },
            '(' => { return try Token.init(allocator, Token.Kind.Lparen, &chars); },
            ')' => { return try Token.init(allocator, Token.Kind.Rparen, &chars); },
            '{' => { return try Token.init(allocator, Token.Kind.Lbrace, &chars); },
            '}' => { return try Token.init(allocator, Token.Kind.Rbrace, &chars); },
            0 => { return try Token.init(allocator, Token.Kind.Eof, ""); },
            else => {
                if (isLetter(l.ch)) {

                    read_next_char = false;
                    const start = l.position;
                    l.readIdentifier();
                    const end = l.position;

                    const identifer_str = l.input[start..end];
                    const token_kind = key_words.words.get(identifer_str) orelse Token.Kind.Ident;

                    return try Token.init(allocator, token_kind, identifer_str);

                } if (isDigit(l.ch)) {
                    read_next_char = false;

                    const start = l.position;
                    l.readNumber();
                    const end = l.position;

                    const number_str = l.input[start..end];
                    return try Token.init(allocator, Token.Kind.Int, number_str);

                } else {
                    return try Token.init(allocator, Token.Kind.Illegal, &chars);
                }
            }
        }

    }

    fn readIdentifier(l: *Lexer) void {
        while (isLetter(l.ch)) l.readChar();
    }

    fn readNumber(l: *Lexer) void {
        while (isDigit(l.ch)) l.readChar();

    }

    fn skipWhiteSpace(l: *Lexer) void {
        while (l.ch == ' ' or l.ch == '\t' or l.ch == '\n' or l.ch == '\r') {
            l.readChar();
        }
    }
};

fn isLetter(ch: u8) bool {

    const is_letter = ('a' <= ch and ch <= 'z') or 
        ('A' <= ch and ch <= 'Z') or ch == '_';

    return is_letter;
}

fn isDigit(ch: u8) bool {
    return '0' <= ch and ch <= '9';
}

test "next token" {
    const allocator = std.testing.allocator;

    var key_words = try Keywords.init(allocator);
    defer key_words.deinit();

    const input = 
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
    ;

    var l = Lexer.new(input);

    const token_tests = [_]Token {
        try Token.init(allocator, Token.Kind.Let, "let"),
        try Token.init(allocator, Token.Kind.Ident, "five"),
        try Token.init(allocator, Token.Kind.Assign, "="),
        try Token.init(allocator, Token.Kind.Int, "5"),
        try Token.init(allocator, Token.Kind.Semicolon, ";"),
        try Token.init(allocator, Token.Kind.Let, "let"),
        try Token.init(allocator, Token.Kind.Ident, "ten"),
        try Token.init(allocator, Token.Kind.Assign, "="),
        try Token.init(allocator, Token.Kind.Int, "10"),
        try Token.init(allocator, Token.Kind.Semicolon, ";"),
        try Token.init(allocator, Token.Kind.Let, "let"),
        try Token.init(allocator, Token.Kind.Ident, "add"),
        try Token.init(allocator, Token.Kind.Assign, "="),
        try Token.init(allocator, Token.Kind.Function, "fn"),
        try Token.init(allocator, Token.Kind.Lparen, "("),
        try Token.init(allocator, Token.Kind.Ident, "x"),
        try Token.init(allocator, Token.Kind.Comma, ","),
        try Token.init(allocator, Token.Kind.Ident, "y"),
        try Token.init(allocator, Token.Kind.Rparen, ")"),
        try Token.init(allocator, Token.Kind.Lbrace, "{"),
        try Token.init(allocator, Token.Kind.Ident, "x"),
        try Token.init(allocator, Token.Kind.Plus, "+"),
        try Token.init(allocator, Token.Kind.Ident, "y"),
        try Token.init(allocator, Token.Kind.Semicolon, ";"),
        try Token.init(allocator, Token.Kind.Rbrace, "}"),
        try Token.init(allocator, Token.Kind.Semicolon, ";"),
        try Token.init(allocator, Token.Kind.Let, "let"),
        try Token.init(allocator, Token.Kind.Ident, "result"),
        try Token.init(allocator, Token.Kind.Assign, "="),
        try Token.init(allocator, Token.Kind.Ident, "add"),
        try Token.init(allocator, Token.Kind.Lparen, "("),
        try Token.init(allocator, Token.Kind.Ident, "five"),
        try Token.init(allocator, Token.Kind.Comma, ","),
        try Token.init(allocator, Token.Kind.Ident, "ten"),
        try Token.init(allocator, Token.Kind.Rparen, ")"),
        try Token.init(allocator, Token.Kind.Semicolon, ";"),
        try Token.init(allocator, Token.Kind.Bang, "!"),
        try Token.init(allocator, Token.Kind.Minus, "-"),
        try Token.init(allocator, Token.Kind.Slash, "/"),
        try Token.init(allocator, Token.Kind.Asterisk, "*"),
        try Token.init(allocator, Token.Kind.Int, "5"),
        try Token.init(allocator, Token.Kind.Semicolon, ";"),
        try Token.init(allocator, Token.Kind.Int, "5"),
        try Token.init(allocator, Token.Kind.Lt, "<"),
        try Token.init(allocator, Token.Kind.Int, "10"),
        try Token.init(allocator, Token.Kind.Gt, ">"),
        try Token.init(allocator, Token.Kind.Int, "5"),
        try Token.init(allocator, Token.Kind.Semicolon, ";"),
        try Token.init(allocator, Token.Kind.Eof, ""),
    };

    defer {
        for (token_tests) |token| {
            token.deinit();
        }
    }

    for (token_tests, 0..) |expected_token, i| {

        const tok = l.NextToken(allocator, &key_words) catch |err| {
            print("token test {}: Failed with Error: {}.\n", .{
                i, err
            });
            return err;
        };

        defer allocator.free(tok.literal);

        expect(tok.kind == expected_token.kind) catch |err| {

            print("token test {}: Failed, tokenKind wrong. expected={}, got={}.\n", .{
                i, expected_token.kind, tok.kind
            });

            print("Token: kind = {}, .literal = '{s}'\n", .{
                tok.kind, tok.literal
            });
            print("Error: {}", .{err});
            return err;
        };

        expectEqualSlices(u8, expected_token.literal, tok.literal) catch |err| {

            print("token tests {}: Failed, literal wrong, expected='{s}', got='{s}'.\n", .{
                i, expected_token.literal, tok.literal

            });

            return err;

        };

        print("\u{001b}[31mtoken test {}: succeded\u{001b}[0m, kind={}, literal='{s}'\n", .{
            i, tok.kind,  tok.literal
        });



    }
}
