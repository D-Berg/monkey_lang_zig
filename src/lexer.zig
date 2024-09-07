const std = @import("std");
const Token = @import("Token.zig");

const Allocator = std.mem.Allocator;

const Keywords = Token.Keywords;

const print = std.debug.print;
const log = std.log;
const expect = std.testing.expect;
const expectEqualSlices = std.testing.expectEqualSlices;

const TokenError = error {
    unsupported_char,
    no_next_token
};

const Lexer = @This();

allocator: Allocator,
input: []const u8,
position: usize = 0,
readPosition: usize = 0,
ch: u8 = 0,
key_words: Keywords,

pub fn init(allocator: Allocator, input: []const u8) !Lexer {
    var l = Lexer {
        .allocator = allocator,
        .input = input,
        .key_words = try Keywords.init(allocator)
    };

    l.readChar();

    return l;
}

pub fn deinit(l: *Lexer) void {
    l.key_words.deinit();
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

fn peekChar(l: *Lexer) u8 {
    if (l.readPosition >= l.input.len) {
        return 0;
    } else {
        return l.input[l.readPosition];
    }
}

pub fn NextToken(l: *Lexer) Token {

    var read_next_char = true; 

    defer {
        if (read_next_char) l.readChar();
    }

    l.skipWhiteSpace();

    const chars = [1]u8{ l.ch };
    switch (l.ch) {
        '=' => { 
            if (l.peekChar() == '=') {
                const ch = l.ch;
                l.readChar();
                const eq_str = [2]u8{ ch, l.ch};
                return Token.init(Token.Kind.Eq, &eq_str);
            } else {
                return Token.init(Token.Kind.Assign, &chars); 
            }
        },
        '+' => { return Token.init(Token.Kind.Plus, &chars); },
        '-' => { return Token.init(Token.Kind.Minus, &chars); },
        '!' => { 
            if (l.peekChar() == '=') {
                const ch = l.ch;
                l.readChar();
                const neq_str = [2]u8{ ch, l.ch };
                return Token.init(Token.Kind.Neq, &neq_str);
            } else {
                return Token.init(Token.Kind.Bang, &chars); 
            }
        },
        '/' => { return Token.init(Token.Kind.Slash, &chars); },
        '*' => { return Token.init(Token.Kind.Asterisk, &chars); },
        '<' => { return Token.init(Token.Kind.Lt, &chars); },
        '>' => { return Token.init(Token.Kind.Gt, &chars); },
        ';' => { return Token.init(Token.Kind.Semicolon, &chars); },
        ',' => { return Token.init(Token.Kind.Comma, &chars); },
        '(' => { return Token.init(Token.Kind.Lparen, &chars); },
        ')' => { return Token.init(Token.Kind.Rparen, &chars); },
        '{' => { return Token.init(Token.Kind.Lbrace, &chars); },
        '}' => { return Token.init(Token.Kind.Rbrace, &chars); },
        0 => { return Token.init(Token.Kind.Eof, ""); },
        else => {
            if (isLetter(l.ch)) {

                read_next_char = false;
                const start = l.position;
                l.readIdentifier();
                const end = l.position;

                const identifer_str = l.input[start..end];
                const token_kind = l.key_words.words.get(identifer_str) orelse Token.Kind.Ident;

                return Token.init(token_kind, identifer_str);

            } if (isDigit(l.ch)) {
                read_next_char = false;

                const start = l.position;
                l.readNumber();
                const end = l.position;

                const number_str = l.input[start..end];
                return Token.init(Token.Kind.Int, number_str);

            } else {
                return Token.init(Token.Kind.Illegal, &chars);
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
        \\
        \\if (5 < 10) {
        \\  return true;
        \\} else {
        \\  return false;
        \\}
        \\
        \\10 == 10;
        \\10 != 9;
    ;

    var l = try Lexer.init(allocator, input);
    defer l.deinit();

    var passed_tests: u32 = 0;

    const token_tests = [_]Token {
        Token.init(Token.Kind.Let, "let"),
        Token.init(Token.Kind.Ident, "five"),
        Token.init(Token.Kind.Assign, "="),
        Token.init(Token.Kind.Int, "5"),
        Token.init(Token.Kind.Semicolon, ";"),
        Token.init(Token.Kind.Let, "let"),
        Token.init(Token.Kind.Ident, "ten"),
        Token.init(Token.Kind.Assign, "="),
        Token.init(Token.Kind.Int, "10"),
        Token.init(Token.Kind.Semicolon, ";"),
        Token.init(Token.Kind.Let, "let"),
        Token.init(Token.Kind.Ident, "add"),
        Token.init(Token.Kind.Assign, "="),
        Token.init(Token.Kind.Function, "fn"),
        Token.init(Token.Kind.Lparen, "("),
        Token.init(Token.Kind.Ident, "x"),
        Token.init(Token.Kind.Comma, ","),
        Token.init(Token.Kind.Ident, "y"),
        Token.init(Token.Kind.Rparen, ")"),
        Token.init(Token.Kind.Lbrace, "{"),
        Token.init(Token.Kind.Ident, "x"),
        Token.init(Token.Kind.Plus, "+"),
        Token.init(Token.Kind.Ident, "y"),
        Token.init(Token.Kind.Semicolon, ";"),
        Token.init(Token.Kind.Rbrace, "}"),
        Token.init(Token.Kind.Semicolon, ";"),
        Token.init(Token.Kind.Let, "let"),
        Token.init(Token.Kind.Ident, "result"),
        Token.init(Token.Kind.Assign, "="),
        Token.init(Token.Kind.Ident, "add"),
        Token.init(Token.Kind.Lparen, "("),
        Token.init(Token.Kind.Ident, "five"),
        Token.init(Token.Kind.Comma, ","),
        Token.init(Token.Kind.Ident, "ten"),
        Token.init(Token.Kind.Rparen, ")"),
        Token.init(Token.Kind.Semicolon, ";"),
        Token.init(Token.Kind.Bang, "!"),
        Token.init(Token.Kind.Minus, "-"),
        Token.init(Token.Kind.Slash, "/"),
        Token.init(Token.Kind.Asterisk, "*"),
        Token.init(Token.Kind.Int, "5"),
        Token.init(Token.Kind.Semicolon, ";"),
        Token.init(Token.Kind.Int, "5"),
        Token.init(Token.Kind.Lt, "<"),
        Token.init(Token.Kind.Int, "10"),
        Token.init(Token.Kind.Gt, ">"),
        Token.init(Token.Kind.Int, "5"),
        Token.init(Token.Kind.Semicolon, ";"),
        Token.init(Token.Kind.If, "if"),
        Token.init(Token.Kind.Lparen, "("),
        Token.init(Token.Kind.Int, "5"),
        Token.init(Token.Kind.Lt, "<"),
        Token.init(Token.Kind.Int, "10"),
        Token.init(Token.Kind.Rparen, ")"),
        Token.init(Token.Kind.Lbrace, "{"),
        Token.init(Token.Kind.Return, "return"),
        Token.init(Token.Kind.True, "true"),
        Token.init(Token.Kind.Semicolon, ";"),
        Token.init(Token.Kind.Rbrace, "}"),
        Token.init(Token.Kind.Else, "else"),
        Token.init(Token.Kind.Lbrace, "{"),
        Token.init(Token.Kind.Return, "return"),
        Token.init(Token.Kind.False, "false"),
        Token.init(Token.Kind.Semicolon, ";"),
        Token.init(Token.Kind.Rbrace, "}"),
        Token.init(Token.Kind.Int, "10"),
        Token.init(Token.Kind.Eq, "=="),
        Token.init(Token.Kind.Int, "10"),
        Token.init(Token.Kind.Semicolon, ";"),
        Token.init(Token.Kind.Int, "10"),
        Token.init(Token.Kind.Neq, "!="),
        Token.init(Token.Kind.Int, "9"),
        Token.init(Token.Kind.Semicolon, ";"),

        Token.init(Token.Kind.Eof, ""),
    };

    const n_tests = token_tests.len;

    for (token_tests, 0..) |expected_token, i| {

        const tok = l.NextToken();
        // print("{s}\n", .{&tok.literal});

        expect(tok.kind == expected_token.kind) catch |err| {

            print("token test {}/{}: Failed, tokenKind wrong. expected={}, got={}.\n", .{
                i, n_tests, expected_token.kind, tok.kind
            });

            print("Token: kind = {}, .literal = '{s}'\n", .{
                tok.kind, tok.literal
            });
            print("Error: {}", .{err});
            return err;
        };

        expectEqualSlices(u8, &expected_token.literal, &tok.literal) catch |err| {

            print("token tests {}/{}: Failed, literal wrong, expected='{s}', got='{s}'.\n", .{
                i, n_tests, expected_token.literal, tok.literal

            });

            return err;

        };

        passed_tests += 1;

    }

    // print("\u{001b}[32mtoken test {}/{}: succeded\u{001b}[0m\n", .{
    //     passed_tests, n_tests
    // });
}
