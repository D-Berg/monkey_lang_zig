const std = @import("std");
const Token = @import("Token.zig");

const Allocator = std.mem.Allocator;

const print = std.debug.print;
const log = std.log;
const expect = std.testing.expect;
const expectEqualSlices = std.testing.expectEqualSlices;

// const TokenError = error {
//     unsupported_char,
//     no_next_token
// };
//
const LexError = error{} || Allocator.Error;

const Lexer = @This();

allocator: Allocator,
input: []const u8,
position: usize = 0,
readPosition: usize = 0,
ch: u8 = 0,

const KeyWordsStr = [7][]const u8{ "let", "fn", "true", "false", "if", "else", "return" };

const KeyWordsKinds = [7]Token.Kind{ .Let, .Function, .True, .False, .If, .Else, .Return };

pub fn init(allocator: Allocator, input: []const u8) Lexer {
    var l = Lexer{
        .allocator = allocator,
        .input = input,
    };

    l.readChar();

    return l;
}

// pub fn deinit(l: *Lexer) void {
//     l.key_words.deinit();
// }

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

fn GetKindFromKeyWord(word: []const u8) Token.Kind {
    for (KeyWordsStr, KeyWordsKinds) |str, kind| {
        if (std.mem.eql(u8, word, str)) {
            return kind;
        }
    }

    return .Ident;
}

/// Creates a new token
pub fn NextToken(l: *Lexer) LexError!Token {
    const a = l.allocator;

    var read_next_char = true;

    defer {
        if (read_next_char) l.readChar();
    }

    l.skipWhiteSpace();

    const chars = [1]u8{l.ch};
    switch (l.ch) {
        '=' => {
            if (l.peekChar() == '=') {
                const ch = l.ch;
                l.readChar();
                const eq_str = [2]u8{ ch, l.ch };
                return try Token.init(a, Token.Kind.Eq, &eq_str);
            } else {
                return try Token.init(a, Token.Kind.Assign, &chars);
            }
        },
        '+' => {
            return try Token.init(a, Token.Kind.Plus, &chars);
        },
        '-' => {
            return try Token.init(a, Token.Kind.Minus, &chars);
        },
        '!' => {
            if (l.peekChar() == '=') {
                const ch = l.ch;
                l.readChar();
                const neq_str = [2]u8{ ch, l.ch };
                return try Token.init(a, Token.Kind.Neq, &neq_str);
            } else {
                return try Token.init(a, Token.Kind.Bang, &chars);
            }
        },
        '/' => {
            return try Token.init(a, Token.Kind.Slash, &chars);
        },
        '*' => {
            return try Token.init(a, Token.Kind.Asterisk, &chars);
        },
        '<' => {
            return try Token.init(a, Token.Kind.Lt, &chars);
        },
        '>' => {
            return try Token.init(a, Token.Kind.Gt, &chars);
        },
        ';' => {
            return try Token.init(a, Token.Kind.Semicolon, &chars);
        },
        ',' => {
            return try Token.init(a, Token.Kind.Comma, &chars);
        },
        '(' => {
            return try Token.init(a, Token.Kind.Lparen, &chars);
        },
        ')' => {
            return try Token.init(a, Token.Kind.Rparen, &chars);
        },
        '{' => {
            return try Token.init(a, Token.Kind.Lbrace, &chars);
        },
        '}' => {
            return try Token.init(a, Token.Kind.Rbrace, &chars);
        },
        '"' => {
            const position = l.position + 1;

            while (true) { // TODO: fix infinite
                l.readChar();

                if (l.ch == '"' or l.ch == 0) {
                    break;
                }
            }

            const string = l.input[position..l.position];

            return try Token.init(a, .String, string);
        },
        0 => {
            return try Token.init(a, Token.Kind.Eof, "");
        },
        else => {
            if (isLetter(l.ch)) {
                read_next_char = false;
                const start = l.position;
                l.readIdentifier();
                const end = l.position;

                const identifer_str = l.input[start..end];
                const token_kind = GetKindFromKeyWord(identifer_str);

                return try Token.init(a, token_kind, identifer_str);
            }
            if (isDigit(l.ch)) {
                read_next_char = false;

                const start = l.position;
                l.readNumber();
                const end = l.position;

                const number_str = l.input[start..end];
                return try Token.init(a, Token.Kind.Int, number_str);
            } else {
                return try Token.init(a, Token.Kind.Illegal, &chars);
            }
        },
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

test "Create Tokens" {
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
        \\"foobar"
        \\"foo bar"
        \\[1, 2];
    ;

    var l = Lexer.init(allocator, input);

    var passed_tests: u32 = 0;

    const token_tests = [_]Token{
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
        try Token.init(allocator, Token.Kind.If, "if"),
        try Token.init(allocator, Token.Kind.Lparen, "("),
        try Token.init(allocator, Token.Kind.Int, "5"),
        try Token.init(allocator, Token.Kind.Lt, "<"),
        try Token.init(allocator, Token.Kind.Int, "10"),
        try Token.init(allocator, Token.Kind.Rparen, ")"),
        try Token.init(allocator, Token.Kind.Lbrace, "{"),
        try Token.init(allocator, Token.Kind.Return, "return"),
        try Token.init(allocator, Token.Kind.True, "true"),
        try Token.init(allocator, Token.Kind.Semicolon, ";"),
        try Token.init(allocator, Token.Kind.Rbrace, "}"),
        try Token.init(allocator, Token.Kind.Else, "else"),
        try Token.init(allocator, Token.Kind.Lbrace, "{"),
        try Token.init(allocator, Token.Kind.Return, "return"),
        try Token.init(allocator, Token.Kind.False, "false"),
        try Token.init(allocator, Token.Kind.Semicolon, ";"),
        try Token.init(allocator, Token.Kind.Rbrace, "}"),
        try Token.init(allocator, Token.Kind.Int, "10"),
        try Token.init(allocator, Token.Kind.Eq, "=="),
        try Token.init(allocator, Token.Kind.Int, "10"),
        try Token.init(allocator, Token.Kind.Semicolon, ";"),
        try Token.init(allocator, Token.Kind.Int, "10"),
        try Token.init(allocator, Token.Kind.Neq, "!="),
        try Token.init(allocator, Token.Kind.Int, "9"),
        try Token.init(allocator, Token.Kind.Semicolon, ";"),
        try Token.init(allocator, .String, "foobar"),
        try Token.init(allocator, .String, "foo bar"),
        try Token.init(allocator, .Lbracket, "["),
        try Token.init(allocator, .Int, "1"),
        try Token.init(allocator, .Comma, ","),
        try Token.init(allocator, .Int, "2"),
        try Token.init(allocator, .Rbracket, "]"),
        try Token.init(allocator, .Semicolon, ";"),


        try Token.init(allocator, Token.Kind.Eof, ""),
    };

    defer {
        for (token_tests) |tok| {
            tok.deinit();
        }
    }

    const n_tests = token_tests.len;

    for (token_tests, 0..) |expected_token, i| {

        const tok = try l.NextToken();
        defer tok.deinit();
        // print("{s}\n", .{&tok.literal});

        expect(tok.kind == expected_token.kind) catch |err| {

            print("token test {}/{}: Failed, tokenKind wrong. expected={}, got={}.\n", .{
                i, n_tests, expected_token.kind, tok.kind
            });

            print("Token: kind = {}, .literal = '{s}'\n", .{
                tok.kind, tok.literal
            });
            print("Error: {}\n", .{err});
            return err;
        };

        expectEqualSlices(u8, expected_token.literal, tok.literal) catch |err| {

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
