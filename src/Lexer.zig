const std = @import("std");
const Token = @import("Token.zig");

const Allocator = std.mem.Allocator;

const print = std.debug.print;
const log = std.log;
const expect = std.testing.expect;
const expectEqualSlices = std.testing.expectEqualSlices;

const Lexer = @This();

source: []const u8,
/// the current character being analyzed
position: usize = 0,
/// the next position to be read
read_position: usize = 0,
line: usize = 1,
ch: u8 = 0,

const KeyWordsStr = [7][]const u8{ "let", "fn", "true", "false", "if", "else", "return" };

const KeyWordsKinds = [7]Token.Kind{ .Let, .Function, .True, .False, .If, .Else, .Return };

pub fn init(input: []const u8) Lexer {
    var l = Lexer{
        .source = input,
    };

    l.readChar();

    return l;
}

// pub fn deinit(l: *Lexer) void {
//     l.key_words.deinit();
// }

fn readChar(l: *Lexer) void {
    if (l.read_position >= l.source.len) {
        l.ch = 0;
    } else {
        l.ch = l.source[l.read_position];
    }

    l.position = l.read_position;
    l.read_position += 1;
}

fn peekChar(l: *Lexer) u8 {
    if (l.read_position >= l.source.len) {
        return 0;
    } else {
        return l.source[l.read_position];
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
pub fn nextToken(l: *Lexer) Token {

    var read_next_char = true;

    defer {
        if (read_next_char) l.readChar();
    }

    l.skipWhiteSpace();

    
    if (l.read_position > l.source.len ) {
        return Token { .kind =.Eof, .literal = "" };
    }

    std.debug.assert(l.position < l.source.len);

    const chars = l.source[l.position..l.read_position];

    std.debug.assert(chars.len == 1);
    std.debug.assert(chars[0] == l.ch);

    switch (l.ch) {
        '=' => {
            if (l.peekChar() == '=') {
                const start = l.position;
                l.readChar();
                const end = l.position + 1;
                return Token { .kind = .Eq, .literal = l.source[start..end] };
            } else {
                return Token{ .kind = .Assign, .literal = chars };
            }
        },
        '+' => return Token{ .kind = .Plus, .literal = chars },
        '-' => return Token{ .kind = .Minus, .literal = chars },
        '!' => {
            if (l.peekChar() == '=') {
                const start = l.position;
                l.readChar();
                const end = l.position + 1;
                return Token { .kind = .Neq, .literal = l.source[start..end] };
            } else {
                return Token { .kind = .Bang, .literal = chars };
            }
        },
        '/' => {
            if (l.peekChar() == '/') { // skip over comments

                while (l.ch != '\n') : (l.readChar()) {}

                return l.nextToken();

            } else {
                return Token{ .kind = .Slash, .literal = chars };
            }
        },
        '*' => return Token { .kind = .Asterisk, .literal = chars },
        '<' => return Token { .kind = .Lt, .literal = chars },
        '>' => return Token { .kind = .Gt, .literal = chars },
        ';' => return Token { .kind = .Semicolon, .literal = chars },
        ':' => return Token { .kind = .Colon, .literal = chars },
        ',' => return Token { .kind = .Comma, .literal = chars },
        '(' => return Token { .kind = .Lparen, .literal = chars },
        ')' => return Token { .kind = .Rparen, .literal = chars },
        '{' => return Token { .kind = .Lbrace, .literal = chars },
        '}' => return Token { .kind = .Rbrace, .literal = chars },
        '[' => return Token { .kind = .Lbracket, .literal = chars },
        ']' => return Token { .kind = .Rbracket, .literal = chars },
        // 0 => {
        //     return Token { .kind =.Eof, .literal = "" };
        // },
        '"' => {
            const position = l.position + 1;

            while (true) { // TODO: fix infinite
                l.readChar();

                if (l.ch == '"' or l.ch == 0) {
                    break;
                }
            }

            const string = l.source[position..l.position];

            return Token { .kind = .String, .literal = string };
        },
        else => {
            if (isLetter(l.ch)) {
                read_next_char = false;
                const start = l.position;
                l.readIdentifier();
                const end = l.position;

                const identifer_str = l.source[start..end];
                const token_kind = GetKindFromKeyWord(identifer_str);

                return Token { .kind = token_kind, .literal = identifer_str };
            }
            if (isDigit(l.ch)) {
                read_next_char = false;

                const start = l.position;
                l.readNumber();
                const end = l.position;

                const number_str = l.source[start..end];
                return Token { .kind = .Int, .literal = number_str };
            } else {
                return Token { .kind = .Illegal, .literal =  chars };
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
    sw: switch (l.ch) {
        ' ', '\t', '\r' => {
            l.readChar();
            continue :sw l.ch;
        },
        '\n' => {
            l.readChar();
            l.line += 1;

            continue :sw l.ch;
        },
        else => break :sw,
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
        \\{"name": "Anna", "age": 24}
    ;

    var l = Lexer.init(input);

    var passed_tests: u32 = 0;

    const token_tests = [_]Token{
        Token { .kind = .Let, .literal = "let" },
        Token { .kind = .Ident, .literal = "five" },
        Token { .kind = .Assign, .literal = "=" },
        Token { .kind = .Int, .literal = "5" },
        Token { .kind = .Semicolon, .literal = ";" },
        Token { .kind = .Let, .literal = "let" },
        Token { .kind = .Ident, .literal = "ten" },
        Token { .kind = .Assign, .literal = "=" },
        Token { .kind = .Int, .literal = "10" },
        Token { .kind = .Semicolon, .literal = ";" },
        Token { .kind = .Let, .literal = "let" },
        Token { .kind = .Ident, .literal = "add" },
        Token { .kind = .Assign, .literal = "=" },
        Token { .kind = .Function, .literal = "fn" },
        Token { .kind = .Lparen, .literal = "(" },
        Token { .kind = .Ident, .literal = "x" },
        Token { .kind = .Comma, .literal = "," },
        Token { .kind = .Ident, .literal = "y" },
        Token { .kind = .Rparen, .literal = ")" },
        Token { .kind = .Lbrace, .literal = "{" },
        Token { .kind = .Ident, .literal = "x" },
        Token { .kind = .Plus, .literal = "+" },
        Token { .kind = .Ident, .literal = "y" },
        Token { .kind = .Semicolon, .literal = ";" },
        Token { .kind = .Rbrace, .literal = "}" },
        Token { .kind = .Semicolon, .literal = ";" },
        Token { .kind = .Let, .literal = "let" },
        Token { .kind = .Ident, .literal = "result" },
        Token { .kind = .Assign, .literal = "=" },
        Token { .kind = .Ident, .literal = "add" },
        Token { .kind = .Lparen, .literal = "(" },
        Token { .kind = .Ident, .literal = "five" },
        Token { .kind = .Comma, .literal = "," },
        Token { .kind = .Ident, .literal = "ten" },
        Token { .kind = .Rparen, .literal = ")" },
        Token { .kind = .Semicolon, .literal = ";" },
        Token { .kind = .Bang, .literal = "!" },
        Token { .kind = .Minus, .literal = "-" },
        Token { .kind = .Slash, .literal = "/" },
        Token { .kind = .Asterisk, .literal = "*" },
        Token { .kind = .Int, .literal = "5" },
        Token { .kind = .Semicolon, .literal = ";" },
        Token { .kind = .Int, .literal = "5" },
        Token { .kind = .Lt, .literal = "<" },
        Token { .kind = .Int, .literal = "10" },
        Token { .kind = .Gt, .literal = ">" },
        Token { .kind = .Int, .literal = "5" },
        Token { .kind = .Semicolon, .literal = ";" },
        Token { .kind = .If, .literal = "if" },
        Token { .kind = .Lparen, .literal = "(" },
        Token { .kind = .Int, .literal = "5" },
        Token { .kind = .Lt, .literal = "<" },
        Token { .kind = .Int, .literal = "10" },
        Token { .kind = .Rparen, .literal = ")" },
        Token { .kind = .Lbrace, .literal = "{" },
        Token { .kind = .Return, .literal = "return" },
        Token { .kind = .True, .literal = "true" },
        Token { .kind = .Semicolon, .literal = ";" },
        Token { .kind = .Rbrace, .literal = "}" },
        Token { .kind = .Else, .literal = "else" },
        Token { .kind = .Lbrace, .literal = "{" },
        Token { .kind = .Return, .literal = "return" },
        Token { .kind = .False, .literal = "false" },
        Token { .kind = .Semicolon, .literal = ";" },
        Token { .kind = .Rbrace, .literal = "}" },
        Token { .kind = .Int, .literal = "10" },
        Token { .kind = .Eq, .literal = "==" },
        Token { .kind = .Int, .literal = "10" },
        Token { .kind = .Semicolon, .literal = ";" },
        Token { .kind = .Int, .literal = "10" },
        Token { .kind = .Neq, .literal = "!=" },
        Token { .kind = .Int, .literal = "9" },
        Token { .kind = .Semicolon, .literal = ";" },
        Token { .kind = .String, .literal = "foobar" },
        Token { .kind = .String, .literal = "foo bar" },
        Token { .kind = .Lbracket, .literal = "[" },
        Token { .kind = .Int, .literal = "1" },
        Token { .kind = .Comma, .literal = "," },
        Token { .kind = .Int, .literal = "2" },
        Token { .kind = .Rbracket, .literal = "]" },
        Token { .kind = .Semicolon, .literal = ";" },
        Token { .kind = .Lbrace, .literal = "{" },
        Token { .kind = .String, .literal = "name" },
        Token { .kind = .Colon, .literal = ":" },
        Token { .kind = .String, .literal = "Anna" },
        Token { .kind = .Comma, .literal = "," },
        Token { .kind = .String, .literal = "age" },
        Token { .kind = .Colon, .literal = ":" },
        Token { .kind = .Int, .literal = "24" },
        Token { .kind = .Rbrace, .literal = "}" },

        Token { .kind = .Eof, .literal = "" },
    };

    const n_tests = token_tests.len;

    for (token_tests, 0..) |expected_token, i| {

        const tok = l.nextToken();
        // defer tok.deinit();
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

    try std.testing.expectEqual(23, l.line);

    // print("\u{001b}[32mtoken test {}/{}: succeded\u{001b}[0m\n", .{
    //     passed_tests, n_tests
    // });
}
