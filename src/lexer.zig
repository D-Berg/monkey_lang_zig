const std = @import("std");
const Token = @import("token.zig").Token;
const Allocator = std.mem.Allocator;


const print = std.debug.print;
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

    pub fn NextToken(l: *Lexer, allocator: Allocator, ) !Token {

        const maybe_tok: TokenError!Token = switch (l.ch) {
            '=' => try Token.new(allocator, Token.Kind.Assign, l.ch),
            ';' => try Token.new(allocator, Token.Kind.Semicolon, l.ch),
            '(' => try Token.new(allocator, Token.Kind.Lparen, l.ch),
            ')' => try Token.new(allocator, Token.Kind.Rparen, l.ch),
            '{' => try Token.new(allocator, Token.Kind.Lbrace, l.ch),
            '}' => try Token.new(allocator, Token.Kind.Rbrace, l.ch),
            '+' => try Token.new(allocator, Token.Kind.Plus, l.ch),
            ',' => try Token.new(allocator, Token.Kind.Comma, l.ch),
            0 => Token { .kind = Token.Kind.Eof, .literal = "" },
            else => TokenError.unsupported_char,

        };

        l.readChar();
        return maybe_tok;

    }
};

test "next token" {

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const input = "=+(){},;";

    var l = Lexer.new(input);

    const token_tests = [_]Token {
        Token { .kind = Token.Kind.Assign, .literal = "="},
        Token { .kind = Token.Kind.Plus, .literal = "+"},
    };

    for (token_tests, 0..) |expected_token, i| {

        const tok = try l.NextToken(allocator);
        defer allocator.free(tok.literal);

        expect(tok.kind == expected_token.kind) catch |err| {

            print("token_test[{}] - tokenKind wrong. expected={}, got={}.\n", .{
                i, expected_token.kind, tok.kind
            });

            print("l.char = {}, l.position = {}, l.readPosition = {}\n", .{
                l.ch, l.position, l.readPosition
            });
            print("Error: {}", .{err});
            return err;
        };

        expectEqualSlices(u8, expected_token.literal, tok.literal) catch |err| {

            print("token_tests[{}]-literal wrong, expected='{s}', got='{s}'.\n", .{
                i, expected_token.literal, tok.literal

            });

            return err;

        };
        print("literal: {s}\n", .{tok.literal});

    }
}
