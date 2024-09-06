const std = @import("std");
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");
const ast = @import("ast.zig");
const Program = ast.Program;
const Statement = ast.Statement;
const LetStatement = ast.LetStatement;

const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

const Parser = @This();

lexer: *Lexer,
current_token: Token,
peek_token: Token,

pub fn init(lexer: *Lexer) !Parser {

    return .{ 
        .lexer = lexer,
        .current_token = try lexer.NextToken(),
        .peek_token = try lexer.NextToken()
    };

}

pub fn deinit(parser: *Parser) void {
    parser.current_token.deinit();
    parser.peek_token.deinit();
}

fn nextToken(parser: *Parser) !void {
    parser.current_token.deinit();
    parser.current_token = parser.peek_token;
    parser.peek_token = try parser.lexer.NextToken();
}

fn parseStatement(parser: *Parser) ?Statement {

    switch (parser.current_token.kind) {
        Token.Kind.Let => {
            return parser.parseLetStatement();
        },
        else => {
            return null;
        }
    }

}

fn parseLetStatement(parser: *Parser) ?LetStatement {

    if (!parser.expectPeek(Token.Kind.Ident)) return null;

    if (!parser.expectPeek(Token.Kind.Assign)) return null;

}

fn curTokenIs(parser: *Parser, kind: Token.Kind) bool {
    return parser.current_token.kind == kind;
}

fn peekTokenIs(parser: *Parser, kind: Token.Kind) bool {
    return parser.peek_token.kind == kind;
}

fn expectPeek(parser: *Parser, kind: Token.Kind) bool {

    if (parser.peekTokenIs(kind)) {
        // TODO: parser.nextToken();
        return true;
    } else {
        return false;
    }

}

pub fn ParseProgram(parser: *Parser, allocator: Allocator) !Program {

    var program = Program.init(allocator);

    while (parser.current_token.kind != Token.Kind.Eof) {
        const maybe_statement = parser.parseStatement();

        if (maybe_statement) |statement| {
            try program.statements.append(statement);
        }

        try parser.nextToken();

    }
    
    return program;

}






test "LetStatements" {

    const allocator = std.testing.allocator;

    const input = 
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();

    var parser = try Parser.init(&lexer);
    defer parser.deinit();

    const program = try parser.ParseProgram(allocator);
    // _ = program;

    try expect(program.statements.items.len == 3);


}
