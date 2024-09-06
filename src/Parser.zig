const std = @import("std");
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");
const ast = @import("ast.zig");
const Program = ast.Program;
const Statement = ast.Statement;
const Identifier = ast.Identifier;

const Allocator = std.mem.Allocator;
const expect = std.testing.expect;
const expectEqualSlices = std.testing.expectEqualSlices;

const print = std.debug.print;

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

fn nextToken(parser: *Parser) void {
    // parser.current_token.deinit();
    parser.current_token = parser.peek_token;
    parser.peek_token = parser.lexer.NextToken() catch {
        @panic("nextToken failed");
    };
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

fn parseLetStatement(parser: *Parser) ?Statement {

    var statement = Statement { .token = parser.current_token };
    // TODO: possible mem leak

    if (!parser.expectPeek(Token.Kind.Ident)) return null;

    statement.name = Identifier { 
        .token = parser.current_token, 
        .value = parser.current_token.literal // do I need to mem copy?
    }; // TODO: possible mem leak

    if (!parser.expectPeek(Token.Kind.Assign)) return null;

    while (!parser.curTokenIs(Token.Kind.Semicolon)) {
        parser.nextToken();
    }

    return statement;
}

fn curTokenIs(parser: *Parser, kind: Token.Kind) bool {
    return parser.current_token.kind == kind;
}

fn peekTokenIs(parser: *Parser, kind: Token.Kind) bool {
    return parser.peek_token.kind == kind;
}

fn expectPeek(parser: *Parser, kind: Token.Kind) bool {

    if (parser.peekTokenIs(kind)) {
        parser.nextToken();
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

        parser.nextToken();

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

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    expect(program.statements.items.len == 3) catch |err| {
        print("Statements len is not 3: Failed with error", .{});
        return err;
    };

    const expected_identiefer = [3][]const u8 {
        "x", "y", "foobar"
    };

    _ = expected_identiefer;

    for (0..3) |i| {
        
        expectEqualSlices(
            u8,
            program.statements.items[i].token.literal, 
            "let"
        ) catch |err| {
            print("token literal is let", .{});
            return err;
        };


    }


}
