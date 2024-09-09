const std = @import("std");
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");
const ast = @import("ast.zig");
const Program = ast.Program;
const Statement = ast.Statement;
const Identifier = ast.Identifier;
const Expression = ast.Expression;
const ArrayList = std.ArrayList;

const Allocator = std.mem.Allocator;
const expect = std.testing.expect;
const expectEqualStrings = std.testing.expectEqualStrings;

const print = std.debug.print;

const Parser = @This();

allocator: Allocator,
lexer: *Lexer,
current_token: Token,
peek_token: Token,
errors: ArrayList([]const u8),

const Precedence = enum {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
};

pub fn init(lexer: *Lexer, allocator: Allocator) Parser {

    return .{ 
        .allocator = allocator, 
        .lexer = lexer,
        .current_token = lexer.NextToken(),
        .peek_token = lexer.NextToken(),
        .errors = ArrayList([]const u8).init(allocator)
    };

}

pub fn deinit(parser: Parser) void {
    for (parser.errors.items) |parse_err| {

        parser.allocator.free(parse_err);

    }

    parser.errors.deinit();
    
}


fn nextToken(parser: *Parser) void {
    parser.current_token = parser.peek_token;
    parser.peek_token = parser.lexer.NextToken();

}

fn parseStatement(parser: *Parser) ?Statement {
    switch (parser.current_token.kind) {
        Token.Kind.Let => {
            return parser.parseLetStatement();
        },
        Token.Kind.Return => {

            return parser.parseReturnStatement();
        },
        else => {
            return parser.parseExpressionStatement();
        }
    }
}

fn parseLetStatement(parser: *Parser) ?Statement {

    var statement = Statement { .token = parser.current_token, .kind = .Let};
    // TODO: possible mem leak

    if (!parser.expectPeek(Token.Kind.Ident)) return null;

    statement.identifier = Identifier { .token = parser.current_token };

    if (!parser.expectPeek(Token.Kind.Assign)) return null;

    while (!parser.curTokenIs(Token.Kind.Semicolon)) {
        parser.nextToken();
    }

    return statement;
}

fn parseReturnStatement(parser: *Parser) ?Statement {

    const stmt = Statement { .token = parser.current_token, .kind = .Return };

    parser.nextToken();

    while (!parser.curTokenIs(Token.Kind.Semicolon)) {
        parser.nextToken();
    }

    return stmt;

}

fn parseExpressionStatement(parser: *Parser) ?Statement {

    var stmt = Statement { 
        .token = parser.current_token,
        .kind = .Expression
    };

    stmt.expression = parser.parseExpression();

    while (parser.peekTokenIs(Token.Kind.Semicolon)) {
        parser.nextToken();
    }

    return stmt;

}

fn parseExpression(parser: *Parser) ?Expression {

    switch (parser.current_token.kind) {

        Token.Kind.Ident => {
            return parser.parseIdentifier();
        },

        Token.Kind.Int =>{
            return parser.parseIntegerLiteral();
        },

        else => {
            return null;
        }

    }
}

fn parseIdentifier(parser: *Parser) Expression {

    return Expression {
        .token = parser.current_token,
        .kind = .Identifier,
    };

}

fn parseIntegerLiteral(parser: *Parser) Expression {
    
    var stmt = Expression { 
        .token = parser.current_token,
        .kind = .IntegerLiteral,
    };

    const maybe_val = std.fmt.parseInt(u32, stmt.token.tokenLiteral(), 0) catch null;

    if (maybe_val) |val| {
        stmt.value = Expression.Value {.int_val = val};
    } else {
        parser.errors.append("Error: Couldnt parse integer") catch {
            @panic("out of mem in append");
        };
    }

    return stmt;

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
        parser.peekError(kind);
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

fn checkParseErrors(parser: *Parser) error{ParsingError}!void {

    if (parser.errors.items.len > 0) return error.ParsingError;

}

fn peekError(parser: *Parser, kind: Token.Kind) void {
    const msg = std.fmt.allocPrint(parser.allocator, "Expected next token to be {any}, got {any} instead", .{
        kind, parser.peek_token.kind 
    }) catch {
        @panic("failed to create error msg");
    };

    parser.errors.append(msg) catch {
        @panic("failed to allocate error msg");
    };


}


test "Let Statements" {

    const allocator = std.testing.allocator;

    const correct_input = 
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    var lexer = try Lexer.init(allocator, correct_input);
    defer lexer.deinit();

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    try parser.checkParseErrors();

    expect(parser.errors.items.len == 0) catch |err| {

        print("Encountered {} parsing errors\n", .{
            parser.errors.items.len
        });

        for (parser.errors.items) |parse_err| {

            print("parse error: {s}\n", .{parse_err});

        }

        return err;

    };

    expect(program.statements.items.len == 3) catch |err| {
        print("Statements len is not 3: Failed with error", .{});
        return err;
    };

    const expected_identiefers = [3][]const u8 {
        "x", "y", "foobar"
    };


    for (program.statements.items, 0..) |*statement, i| {
        try expect(statement.kind == .Let);
        try expectEqualStrings(statement.tokenLiteral(), "let");

        try expectEqualStrings(statement.identifier.?.token.tokenLiteral(), expected_identiefers[i]);

    }
}



test "Return Statements" {
    const allocator = std.testing.allocator;

    const input = 
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    try parser.checkParseErrors();

    try expect(program.statements.items.len == 3);

    for (program.statements.items) |*statement| {
        try expect(statement.kind == .Return);
        try expectEqualStrings("return", statement.tokenLiteral());
    }

}

test "Identifier Expression" {
    const allocator = std.testing.allocator;

    const input = "foobar;";

    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    try parser.checkParseErrors();

    try expect(program.statements.items.len == 1);

    var stmt = program.statements.items[0];

    try expect(stmt.kind == .Expression);

    // try expectEqualStrings(ident.value.?)
    
    try expectEqualStrings("foobar", Statement.tokenLiteral(&stmt));

    try expectEqualStrings("foobar", stmt.expression.?.token.tokenLiteral());

    try expect(stmt.expression.?.kind == .Identifier);
}


test "Integer Literal Expression" {

    const allocator = std.testing.allocator;

    const input = "5;";
    const input_int: u32 = 5;

    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    try parser.checkParseErrors();

    try expect(program.statements.items.len == 1);

    const stmt = program.statements.items[0];

    try expect(stmt.kind == .Expression);

    try expect(stmt.expression.?.kind == .IntegerLiteral);

    switch (stmt.expression.?.value.?) {
        .int_val => |val| try expect(val == input_int)
    }

}

test "Parsing Errors" {

    const allocator = std.testing.allocator;

    const input = 
        \\let x 5;
        \\let = 10;
        \\let 838383;
    ;

    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();


    expect(parser.errors.items.len == 3) catch |err| {
        print("parser didnt find eny error", .{});
        return err;
    };


}
