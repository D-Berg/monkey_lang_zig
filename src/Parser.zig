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
const log = std.log;

const Parser = @This();

allocator: Allocator,
lexer: *Lexer,
current_token: Token,
peek_token: Token,
errors: ArrayList([]const u8),

const ParseError = error {
    WrongExpressionType
};

const Precedence = enum {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    
    fn get(tk: Token.Kind) ?Precedence {

        const pd = switch (tk) {
            .Eq => Precedence.Equals,
            .Neq => Precedence.Equals,
            .Lt => Precedence.LessGreater,
            .Gt => Precedence.LessGreater,
            .Plus => Precedence.Sum,
            .Minus => Precedence.Sum,
            .Slash => Precedence.Product,
            .Asterisk => Precedence.Product,
            else => .Lowest
        };

        return pd;

    }
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
            // log.debug("parsing let statement", .{});
            return parser.parseLetStatement();
        },
        Token.Kind.Return => {
            // log.debug("parsing return statement", .{});
            return parser.parseReturnStatement();
        },
        else => {
            // log.debug("parsing expression statement", .{});
            return parser.parseExpressionStatement();
        }
    }
}

fn parseLetStatement(parser: *Parser) ?Statement {

    const token = parser.current_token;
    // TODO: possible mem leak

    if (!parser.expectPeek(Token.Kind.Ident)) return null;


    const identifier = Identifier { .token = parser.current_token };

    if (!parser.expectPeek(Token.Kind.Assign)) return null;

    while (!parser.curTokenIs(Token.Kind.Semicolon)) {
        parser.nextToken();
    }

    return Statement { 
        .let_stmt = .{
            .token = token,
            .name = identifier,
            .value = null
        }
    };
}

fn parseReturnStatement(parser: *Parser) ?Statement {

    const stmt = Statement { 
        .ret_stmt = .{
            .token = parser.current_token,
            .value = null
        }
    };

    parser.nextToken();

    while (!parser.curTokenIs(Token.Kind.Semicolon)) {
        parser.nextToken();
    }

    return stmt;

}

fn parseExpressionStatement(parser: *Parser) ?Statement {

    const token = parser.current_token;

    const expression = parser.parseExpression(.Lowest);

    if (parser.peekTokenIs(Token.Kind.Semicolon)) {
        parser.nextToken();
    }

    return Statement { 
        .expr_stmt = .{
            .token = token,
            .expression = expression
        }
    };

}

fn parseExpression(parser: *Parser, precedence: Precedence) ?Expression {

    const prefix = switch (parser.current_token.kind) { // same as looking into the hashmap p.prefixParseFns in GO
        .Ident => parser.parseIdentifier(),
        .Int => parser.parseIntegerLiteral(),
        .Bang, .Minus => parser.parsePrefixExpression(),
        else => null
    };


    if (prefix == null) {
        parser.noPrefixParseFunction(parser.current_token.kind) catch |err| {
            log.err("Failed to create error msg because {}", .{err});
        };
        return null; // TODO: add to parser.errors
    }
    
    var left_expr: Expression = prefix.?;

    while (!parser.peekTokenIs(.Semicolon) and @intFromEnum(precedence) < @intFromEnum(parser.peekPrecedence())) {

        const peek_kind = parser.peek_token.kind;

        parser.nextToken();
        
        const infix = switch (peek_kind) {
            .Plus, .Minus, .Asterisk, .Slash, .Gt, .Lt, .Eq, .Neq => parser.parseInfixExpression(left_expr),
            else => null
        };


        if (infix == null) return left_expr;

        left_expr = infix.?;

    }

    return left_expr;
}

fn parseIdentifier(parser: *Parser) Expression {

    return Expression {
        .identifier = .{
            .token = parser.current_token,
        }
    };

}

fn parseIntegerLiteral(parser: *Parser) ?Expression {
    
    const maybe_val = std.fmt.parseInt(u32, parser.current_token.tokenLiteral(), 0);

    if (maybe_val) |val| {
        return Expression { 
            .integer_literal = .{
                .token = parser.current_token,
                .value = val
            }
        };
    } else |_| {

        // TODO: format and also handle error better
        parser.errors.append("Failed to parse Integer Literal") catch unreachable;
        return null;
    }
    
}

fn parsePrefixExpression(parser: *Parser) Expression {
    
    // log.debug("expression=prefix_expression", .{});

    const tok = parser.current_token;


    parser.nextToken();

    const expr_ptr = parser.allocator.create(Expression) catch {
        @panic("Failed to create an expression pointer");
    };

    expr_ptr.* = parser.parseExpression(.Prefix).?;

    // // log.debug("token = {}, right ={any}", .{tok.kind, expr_ptr.*});

    return Expression {
        .prefix_expression = .{ 
            .token = tok,
            .right = expr_ptr,
        }
    };

}

fn parseInfixExpression(parser: *Parser, left: Expression) Expression {

    // log.debug("parsing infix expression", .{});

    var token = parser.current_token;

    log.debug("token = {}, {s}", .{token.kind, Token.tokenLiteral(&token)});

    const precedence = parser.curPrecedence();

    parser.nextToken();

    // TODO: better mem management
    const left_ptr = parser.allocator.create(Expression) catch {
        @panic("Failed to create left expression in infix");
    };
    const right_ptr = parser.allocator.create(Expression) catch {
        @panic("Failed to create right expression in infix");
    };

    left_ptr.* = left;
    right_ptr.* = parser.parseExpression(precedence).?;

    // log.debug("token={}, \n\tleft={}, \n\tright={}", .{
    //     token.kind, left_ptr.*, right_ptr.*
    // });


    return Expression {
        .infix_expression = .{
            .token = token,
            .left = left_ptr,
            .right = right_ptr
        }
    };

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

fn curPrecedence(parser: *Parser) Precedence {
    const prec = Precedence.get(parser.current_token.kind) orelse .Lowest;
    return prec;
}

fn peekPrecedence(parser: *Parser) Precedence {
    const prec = Precedence.get(parser.peek_token.kind) orelse .Lowest;
    return prec;
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

fn noPrefixParseFunction(parser: *Parser, kind: Token.Kind) !void {
    const err_msg = try std.fmt.allocPrint(parser.allocator, "no prefix parse function for {} found", .{kind});

    try parser.errors.append(err_msg);
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
        try expect(statement.* == .let_stmt);
        try expectEqualStrings(statement.tokenLiteral(), "let");

        try expectEqualStrings(statement.let_stmt.name.token.tokenLiteral(), expected_identiefers[i]);

        // TODO: check values

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
        try expect(statement.* == .ret_stmt);
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

    try expect(stmt == .expr_stmt);

    // try expectEqualStrings(ident.value.?)
    
    try expectEqualStrings("foobar", Statement.tokenLiteral(&stmt));

    try expect(stmt.expr_stmt.expression.? == .identifier);

    try expectEqualStrings("foobar", stmt.expr_stmt.expression.?.identifier.token.tokenLiteral());

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

    try expect(stmt == .expr_stmt);

    try expect(stmt.expr_stmt.expression.? == .integer_literal);

    switch (stmt.expr_stmt.expression.?) {
        .integer_literal => |il| {
            try expect(il.value == input_int);
        }, 
        else =>  {
            return error.WrongExpressionType;
        }
    }

}

test "Prefix Expression" {
    
    const allocator = std.testing.allocator;

    const input = [_][]const u8 {
        "!5;",
        "-15;"
    };

    const operators = [_][]const u8 {
        "!",
        "-"
    };

    const int_values = [_]u32 { 5, 15 };

    for (0..2) |i| {

        var lexer = try Lexer.init(allocator, input[i]);
        defer lexer.deinit();

        var parser = Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.ParseProgram(allocator);
        defer program.deinit();

        try parser.checkParseErrors();

        try expect(program.statements.items.len == 1);

        var stmt = program.statements.items[0];

        try expect(stmt == .expr_stmt);

        try expect(stmt.expr_stmt.expression.? == .prefix_expression);

        try expect(stmt.expr_stmt.expression.?.prefix_expression.right.* == .integer_literal);

        try expect(stmt.expr_stmt.expression.?.prefix_expression.right.integer_literal.value == int_values[i]);

        try expectEqualStrings(operators[i], Token.tokenLiteral(&stmt.expr_stmt.expression.?.prefix_expression.token));
    }

}

test "Infix Expression" {

    const allocator = std.testing.allocator;

    const input = [_][]const u8 {
        "5 + 10;",
        "5 - 10;",
        "5 * 10;",
        "5 / 10;",
        "5 > 10;",
        "5 < 10;",
        "5 == 10;",
        "5 != 10;"
    };

    const token_kinds = [_]Token.Kind {
        .Plus,
        .Minus,
        .Asterisk,
        .Slash,
        .Gt,
        .Lt,
        .Eq,
        .Neq
    };

    const left_value = 5;
    const right_value = 10;

    for (0..input.len) |i| {

        var lexer = try Lexer.init(allocator, input[i]);
        defer lexer.deinit();

        var parser = Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.ParseProgram(allocator);
        defer program.deinit();

        try parser.checkParseErrors();

        expect(program.statements.items.len == 1) catch |err| {

            print("wront stmt len: expected=1, got={}\n", .{
                program.statements.items.len
            });
            return err;
        };

        var stmt = program.statements.items[0];

        try expect(stmt == .expr_stmt);

        try expect(stmt.expr_stmt.expression.? == .infix_expression);

        expect(stmt.expr_stmt.expression.?.infix_expression.token.kind == token_kinds[i]) catch |err| {
            print("wrong token: expected=[{}], got=[{}]\n", .{token_kinds[i], stmt.expr_stmt.token.kind});

            print("token literal: {s}\n", .{stmt.tokenLiteral()});

            return err;
        };

        try expect(stmt.expr_stmt.expression.?.infix_expression.left.*.integer_literal.value == left_value);
        try expect(stmt.expr_stmt.expression.?.infix_expression.right.*.integer_literal.value == right_value);


    }

}



test "Get Program String" {

    const allocator = std.testing.allocator;

    var statements = ArrayList(Statement).init(allocator);

    try statements.append(
        Statement { 
            .let_stmt = .{ 
                .token = Token.init(.Let, "let"),
                .name = Identifier {
                    .token = Token.init(.Ident, "myVar"),
                },
                .value = Expression {
                    .identifier = Identifier {
                        .token = Token.init(.Ident, "anotherVar")
                    }
                }
            }
        }

    );

    var prog = Program {
        .allocator = allocator,
        .statements = statements
    };

    defer prog.deinit();

    const prog_str = try prog.String();
    defer prog.allocator.free(prog_str);

    try expectEqualStrings("let myVar = anotherVar;", prog_str);

}

test "Operator Precedence" {

    const allocator = std.testing.allocator;

    const input = [_][]const u8 {
        "-a * b",
        "!-a",
        "a + b + c"
    };

    const answer = [_][]const u8 {
        "((-a) * b)",
        "(!(-a))",
        "((a + b) + c)"
    };

    for (0..input.len) |i| {

        var lexer = try Lexer.init(allocator, input[i]);
        defer lexer.deinit();

        var parser = Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.ParseProgram(allocator);
        defer program.deinit();

        try parser.checkParseErrors();

        const prog_str = try program.String();
        defer program.allocator.free(prog_str);

        try expectEqualStrings(answer[i], prog_str);
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
        print("parser didnt find eny error\n", .{});
        return err;
    };


}
