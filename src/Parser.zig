const std = @import("std");
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");
const ast = @import("ast.zig");
const Program = ast.Program;
const Statement = ast.Statement;
const Identifier = ast.Identifier;
const Expression = ast.Expression;
const BlockStatement = ast.BlockStatement;
const Node = ast.Node;
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
    WrongExpressionType,
    ExpectedNextTokenIdentifier,
    ExpectedNextTokenAssign,
    ExpectedNextTokenRbrace,
    ExpectedNextTokenLbrace,
    ExpectedNextTokenRparen,
    ExpectedNextTokenLparen,
    FailedToConvertStringToInt,
    NoPrefixFunction,
    NoInfixFunction
} || Allocator.Error;

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
            .Lparen => Precedence.Call,
            else => .Lowest
        };

        return pd;

    }
};

pub fn init(lexer: *Lexer, allocator: Allocator) !Parser {

    return .{ 
        .allocator = allocator, 
        .lexer = lexer,
        .current_token = try lexer.NextToken(),
        .peek_token = try lexer.NextToken(),
        .errors = ArrayList([]const u8).init(allocator),
    };

}

pub fn deinit(parser: Parser) void {


    for (parser.errors.items) |parse_err| {
        parser.allocator.free(parse_err);
    }

    parser.errors.deinit();

    // parser.root_stmt_idx.deinit();
    // parser.statements.deinit();
    //
    // for (parser.expressions.items) |*expr| {
    //     expr.deinit();
    // }
    // parser.expressions.deinit();
    
}

fn getNodeIdx(parser: *Parser) usize {
    const n_nodes = parser.nodes.items.len;
    if (n_nodes > 0) {
        return n_nodes - 1;
    } else {
        return 0;
    }
}


fn nextToken(parser: *Parser) void {
    // print("current token: {}\n", .{parser.current_token.kind});
    parser.current_token = parser.peek_token;
    parser.peek_token = parser.lexer.NextToken() catch {
        @panic("Failed to get NextToken");

    };
}



fn parseStatement(parser: *Parser) ParseError!Statement {

    const stmt = switch (parser.current_token.kind) {
        .Let => try parser.parseLetStatement(),
        .Return => try parser.parseReturnStatement(),
        else => try parser.parseExpressionStatement(),
    };

    return stmt;

}

fn parseLetStatement(parser: *Parser) ParseError!Statement {

    const token = parser.current_token;

    if (!parser.expectPeek(Token.Kind.Ident)) return ParseError.ExpectedNextTokenIdentifier;

    const identifier = Identifier { .token = parser.current_token };

    if (!parser.expectPeek(Token.Kind.Assign)) return ParseError.ExpectedNextTokenAssign;

    parser.nextToken();

    const expr = try parser.parseExpression(.Lowest);
    const expr_ptr = try parser.allocator.create(Expression);
    expr_ptr.* = expr;

    while (!parser.curTokenIs(Token.Kind.Semicolon)) {
        parser.nextToken();
    }

    return Statement { 
        .let_stmt = .{
            .allocator = parser.allocator,
            .token = token,
            .name = identifier,
            .value = expr_ptr  
        }
    };
}

fn parseReturnStatement(parser: *Parser) ParseError!Statement {

    // TODO: is "return;" valid syntax?
    const curr_tok = parser.current_token;

    parser.nextToken();

    const expr = try parser.parseExpression(.Lowest);

    const expr_ptr = try parser.allocator.create(Expression);
    expr_ptr.* = expr;

    while (!parser.curTokenIs(Token.Kind.Semicolon)) {
        parser.nextToken();
    }

    return Statement { 
        .ret_stmt = .{
            .allocator = parser.allocator,
            .token = curr_tok,
            .value = expr_ptr
        }
    };
}

fn parseExpressionStatement(parser: *Parser) ParseError!Statement {

    const token = parser.current_token;

    const expr = try parser.parseExpression(.Lowest);
    const expr_ptr = try parser.allocator.create(Expression);
    expr_ptr.* = expr;

    if (parser.peekTokenIs(Token.Kind.Semicolon)) {
        parser.nextToken();
    }

    return Statement { 
        .expr_stmt = .{
            .allocator = parser.allocator,
            .token = token,
            .expression = expr_ptr
        }
    };

}

fn parseExpression(parser: *Parser, precedence: Precedence) ParseError!Expression {

    const prefix = switch (parser.current_token.kind) { // same as looking into the hashmap p.prefixParseFns in GO
        .Ident => parser.parseIdentifier(),
        .Int => try parser.parseIntegerLiteral(),
        .Bang, .Minus => parser.parsePrefixExpression(),
        .False, .True => parser.parseBooleanExpression(),
        .Lparen => try parser.parseGroupedExpression(),
        .If => try parser.parseIfExpression(),
        .Function => try parser.parseFunctionLiteral(),
        inline else => |kind| {
            print("No prefix func for {}\n", .{kind});
            return ParseError.NoPrefixFunction;
        }
    };

    var left_expr: Expression = try prefix;

    while (!parser.peekTokenIs(.Semicolon) and @intFromEnum(precedence) < @intFromEnum(parser.peekPrecedence())) {

        const peek_kind = parser.peek_token.kind;

        parser.nextToken();

        const infix = switch (peek_kind) {
            .Plus, .Minus, .Asterisk, .Slash, .Gt, .Lt, .Eq, .Neq => try parser.parseInfixExpression(left_expr),
            .Lparen => try parser.parseCallExpression(left_expr),
            else => ParseError.NoPrefixFunction
        };


        // if (infix == ParseError.NoPrefixFunction) return left_expr;

        left_expr = infix catch return left_expr;

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
//
fn parseIntegerLiteral(parser: *Parser) ParseError!Expression {

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
        const err_str = std.fmt.allocPrint(parser.allocator, "Failed to parse Integer Literal {s}", .{
            parser.current_token.tokenLiteral()
        }) catch |err| {
            print("Failed to allocPrint with err {}", .{err});
            @panic("cant allocPrint");
        };
        parser.errors.append(err_str) catch unreachable;
        return ParseError.FailedToConvertStringToInt;
    }

}

fn parseBooleanExpression(parser: *Parser) Expression {

    return Expression {
        .boolean_literal = .{
            .token = parser.current_token,
            .value = parser.curTokenIs(.True),
        }
    };

}

fn parsePrefixExpression(parser: *Parser) ParseError!Expression {

    // log.debug("expression=prefix_expression", .{});

    const tok = parser.current_token;


    parser.nextToken();

    const expr = try parser.parseExpression(.Prefix);
    const expr_ptr = try parser.allocator.create(Expression);
    expr_ptr.* = expr;
    // // log.debug("token = {}, right ={any}", .{tok.kind, expr_ptr.*});

    return Expression {
        .prefix_expression = .{ 
            .allocator = parser.allocator,
            .token = tok,
            .right = expr_ptr,
        }
    };

}

fn parseInfixExpression(parser: *Parser, left: Expression) ParseError!Expression {

    // log.debug("parsing infix expression", .{});

    const token = parser.current_token;

    // log.debug("token = {}, {s}", .{token.kind, Token.tokenLiteral(&token)});

    const precedence = parser.curPrecedence();

    parser.nextToken();

    const right = try parser.parseExpression(precedence);

    const left_ptr = try parser.allocator.create(Expression);
    const right_ptr = try parser.allocator.create(Expression);

    left_ptr.* = left;
    right_ptr.* = right;



    return Expression {
        .infix_expression = .{
            .allocator = parser.allocator,
            .token = token,
            .left = left_ptr,
            .right = right_ptr
        }
    };

}

fn parseGroupedExpression(parser: *Parser) ParseError!Expression {
    parser.nextToken();

    const expr = try parser.parseExpression(.Lowest);

    if (!parser.expectPeek(.Rparen)) {
        return ParseError.ExpectedNextTokenRparen;
    }

    return expr;
}

fn parseIfExpression(parser: *Parser) ParseError!Expression {

    const curr_tok = parser.current_token;

    // print("if curr_tot = {}\n", .{curr_tok.kind});

    if (!parser.expectPeek(.Lparen)) return ParseError.ExpectedNextTokenLparen;

    parser.nextToken();
    const condition = try parser.parseExpression(.Lowest);
    // print("if condition = {}\n", .{condition});

    if (!parser.expectPeek(.Rparen)) return ParseError.ExpectedNextTokenRparen;

    if (!parser.expectPeek(.Lbrace)) return ParseError.ExpectedNextTokenLbrace;

    const consequence = try parser.parseBlockStatement();

    const condition_ptr = try parser.allocator.create(Expression);
    condition_ptr.* = condition;

    var alternative: ?BlockStatement = null;
    if (parser.peekTokenIs(.Else)) {
        parser.nextToken();

        if (!parser.expectPeek(.Lbrace)) return ParseError.ExpectedNextTokenLbrace;

        alternative = try parser.parseBlockStatement();
    }

    // if (<condition>) {<consequence>} else {<alternative>}
    return Expression {
        .if_expression = .{ 
            .allocator = parser.allocator,
            .token = curr_tok,
            .condition = condition_ptr,
            .consequence = consequence,
            .alternative = alternative,
        }
    };

}

fn parseBlockStatement(parser: *Parser) ParseError!BlockStatement {

    const curr_tok = parser.current_token;

    parser.nextToken();

    var block_statements = ArrayList(Statement).init(parser.allocator);

    while (!parser.curTokenIs(.Rbrace) and !parser.curTokenIs(.Eof)) : (parser.nextToken()){

        const stmt = try parser.parseStatement();
        try block_statements.append(stmt);
    }

    return BlockStatement {
        .token = curr_tok,
        .statements = block_statements
    };

}

fn parseFunctionLiteral(parser: *Parser) ParseError!Expression {
    const curr_tok = parser.current_token;

    if (!parser.expectPeek(.Lparen)) return ParseError.ExpectedNextTokenLparen;

    const parameters = parser.parseFunctionParameters();

    if (!parser.expectPeek(.Lbrace)) {
        if (parameters != null) parameters.?.deinit();
        return ParseError.ExpectedNextTokenLbrace;
    }

    const body = try parser.parseBlockStatement();

    return Expression {
        .fn_literal = .{
            .token = curr_tok,
            .parameters = parameters,
            .body = body,
        }
    };

}

fn parseFunctionParameters(parser: *Parser) ?ArrayList(Identifier) {

    var identifiers = ArrayList(Identifier).init(parser.allocator);

    if (parser.peekTokenIs(.Rparen)) {
        parser.nextToken();
        return identifiers;
    }

    parser.nextToken();

    identifiers.append(.{ .token = parser.current_token }) catch {
        @panic("failed to append identifier");
    };

    while (parser.peekTokenIs(.Comma)) {
        parser.nextToken();
        parser.nextToken();

        identifiers.append(.{ .token = parser.current_token }) catch {
            @panic("failed to append identifier");
        };

    }

    if (!parser.expectPeek(.Rparen)) {
        identifiers.deinit();
        return null; // TODO: replace with err
    }

    return identifiers;

}


fn parseCallExpression(parser: *Parser, function: Expression) ParseError!Expression {
    const curr_tok = parser.current_token;

    const arguments = try parser.parseCallArguments();


    const func_ptr = try parser.allocator.create(Expression);
    func_ptr.* = function;

    return Expression {
        .call_expression = .{
            .allocator = parser.allocator,
            .token = curr_tok,
            .function = func_ptr,
            .args = arguments
        }
    };
}

fn parseCallArguments(parser: *Parser) ParseError!ArrayList(Expression) {

    var args = ArrayList(Expression).init(parser.allocator);

    if (parser.peekTokenIs(.Rparen)) {
        parser.nextToken();
        return args;
    }

    parser.nextToken();

    var arg_expr = try parser.parseExpression(.Lowest);

    try args.append(arg_expr);

    while (parser.peekTokenIs(.Comma)) {
        parser.nextToken();
        parser.nextToken();

        arg_expr = try parser.parseExpression(.Lowest);

        try args.append(arg_expr);
    }

    // TODO: handle if !expectpeek(.Rparent) return null
    if (!parser.expectPeek(.Rparen)) {
        return ParseError.ExpectedNextTokenRparen;
    }

    return args;

}
//
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


pub fn ParseProgram(parser: *Parser, allocator: Allocator) ParseError!Program {

    var program = Program.init(allocator);

    while (parser.current_token.kind != Token.Kind.Eof) {

        const stmt = try parser.parseStatement();
        try program.statements.append(stmt);
        parser.nextToken();

    }

    // does not clone, parsers arraylists gets deinited by program
    // TODO: make program own parser
    return program;
}

fn checkParseErrors(parser: *Parser) error{ParsingError}!void {

    if (parser.errors.items.len > 0) return error.ParsingError;

}

fn peekError(parser: *Parser, kind: Token.Kind) void {

    const err_str = std.fmt.allocPrint(parser.allocator, "expected next token to be {}, got {} instead", .{
        kind, parser.peek_token.kind
    }) catch |err| {
        print("Error: {}\n", .{err});
        @panic("Failed allocPrint");
    };

    parser.errors.append(err_str) catch {
        @panic("failed to allocate error msg");
    };


}

fn noPrefixParseFunction(parser: *Parser, kind: Token.Kind) !void {
    // TODO: fix error handling for monkey
    const err_str = try std.fmt.allocPrint(parser.allocator, "no defined prefix parse function for {}", .{
        kind
    });
    try parser.errors.append(err_str);
}


// TODO: fn for creating parser, lexer program for tests

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


    for (program.statements.items, 0..) |stmt, i| {
        try expect(stmt == .let_stmt);
        try expectEqualStrings(stmt.tokenLiteral(), "let");

        try expectEqualStrings(stmt.let_stmt.name.token.tokenLiteral(), expected_identiefers[i]);

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

    for (program.statements.items) |stmt| {
        try expect(stmt == .ret_stmt);
        try expectEqualStrings("return", stmt.tokenLiteral());
    }
}

test "Ident Expression" {
    const allocator = std.testing.allocator;

    const input = "foobar;";

    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    // try parser.checkParseErrors();
    //
    const n_stmts = program.statements.items.len;

    expect(n_stmts == 1) catch |err| {
        print("statement_len = {}\n", .{n_stmts});
        return err;
    };


    const stmt = program.statements.items[0];

    try expect(stmt == .expr_stmt);

    // try expectEqualStrings(ident.value.?);

    try expectEqualStrings("foobar", Statement.tokenLiteral(&stmt));

    // try expect(stmt.expr_stmt.expression.? == .identifier);
    const expr = stmt.expr_stmt.expression.*;

    try expect(expr == .identifier);

    try expectEqualStrings("foobar", expr.identifier.tokenLiteral());

}

test "Int Lit Expression" {

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

    const expr = stmt.expr_stmt.expression.*;

    try expect(expr == .integer_literal);

    switch (expr) {
        .integer_literal => |il| {
            try expect(il.value == input_int);
        }, 
        else =>  {
            return error.WrongExpressionType;
        }
    }

}

test "Boolean lit expr" {

    const allocator = std.testing.allocator;

    const input = [_][]const u8 {
        "true",
        "false",
        "3 > 5 == false",
        "3 < 5 == true",
    };
    const answer = [_][]const u8 {
        "true",
        "false",
        "((3 > 5) == false)",
        "((3 < 5) == true)",
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
        defer allocator.free(prog_str);

        try expectEqualStrings(answer[i], prog_str);

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

        const stmt = program.statements.items[0];

        try expect(stmt == .expr_stmt);

        const expr = stmt.expr_stmt.expression.*;

        try expect(expr == .prefix_expression);

        const right_expr = expr.prefix_expression.right.*;

        try expect(right_expr == .integer_literal);

        try expect(right_expr.integer_literal.value == int_values[i]);

        try expectEqualStrings(operators[i], Token.tokenLiteral(&expr.prefix_expression.token));
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

        const stmt = program.statements.items[0];

        try expect(stmt == .expr_stmt);

        const expr = stmt.expr_stmt.expression.*;

        try expect(expr == .infix_expression);

        expect(expr.infix_expression.token.kind == token_kinds[i]) catch |err| {
            print("wrong token: expected=[{}], got=[{}]\n", .{token_kinds[i], stmt.expr_stmt.token.kind});

            print("token literal: {s}\n", .{stmt.tokenLiteral()});

            return err;
        };

        const right_expr = expr.infix_expression.right.*;
        const left_expr = expr.infix_expression.left.*;

        try expect(left_expr.integer_literal.value == left_value);
        try expect(right_expr.integer_literal.value == right_value);


    }

}
//


test "Get Program String" {

    const allocator = std.testing.allocator;

    var statements = ArrayList(Statement).init(allocator);

    const expr = Expression {
        .identifier = Identifier {
            .token = Token.init(.Ident, "anotherVar")
        }
    };

    const expr_ptr = try allocator.create(Expression);
    expr_ptr.* = expr;



    try statements.append(
        Statement { 
            .let_stmt = .{ 
                .allocator = allocator,
                .token = Token.init(.Let, "let"),
                .name = Identifier {
                    .token = Token.init(.Ident, "myVar"),
                },
                .value = expr_ptr 
            }
        }
    );


    var prog = Program {
        .allocator = allocator,
        .statements = statements,
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
        "a + b + c",
        "a + b - c",
        "a * b * c",
        "a * b / c",
        "a + b / c",
        "a + b * c + d / e - f",
        "3 + 4; -5 * 5",
        "5 > 4 == 3 < 4",
        "5 < 4 != 3 > 4",
        "3 + 4 * 5 == 3 * 1 + 4 * 5",
        "3 + 4 * 5 == 3 * 1 + 4 * 5",
        "1 + (2 + 3) + 4",
        "(5 + 5) * 2",
        "2 / (5 + 5)",
        "-(5 + 5)",
        "!(true == true)",
        // relies on call exprssions
        "a + add(b * c) + d",
        "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
        "add(a + b + c * d / f + g)",
    };

    const answer = [_][]const u8 {
        "((-a) * b)",
        "(!(-a))",
        "((a + b) + c)",
        "((a + b) - c)",
        "((a * b) * c)",
        "((a * b) / c)",
        "(a + (b / c))",
        "(((a + (b * c)) + (d / e)) - f)",
        "(3 + 4)((-5) * 5)",
        "((5 > 4) == (3 < 4))",
        "((5 < 4) != (3 > 4))",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        "((1 + (2 + 3)) + 4)",
        "((5 + 5) * 2)",
        "(2 / (5 + 5))",
        "(-(5 + 5))",
        "(!(true == true))",
        "((a + add((b * c))) + d)",
        "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        "add((((a + b) + ((c * d) / f)) + g))",

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

test "If Expression" {
    const allocator = std.testing.allocator;

    const input = "if (x < y) { x }";

    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    try parser.checkParseErrors();

    try expect(program.statements.items.len == 1);

    // TODO: expand test 


}
//

test "Func Lit Expr" {

    const allocator = std.testing.allocator;
    const input = "fn(x, y) { x + y; }";

    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    expect(program.statements.items.len == 1) catch |err| {
        print("Expected 1 stmt, got {}\n", .{program.statements.items.len});
        return err;
    };

    const stmt = program.statements.items[0];

    try expect(stmt == .expr_stmt);

    const expr = stmt.expr_stmt.expression.*;

    try expect(expr == .fn_literal);

    const fn_lit = expr.fn_literal;

    try expect(fn_lit.parameters.?.items.len == 2);


    var p_1 = fn_lit.parameters.?.items[0];
    var p_2 = fn_lit.parameters.?.items[1];


    try expectEqualStrings("x", p_1.tokenLiteral());


    try expectEqualStrings("y", p_2.tokenLiteral());

    try expect(fn_lit.body.statements.items.len == 1);

    const body_stmt = fn_lit.body.statements.items[0];
    try expect(body_stmt == .expr_stmt);

}

test "Call expression" {

    const allocator = std.testing.allocator;

    // const input = "add(1);";
    // const input = "add(1, 2);";
    // const input = "add(3 + 4);";
    // const input = "add(1, 2 * 3, 4 + 5);";
    const input = "add(1, 2 * 3, add(5, 5));";


    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    const n_stmts = program.statements.items.len;
    expect(n_stmts == 1) catch |err| {
        print("expected 1 stmt, got {}\n", .{n_stmts});
        return err;
    };

    const stmt = program.statements.items[0];

    try expect(stmt == .expr_stmt);
    //
    const expr = stmt.expr_stmt.expression.*;

    try expect(expr == .call_expression);


    try expect(expr.call_expression.args.items.len == 3);

}

// test "Parsing Errors" {
//
//     const allocator = std.testing.allocator;
//
//     const input = 
//         \\let x 5;
//         \\let = 10;
//         \\let 838383;
//     ;
//
//     var lexer = try Lexer.init(allocator, input);
//     defer lexer.deinit();
//
//     var parser = Parser.init(&lexer, allocator);
//     defer parser.deinit();
//
//     var program = try parser.ParseProgram(allocator);
//     defer program.deinit();
//
//     expect(parser.errors.items.len == 3) catch |err| {
//         print("parser didnt find eny error\n", .{});
//         return err;
//     };
//
//
// }
