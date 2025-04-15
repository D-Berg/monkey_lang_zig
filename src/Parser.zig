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
    ExpectedNextTokenRbraket, 
    ExpectedNextTokenString,
    ExpectedNextTokenColon,
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
    Index,
    
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
            .Lbracket => Precedence.Index,
            else => .Lowest,
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

/// Can panic
fn nextToken(parser: *Parser) !void {

    // print("current token: {}\n", .{parser.current_token.kind});

    parser.current_token.deinit();

    parser.current_token = try parser.peek_token.clone();
    errdefer parser.current_token.deinit();

    parser.peek_token.deinit();

    parser.peek_token = try parser.lexer.NextToken();
    errdefer parser.peek_token.deinit();
}

fn parseStatement(parser: *Parser) ParseError!Statement {
    const stmt = switch (parser.current_token.kind) {
        .Let => try parser.parseLetStatement(),
        .Return => try parser.parseReturnStatement(),
        else => try parser.parseExpressionStatement(),
    };
    errdefer stmt.deinit();

    return stmt;
}

fn parseLetStatement(parser: *Parser) ParseError!Statement {
    const token = try parser.current_token.clone();
    errdefer token.deinit();

    if (!try parser.expectPeek(Token.Kind.Ident)) return ParseError.ExpectedNextTokenIdentifier;

    const ident_token = try parser.current_token.clone();
    errdefer ident_token.deinit();

    const identifier = Identifier{ .token = ident_token };
    errdefer identifier.deinit();

    if (!try parser.expectPeek(Token.Kind.Assign)) return ParseError.ExpectedNextTokenAssign;

    try parser.nextToken();

    const expr = try parser.parseExpression(.Lowest);
    errdefer expr.deinit();

    const expr_ptr = try parser.allocator.create(Expression);
    errdefer parser.allocator.destroy(expr_ptr);

    expr_ptr.* = expr;

    while (!parser.curTokenIs(Token.Kind.Semicolon)) {
        try parser.nextToken();
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
    const curr_tok = try parser.current_token.clone();
    errdefer curr_tok.deinit();

    try parser.nextToken();

    const expr = try parser.parseExpression(.Lowest);

    const expr_ptr = try parser.allocator.create(Expression);
    expr_ptr.* = expr;

    while (!parser.curTokenIs(Token.Kind.Semicolon)) {
        try parser.nextToken();
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

    const token = try parser.current_token.clone();
    errdefer token.deinit();

    const expr = try parser.parseExpression(.Lowest);
    errdefer expr.deinit();

    const expr_ptr = try parser.allocator.create(Expression);
    errdefer parser.allocator.destroy(expr_ptr);
    expr_ptr.* = expr;

    if (parser.peekTokenIs(Token.Kind.Semicolon)) {
        try parser.nextToken();
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
        .Int => parser.parseIntegerLiteral(),
        .Bang, .Minus => parser.parsePrefixExpression(),
        .False, .True => parser.parseBooleanExpression(),
        .Lparen => parser.parseGroupedExpression(),
        .If => parser.parseIfExpression(),
        .Function => try parser.parseFunctionLiteral(),
        .String => parser.parseStringLiteral(),
        .Lbracket => parser.parseArrayLiteral(),
        .Lbrace => parser.parseDictionaryLiteral(), // {
        inline else => |kind| {
            // TODO: add parsing Error
            log.err("no prefix for {}", .{kind});
            return ParseError.NoPrefixFunction;
        }
    };

    var left_expr: Expression = try prefix;
    errdefer left_expr.deinit();

    while (!parser.peekTokenIs(.Semicolon) and @intFromEnum(precedence) < @intFromEnum(parser.peekPrecedence())) {

        const peek_kind = parser.peek_token.kind;

        try parser.nextToken();

        const infix = switch (peek_kind) {
            .Plus, .Minus, .Asterisk, .Slash, .Gt, .Lt, .Eq, .Neq => try parser.parseInfixExpression(left_expr),
            .Lparen => try parser.parseCallExpression(left_expr),
            .Lbracket => try parser.parseIndexExpression(left_expr),
            else => ParseError.NoPrefixFunction,
        };
        errdefer infix.deinit();

        // if (infix == ParseError.NoPrefixFunction) return left_expr;

        left_expr = infix catch return left_expr;
        errdefer left_expr.deinit();
    }

    return left_expr;
}

fn parseIdentifier(parser: *Parser) !Expression {
    const token = try parser.current_token.clone();
    errdefer token.deinit();
    return Expression {
        .identifier = .{
            .token = token,
        }
    };

}
//
fn parseIntegerLiteral(parser: *Parser) ParseError!Expression {

    const maybe_val = std.fmt.parseInt(u32, parser.current_token.tokenLiteral(), 0);
    // TODO handle overflow

    const token = try parser.current_token.clone();
    errdefer token.deinit();

    if (maybe_val) |val| {
        return Expression { 
            .integer_literal = .{
                .token = token,
                .value = val
            }
        };
    } else |_| {

        // TODO: format and also handle error better
        // const err_str = std.fmt.allocPrint(parser.allocator, "Failed to parse Integer Literal {s}", .{
        //     parser.current_token.tokenLiteral()
        // }) catch |err| {
        //     print("Failed to allocPrint with err {}", .{err});
        //     @panic("cant allocPrint");
        // };
        // parser.errors.append(err_str) catch unreachable;
        return ParseError.FailedToConvertStringToInt;
    }
}

fn parseBooleanExpression(parser: *Parser) ParseError!Expression {
    const token = try parser.current_token.clone();

    return Expression{ .boolean_literal = .{
        .token = token,
        .value = parser.curTokenIs(.True),
    } };
}

fn parsePrefixExpression(parser: *Parser) ParseError!Expression {

    // log.debug("expression=prefix_expression", .{});

    const tok = try parser.current_token.clone();
    errdefer tok.deinit();

    try parser.nextToken();

    const expr = try parser.parseExpression(.Prefix);
    const expr_ptr = try parser.allocator.create(Expression);
    expr_ptr.* = expr;
    // // log.debug("token = {}, right ={any}", .{tok.kind, expr_ptr.*});

    return Expression{ .prefix_expression = .{
        .allocator = parser.allocator,
        .token = tok,
        .right = expr_ptr,
    } };
}

fn parseInfixExpression(parser: *Parser, left: Expression) ParseError!Expression {

    // log.debug("parsing infix expression", .{});

    const token = try parser.current_token.clone();
    errdefer token.deinit();

    // log.debug("token = {}, {s}", .{token.kind, Token.tokenLiteral(&token)});

    const precedence = parser.curPrecedence();

    try parser.nextToken();

    const right = try parser.parseExpression(precedence);
    errdefer right.deinit();

    const left_ptr = try parser.allocator.create(Expression);
    errdefer parser.allocator.destroy(left_ptr);

    const right_ptr = try parser.allocator.create(Expression);
    errdefer parser.allocator.destroy(right_ptr);

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
    try parser.nextToken();

    const expr = try parser.parseExpression(.Lowest);

    if (!try parser.expectPeek(.Rparen)) {
        return ParseError.ExpectedNextTokenRparen;
    }

    return expr;
}

fn parseIfExpression(parser: *Parser) ParseError!Expression {
    const curr_tok = try parser.current_token.clone();
    errdefer curr_tok.deinit();

    // print("if curr_tot = {}\n", .{curr_tok.kind});

    if (!try parser.expectPeek(.Lparen)) return ParseError.ExpectedNextTokenLparen;

    try parser.nextToken();
    const condition = try parser.parseExpression(.Lowest);
    errdefer condition.deinit();
    // print("if condition = {}\n", .{condition});

    if (!try parser.expectPeek(.Rparen)) return ParseError.ExpectedNextTokenRparen;

    if (!try parser.expectPeek(.Lbrace)) return ParseError.ExpectedNextTokenLbrace;

    const consequence = try parser.parseBlockStatement();
    errdefer consequence.deinit();

    const condition_ptr = try parser.allocator.create(Expression);
    errdefer parser.allocator.destroy(condition_ptr);
    condition_ptr.* = condition;

    var alternative: ?BlockStatement = null;
    errdefer {
        if (alternative) |alt| alt.deinit();
    }

    if (parser.peekTokenIs(.Else)) {
        try parser.nextToken();

        if (!try parser.expectPeek(.Lbrace)) return ParseError.ExpectedNextTokenLbrace;

        alternative = try parser.parseBlockStatement();
    }

    // if (<condition>) {<consequence>} else {<alternative>}
    return Expression{ .if_expression = .{
        .allocator = parser.allocator,
        .token = curr_tok,
        .condition = condition_ptr,
        .consequence = consequence,
        .alternative = alternative,
    } };
}

fn parseBlockStatement(parser: *Parser) ParseError!BlockStatement {
    errdefer {
        parser.current_token.deinit();
        parser.peek_token.deinit();
    }

    const curr_tok = try parser.current_token.clone();
    errdefer curr_tok.deinit();

    try parser.nextToken();

    var block_statements = ArrayList(Statement).init(parser.allocator);
    errdefer {
        for (block_statements.items) |stmt| stmt.deinit();
        block_statements.deinit();
    }

    while (!parser.curTokenIs(.Rbrace) and !parser.curTokenIs(.Eof)) : (try parser.nextToken()) {
        const stmt = try parser.parseStatement();
        errdefer stmt.deinit();
        try block_statements.append(stmt);
    }

    return BlockStatement{ .token = curr_tok, .statements = block_statements };
}

fn parseFunctionLiteral(parser: *Parser) ParseError!Expression {
    const curr_tok = try parser.current_token.clone();
    errdefer curr_tok.deinit();

    if (!try parser.expectPeek(.Lparen)) return ParseError.ExpectedNextTokenLparen;

    const parameters = try parser.parseFunctionParameters();
    errdefer parameters.deinit();

    if (!try parser.expectPeek(.Lbrace)) {
        return ParseError.ExpectedNextTokenLbrace;
    }

    const body = try parser.parseBlockStatement();

    return Expression{ .fn_literal = .{
        .token = curr_tok,
        .parameters = parameters,
        .body = body,
    } };
}

fn parseFunctionParameters(parser: *Parser) ParseError!ArrayList(Identifier) {
    var identifiers = ArrayList(Identifier).init(parser.allocator);

    if (parser.peekTokenIs(.Rparen)) {
        try parser.nextToken();
        return identifiers;
    }

    try parser.nextToken();

    var token = try parser.current_token.clone();
    errdefer token.deinit();

    try identifiers.append(.{ .token = token });

    while (parser.peekTokenIs(.Comma)) {
        try parser.nextToken();
        try parser.nextToken();

        token = try parser.current_token.clone();
        try identifiers.append(.{ .token = token });
    }

    if (!try parser.expectPeek(.Rparen)) {
        identifiers.deinit();
        return error.ExpectedNextTokenRparen;
    }

    return identifiers;

}


fn parseCallExpression(parser: *Parser, function: Expression) ParseError!Expression {
    const curr_tok = try parser.current_token.clone();
    errdefer curr_tok.deinit();

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
        try parser.nextToken();
        return args;
    }

    try parser.nextToken();

    var arg_expr = try parser.parseExpression(.Lowest);

    try args.append(arg_expr);

    while (parser.peekTokenIs(.Comma)) {
        try parser.nextToken();
        try parser.nextToken();

        arg_expr = try parser.parseExpression(.Lowest);

        try args.append(arg_expr);
    }

    // TODO: handle if !expectpeek(.Rparent) return null
    if (!try parser.expectPeek(.Rparen)) {
        return ParseError.ExpectedNextTokenRparen;
    }

    return args;
}

fn parseStringLiteral(parser: *Parser) ParseError!Expression {
    const tok = try parser.current_token.clone();

    return Expression{ .string_expression = .{ .token = tok, .value = tok.literal } };
}

fn parseArrayLiteral(parser: *Parser) ParseError!Expression {

    const curr_tok = try parser.current_token.clone();
    errdefer curr_tok.deinit();

    var elements = ArrayList(Expression).init(parser.allocator);
    errdefer {
        for (elements.items) |expr| {
            expr.deinit();
        }
        elements.deinit();
    }

    if (parser.peekTokenIs(.Rbracket)) return Expression{ .array_literal_expr = .{ .token = curr_tok, .elements = elements }};

    try parser.nextToken();

    try elements.append(try parser.parseExpression(.Lowest));

    while (parser.peekTokenIs(.Comma)) {
        try parser.nextToken();
        try parser.nextToken(); // TODO: FIx mem leak when forgetting to close array, i.e [1, 2, 3
        try elements.append(try parser.parseExpression(.Lowest));
    }

    if (!try parser.expectPeek(.Rbracket)) return error.ExpectedNextTokenRbraket;

    return Expression{
        .array_literal_expr = .{
            .token = curr_tok,
            .elements = elements
        }
    };
}

fn parseDictionaryLiteral(parser: *Parser) ParseError!Expression {

    const allocator = parser.allocator;

    const curr_tok = try parser.current_token.clone();
    errdefer curr_tok.deinit();

    var keys = ArrayList(Expression).init(allocator);
    var values = ArrayList(Expression).init(allocator);

    // errdefer {
    //     for (keys.items) |key| key.deinit();
    //
    //     for (values.items) |value| value.deinit();
    //
    //     keys.deinit(); values.deinit();
    // }

    if (parser.peekTokenIs(.Rbrace)) { // empty dictionary
        return Expression {
            .dictionary = .{
                .allocator = allocator,
                .token = curr_tok,
                .keys = keys,
                .values = values
            }
        };
    }

    var i: usize = 0;
    const max_iterations = 1000;

    while (!parser.peekTokenIs(.Rbrace) and i < max_iterations) : (i += 1) {

        try parser.nextToken();

        const key = try parser.parseExpression(.Lowest);
        try keys.append(key);

        if (!try parser.expectPeek(.Colon)) return ParseError.ExpectedNextTokenColon;

        try parser.nextToken(); // skip colon

        const value = try parser.parseExpression(.Lowest);
        try values.append(value);

        if (!parser.peekTokenIs(.Rbrace) and !try parser.expectPeek(.Comma)) {
            return ParseError.ExpectedNextTokenRbrace;
        }

    }

    if (!try parser.expectPeek(.Rbrace)) return ParseError.ExpectedNextTokenRbrace;
    
    return Expression {
        .dictionary = .{
            .allocator = allocator,
            .token = curr_tok,
            .keys = keys,
            .values = values
        }
    };


}

fn parseIndexExpression(parser: *Parser, left: Expression) ParseError!Expression {

    const curr_tok = try parser.current_token.clone();
    errdefer curr_tok.deinit();

    try parser.nextToken();
    
    const index_ptr = try parser.allocator.create(Expression);
    errdefer parser.allocator.destroy(index_ptr);
    index_ptr.* = try parser.parseExpression(.Lowest);

    if (!try parser.expectPeek(.Rbracket)) return ParseError.ExpectedNextTokenRbraket;

    const left_ptr = try parser.allocator.create(Expression);
    left_ptr.* = left;

    return Expression {
        .index_expr = .{
            .allocator = parser.allocator,
            .token = curr_tok,
            .left = left_ptr,
            .index = index_ptr
        }
    };

}

//
fn curTokenIs(parser: *Parser, kind: Token.Kind) bool {
    return parser.current_token.kind == kind;
}

fn peekTokenIs(parser: *Parser, kind: Token.Kind) bool {
    return parser.peek_token.kind == kind;
}

fn expectPeek(parser: *Parser, kind: Token.Kind) !bool {
    if (parser.peekTokenIs(kind)) {
        try parser.nextToken();
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
    errdefer program.deinit();

    while (parser.current_token.kind != Token.Kind.Eof) {
        const stmt = try parser.parseStatement();
        errdefer stmt.deinit();
        try program.statements.append(stmt);
        try parser.nextToken();
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
    const err_str = try std.fmt.allocPrint(parser.allocator, "no defined prefix parse function for {}", .{kind});
    try parser.errors.append(err_str);
}

// TODO: fn for creating parser, lexer program for tests
//
test "Let Statements" {
    const allocator = std.testing.allocator;

    const correct_input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    var lexer = Lexer.init(allocator, correct_input); // working

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    try parser.checkParseErrors();

    expect(parser.errors.items.len == 0) catch |err| {
        print("Encountered {} parsing errors\n", .{parser.errors.items.len});

        for (parser.errors.items) |parse_err| {
            print("parse error: {s}\n", .{parse_err});
        }

        return err;
    };

    expect(program.statements.items.len == 3) catch |err| {
        print("Statements len is not 3: Failed with error", .{});
        return err;
    };

    const expected_identiefers = [3][]const u8{ "x", "y", "foobar" };

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

    var lexer = Lexer.init(allocator, input);

    var parser = try Parser.init(&lexer, allocator);
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

    var lexer = Lexer.init(allocator, input);

    var parser = try Parser.init(&lexer, allocator);
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

    var lexer = Lexer.init(allocator, input);

    var parser = try Parser.init(&lexer, allocator);
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
        else => {
            return error.WrongExpressionType;
        },
    }
}

test "Boolean lit expr" {
    const allocator = std.testing.allocator;

    const input = [_][]const u8{
        "true",
        "false",
        "3 > 5 == false",
        "3 < 5 == true",
    };
    const answer = [_][]const u8{
        "true",
        "false",
        "((3 > 5) == false)",
        "((3 < 5) == true)",
    };

    for (0..input.len) |i| {
        var lexer = Lexer.init(allocator, input[i]);

        var parser = try Parser.init(&lexer, allocator);
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

    const input = [_][]const u8{ "!5;", "-15;" };

    const operators = [_][]const u8{ "!", "-" };

    const int_values = [_]u32{ 5, 15 };

    for (0..2) |i| {
        var lexer = Lexer.init(allocator, input[i]);

        var parser = try Parser.init(&lexer, allocator);
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

    const input = [_][]const u8{ "5 + 10;", "5 - 10;", "5 * 10;", "5 / 10;", "5 > 10;", "5 < 10;", "5 == 10;", "5 != 10;" };

    const token_kinds = [_]Token.Kind{ .Plus, .Minus, .Asterisk, .Slash, .Gt, .Lt, .Eq, .Neq };

    const left_value = 5;
    const right_value = 10;

    for (0..input.len) |i| {
        var lexer = Lexer.init(allocator, input[i]);

        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.ParseProgram(allocator);
        defer program.deinit();

        try parser.checkParseErrors();

        expect(program.statements.items.len == 1) catch |err| {
            print("wront stmt len: expected=1, got={}\n", .{program.statements.items.len});
            return err;
        };

        const stmt = program.statements.items[0];

        try expect(stmt == .expr_stmt);

        const expr = stmt.expr_stmt.expression.*;

        try expect(expr == .infix_expression);

        expect(expr.infix_expression.token.kind == token_kinds[i]) catch |err| {
            print("wrong token: expected=[{}], got=[{}]\n", .{ token_kinds[i], stmt.expr_stmt.token.kind });

            print("token literal: {s}\n", .{stmt.tokenLiteral()});

            return err;
        };

        const right_expr = expr.infix_expression.right.*;
        const left_expr = expr.infix_expression.left.*;

        try expect(left_expr.integer_literal.value == left_value);
        try expect(right_expr.integer_literal.value == right_value);
    }
}

test "Get Program String" {
    const allocator = std.testing.allocator;

    var statements = ArrayList(Statement).init(allocator);

    const expr = Expression{ .identifier = Identifier{ .token = try Token.init(allocator, .Ident, "anotherVar") } };

    const expr_ptr = try allocator.create(Expression);
    expr_ptr.* = expr;

    try statements.append(Statement{ .let_stmt = .{ .allocator = allocator, .token = try Token.init(allocator, .Let, "let"), .name = Identifier{
        .token = try Token.init(allocator, .Ident, "myVar"),
    }, .value = expr_ptr } });

    var prog = Program{
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

    const input = [_][]const u8{
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
        "a * [1, 2, 3, 4][b * c] * d",
        "add(a * b[2], b[1], 2 * [1, 2][1])",
    };

    const answer = [_][]const u8{
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
        "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
    };

    for (0..input.len) |i| {
        var lexer = Lexer.init(allocator, input[i]);

        var parser = try Parser.init(&lexer, allocator);
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

    var lexer = Lexer.init(allocator, input);

    var parser = try Parser.init(&lexer, allocator);
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

    var lexer = Lexer.init(allocator, input);

    var parser = try Parser.init(&lexer, allocator);
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

    try expect(fn_lit.parameters.items.len == 2);

    var p_1 = fn_lit.parameters.items[0];
    var p_2 = fn_lit.parameters.items[1];

    try expectEqualStrings("x", p_1.tokenLiteral());

    try expectEqualStrings("y", p_2.tokenLiteral());

    try expect(fn_lit.body.statements.items.len == 1);

    const body_stmt = fn_lit.body.statements.items[0];
    try expect(body_stmt == .expr_stmt);
}
//
test "Call expression" {
    const allocator = std.testing.allocator;

    // const input = "add(1);";
    // const input = "add(1, 2);";
    // const input = "add(3 + 4);";
    // const input = "add(1, 2 * 3, 4 + 5);";
    const input = "add(1, 2 * 3, add(5, 5));";

    var lexer = Lexer.init(allocator, input);

    var parser = try Parser.init(&lexer, allocator);
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

test "String Expr" {
    const allocator = std.testing.allocator;

    const input =
        \\"hello world"
    ;

    const answer = "hello world";

    var lexer = Lexer.init(allocator, input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    const n_stmts = program.statements.items.len;

    try expect(n_stmts == 1);

    const stmt = program.statements.items[0];

    try expect(stmt == .expr_stmt);

    const expr = stmt.expr_stmt.expression.*;

    try expect(expr == .string_expression);

    try expectEqualStrings(answer, expr.string_expression.value);
}

test "Array Literal" {

    const allocator = std.testing.allocator;
    const input = "[1, 2 * 2, 3 + 4]";


    var lexer = Lexer.init(allocator, input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    const n_stmts = program.statements.items.len;

    try expect(n_stmts == 1);

    const stmt = program.statements.items[0];

    try expect(stmt == .expr_stmt);

    const expr = stmt.expr_stmt.expression.*;

    try expect(expr == .array_literal_expr);

    const n_elements = expr.array_literal_expr.elements.items.len;

    try expect(n_elements == 3);

    const prog_str = try program.String();
    defer allocator.free(prog_str);

    try expectEqualStrings("[1, (2 * 2), (3 + 4)]", prog_str);

}

test "Dictionary Literal" {

    std.testing.log_level = .debug;

    const allocator = std.testing.allocator;
    const input = 
        \\{ "key1": 21, "key2": "string_val" }
    ;

    var lexer = Lexer.init(allocator, input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    const n_stmts = program.statements.items.len;

    try expect(n_stmts == 1);

    const stmt = program.statements.items[0];

    try expect(stmt == .expr_stmt);

    const expr = stmt.expr_stmt.expression.*;

    try expect(expr == .dictionary);

    const prog_str = try program.String();
    defer allocator.free(prog_str);
    //
    try expectEqualStrings(prog_str, "{ key1: 21, key2: string_val }");

}
test "Index Expr" {
        
    const allocator = std.testing.allocator;

    const input = "myArray[1 + 1]";

    var lexer = Lexer.init(allocator, input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    const n_stmts = program.statements.items.len;

    try expect(n_stmts == 1);

    const stmt = program.statements.items[0];

    try expect(stmt == .expr_stmt);

    const expr = stmt.expr_stmt.expression.*;

    try expect(expr == .index_expr);
    
    const left = expr.index_expr.left.*;
    const index = expr.index_expr.index.*;

    try expect(left == .identifier);

    try expect(index == .infix_expression);

}

test "unexected prefix tkn" {

    const allocator = std.testing.allocator;

    const input = 
        \\let x = 10;
        \\if (x > 4) { x = x + 3 };
    ;

    var lexer = Lexer.init(allocator, input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    const programError = parser.ParseProgram(allocator);
    //defer program.deinit();

    try std.testing.expectError(error.NoPrefixFunction, programError);


}
//
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
//     var lexer = Lexer.init(allocator, input);
//
//     var parser = try Parser.init(&lexer, allocator);
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

