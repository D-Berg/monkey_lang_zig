const std = @import("std");
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");
const ast = @import("ast.zig");
const Program = ast.Program;
const Expression = ast.Expression;
const Statement = ast.Statement;
const LetStatement = ast.LetStatement;
const Identifier = ast.Identifier;
const BlockStatement = ast.BlockStatement;
const Node = ast.Node;
const ArrayList = std.ArrayList;

const Allocator = std.mem.Allocator;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
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
        .current_token = lexer.nextToken(),
        .peek_token = lexer.nextToken(),
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
    parser.current_token = parser.peek_token;
    parser.peek_token = parser.lexer.nextToken();
}

fn parseStatement(parser: *Parser, allocator: Allocator) ParseError!Statement {
    const stmt: Statement = switch (parser.current_token.kind) {
        .Let => .{ .let_stmt = try parser.parseLetStatement(allocator) },
        .Return => try parser.parseReturnStatement(allocator),
        else => try parser.parseExpressionStatement(allocator),
    };
    errdefer stmt.deinit(allocator);

    return stmt;
}

fn parseLetStatement(parser: *Parser, allocator: Allocator) ParseError!LetStatement {
    const token = parser.current_token;

    if (!parser.expectPeek(Token.Kind.Ident)) return ParseError.ExpectedNextTokenIdentifier;

    const ident_token = parser.current_token;

    const identifier = Identifier{ .token = ident_token };

    if (!parser.expectPeek(Token.Kind.Assign)) return ParseError.ExpectedNextTokenAssign;

    parser.nextToken();

    const expr = try parser.parseExpression(allocator, .Lowest);
    errdefer expr.deinit(allocator);

    const expr_ptr = try parser.allocator.create(Expression);
    errdefer parser.allocator.destroy(expr_ptr);

    expr_ptr.* = expr;

    while (!parser.curTokenIs(Token.Kind.Semicolon)) {
        parser.nextToken();
    }

    return ast.LetStatement{ 
        .token = token,
        .name = identifier,
        .value = expr_ptr  
    };
}

fn parseReturnStatement(parser: *Parser, allocator: Allocator) ParseError!Statement {

    const curr_tok = parser.current_token;

    parser.nextToken();

    const expr = try parser.parseExpression(allocator, .Lowest);

    const expr_ptr = try parser.allocator.create(Expression);
    expr_ptr.* = expr;

    while (!parser.curTokenIs(Token.Kind.Semicolon)) {
        parser.nextToken();
    }

    return Statement { 
        .ret_stmt = .{
            .token = curr_tok,
            .value = expr_ptr
        }
    };
}

fn parseExpressionStatement(parser: *Parser, allocator: Allocator) ParseError!Statement {

    const token = parser.current_token;

    const expr = try parser.parseExpression(allocator, .Lowest);
    errdefer expr.deinit(allocator);

    const expr_ptr = try allocator.create(Expression);
    errdefer allocator.destroy(expr_ptr);
    expr_ptr.* = expr;

    if (parser.peekTokenIs(Token.Kind.Semicolon)) {
        parser.nextToken();
    }

    return Statement { 
        .expr_stmt = .{
            .token = token,
            .expression = expr_ptr
        }
    };

}

fn parseExpression(parser: *Parser, allocator: Allocator, precedence: Precedence) ParseError!ast.Expression {

    const prefix = switch (parser.current_token.kind) { // same as looking into the hashmap p.prefixParseFns in GO
        .Ident => parser.parseIdentifier(),
        .Int => parser.parseIntegerLiteral(),
        .Bang, .Minus => parser.parsePrefixExpression(allocator),
        .False, .True => parser.parseBooleanExpression(),
        .Lparen => parser.parseGroupedExpression(allocator),
        .If => parser.parseIfExpression(allocator),
        .Function => try parser.parseFunctionLiteral(allocator),
        .String => parser.parseStringLiteral(),
        .Lbracket => parser.parseArrayLiteral(allocator),
        .Lbrace => parser.parseDictionaryLiteral(allocator), // {
        inline else => |kind| {
            // TODO: add parsing Error
            log.debug("no prefix for {}", .{kind});
            return ParseError.NoPrefixFunction;
        }
    };

    var left_expr: Expression = try prefix;
    errdefer left_expr.deinit(allocator);

    while (!parser.peekTokenIs(.Semicolon) and @intFromEnum(precedence) < @intFromEnum(parser.peekPrecedence())) {

        const peek_kind = parser.peek_token.kind;

        parser.nextToken();

        const infix = switch (peek_kind) {
            .Plus, 
            .Minus, 
            .Asterisk, 
            .Slash, 
            .Gt, .Lt, 
            .Eq, .Neq => try parser.parseInfixExpression(allocator, left_expr),
            .Lparen => try parser.parseCallExpression(allocator, left_expr),
            .Lbracket => try parser.parseIndexExpression(allocator, left_expr),
            else => ParseError.NoPrefixFunction,
        };
        errdefer infix.deinit();

        // if (infix == ParseError.NoPrefixFunction) return left_expr;

        left_expr = infix catch return left_expr;
        errdefer left_expr.deinit();
    }

    return left_expr;
}

fn parseIdentifier(parser: *Parser) Expression {
    const token = parser.current_token;

    return Expression {
        .identifier = .{
            .token = token,
        }
    };

}

fn parseIntegerLiteral(parser: *Parser) ParseError!Expression {

    const maybe_val = std.fmt.parseInt(u32, parser.current_token.literal, 0);
    // TODO handle overflow

    const token = parser.current_token;

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

fn parseBooleanExpression(parser: *Parser) Expression {
    const token = parser.current_token;

    return Expression{ 
        .boolean_literal = .{
            .token = token,
            .value = parser.curTokenIs(.True),
        } 
    };
}

fn parsePrefixExpression(parser: *Parser, allocator: Allocator) ParseError!Expression {

    // log.debug("expression=prefix_expression", .{});

    const tok = parser.current_token;

    parser.nextToken();

    const expr = try parser.parseExpression(allocator, .Prefix);
    errdefer expr.deinit(allocator);

    const expr_ptr = try allocator.create(Expression);
    expr_ptr.* = expr;
    // // log.debug("token = {}, right ={any}", .{tok.kind, expr_ptr.*});

    return Expression{ 
        .prefix_expression = .{
            .token = tok,
            .right = expr_ptr,
        } 
    };
}

fn parseInfixExpression(parser: *Parser, allocator: Allocator, left: Expression) ParseError!Expression {

    // log.debug("parsing infix expression", .{});

    const token = parser.current_token;

    // log.debug("token = {}, {s}", .{token.kind, Token.tokenLiteral(&token)});

    const precedence = parser.curPrecedence();

    parser.nextToken();

    const right = try parser.parseExpression(allocator, precedence);
    errdefer right.deinit(allocator);

    const left_ptr = try allocator.create(Expression);
    errdefer allocator.destroy(left_ptr);

    const right_ptr = try allocator.create(Expression);
    errdefer allocator.destroy(right_ptr);

    left_ptr.* = left;
    right_ptr.* = right;

    return Expression {
        .infix_expression = .{
            .token = token,
            .left = left_ptr,
            .right = right_ptr
        }
    };

}

fn parseGroupedExpression(parser: *Parser, allocator: Allocator) ParseError!Expression {
    parser.nextToken();

    const expr = try parser.parseExpression(allocator, .Lowest);

    if (!parser.expectPeek(.Rparen)) {
        return ParseError.ExpectedNextTokenRparen;
    }

    return expr;
}

fn parseIfExpression(parser: *Parser, allocator: Allocator) ParseError!Expression {
    const curr_tok = parser.current_token;

    // print("if curr_tot = {}\n", .{curr_tok.kind});

    if (!parser.expectPeek(.Lparen)) return ParseError.ExpectedNextTokenLparen;

    parser.nextToken();
    const condition = try parser.parseExpression(allocator, .Lowest);
    errdefer condition.deinit(allocator);
    // print("if condition = {}\n", .{condition});

    if (!parser.expectPeek(.Rparen)) return ParseError.ExpectedNextTokenRparen;

    if (!parser.expectPeek(.Lbrace)) return ParseError.ExpectedNextTokenLbrace;

    const consequence = try parser.parseBlockStatement(allocator);
    errdefer consequence.deinit(allocator);

    const condition_ptr = try allocator.create(Expression);
    errdefer allocator.destroy(condition_ptr);
    condition_ptr.* = condition;

    var alternative: ?BlockStatement = null;
    errdefer {
        if (alternative) |alt| alt.deinit(allocator);
    }

    if (parser.peekTokenIs(.Else)) {
        parser.nextToken();

        if (!parser.expectPeek(.Lbrace)) return ParseError.ExpectedNextTokenLbrace;

        alternative = try parser.parseBlockStatement(allocator);
    }

    // if (<condition>) {<consequence>} else {<alternative>}
    return Expression { 
        .if_expression = .{
            .token = curr_tok,
            .condition = condition_ptr,
            .consequence = consequence,
            .alternative = alternative,
        } 
    };
}

fn parseBlockStatement(parser: *Parser, allocator: Allocator) ParseError!BlockStatement {

    const curr_tok = parser.current_token;

    parser.nextToken();

    var block_statements = ArrayList(Statement).init(allocator);
    errdefer {
        for (block_statements.items) |stmt| stmt.deinit(allocator);
        block_statements.deinit();
    }

    while (!parser.curTokenIs(.Rbrace) and !parser.curTokenIs(.Eof)) : (parser.nextToken()) {
        const stmt = try parser.parseStatement(allocator);
        errdefer stmt.deinit(allocator);
        try block_statements.append(stmt);
    }

    return BlockStatement{ 
        .token = curr_tok, 
        .statements = try block_statements.toOwnedSlice() 
    };
}

fn parseFunctionLiteral(parser: *Parser, allocator: Allocator) ParseError!Expression {
    const curr_tok = parser.current_token;

    if (!parser.expectPeek(.Lparen)) return ParseError.ExpectedNextTokenLparen;

    const parameters = try parser.parseFunctionParameters();
    errdefer allocator.free(parameters);

    if (!parser.expectPeek(.Lbrace)) {
        return ParseError.ExpectedNextTokenLbrace;
    }

    const body = try parser.parseBlockStatement(allocator);

    return Expression{ .fn_literal = .{
        .token = curr_tok,
        .parameters = parameters,
        .body = body,
    } };
}

fn parseFunctionParameters(parser: *Parser) ParseError![]const Identifier {

    var identifiers = ArrayList(Identifier).init(parser.allocator);
    errdefer identifiers.deinit();

    if (parser.peekTokenIs(.Rparen)) {
        parser.nextToken();
        return try identifiers.toOwnedSlice();
    }

    parser.nextToken();

    var token = parser.current_token;

    try identifiers.append(.{ .token = token });

    while (parser.peekTokenIs(.Comma)) {
        parser.nextToken();
        parser.nextToken();

        token = parser.current_token;
        try identifiers.append(.{ .token = token });
    }

    if (!parser.expectPeek(.Rparen)) {
        return error.ExpectedNextTokenRparen;
    }

    return try identifiers.toOwnedSlice();

}


fn parseCallExpression(parser: *Parser, allocator: Allocator, function: Expression) ParseError!Expression {
    const curr_tok = parser.current_token;

    // TODO: errdefer deinit args
    const arguments = try parser.parseCallArguments(allocator);
    errdefer {
        for (arguments) |arg| arg.deinit(allocator);
        allocator.free(arguments);
    }

    const func_ptr = try allocator.create(Expression);
    func_ptr.* = function;

    return Expression {
        .call_expression = .{
            .token = curr_tok,
            .function = func_ptr,
            .args = arguments
        }
    };
}

fn parseCallArguments(parser: *Parser, allocator: Allocator) ParseError![]const Expression {

    var args = ArrayList(Expression).init(allocator);
    // TODO: errdefer 

    if (parser.peekTokenIs(.Rparen)) {
        parser.nextToken();
        return try args.toOwnedSlice();
    }

    parser.nextToken();

    var arg_expr = try parser.parseExpression(allocator, .Lowest);
    // TODO: errdefer

    try args.append(arg_expr);

    while (parser.peekTokenIs(.Comma)) {
        parser.nextToken();
        parser.nextToken();

        arg_expr = try parser.parseExpression(allocator, .Lowest);

        try args.append(arg_expr);
    }

    // TODO: handle if !expectpeek(.Rparent) return null
    if (!parser.expectPeek(.Rparen)) {
        return ParseError.ExpectedNextTokenRparen;
    }

    return try args.toOwnedSlice();
}

fn parseStringLiteral(parser: *Parser) ParseError!Expression {
    const tok = parser.current_token;

    return Expression{ .string_expression = .{ .token = tok, .value = tok.literal } };
}

fn parseArrayLiteral(parser: *Parser, allocator: Allocator) ParseError!Expression {

    const curr_tok = parser.current_token;

    var elements = ArrayList(Expression).init(allocator);
    errdefer {
        for (elements.items) |expr| {
            expr.deinit(allocator);
        }
        elements.deinit();
    }

    if (parser.peekTokenIs(.Rbracket)) { // empty array
        return Expression{ 
            .array_literal_expr = .{ 
                .token = curr_tok, 
                .elements = try elements.toOwnedSlice() 
            }
        };
    }

    parser.nextToken();

    try elements.append(try parser.parseExpression(allocator, .Lowest));

    while (parser.peekTokenIs(.Comma)) {
        parser.nextToken();
        parser.nextToken(); 
        // BUG: Fix mem leak when forgetting to close array, i.e [1, 2, 3
        try elements.append(try parser.parseExpression(allocator, .Lowest));
    }

    if (!parser.expectPeek(.Rbracket)) return error.ExpectedNextTokenRbraket;

    return Expression {
        .array_literal_expr = .{
            .token = curr_tok,
            .elements = try elements.toOwnedSlice()
        }
    };
}

fn parseDictionaryLiteral(parser: *Parser, allocator: Allocator) ParseError!Expression {

    const curr_tok = parser.current_token;

    var keys = ArrayList(Expression).init(allocator);
    var values = ArrayList(Expression).init(allocator);

    errdefer { // clean up on error
        for (keys.items) |key| key.deinit(allocator);
        for (values.items) |value| value.deinit(allocator);
        keys.deinit(); values.deinit();
    }

    if (parser.peekTokenIs(.Rbrace)) { // empty dictionary
        return Expression {
            .dictionary = .{
                .token = curr_tok,
                .keys = try keys.toOwnedSlice(),
                .values = try values.toOwnedSlice()
            }
        };
    }

    var i: usize = 0;
    const max_iterations = 1000;

    while (!parser.peekTokenIs(.Rbrace) and i < max_iterations) : (i += 1) {

        parser.nextToken();

        const key = try parser.parseExpression(allocator, .Lowest);
        try keys.append(key);

        if (!parser.expectPeek(.Colon)) return ParseError.ExpectedNextTokenColon;

        parser.nextToken(); // skip colon

        const value = try parser.parseExpression(allocator, .Lowest);
        try values.append(value);

        if (!parser.peekTokenIs(.Rbrace) and !parser.expectPeek(.Comma)) {
            return ParseError.ExpectedNextTokenRbrace;
        }

    }

    if (!parser.expectPeek(.Rbrace)) return ParseError.ExpectedNextTokenRbrace;
    
    return Expression {
        .dictionary = .{
            .token = curr_tok,
            .keys = try keys.toOwnedSlice(),
            .values = try values.toOwnedSlice()
        }
    };


}

fn parseIndexExpression(parser: *Parser, allocator: Allocator, left: Expression) ParseError!Expression {

    const curr_tok = parser.current_token;

    parser.nextToken();
    
    const index_ptr = try allocator.create(Expression);
    errdefer {
        index_ptr.deinit(allocator);
        allocator.destroy(index_ptr);
    }
    index_ptr.* = try parser.parseExpression(allocator, .Lowest);

    if (!parser.expectPeek(.Rbracket)) return ParseError.ExpectedNextTokenRbraket;

    const left_ptr = try parser.allocator.create(Expression);
    left_ptr.* = left;

    return Expression {
        .index_expr = .{
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

    var statements: ArrayList(Statement) = .init(allocator);
    errdefer {
        for (statements.items) |stmt| stmt.deinit(allocator);
        statements.deinit();
    }

    while (parser.current_token.kind != Token.Kind.Eof) {
        const stmt = try parser.parseStatement(allocator);
        errdefer stmt.deinit(allocator);
        try statements.append(stmt);
        parser.nextToken();
    }

    // does not clone, parsers arraylists gets deinited by program
    return Program {
        .statements = try statements.toOwnedSlice()
    };
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

    var lexer = Lexer.init(correct_input); // working

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit(allocator);

    try parser.checkParseErrors();

    expect(parser.errors.items.len == 0) catch |err| {
        print("Encountered {} parsing errors\n", .{parser.errors.items.len});

        for (parser.errors.items) |parse_err| {
            print("parse error: {s}\n", .{parse_err});
        }

        return err;
    };

    expect(program.statements.len == 3) catch |err| {
        print("Statements len is not 3: Failed with error", .{});
        return err;
    };

    const expected_identiefers = [3][]const u8{ "x", "y", "foobar" };

    for (program.statements, 0..) |stmt, i| {
        try expect(stmt == .let_stmt);
        try expectEqualStrings(stmt.tokenLiteral(), "let");

        try expectEqualStrings(stmt.let_stmt.name.token.literal, expected_identiefers[i]);

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

    var lexer = Lexer.init(input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit(allocator);

    try parser.checkParseErrors();

    try expect(program.statements.len == 3);

    for (program.statements) |stmt| {
        try expect(stmt == .ret_stmt);
        try expectEqualStrings("return", stmt.tokenLiteral());
    }
}

test "Ident Expression" {
    const allocator = std.testing.allocator;

    const input = "foobar;";

    var lexer = Lexer.init(input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit(allocator);

    // try parser.checkParseErrors();
    //
    const n_stmts = program.statements.len;

    expect(n_stmts == 1) catch |err| {
        print("statement_len = {}\n", .{n_stmts});
        return err;
    };

    const stmt = program.statements[0];

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

    var lexer = Lexer.init(input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit(allocator);

    try parser.checkParseErrors();

    try expect(program.statements.len == 1);

    const stmt = program.statements[0];

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
        var lexer = Lexer.init(input[i]);

        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.ParseProgram(allocator);
        defer program.deinit(allocator);

        try parser.checkParseErrors();

        const prog_str = try program.String(allocator);
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
        var lexer = Lexer.init(input[i]);

        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.ParseProgram(allocator);
        defer program.deinit(allocator);

        try parser.checkParseErrors();

        try expect(program.statements.len == 1);

        const stmt = program.statements[0];

        try expect(stmt == .expr_stmt);

        const expr = stmt.expr_stmt.expression.*;

        try expect(expr == .prefix_expression);

        const right_expr = expr.prefix_expression.right.*;

        try expect(right_expr == .integer_literal);

        try expect(right_expr.integer_literal.value == int_values[i]);

        try expectEqualStrings(operators[i], expr.prefix_expression.token.literal);
    }
}

test "Infix Expression" {
    const allocator = std.testing.allocator;

    const input = [_][]const u8{ "5 + 10;", "5 - 10;", "5 * 10;", "5 / 10;", "5 > 10;", "5 < 10;", "5 == 10;", "5 != 10;" };

    const token_kinds = [_]Token.Kind{ .Plus, .Minus, .Asterisk, .Slash, .Gt, .Lt, .Eq, .Neq };

    const left_value = 5;
    const right_value = 10;

    for (0..input.len) |i| {
        var lexer = Lexer.init(input[i]);

        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.ParseProgram(allocator);
        defer program.deinit(allocator);

        try parser.checkParseErrors();

        try expectEqual(1, program.statements.len);

        const stmt = program.statements[0];

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

    const expr = Expression{ 
        .identifier = .{ 
            .token = .{ .kind = .Ident, .literal = "anotherVar" } 
        }
    };

    const expr_ptr = try allocator.create(Expression);
    expr_ptr.* = expr;

    try statements.append(
        Statement { 
            .let_stmt = .{ 
                .token = .{ .kind = .Let, .literal = "let" }, 
                .name = Identifier{
                    .token = .{ .kind = .Ident, .literal = "myVar" },
                }, 
                .value = expr_ptr 
            } 
        }
    );

    var prog = Program {
        .statements = try statements.toOwnedSlice(),
    };
    defer prog.deinit(allocator);

    const prog_str = try prog.String(allocator);
    defer allocator.free(prog_str);

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
        var lexer = Lexer.init(input[i]);

        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.ParseProgram(allocator);
        defer program.deinit(allocator);

        try parser.checkParseErrors();

        const prog_str = try program.String(allocator);
        defer allocator.free(prog_str);

        try expectEqualStrings(answer[i], prog_str);
    }
}

test "If Expression" {
    const allocator = std.testing.allocator;

    const input = "if (x < y) { x }";

    var lexer = Lexer.init(input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit(allocator);

    try parser.checkParseErrors();

    try expect(program.statements.len == 1);

    // TODO: expand test

}

//
test "Func Lit Expr" {
    const allocator = std.testing.allocator;
    const input = "fn(x, y) { x + y; }";

    var lexer = Lexer.init(input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit(allocator);

    expect(program.statements.len == 1) catch |err| {
        print("Expected 1 stmt, got {}\n", .{program.statements.len});
        return err;
    };

    const stmt = program.statements[0];

    try expect(stmt == .expr_stmt);

    const expr = stmt.expr_stmt.expression.*;

    try expect(expr == .fn_literal);

    const fn_lit = expr.fn_literal;

    try expect(fn_lit.parameters.len == 2);

    var p_1 = fn_lit.parameters[0];
    var p_2 = fn_lit.parameters[1];

    try expectEqualStrings("x", p_1.tokenLiteral());

    try expectEqualStrings("y", p_2.tokenLiteral());

    try expect(fn_lit.body.statements.len == 1);

    const body_stmt = fn_lit.body.statements[0];
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

    var lexer = Lexer.init(input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit(allocator);

    const n_stmts = program.statements.len;
    expect(n_stmts == 1) catch |err| {
        print("expected 1 stmt, got {}\n", .{n_stmts});
        return err;
    };

    const stmt = program.statements[0];

    try expect(stmt == .expr_stmt);
    //
    const expr = stmt.expr_stmt.expression.*;

    try expect(expr == .call_expression);

    try expect(expr.call_expression.args.len == 3);
}

test "String Expr" {
    const allocator = std.testing.allocator;

    const input =
        \\"hello world"
    ;

    const answer = "hello world";

    var lexer = Lexer.init(input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit(allocator);

    const n_stmts = program.statements.len;

    try expect(n_stmts == 1);

    const stmt = program.statements[0];

    try expect(stmt == .expr_stmt);

    const expr = stmt.expr_stmt.expression.*;

    try expect(expr == .string_expression);

    try expectEqualStrings(answer, expr.string_expression.value);
}

test "Array Literal" {

    const allocator = std.testing.allocator;
    const input = "[1, 2 * 2, 3 + 4]";


    var lexer = Lexer.init(input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit(allocator);

    const n_stmts = program.statements.len;

    try expect(n_stmts == 1);

    const stmt = program.statements[0];

    try expect(stmt == .expr_stmt);

    const expr = stmt.expr_stmt.expression.*;

    try expect(expr == .array_literal_expr);

    const n_elements = expr.array_literal_expr.elements.len;

    try expect(n_elements == 3);

    const prog_str = try program.String(allocator);
    defer allocator.free(prog_str);

    try expectEqualStrings("[1, (2 * 2), (3 + 4)]", prog_str);

}

test "Dictionary Literal" {

    std.testing.log_level = .debug;

    const allocator = std.testing.allocator;
    const input = 
        \\{ "key1": 21, "key2": "string_val" }
    ;

    var lexer = Lexer.init(input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit(allocator);

    const n_stmts = program.statements.len;

    try expect(n_stmts == 1);

    const stmt = program.statements[0];

    try expect(stmt == .expr_stmt);

    const expr = stmt.expr_stmt.expression.*;

    try expect(expr == .dictionary);

    const prog_str = try program.String(allocator);
    defer allocator.free(prog_str);
    //
    try expectEqualStrings(prog_str, "{ key1: 21, key2: string_val }");

}
test "Index Expr" {
        
    const allocator = std.testing.allocator;

    const input = "myArray[1 + 1]";

    var lexer = Lexer.init(input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit(allocator);

    const n_stmts = program.statements.len;

    try expect(n_stmts == 1);

    const stmt = program.statements[0];

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

    var lexer = Lexer.init(input);

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    const programError = parser.ParseProgram(allocator);
    // defer program.deinit(allocator);

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
//     var lexer = Lexer.init(input);
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

