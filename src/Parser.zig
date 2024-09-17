const std = @import("std");
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");
const ast = @import("ast.zig");
const Program = ast.Program;
const Statement = ast.Statement;
const Identifier = ast.Identifier;
const Expression = ast.Expression;
const BlockStatement = ast.BlockStatement;
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
            .Lparen => Precedence.Call,
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
    // for (parser.errors.items) |parse_err| {
    //
    //     parser.allocator.free(parse_err);
    //
    // }
    //
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
        .False, .True => parser.parseBooleanExpression(),
        .Lparen => parser.parseGroupedExpression(),
        .If => parser.parseIfExpression(),
        .Function => parser.parseFunctionLiteral(),
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
            .Lparen => parser.parseCallExpression(left_expr),
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

fn parseBooleanExpression(parser: *Parser) Expression {

    return Expression {
        .boolean_literal = .{
            .token = parser.current_token,
            .value = parser.curTokenIs(.True),
        }
    };

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

    const token = parser.current_token;

    // log.debug("token = {}, {s}", .{token.kind, Token.tokenLiteral(&token)});

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

    // log.debug("infix: token={}, \n\tleft={}, \n\tright={}\n", .{
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

fn parseGroupedExpression(parser: *Parser) ?Expression {
    parser.nextToken();

    const expr = parser.parseExpression(.Lowest);

    if (!parser.expectPeek(.Rparen)) {
        return null;
    }

    return expr;
}

fn parseIfExpression(parser: *Parser) ?Expression {
    
    const curr_tok = parser.current_token;
    
    // print("if curr_tot = {}\n", .{curr_tok.kind});

    if (!parser.expectPeek(.Lparen)) return null;
    
    parser.nextToken();
    const condition = parser.parseExpression(.Lowest).?;
    // print("if condition = {}\n", .{condition});

    if (!parser.expectPeek(.Rparen)) return null;

    if (!parser.expectPeek(.Lbrace)) return null;

    const consequence = parser.parseBlockStatement();

    const condition_ptr = parser.allocator.create(Expression) catch {
        @panic("failed to allocate mem");
    };

    condition_ptr.* = condition;

    var alternative: ?BlockStatement = null;
    if (parser.peekTokenIs(.Else)) {
        parser.nextToken();

        if (!parser.expectPeek(.Lbrace)) return null;

        alternative = parser.parseBlockStatement();
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

fn parseBlockStatement(parser: *Parser) BlockStatement {

    const curr_tok = parser.current_token;

    parser.nextToken();

    var statements = parser.allocator.alloc(Statement, 0) catch {
        @panic("Failed to create statements slice");
    };

    while (!parser.curTokenIs(.Rbrace) and !parser.curTokenIs(.Eof)) : (parser.nextToken()){
        const maybe_stmt = parser.parseStatement();

        if (maybe_stmt) |stmt| {
            const old_len = statements.len;
            statements = parser.allocator.realloc(statements, old_len + 1) catch {
                @panic("Failed to alloc mem");
            };
            statements[old_len] = stmt;
        }
    }

    return BlockStatement {
        .token = curr_tok,
        .statements = statements
    };

}

fn parseFunctionLiteral(parser: *Parser) ?Expression {
    const curr_tok = parser.current_token;

    if (!parser.expectPeek(.Lparen)) return null;

    const parameters = parser.parseFunctionParameters();

    if (!parser.expectPeek(.Lbrace)) {
        if (parameters != null) parameters.?.deinit();
        return null;
    }

    const body = parser.parseBlockStatement();

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

fn parseCallExpression(parser: *Parser, function: Expression) Expression {
    const curr_tok = parser.current_token;

    const arguments = parser.parseCallArguments();

    const func_ptr = parser.allocator.create(Expression) catch {
        @panic("Failed to create func_ptr");
    };

    func_ptr.* = function;

    return Expression {
        .call_expression = .{
            .token = curr_tok,
            .function = func_ptr,
            .args = arguments
        }
    };
}

fn parseCallArguments(parser: *Parser) ArrayList(Expression) {

    var args = ArrayList(Expression).init(parser.allocator);

    if (parser.peekTokenIs(.Rparen)) {
        parser.nextToken();
        return args;
    }

    parser.nextToken(); 
    args.append(parser.parseExpression(.Lowest).?) catch {
        @panic("Failed to append expression");
    };

    while (parser.peekTokenIs(.Comma)) {
        parser.nextToken();
        parser.nextToken();

        
        args.append(parser.parseExpression(.Lowest).?) catch {
            @panic("Failed to append expression");
        };
    }

    // TODO: handle if !expectpeek(.Rparent) return null

    return args;


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

// TODO: Fix dynamic error message
fn peekError(parser: *Parser, kind: Token.Kind) void {

    // const msg = std.fmt.allocPrint(parser.allocator, "Expected next token to be {any}, got {any} instead", .{
    //     kind, parser.peek_token.kind 
    // }) catch {
    //     @panic("failed to create error msg");
    // };
    _ = kind;
    
    const msg = "peek error";

    parser.errors.append(msg) catch {
        @panic("failed to allocate error msg");
    };


}

fn noPrefixParseFunction(parser: *Parser, kind: Token.Kind) !void {
    _ = kind;
    // TODO: fix error handling for monkey
    try parser.errors.append("no defined prefix parse function");
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

test "Ident Expression" {
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


test "Func Lit Expr" {

    const allocator = std.testing.allocator;
    const input = "fn(x, y) { x + y; }";

    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    try expect(program.statements.items.len == 1);

    const stmt = program.statements.items[0];

    try expect(stmt == .expr_stmt);

    try expect(stmt.expr_stmt.expression.? == .fn_literal);

    const fn_lit = stmt.expr_stmt.expression.?.fn_literal;

    try expect(fn_lit.parameters.?.items.len == 2);

    
    var p_1 = fn_lit.parameters.?.items[0];
    var p_2 = fn_lit.parameters.?.items[1];


    try expectEqualStrings("x", p_1.tokenLiteral());


    try expectEqualStrings("y", p_2.tokenLiteral());

    try expect(fn_lit.body.statements.len == 1);

    try expect(fn_lit.body.statements[0] == .expr_stmt);
}

test "Call expression" {
    
    const allocator = std.testing.allocator;

    const input = "add(1, 2 * 3, 4 + 5);";

    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.ParseProgram(allocator);
    defer program.deinit();

    try expect(program.statements.items.len == 1);

    const stmt = program.statements.items[0];

    try expect(stmt == .expr_stmt);

    const expr = stmt.expr_stmt.expression.?;

    try expect(expr == .call_expression);

    try expect(expr.call_expression.args.items.len == 3);


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
