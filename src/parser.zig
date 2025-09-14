const TokenType = @import("token_type.zig").TokenType;
const TokenLib = @import("token.zig");
const Token = TokenLib.Token;
const LiteralType = TokenLib.Literal;
const std = @import("std");
const ExprLib = @import("Expr.zig");
const Expr = ExprLib.Expr;

const ParserError = error{ UnexpectedToken, OutOfMemory };

// TODO:
// - Make some research for understand how parser work generaly
// - Refactor
//      - match argument like line: 42
//      - Zig way
// - How should translate private methode for OOP non pub fn to struct or fn in file?
// - See for not passing all with *Parser
// - Make more test
pub const Parser = struct {
    tokens: std.ArrayList(Token),
    current: usize,

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, tokens: std.ArrayList(Token)) Parser {
        return .{
            .tokens = tokens,
            .current = 0,
            .allocator = allocator,
        };
    }

    pub fn parse(self: *Parser) ParserError!*Expr {
        return try self.expression();
    }

    fn expression(self: *Parser) ParserError!*Expr {
        return try self.equality();
    }

    fn equality(self: *Parser) ParserError!*Expr {
        var expr: *Expr = try self.comparison();

        while (self.match(&[_]TokenType{ .BANG_EQUAL, .EQUAL_EQUAL })) {
            const operator: Token = self.previous();
            const right: *Expr = try self.comparison();

            const new_expr = try self.allocator.create(Expr);

            new_expr.* = Expr{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = new_expr;
        }

        return expr;
    }

    fn comparison(self: *Parser) ParserError!*Expr {
        var expr: *Expr = try self.term();

        const match_types = [4]TokenType{ TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL };
        while (self.match(&match_types)) {
            const operator: Token = self.previous();
            const right: *Expr = try self.term();
            const new_expr = try self.allocator.create(Expr);
            new_expr.* = Expr{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = new_expr;
        }

        return expr;
    }

    fn term(self: *Parser) ParserError!*Expr {
        var expr: *Expr = try self.factor();

        const match_types = [2]TokenType{ TokenType.MINUS, TokenType.PLUS };

        while (self.match(&match_types)) {
            const operator: Token = self.previous();
            const right: *Expr = try self.factor();

            const new_expr = try self.allocator.create(Expr);

            new_expr.* = Expr{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = new_expr;
        }

        return expr;
    }

    fn factor(self: *Parser) ParserError!*Expr {
        var expr = try self.unary();
        const match_types = [2]TokenType{ TokenType.STAR, TokenType.SLASH };

        while (self.match(&match_types)) {
            const operator: Token = self.previous();
            const right: *Expr = try self.unary();
            const new_expr = try self.allocator.create(Expr);

            new_expr.* = Expr{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = new_expr;
        }
        return expr;
    }

    fn unary(self: *Parser) ParserError!*Expr {
        const match_types = [2]TokenType{ TokenType.BANG, TokenType.MINUS };

        if (self.match(&match_types)) {
            const operator: Token = self.previous();
            const right: *Expr = try self.unary();
            const expr = try self.allocator.create(Expr);
            expr.* = Expr{ .Unary = .{ .left = operator, .right = right } };
            return expr;
        }

        return self.primary();
    }

    fn primary(self: *Parser) ParserError!*Expr {
        const expr: *Expr = try self.allocator.create(Expr);

        if (self.match(&[1]TokenType{TokenType.FALSE})) {
            expr.* = Expr{ .Literal = .{ .value = LiteralType{ .Boolean = false } } };
            return expr;
        }
        if (self.match(&[1]TokenType{TokenType.TRUE})) {
            expr.* = Expr{ .Literal = .{ .value = LiteralType{ .Boolean = true } } };
            return expr;
        }
        if (self.match(&[1]TokenType{TokenType.NIL})) {
            expr.* = Expr{ .Literal = .{ .value = LiteralType.None } };
            return expr;
        }

        const match_types = [2]TokenType{ TokenType.NUMBER, TokenType.STRING };
        if (self.match(&match_types)) {
            expr.* = Expr{ .Literal = .{ .value = self.previous().literal } };
            return expr;
        }

        if (self.match(&[1]TokenType{TokenType.LEFT_PAREN})) {
            const new_expr: *Expr = try self.expression();
            expr.* = Expr{ .Grouping = .{ .expr = new_expr } };
            return expr;
        }

        return error.UnexpectedToken;
    }

    fn match(self: *Parser, types: []const TokenType) bool {
        for (types) |token_type| {
            if (self.check(token_type)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().type == token_type;
    }

    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn isAtEnd(self: *Parser) bool {
        return self.peek().type == TokenType.EOF;
    }

    fn peek(self: *Parser) Token {
        return self.tokens.items[self.current];
    }

    fn previous(self: *Parser) Token {
        // TODO error handling previous < 0
        return self.tokens.items[self.current - 1];
    }
};

test "Parser.parse should work" {
    const test_allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tokens = std.ArrayList(Token).init(test_allocator);
    defer tokens.deinit();

    // test case on -123 * (45.67)
    const minus_token = Token{
        .type = TokenType.MINUS,
        .lexeme = "-",
        .literal = LiteralType.None,
        .line = 1,
    };
    const litral_123 = Token.init(TokenType.NUMBER, "123", LiteralType{ .Number = 123 }, 1);
    const star_token = Token{
        .type = TokenType.STAR,
        .lexeme = "*",
        .literal = LiteralType.None,
        .line = 1,
    };
    const left_paren_token = Token.init(TokenType.LEFT_PAREN, "(", LiteralType.None, 1);
    const literal_4567 = Token.init(TokenType.NUMBER, "45.67", LiteralType{ .Number = 45.67 }, 1);
    const right_paren_token = Token.init(TokenType.RIGHT_PAREN, ")", LiteralType.None, 1);

    try tokens.append(minus_token);
    try tokens.append(litral_123);
    try tokens.append(star_token);
    try tokens.append(left_paren_token);
    try tokens.append(literal_4567);
    try tokens.append(right_paren_token);

    var parser = Parser.init(allocator, tokens);

    const expr: *Expr = try parser.parse();
    const result = try ExprLib.toString(test_allocator, expr);
    defer test_allocator.free(result);

    try std.testing.expectEqualStrings("(* (- 123) (group 45.67))", result);
}
