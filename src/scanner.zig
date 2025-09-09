const std = @import("std");
const TokenLib = @import("token.zig");
const Token = @import("token.zig").Token;
const Literal = TokenLib.Literal;
const TokenTypeLib = @import("token_type.zig");
const TokenType = TokenTypeLib.TokenType;

const keywords_map = std.StaticStringMap(TokenType).initComptime(.{
    .{ "and", .AND },
    .{ "class", .CLASS },
    .{ "else", .ELSE },
    .{ "false", .FALSE },
    .{ "for", .FOR },
    .{ "fun", .FUN },
    .{ "if", .IF },
    .{ "nil", .NIL },
    .{ "or", .OR },
    .{ "print", .PRINT },
    .{ "return", .RETURN },
    .{ "super", .SUPER },
    .{ "this", .THIS },
    .{ "true", .TRUE },
    .{ "var", .VAR },
    .{ "while", .WHILE },
});

const ScanError = error{ UnexpectedChar, UnterminedString };
const ScannerError = ScanError || std.mem.Allocator.Error || std.fmt.ParseFloatError;

pub const Scanner = struct {
    source: []const u8,
    tokens: std.ArrayList(Token),

    start: usize = 0,
    current: usize = 0,
    line: usize = 1,

    pub fn init(source: []const u8, allocator: std.mem.Allocator) Scanner {
        return .{
            .source = source,
            .tokens = std.ArrayList(Token).init(allocator),
        };
    }

    pub fn deinit(self: Scanner) void {
        self.tokens.deinit();
    }

    pub fn scanTokens(self: *Scanner) !void {
        while (!self.isAtEnd()) {
            self.start = self.current;
            try self.scanToken();
        }

        try self.tokens.append(Token.init(TokenType.EOF, "", Literal.None, self.line));
    }

    fn isAtEnd(self: Scanner) bool {
        return self.current >= self.source.len;
    }

    fn scanToken(self: *Scanner) ScannerError!void {
        const char: u8 = self.advance() orelse return;

        switch (char) {
            '(' => try self.addToken(TokenType.LEFT_PAREN, null),
            ')' => try self.addToken(TokenType.RIGHT_PAREN, null),
            '{' => try self.addToken(TokenType.LEFT_BRACE, null),
            '}' => try self.addToken(TokenType.RIGHT_BRACE, null),
            ',' => try self.addToken(TokenType.COMMA, null),
            '.' => try self.addToken(TokenType.DOT, null),
            '-' => try self.addToken(TokenType.MINUS, null),
            '+' => try self.addToken(TokenType.PLUS, null),
            ';' => try self.addToken(TokenType.SEMICOLON, null),
            '*' => try self.addToken(TokenType.STAR, null),
            '!' => try self.addToken(if (self.match('=')) TokenType.BANG_EQUAL else TokenType.BANG, null),
            '=' => try self.addToken(if (self.match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL, null),
            '<' => try self.addToken(if (self.match('=')) TokenType.LESS_EQUAL else TokenType.LESS, null),
            '>' => try self.addToken(if (self.match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER, null),
            '"' => try self.string(),
            '/' => {
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.isAtEnd()) {
                        _ = self.advance();
                    }
                } else {
                    try self.addToken(TokenType.SLASH, null);
                }
            },
            '\n' => self.line += 1,
            // TODO make error handling (unexpected character) with the line
            else => {
                // TODO refactor this with maybe a switch ?
                if (isDigit(char)) {
                    try self.number();
                } else if (isAlpha(char)) {
                    try self.identifier();
                } else if (char == ' ' or char == '\r' or char == '\t') {
                    // ignore white space
                } else {
                    return ScannerError.UnexpectedChar;
                }
            },
        }
    }

    fn advance(self: *Scanner) ?u8 {
        if (self.current >= self.source.len) return null;
        defer self.current += 1;
        return self.source[self.current];
    }

    fn addToken(self: *Scanner, token_type: TokenType, literal: ?Literal) !void {
        const text = self.source[self.start..self.current];
        const token = Token.init(token_type, text, literal orelse Literal.None, self.line);

        try self.tokens.append(token);
    }

    fn match(self: *Scanner, expected_char: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected_char) return false;

        self.current += 1;

        return true;
    }

    fn peek(self: Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    fn string(self: *Scanner) ScannerError!void {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return ScannerError.UnterminedString;
        }

        _ = self.advance();

        // +1 and  -1 for '"' in "test"
        const value: []const u8 = self.source[self.start + 1 .. self.current - 1];
        try self.addToken(TokenType.STRING, Literal{ .String = value });
    }

    fn number(self: *Scanner) !void {
        while (isDigit(self.peek())) _ = self.advance();

        if (self.peek() == '.' and self.peekNext() != null and isDigit(self.peekNext().?)) {
            _ = self.advance();
            while (isDigit(self.peek())) _ = self.advance();
        }

        // Note: with this function function we take the dote ex 12.
        // And zig parseFloat handle it 12. -> 12
        // Maybe we should remove the dote before
        const value: f64 = try std.fmt.parseFloat(f64, self.source[self.start..self.current]);

        try self.addToken(TokenType.NUMBER, Literal{ .Number = value });
    }

    fn peekNext(self: Scanner) ?u8 {
        if (self.current + 1 >= self.source.len) return null;
        return self.source[self.current + 1];
    }

    fn identifier(self: *Scanner) !void {
        while (isAlphanumeric(self.peek())) _ = self.advance();

        const text = self.source[self.start..self.current];
        const token_type: TokenType = keywords_map.get(text) orelse TokenType.IDENTIFIER;

        try self.addToken(token_type, Literal.None);
    }
};

fn isDigit(char: u8) bool {
    return char >= '0' and char <= '9';
}

fn isAlpha(char: u8) bool {
    return (char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z') or char == '_';
}

fn isAlphanumeric(char: u8) bool {
    return isAlpha(char) or isDigit(char);
}

test "Scanner.init should init Scanner" {
    const source: []const u8 = "var test = \"test\"";
    const allocator: std.mem.Allocator = std.testing.allocator;
    const scanner = Scanner.init(source, allocator);
    defer scanner.deinit();

    try std.testing.expectEqualStrings(source, scanner.source);
}

test "Scanner.scanTokens should extract common tokens from source" {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const gpa_alloc = gpa.allocator();

    var expected_tokens = std.ArrayList(Token).init(gpa_alloc);
    defer expected_tokens.deinit();
    // line 2
    try expected_tokens.append(Token.init(TokenType.LEFT_PAREN, "(", Literal.None, 2));
    try expected_tokens.append(Token.init(TokenType.LEFT_PAREN, "(", Literal.None, 2));
    try expected_tokens.append(Token.init(TokenType.RIGHT_PAREN, ")", Literal.None, 2));
    try expected_tokens.append(Token.init(TokenType.RIGHT_PAREN, ")", Literal.None, 2));
    try expected_tokens.append(Token.init(TokenType.LEFT_BRACE, "{", Literal.None, 2));
    try expected_tokens.append(Token.init(TokenType.RIGHT_BRACE, "}", Literal.None, 2));

    //line 3
    try expected_tokens.append(Token.init(TokenType.BANG, "!", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.STAR, "*", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.PLUS, "+", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.MINUS, "-", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.SLASH, "/", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.EQUAL, "=", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.LESS, "<", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.GREATER, ">", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.LESS_EQUAL, "<=", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.GREATER_EQUAL, ">=", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.EQUAL_EQUAL, "==", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.COMMA, ",", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.SEMICOLON, ";", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.DOT, ".", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.EOF, "", Literal.None, 3));

    // !*+-/=<><= == //

    const source: []const u8 =
        \\ // this is a comment
        \\(( )){} // grouping stuff
        \\!*+-/=<> <= >= == , ;. // operators
    ;

    const allocator: std.mem.Allocator = std.testing.allocator;
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();

    try scanner.scanTokens();

    try std.testing.expectEqual(expected_tokens.items.len, scanner.tokens.items.len);

    for (scanner.tokens.items, 0..) |token, i| {
        try std.testing.expectEqualStrings(expected_tokens.items[i].lexeme, token.lexeme);
        try std.testing.expectEqual(expected_tokens.items[i].type, token.type);
        try std.testing.expectEqual(expected_tokens.items[i].literal, token.literal);
        try std.testing.expectEqual(expected_tokens.items[i].line, token.line);
    }
}

test "Scanner.scanTokens should parse literals" {
    const source: []const u8 =
        \\ "string"
        \\ 1.23
        \\ 21
    ;

    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const gpa_alloc = gpa.allocator();

    var expected_tokens = std.ArrayList(Token).init(gpa_alloc);
    defer expected_tokens.deinit();
    // line 1
    try expected_tokens.append(Token.init(TokenType.STRING, "\"string\"", Literal{ .String = "string" }, 1));
    try expected_tokens.append(Token.init(TokenType.NUMBER, "1.23", Literal{ .Number = 1.23 }, 2));
    try expected_tokens.append(Token.init(TokenType.NUMBER, "21", Literal{ .Number = 21 }, 3));
    try expected_tokens.append(Token.init(TokenType.EOF, "", Literal.None, 3));

    const allocator: std.mem.Allocator = std.testing.allocator;
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();

    try scanner.scanTokens();

    try std.testing.expectEqual(expected_tokens.items.len, scanner.tokens.items.len);

    for (scanner.tokens.items, 0..) |actual_token, i| {
        try std.testing.expectEqualStrings(expected_tokens.items[i].lexeme, actual_token.lexeme);
        try std.testing.expectEqual(expected_tokens.items[i].type, actual_token.type);
        if (actual_token.literal == Literal.String) {
            try std.testing.expectEqualStrings(expected_tokens.items[i].literal.String, actual_token.literal.String);
        } else {
            try std.testing.expectEqual(expected_tokens.items[i].literal, actual_token.literal);
        }
        try std.testing.expectEqual(expected_tokens.items[i].line, actual_token.line);
    }
}

test "Scanner.scanTokens should extract keywords from source" {
    //const Keywords = enum { AND, CLASS, ELSE, FALSE, FOR, FUN, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE };
    const source: []const u8 =
        \\ and class else
        \\ false for fun
        \\ if nil or
        \\ print return super
        \\ this true var while
    ;

    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const gpa_alloc = gpa.allocator();

    var expected_tokens = std.ArrayList(Token).init(gpa_alloc);
    defer expected_tokens.deinit();
    // line 1
    try expected_tokens.append(Token.init(TokenType.AND, "and", Literal.None, 1));
    try expected_tokens.append(Token.init(TokenType.CLASS, "class", Literal.None, 1));
    try expected_tokens.append(Token.init(TokenType.ELSE, "else", Literal.None, 1));
    try expected_tokens.append(Token.init(TokenType.FALSE, "false", Literal.None, 2));
    try expected_tokens.append(Token.init(TokenType.FOR, "for", Literal.None, 2));
    try expected_tokens.append(Token.init(TokenType.FUN, "fun", Literal.None, 2));
    try expected_tokens.append(Token.init(TokenType.IF, "if", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.NIL, "nil", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.OR, "or", Literal.None, 3));
    try expected_tokens.append(Token.init(TokenType.PRINT, "print", Literal.None, 4));
    try expected_tokens.append(Token.init(TokenType.RETURN, "return", Literal.None, 4));
    try expected_tokens.append(Token.init(TokenType.SUPER, "super", Literal.None, 4));
    try expected_tokens.append(Token.init(TokenType.THIS, "this", Literal.None, 5));
    try expected_tokens.append(Token.init(TokenType.TRUE, "true", Literal.None, 5));
    try expected_tokens.append(Token.init(TokenType.VAR, "var", Literal.None, 5));
    try expected_tokens.append(Token.init(TokenType.WHILE, "while", Literal.None, 5));
    try expected_tokens.append(Token.init(TokenType.EOF, "", Literal.None, 5));

    const allocator: std.mem.Allocator = std.testing.allocator;
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();

    try scanner.scanTokens();

    try std.testing.expectEqual(expected_tokens.items.len, scanner.tokens.items.len);

    for (scanner.tokens.items, 0..) |token, i| {
        try std.testing.expectEqualStrings(expected_tokens.items[i].lexeme, token.lexeme);
        try std.testing.expectEqual(expected_tokens.items[i].type, token.type);
        try std.testing.expectEqual(expected_tokens.items[i].literal, token.literal);
        try std.testing.expectEqual(expected_tokens.items[i].line, token.line);
    }
}

test "Scanner.scanTokens should extract token from source" {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const gpa_alloc = gpa.allocator();

    var expected_tokens = std.ArrayList(Token).init(gpa_alloc);
    defer expected_tokens.deinit();

    try expected_tokens.append(Token.init(TokenType.VAR, "var", Literal.None, 1));
    try expected_tokens.append(Token.init(TokenType.IDENTIFIER, "test", Literal.None, 1));
    try expected_tokens.append(Token.init(TokenType.EQUAL, "=", Literal.None, 1));
    try expected_tokens.append(Token.init(TokenType.STRING, "\"test\"", Literal{ .String = "test" }, 1));
    try expected_tokens.append(Token.init(TokenType.SEMICOLON, ";", Literal.None, 1));
    try expected_tokens.append(Token.init(TokenType.EOF, "", Literal.None, 1));

    const source: []const u8 = "var test = \"test\";";
    const allocator: std.mem.Allocator = std.testing.allocator;
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();

    try scanner.scanTokens();

    try std.testing.expectEqual(expected_tokens.items.len, scanner.tokens.items.len);

    for (scanner.tokens.items, 0..) |actual_token, i| {
        try std.testing.expectEqualStrings(expected_tokens.items[i].lexeme, actual_token.lexeme);
        try std.testing.expectEqual(expected_tokens.items[i].type, actual_token.type);
        if (actual_token.literal == Literal.String) {
            try std.testing.expectEqualStrings(expected_tokens.items[i].literal.String, actual_token.literal.String);
        } else {
            try std.testing.expectEqual(expected_tokens.items[i].literal, actual_token.literal);
        }
        try std.testing.expectEqual(expected_tokens.items[i].line, actual_token.line);
    }
}

test "Scanner.scanTokens should error unexpectedChar" {
    const source: []const u8 = "var 😂 = 1";
    const allocator: std.mem.Allocator = std.testing.allocator;
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();

    try std.testing.expectError(ScannerError.UnexpectedChar, scanner.scanTokens());
}

test "Scanner.scanTokens should error UnterminedString" {
    const source: []const u8 = "print \"test";
    const allocator: std.mem.Allocator = std.testing.allocator;
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();

    try std.testing.expectError(ScannerError.UnterminedString, scanner.scanTokens());
}
