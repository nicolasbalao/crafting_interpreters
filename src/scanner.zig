const std = @import("std");
const TokenLib = @import("token.zig");
const Token = @import("token.zig").Token;
const Literal = TokenLib.Literal;
const TokenTypeLib = @import("token_type.zig");
const TokenType = TokenTypeLib.TokenType;

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

    fn scanToken(self: *Scanner) !void {
        const char: u8 = self.advance();

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
                // ignore this
                //' ', '\r', '\t' =>
            },
        }
    }

    fn advance(self: *Scanner) u8 {
        defer self.current += 1;
        if (self.current >= self.source.len) {
            // TODO refactor this with error or something else ?
            return '\x00';
        }
        return self.source[self.current];
    }

    fn addToken(self: *Scanner, token_type: TokenType, literal: ?Literal) !void {
        const text = self.source[self.start..self.current];
        const token = Token.init(token_type, text, literal orelse Literal.None, self.line);

        try self.tokens.append(token);
    }

    fn match(self: *Scanner, expected_char: u8) bool {
        if (self.isAtEnd()) return false;
        // TODO: see if is self.current or self.current + 1
        if (self.source[self.current] != expected_char) return false;

        self.current += 1;

        return true;
    }

    fn peek(self: Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }
};

test "Scanner.init should init Scanner" {
    const source: []const u8 = "var test = \"test\"";
    const allocator: std.mem.Allocator = std.testing.allocator;
    const scanner = Scanner.init(source, allocator);

    try std.testing.expectEqualStrings(source, scanner.source);
}

test "Scanner.scanTokens should extract common tokens from source" {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    const gpa_alloc = gpa.allocator();

    var expected_tokens = std.ArrayList(Token).init(gpa_alloc);
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
    try expected_tokens.append(Token.init(TokenType.EOF, "", Literal.None, 3));

    // !*+-/=<><= == //

    const source: []const u8 =
        \\ // this is a comment
        \\(( )){} // grouping stuff
        \\!*+-/=<> <= >= == // operators
    ;

    const allocator: std.mem.Allocator = std.testing.allocator;
    var scanner = Scanner.init(source, allocator);

    try scanner.scanTokens();

    const t1 = Token.init(TokenType.EOF, "", Literal.None, 3);
    const t2 = Token.init(TokenType.EOF, "", Literal.None, 3);

    if(std.meta.eql(t1, t2)) {
        std.debug.print("EQUAL \n\n\n", .{});
    }


    try std.testing.expectEqual(expected_tokens.items.len, scanner.tokens.items.len);
    try std.testing.expectEqualSlices(Token, expected_tokens.items, scanner.tokens.items);
}

test "Scanner.scanTokens should extract token from source" {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    const gpa_alloc = gpa.allocator();

    var expected_tokens = std.ArrayList(Token).init(gpa_alloc);
    try expected_tokens.append(Token.init(TokenType.VAR, "var", Literal.None, 1));
    try expected_tokens.append(Token.init(TokenType.IDENTIFIER, "test", Literal.None, 1));
    try expected_tokens.append(Token.init(TokenType.EQUAL, "=", Literal.None, 1));
    try expected_tokens.append(Token.init(TokenType.STRING, "test", Literal.None, 1));
    const source: []const u8 = "var test = \"test\"";
    const allocator: std.mem.Allocator = std.testing.allocator;
    var scanner = Scanner.init(source, allocator);

    try scanner.scanTokens();

    try std.testing.expectEqual(expected_tokens.items.len, scanner.tokens.items.len);
    try std.testing.expectEqualSlices(Token, expected_tokens.items, scanner.tokens.items);
}
