const TokenType = @import("token_type.zig").TokenType;

pub const Literal = union(enum) {
    None,
    String: []const u8,
    Number: f64,
    Boolean: bool,
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    literal: Literal,
    line: usize,

    pub fn init(tokenType: TokenType, lexeme: []const u8, literal: Literal, line: usize) Token {
        return .{
            .type = tokenType,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
        };
    }
};
