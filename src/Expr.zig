const tokenLib = @import("token.zig");
const Token = tokenLib.Token;
const LiteralType = tokenLib.Literal;
const std = @import("std");
const Allocator = std.mem.Allocator;
const TokenType = @import("token_type.zig").TokenType;

// TODO make script for generate this file ?
pub const exprEnum = enum {
    binary,
    grouping,
    literal,
    unary,
};

// We use *Expr because a pointer has a known, fixed size,
// while Expr itself would be recursive and lead to an infinite size.
pub const Expr2 = union(enum) {
    Binary: struct {
        left: *Expr2,
        operator: Token,
        right: *Expr2,
    },
    Grouping: struct {
        expr: *Expr2,

        pub fn toString(self: Expr2, buf: *std.ArrayList(u8)) ![]const u8 {
            //try buf.append('(');
            try buf.appendSlice("group");
            //try buf.append(' ');
            const expr_string: []const u8 = try self.expr.toString();
            try buf.appendSlice(expr_string);
        }
    },
    Literal: struct {
        value: LiteralType,
    },
    Unary: struct {
        left: Token,
        right: *Expr2,
    },
};

pub fn toString(allocator: Allocator, expr: *const Expr2) ![]const u8 {
    var buf = std.ArrayList(u8).init(allocator);
    // TODO: make more research about it
    // Because we use toOwnedSlice
    errdefer buf.deinit();

    switch (expr.*) {
        .Binary => |b| {
            try buf.append('(');
            try buf.appendSlice(b.operator.lexeme);
            try buf.append(' ');
            const left_string: []const u8 = try toString(allocator, b.left);
            defer allocator.free(left_string);
            try buf.appendSlice(left_string);
            try buf.append(' ');
            const right_string: []const u8 = try toString(allocator, b.right);
            defer allocator.free(right_string);
            try buf.appendSlice(right_string);

            try buf.append(')');
        },
        .Grouping => |g| {
            try buf.appendSlice("(group ");
            const expr_string = try toString(allocator, g.expr);
            defer allocator.free(expr_string);
            try buf.appendSlice(expr_string);
            try buf.append(')');
        },
        .Literal => |l| {
            switch (l.value) {
                LiteralType.None => try buf.appendSlice("Nil"),
                inline else => |payload| {
                    const string_value = try std.fmt.allocPrint(allocator, "{any}", .{payload});
                    defer allocator.free(string_value);
                    try buf.appendSlice(string_value);
                },
            }
        },
        .Unary => |u| {
            try buf.append('(');
            try buf.appendSlice(u.left.lexeme);
            try buf.append(' ');
            const right_string = try toString(allocator, u.right);
            defer allocator.free(right_string);
            try buf.appendSlice(right_string);
            try buf.append(')');
        },
    }

    //try buf.append(')');
    return buf.toOwnedSlice();
}

test "Expr toString should work as expected" {
    var allocator = std.testing.allocator;
    // Simule les tokens nécessaires
    const minus_token = Token{
        .type = TokenType.MINUS,
        .lexeme = "-",
        .literal = LiteralType.None,
        .line = 1,
    };
    const star_token = Token{
        .type = TokenType.STAR,
        .lexeme = "*",
        .literal = LiteralType.None,
        .line = 1,
    };

    // Alloue les sous-expressions
    const literal_123 = try allocator.create(Expr2);
    defer allocator.destroy(literal_123);

    literal_123.* = Expr2{ .Literal = .{ .value = LiteralType{ .Number = 123 } } };

    const unary_minus = try allocator.create(Expr2);
    defer allocator.destroy(unary_minus);
    unary_minus.* = Expr2{ .Unary = .{ .left = minus_token, .right = literal_123 } };

    const literal_45_67 = try allocator.create(Expr2);
    defer allocator.destroy(literal_45_67);
    literal_45_67.* = Expr2{ .Literal = .{ .value = LiteralType{ .Number = 45.67 } } };

    const grouping = try allocator.create(Expr2);
    defer allocator.destroy(grouping);
    grouping.* = Expr2{ .Grouping = .{ .expr = literal_45_67 } };

    const binary = Expr2{ .Binary = .{
        .left = unary_minus,
        .operator = star_token,
        .right = grouping,
    } };

    const expr_string = try toString(allocator, &binary);
    defer allocator.free(expr_string);

    try std.testing.expectEqualStrings("(* (- 123) (group 45.67))", expr_string);
}
