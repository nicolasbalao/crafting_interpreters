const std = @import("std");
const crafting_interpreters = @import("crafting_interpreters");

pub fn main() !void {
    // Prints to stderr, ignoring potential errors.
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});
    try crafting_interpreters.bufferedPrint();
}

