const constants = @import("constants.zig");
const std = @import("std");

pub fn finishBoard() void {
    std.debug.print("\x1b[{};1H\x1b[m\x1b[?25h\r", .{constants.floor_position});
}

pub fn waitForEnter() void {
    std.debug.print("\x1b[s", .{});
    std.io.getStdIn().reader().skipUntilDelimiterOrEof('\n') catch {};
    std.debug.print("\x1b[u", .{});
}
