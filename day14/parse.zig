const std = @import("std");
const constants = @import("constants.zig");
const Point = constants.Point;

fn parsePoint(s: []const u8) Point {
    var tokens = std.mem.tokenizeScalar(u8, s, ',');
    const x = std.fmt.parseUnsigned(usize, tokens.next().?, 10) catch unreachable;
    const y = std.fmt.parseUnsigned(usize, tokens.next().?, 10) catch unreachable;
    std.debug.assert(tokens.next() == null);

    return .{ x, y };
}

pub fn loadFile(target: anytype, file_name: []const u8) !void {
    const file = try std.fs.cwd().openFile(file_name, .{});
    defer file.close();

    var bufr = std.io.bufferedReader(file.reader());
    var line_buf: [256]u8 = undefined;

    while (true) {
        var fbs = std.io.fixedBufferStream(&line_buf);
        bufr.reader().streamUntilDelimiter(fbs.writer(), '\n', line_buf.len) catch |err| {
            if (err == error.EndOfStream) break;
            // our line buffer has space for the maximum byte count (~170)
            // already.
            std.debug.assert(err != error.StreamTooLong);
            return err;
        };

        const line = line_buf[0..fbs.pos];
        if (line.len == 0) break;

        var point_srcs = std.mem.tokenizeSequence(u8, line, " -> ");
        var start = parsePoint(point_srcs.next().?) - constants.board_left_corner;
        while (point_srcs.next()) |tok| {
            const end = parsePoint(tok) - constants.board_left_corner;
            defer start = end;
            const lt = start < end;
            const min = @select(usize, lt, start, end);
            const max = @select(usize, lt, end, start);
            // vertical
            if (start[0] == end[0]) {
                target.addVertical(start[0], min[1], max[1]);
            } else {
                std.debug.assert(start[1] == end[1]);
                target.addHorizontal(start[1], min[0], max[0]);
            }
        }
    }
}
