const std = @import("std");

const Value = union(enum) {
    list: []const Value,
    num: usize,

    pub fn isPacket(v: Value, comptime num: usize) bool {
        return v == .list and v.list.len == 1 and v.list[0] == .list and v.list[0].list.len == 1 and v.list[0].list[0] == .num and v.list[0].list[0].num == num;
    }

    pub fn format(v: Value, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        switch (v) {
            .num => |n| return w.print("{}", .{n}),
            .list => |values| {
                try w.writeAll("[");
                if (values.len != 0) {
                    try w.print("{}", .{values[0]});
                    for (values[1..]) |val| {
                        try w.print(",{}", .{val});
                    }
                }
                return w.writeAll("]");
            },
        }
    }
};

fn cmpLists(left: []const Value, right: []const Value) std.math.Order {
    const cmp_len = @min(left.len, right.len);

    for (left[0..cmp_len], right[0..cmp_len]) |l, r| {
        const next = cmpValues(l, r);
        if (next != .eq) return next;
    }

    return std.math.order(left.len, right.len);
}

fn cmpValues(left: Value, right: Value) std.math.Order {
    switch (left) {
        .num => |lnum| switch (right) {
            .num => |rnum| return std.math.order(lnum, rnum),
            .list => |values| {
                return cmpLists(&[_]Value{left}, values);
            },
        },
        .list => |lvalues| {
            switch (right) {
                .num => |_| return cmpLists(lvalues, &[_]Value{right}),
                .list => |rvalues| return cmpLists(lvalues, rvalues),
            }
        },
    }
}

fn parsePartial(inp: *[]const u8) usize {
    std.debug.assert(inp.len != 0);
    var res: usize = 0;
    while (inp.len != 0 and std.ascii.isDigit(inp.*[0])) {
        res = res * 10 + (inp.*[0] - '0');
        inp.* = inp.*[1..];
    }
    return res;
}

fn parseFullValue(line: []const u8, alloc: std.mem.Allocator) !Value {
    var line_ = line;
    const res = try parseValue(&line_, alloc);
    std.debug.assert(line_.len == 0);
    return res;
}

fn parseValue(line: *[]const u8, alloc: std.mem.Allocator) !Value {
    if (line.*[0] == '[') {
        var running_line = line.*[1..];

        var items = std.ArrayList(Value).init(alloc);
        errdefer items.deinit();

        while (running_line[0] != ']') {
            const next = try parseValue(&running_line, alloc);
            try items.append(next);
            if (running_line[0] == ',') running_line = running_line[1..];
        }

        line.* = running_line[1..];

        return .{ .list = items.items };
    } else {
        const num = parsePartial(line);
        return .{ .num = num };
    }
}

fn parsePair(
    r: std.io.BufferedReader(4096, std.fs.File.Reader).Reader,
    value_alloc: std.mem.Allocator,
) !?struct { left: Value, right: Value } {
    var line_buf: [256]u8 = undefined;

    var fbs = std.io.fixedBufferStream(&line_buf);

    r.streamUntilDelimiter(fbs.writer(), '\n', line_buf.len) catch |err| {
        std.debug.assert(err != error.StreamTooLong);
        if (err == error.EndOfStream) return null;
        return err;
    };

    const left = try parseFullValue(line_buf[0..fbs.pos], value_alloc);

    fbs.pos = 0;
    r.streamUntilDelimiter(fbs.writer(), '\n', line_buf.len) catch |err| {
        std.debug.assert(err != error.StreamTooLong);
        std.debug.assert(err != error.EndOfStream);
        return err;
    };

    const right = try parseFullValue(line_buf[0..fbs.pos], value_alloc);

    try r.skipUntilDelimiterOrEof('\n');

    return .{ .left = left, .right = right };
}

const is_part1 = false;
const is_real = true;

const filename = if (is_real) "input.txt" else "test.txt";

pub fn main() !void {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var bufr = std.io.bufferedReader(file.reader());

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    if (is_part1) {
        var index: usize = 1;
        var acc: usize = 0;
        while (try parsePair(bufr.reader(), arena.allocator())) |pair| : (index += 1) {
            defer _ = arena.reset(.retain_capacity);
            if (cmpValues(pair.left, pair.right) == .lt) {
                acc += index;
            }
        }

        std.log.info("Result: {}", .{acc});
    } else {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        defer _ = gpa.deinit();

        var all_packets = std.ArrayList(Value).init(gpa.allocator());
        defer all_packets.deinit();

        const marker_packets: [2]Value = .{
            .{ .list = &.{.{ .list = &.{.{ .num = 2 }} }} },
            .{ .list = &.{.{ .list = &.{.{ .num = 6 }} }} },
        };

        (try all_packets.addManyAsArray(2)).* = marker_packets;

        while (try parsePair(bufr.reader(), arena.allocator())) |pair| {
            try all_packets.appendSlice(&.{ pair.left, pair.right });
        }

        std.mem.sortUnstable(
            Value,
            all_packets.items,
            {},
            struct {
                fn lt(_: void, a: Value, b: Value) bool {
                    return cmpValues(a, b) == .lt;
                }
            }.lt,
        );

        var first_index: usize = 0;

        while (true) : (first_index += 1) {
            const v = all_packets.items[first_index];
            if (v.isPacket(2)) break;
        }

        var second_index = first_index;
        first_index += 1;

        while (true) : (second_index += 1) {
            const v = all_packets.items[second_index];
            if (v.isPacket(6)) break;
        }

        second_index += 1;

        std.log.debug("first: {}, second: {}", .{ first_index, second_index });
        std.log.info("Decoder key: {}", .{first_index * second_index});
    }
}
