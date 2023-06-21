const std = @import("std");
const Reader = std.io.BufferedReader(4096, std.fs.File.Reader);

pub const LineReader = struct {
    inner: Reader.Reader,
    line: [64]u8 = undefined,

    pub fn next(r: *LineReader) !?[]const u8 {
        var fbs = std.io.fixedBufferStream(&r.line);
        r.inner.streamUntilDelimiter(fbs.writer(), '\n', 64) catch |err| {
            if (err == error.EndOfStream) return null;
            return err;
        };

        if (fbs.pos == 0) return null;
        return r.line[0..fbs.pos];
    }
};

pub fn State(comptime part: Part) type {
    return struct {
        number_queues: []Queue,
        metas: std.ArrayListUnmanaged(Meta),

        mod_numbers: if (part == .part2) []usize else void,

        counts: []usize,

        numbers_buf: []usize,

        pub fn format(
            s: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            w: anytype,
        ) @TypeOf(w).Error!void {
            try w.writeAll("queues: [");
            try w.print("{any}", .{s.number_queues[0].readableSlice(0)});
            for (s.number_queues[1..]) |q| {
                try w.print(" {any}", .{q.readableSlice(0)});
            }
            try w.print("]\nmetas: {any}\n", .{s.metas.items});
            try w.print("counts: {any}\n", .{s.counts});
            if (part == .part2) {
                try w.writeAll("mod numbers: [");
                try w.print("{any}", .{s.mod_numbers[0..s.counts.len]});
                var i: usize = s.counts.len;
                while (i != s.mod_numbers.len) : (i += s.counts.len) {
                    try w.print(" {any}", .{s.mod_numbers[i..][0..s.counts.len]});
                }
                return w.writeAll("]");
            }
        }

        pub fn monkeynessScore(s: *const @This()) usize {
            var best: usize = 0;
            var second_best: usize = 0;

            for (s.counts) |x| {
                if (x > best) {
                    second_best = best;
                    best = x;
                } else if (x > second_best) {
                    second_best = x;
                }
            }

            std.log.debug("best: {}, second best: {}", .{ best, second_best });
            return best * second_best;
        }

        pub fn deinit(s: @This(), alloc: std.mem.Allocator) void {
            alloc.free(s.numbers_buf);
            alloc.free(s.number_queues);
            var m = s.metas;
            m.deinit(alloc);
            alloc.free(s.counts);
            if (part == .part2) alloc.free(s.mod_numbers);
        }

        pub fn fromLines(l: *LineReader, alloc: std.mem.Allocator) !@This() {
            var metas = std.ArrayList(Meta).init(alloc);
            errdefer metas.deinit();
            var numbers = std.ArrayList(usize).init(alloc);
            defer numbers.deinit();
            var ends = std.ArrayList(usize).init(alloc);
            defer ends.deinit();

            while (try l.next()) |_| {
                try parseInitialNumbers(&numbers, (try l.next()).?);
                const end = numbers.items.len;
                try ends.append(end);
                const meta = try Meta.fromLines(l);
                try metas.append(meta);
                _ = try l.next();
            }

            const monkey_count = metas.items.len;
            const item_count = numbers.items.len;

            const monkey_number_table_size = std.math.mul(
                usize,
                monkey_count,
                item_count,
            ) catch return error.OutOfMemory;

            const numbers_buf = try alloc.alloc(usize, monkey_number_table_size);
            const number_queues = try alloc.alloc(Queue, monkey_count);
            const mod_numbers = if (part == .part1) {} else try alloc.alloc(usize, monkey_number_table_size);
            const counts = try alloc.alloc(usize, monkey_count);
            @memset(counts, 0);

            // initialize the queues
            var start: usize = 0;
            for (number_queues, ends.items, 0..) |*q, end, monkey_index| {
                q.* = Queue.init(numbers_buf[monkey_index * item_count ..][0..item_count]);
                switch (part) {
                    .part1 => {
                        q.writeAssumeCapacity(numbers.items[start..end]);
                    },
                    .part2 => {
                        for (start..end) |i| q.writeItemAssumeCapacity(i);
                    },
                }
                start = end;
            }

            if (part == .part2) {
                for (numbers.items, 0..) |n, num_index| {
                    for (metas.items, 0..) |meta, monkey_index| {
                        mod_numbers[num_index * monkey_count + monkey_index] = n % meta.modulo;
                    }
                }
            }

            return .{
                .number_queues = number_queues,
                .mod_numbers = mod_numbers,
                .metas = metas.moveToUnmanaged(),
                .numbers_buf = numbers_buf,
                .counts = counts,
            };
        }
    };
}
pub const Part = enum { part1, part2 };
pub const Queue = std.fifo.LinearFifo(usize, .Slice);
pub const Meta = struct {
    op: Op,
    modulo: usize,
    if_true: usize,
    if_false: usize,

    pub fn fromLines(lines: *LineReader) !Meta {
        const op = Op.fromLine((try lines.next()).?);
        const modulo = parseModulo((try lines.next()).?);
        const if_true = parseIfTrue((try lines.next()).?);
        const if_false = parseIfFalse((try lines.next()).?);
        return .{
            .op = op,
            .modulo = modulo,
            .if_true = if_true,
            .if_false = if_false,
        };
    }
};

fn parsePartial(src: *[]const u8) usize {
    var result: usize = 0;
    while (src.len != 0 and std.ascii.isDigit(src.*[0])) {
        result = result * 10 + (src.*[0] - '0');
        src.* = src.*[1..];
    }
    return result;
}

pub fn parseInitialNumbers(list: *std.ArrayList(usize), line: []const u8) !void {
    var remaining = line[16..];

    while (remaining.len != 0) {
        remaining = remaining[2..];
        try list.append(parsePartial(&remaining));
    }
}

fn parseModulo(line: []const u8) usize {
    const after_garbo = line[21..];
    return std.fmt.parseUnsigned(usize, after_garbo, 10) catch unreachable;
}

fn parseIfTrue(line: []const u8) usize {
    const after_garbo = line[29..];
    return std.fmt.parseUnsigned(usize, after_garbo, 10) catch unreachable;
}

fn parseIfFalse(line: []const u8) usize {
    const after_garbo = line[30..];
    return std.fmt.parseUnsigned(usize, after_garbo, 10) catch unreachable;
}

pub const Op = union(enum) {
    add: usize,
    mul: usize,
    square: void,

    fn fromLine(line: []const u8) Op {
        const line_after_old = line[23..];
        return switch (line_after_old[0]) {
            '*' => if (line_after_old[2] == 'o') .square else .{
                .mul = std.fmt.parseUnsigned(usize, line_after_old[2..], 10) catch unreachable,
            },
            '+' => .{
                .add = std.fmt.parseUnsigned(usize, line_after_old[2..], 10) catch unreachable,
            },
            else => unreachable,
        };
    }
};
