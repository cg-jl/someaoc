const std = @import("std");
const Direction = enum { right, up, left, down };
const Pos = struct {
    x: i32 = 0,
    y: i32 = 0,

    pub fn format(p: Pos, comptime fmt: []const u8, opts: std.fmt.FormatOptions, w: anytype) @TypeOf(w).Error!void {
        try w.writeAll("(");
        try std.fmt.formatType(p.x, fmt, opts, w, 0);
        try w.writeAll(", ");
        try std.fmt.formatType(p.y, fmt, opts, w, 0);
        return w.writeAll(")");
    }

    fn move(p: *Pos, dir: Direction) void {
        switch (dir) {
            .left => p.x -= 1,
            .right => p.x += 1,
            .up => p.y += 1,
            .down => p.y -= 1,
        }
    }
};

const is_part1 = false;
const is_real = true;

const file_name = if (is_real) "input.txt" else "test.txt";

const pos_count = if (is_part1) 2 else 10;

const State = struct {
    positions: [pos_count]Pos = [_]Pos{.{}} ** pos_count,

    fn move(s: *State, dir: Direction) void {
        s.positions[0].move(dir);

        for (s.positions[1..], s.positions[0 .. s.positions.len - 1]) |*current, prev| {
            // distance in T -> H direction
            const dist_x = prev.x - current.x;
            const dist_y = prev.y - current.y;

            // nothing to move. Since this one doesn't move, and we start in a stable
            // configuration, there's no more movements done.
            if (std.math.absCast(dist_x) <= 1 and std.math.absCast(dist_y) <= 1) {
                break;
            }

            // note that if you do the 5x5 drawing of H and respective T positions,
            // there are 4 quadrants. Which means each T moves a respective step in the
            // direction from T to H.
            switch (std.math.sign(dist_x)) {
                -1 => current.move(.left),
                0 => {},
                1 => current.move(.right),
                else => unreachable,
            }

            switch (std.math.sign(dist_y)) {
                -1 => current.move(.down),
                0 => {},
                1 => current.move(.up),
                else => unreachable,
            }
        }
    }
};

var stdin = std.io.getStdIn();

pub fn main() !void {
    try stdin.lock(.exclusive);
    defer stdin.unlock();
    const file = try std.fs.cwd().openFile(file_name, .{});
    defer file.close();
    var bufr = std.io.bufferedReader(file.reader());

    var line_buf: [10]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&line_buf);

    var state = State{};

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var unique = std.AutoArrayHashMap(Pos, void).init(arena.allocator());
    defer unique.deinit();

    defer stdin.unlock();

    while (true) {
        fbs.reset();

        bufr.reader().streamUntilDelimiter(fbs.writer(), '\n', 10) catch |err| {
            if (err == error.EndOfStream) break;
            return err;
        };

        const line = line_buf[0..fbs.pos];
        if (line.len == 0) break;

        const move_tag = line_buf[0];
        const dir: Direction = switch (move_tag) {
            'R' => .right,
            'L' => .left,
            'U' => .up,
            'D' => .down,
            else => unreachable,
        };

        const count_src = line_buf[2..fbs.pos];
        const count = try std.fmt.parseInt(u32, count_src, 10);

        for (0..count) |_| {
            state.move(dir);
            try unique.put(state.positions[state.positions.len - 1], {});
        }
    }

    std.log.info("Unique tail positions: {}", .{unique.count()});
}
