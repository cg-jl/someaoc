const std = @import("std");

const State = struct {
    // x register starts @ 1
    x: i32 = 1,
    cycle: u32 = 1,
    uop: ?enum { addx_0, addx_1, noop } = null,
    fetched_addx: i32 = undefined,

    pub inline fn tick(s: *State, fetch_reader: anytype) !void {
        // NOTE: here I can remove the `try` branch if I just mmap the file.
        if (s.requiresFetch()) try s.doFetch(fetch_reader);
        s.doCycle();
    }

    pub inline fn requiresFetch(s: *const State) bool {
        return s.uop == null;
    }

    pub fn doFetch(s: *State, fetch_reader: anytype) !void {
        const op = try Op.parseFromReader(fetch_reader);
        s.fetched_addx = op.value;
        s.uop = switch (op.tag) {
            .addx => .addx_0,
            .noop => .noop,
        };
    }

    pub fn doCycle(s: *State) void {
        s.uop = switch (s.uop.?) {
            .addx_0 => .addx_1,
            .addx_1 => addx1: {
                s.x += s.fetched_addx;
                break :addx1 null;
            },
            .noop => noop: {
                break :noop null;
            },
        };
    }
};
const is_part1 = false;
const is_real = true;

const file_name = if (is_real) "input.txt" else "test.txt";

const Op = struct {
    tag: enum(u32) {
        noop = 0x6e6f6f70,
        addx = 0x61646478,
    } = .noop,
    value: i32 = undefined,

    pub fn format(
        o: Op,
        comptime fmt: []const u8,
        opts: std.fmt.FormatOptions,
        w: anytype,
    ) @TypeOf(w).Error!void {
        try w.writeAll(@tagName(o.tag));
        try w.writeAll(" ");
        if (o.tag == .addx) {
            return std.fmt.formatType(o.value, fmt, opts, w, 0);
        }
    }

    fn parseFromReader(r: anytype) !Op {
        var line_buf: [10]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&line_buf);
        fbs.reset();

        try r.streamUntilDelimiter(fbs.writer(), '\n', 10);
        const line = line_buf[0..fbs.pos];

        if (line.len == 0) return error.EndOfStream;

        const opcode = @intToEnum(@TypeOf((Op{}).tag), std.mem.readIntBig(u32, line[0..4]));
        const value = if (opcode != .addx) undefined else std.fmt.parseInt(i32, line[5..], 10) catch unreachable;

        return .{ .tag = opcode, .value = value };
    }
};

const Reader = std.io.BufferedReader(4096, std.fs.File.Reader);

pub fn partOne(bufr: *Reader) !i32 {
    const sample_indices = .{ 20, 60, 100, 140, 180, 220 };

    var signal_sum: i32 = 0;
    var s: State = .{};

    var clock: u32 = 1;
    inline for (sample_indices) |next_sample| {
        while (clock != next_sample) : (clock += 1) {
            try s.tick(bufr.reader());
        }
        std.debug.assert(clock == next_sample);
        signal_sum += @intCast(i32, clock) * s.x;
    }
    return signal_sum;
}

pub const Crt = struct {
    const width = 40;
    const height = 6;
    const Index = std.math.IntFittingRange(0, width * height - 1);
    pub const Screen = std.bit_set.ArrayBitSet(usize, width * height);
    screen: Screen = Screen.initEmpty(),
    current_px: Index = 0,

    pub fn blit(c: *const Crt) void {
        var stderr = std.io.getStdErr();
        stderr.lock(.exclusive) catch unreachable;
        defer stderr.unlock();

        for (0..height) |row| {
            for (0..width) |col| {
                const idx = row * width + col;
                const is_active = c.screen.isSet(idx);
                const sequence = if (is_active) "\x1b[48;5;255m  \x1b[m" else "  ";
                stderr.writeAll(sequence) catch {};
            }
            stderr.writeAll("\n") catch {};
        }
    }

    pub fn tick(c: *Crt, sprite_middle: i32) void {
        defer c.current_px = (c.current_px + 1) % (width * height);
        const current_x = c.current_px % width;

        const was_turned_on = c.screen.isSet(c.current_px);
        const left_is_touching = sprite_middle > 0 and @intCast(Index, sprite_middle - 1) == current_x;
        const middle_is_touching = sprite_middle >= 0 and @intCast(Index, sprite_middle) == current_x;
        const right_is_touching = sprite_middle >= 0 and @intCast(Index, sprite_middle + 1) == current_x;
        const should_turn_on =
            left_is_touching or
            middle_is_touching or
            right_is_touching;
        const should_be_on = was_turned_on or should_turn_on;

        c.screen.setValue(c.current_px, should_be_on);
    }
};

pub fn partTwo(bufr: *Reader) !void {
    var crt = Crt{};
    var cpu = State{};
    while (true) {
        crt.tick(cpu.x);
        cpu.tick(bufr.reader()) catch |err| {
            if (err == error.EndOfStream) break;
            return err;
        };
    }

    crt.blit();
}

pub fn main() !void {
    const file = try std.fs.cwd().openFile(file_name, .{});
    defer file.close();
    var bufr = std.io.bufferedReader(file.reader());

    if (is_part1) {
        const signal_sum = try partOne(&bufr);
        std.log.info("Final signal sum: {}", .{signal_sum});
    } else {
        try partTwo(&bufr);
    }
}
