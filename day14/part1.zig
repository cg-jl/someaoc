const constants = @import("constants.zig");
const interactive = @import("interactive.zig");
const Point = constants.Point;
const std = @import("std");
const parse = @import("parse.zig");

const Board = std.bit_set.ArrayBitSet(usize, constants.board_width * constants.board_height);

// Arrange positions so that the board is indexed row-first.
fn index(x: usize, y: usize) usize {
    return y * constants.board_width + x;
}

fn posYFromIndex(i: usize) usize {
    return i / constants.board_width;
}

fn posXFromIndex(i: usize) usize {
    return i % constants.board_width;
}

fn posIndex(pos: Point) usize {
    return index(pos[0], pos[1]);
}
fn drawBoard(board: *const Board) void {
    // go to top left, clear the screen, hide cursor, set color 15.
    std.debug.print("\x1b[H\x1b[J\x1b[?25l\x1b[38;5;15m", .{});
    var it = board.iterator(.{});
    while (it.next()) |i| {
        const x = posXFromIndex(i);
        const y = posYFromIndex(i);
        std.debug.print("\x1b[{};{}H@", .{ y + 1, x + 1 });
    }
}

const Loader = struct {
    inner: Board = Board.initEmpty(),

    pub fn addVertical(l: *Loader, x: usize, y_from: usize, y_to: usize) void {
        for (y_from..y_to + 1) |y| {
            l.inner.set(index(x, y));
        }
    }

    pub fn addHorizontal(l: *Loader, y: usize, x_from: usize, x_to: usize) void {
        return l.inner.setRangeValue(
            .{
                .start = index(x_from, y),
                .end = index(x_to, y) + 1,
            },
            true,
        );
    }
};

const Sim = struct {
    blocked: Board,
    visited: Board = Board.initEmpty(),
    settled_count: usize = 0,

    fn isBlocked(sim: *const Sim, pos: Point) bool {
        return sim.blocked.isSet(posIndex(pos));
    }

    fn drop(sim: *Sim, from: Point) void {
        const drop_index = posIndex(from);
        // anything beyond the board is assumed to never settle
        if (from[1] >= constants.board_height - 1) return;

        // we already know the outcome of this one: it falls to the void since
        // we're visiting it again.
        if (sim.visited.isSet(drop_index)) return;
        sim.visited.set(drop_index);

        const down = from + Point{ 0, 1 };
        if (!sim.isBlocked(down)) {
            sim.drop(down);
            // dropping down didn't settle, which means we're not settling
            if (!sim.isBlocked(down)) return;
        }

        // dropping left will have no barriers so it will never settle
        if (from[0] == 0) return;

        const down_left = down - Point{ 1, 0 };
        if (!sim.isBlocked(down_left)) {
            sim.drop(down_left);
            // dropping down left didn't settle, so any particle that reaches
            // here isn't going to settle.
            if (!sim.isBlocked(down_left)) return;
        }

        // dropping right will have no barriers so it will never settle
        if (from[0] == constants.board_width - 1) return;

        const down_right = down + Point{ 1, 0 };
        if (!sim.isBlocked(down_right)) {
            sim.drop(down_right);
            // dropping down right didn't settle, so any particle that reaches
            // here isn't going to settle.
            if (!sim.isBlocked(down_right)) return;
        }

        // we settle since there are no more options to fall
        if (constants.will_paint_table) {
            std.debug.print(
                "\x1b[{};{}H\x1b[1;38;5;11m+",
                .{ posYFromIndex(drop_index) + 1, posXFromIndex(drop_index) + 1 },
            );
            std.time.sleep(10 * std.time.ns_per_ms);
        }
        sim.settled_count += 1;
        sim.blocked.set(drop_index);
    }
};

const builtin = @import("builtin");

pub const std_options = struct {
    pub const log_level = if (builtin.mode == .Debug) .debug else .info;
};

pub fn main() !void {
    var loader = Loader{};
    try parse.loadFile(&loader, constants.input_file_name);

    var sim = Sim{ .blocked = loader.inner };

    if (constants.will_paint_table) {
        drawBoard(&sim.blocked);
    }
    sim.drop(constants.drop_point);
    if (constants.will_paint_table) {
        interactive.finishBoard();
    }

    std.log.info("settled count: {}", .{sim.settled_count});
}

fn a() void {}
