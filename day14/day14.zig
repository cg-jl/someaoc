const std = @import("std");
const Point = @Vector(2, usize);
fn parsePoint(s: []const u8) Point {
    var tokens = std.mem.tokenizeScalar(u8, s, ',');
    const x = std.fmt.parseUnsigned(usize, tokens.next().?, 10) catch unreachable;
    const y = std.fmt.parseUnsigned(usize, tokens.next().?, 10) catch unreachable;
    return .{ x, y };
}

const BoardMaskInt = usize;

const is_part2 = false;

const is_real_input = true;

const will_paint_table = false;

const floor_position = if (!is_real_input) 9 + 2 else 173 + 2;

const objs_hz_span = if (!is_real_input) 9 else 56;
const objs_vt_span = if (!is_real_input) 5 else 160;

const board_height = floor_position + (floor_position / 2);
const board_width = objs_hz_span + 1 + 2;

const board_left_corner = Point{ (if (!is_real_input) 494 else 449) - 1, 0 }; //Point{ 300, 0 };
//
const drop_point = Point{ 500, 0 } - Point{ board_left_corner[0], 0 };

const Board = std.bit_set.ArrayBitSet(BoardMaskInt, board_width * board_height);

/// Arranges positions in a table such that the board is indexed column-first.
fn index(pos: Point) usize {
    return pos[0] * board_height + pos[1];
}

fn finishBoard() void {
    std.debug.print("\x1b[{};1H\x1b[m\x1b[?25h\r", .{board_height});
}

fn drawBoard(board: *const Board) void {
    std.debug.print("\x1b[H\x1b[J\x1b[?25l\x1b[38;5;15m", .{});
    var it = board.iterator(.{});
    while (it.next()) |i| {
        const x = i / board_height;
        const y = i % board_height;
        std.debug.print("\x1b[{};{}H@@", .{ y + 1, x + 1 });
    }
}

fn waitEnter() void {
    std.debug.print("\x1b[s", .{});
    std.io.getStdIn().reader().skipUntilDelimiterOrEof('\n') catch {};
    std.debug.print("\x1b[u", .{});
}

const Part2Sim = struct {
    blocked: Board = Board.initEmpty(),
    settled_count: usize = 0,
    left_height: usize = 0,
    right_height: usize = 0,

    fn isBlocked(sim: *const Part2Sim, pos: Point) bool {
        return sim.blocked.isSet(index(pos));
    }

    fn drop(sim: *Part2Sim, pos: Point) void {
        if (pos[1] >= floor_position) return;

        const pos_index = index(pos);

        if (sim.blocked.isSet(pos_index)) return;

        if (will_paint_table) {
            std.debug.print(
                "\x1b[{};{}H\x1b[38;5;11m+",
                .{ pos[1] + 1, pos[0] + 1 },
            );
            std.time.sleep(10 * std.time.ns_per_ms);
        }

        sim.drop(pos + Point{ 0, 1 });

        if (pos[0] != 0) {
            sim.drop(pos + Point{ 0, 1 } - Point{ 1, 0 });
        } else {
            sim.left_height += 1;
        }

        if (pos[0] != board_width - 1) {
            sim.drop(pos + Point{ 1, 1 });
        } else {
            sim.right_height += 1;
        }

        sim.settled_count += 1;
        // ensure we don't visit it again.
        sim.blocked.set(pos_index);
    }

    fn calcCount(sim: *const Part2Sim) usize {

        // calculate the missed amount by the simulation.
        // The big triangle has base 2 * height.
        // It is *not* centered over the box, although the box *contains* the center.

        const dist_start_to_right = board_width - drop_point[0] - 1;
        const dist_start_to_left = drop_point[0];

        const half_base = (floor_position);

        const left_triangle_base = half_base - dist_start_to_left - 1;
        const right_triangle_base = half_base - dist_start_to_right - 1;

        // I'm pretty sure that `left_height` and `right_height` *can* be precomputed
        // but I'm only interested in doing enough geometry to cut the triangles.
        // The heights are _very_ cheap to compute.
        const left_triangle_area = left_triangle_base * sim.left_height / 2;
        const right_triangle_area = right_triangle_base * sim.right_height / 2;

        const missed = left_triangle_area + right_triangle_area;

        return missed + sim.settled_count;
    }
};

const Part1Sim = struct {
    blocked: Board = Board.initEmpty(),
    visited: Board = Board.initEmpty(),
    settled_count: usize = 0,

    fn isBlocked(sim: *const Part1Sim, pos: Point) bool {
        return sim.blocked.isSet(index(pos));
    }

    fn drop(sim: *Part1Sim, pos: Point) void {
        if (pos[1] >= board_height - 1) return;
        const pos_index = index(pos);
        if (sim.visited.isSet(pos_index)) return;
        sim.visited.set(pos_index);

        if (sim.blocked.isSet(pos_index)) return;

        if (will_paint_table) {
            std.debug.print(
                "\x1b[{};{}H\x1b[1;38;5;3m##",
                .{ pos[1] + 1, pos[0] + 1 },
            );
        }

        const down = pos + Point{ 0, 1 };
        const down_index = index(down);
        if (!sim.blocked.isSet(down_index)) {
            sim.drop(down);
            // the particle falls down and later to the void.
            if (!sim.blocked.isSet(down_index)) return;
        }

        // the particle slides down left, falling immediately into the void.
        if (pos[0] == 0) return;

        if (pos[0] != 0) {
            const down_left = pos + Point{ 0, 1 } - Point{ 1, 0 };
            const down_left_index = index(down_left);
            if (!sim.blocked.isSet(down_left_index)) {
                sim.drop(down_left);
                // the particle slides down left, then falls to the void.
                if (!sim.blocked.isSet(down_left_index)) return;
            }
        }

        // the particle slides down right, falling immediately into the void.
        if (pos[0] == board_width - 1) return;

        if (pos[0] != board_width - 1) {
            const down_right = pos + Point{ 1, 1 };
            const down_right_index = index(down_right);
            if (!sim.blocked.isSet(down_right_index)) {
                sim.drop(down_right);
                // the particle slides down right, then falls to the void.
                if (!sim.blocked.isSet(down_right_index)) return;
            }
        }

        // the three bottom sands are set so the particle settles.
        sim.settled_count += 1;
        sim.blocked.set(pos_index);
    }
};

fn parseFile(board: *Board) !void {
    const file = try std.fs.cwd().openFile(if (!is_real_input)
        "test.txt"
    else
        "input.txt", .{});
    defer file.close();

    var bufr = std.io.bufferedReader(file.reader());
    var line_buf: [256]u8 = undefined;

    while (true) {
        var fbs = std.io.fixedBufferStream(&line_buf);
        bufr.reader().streamUntilDelimiter(fbs.writer(), '\n', line_buf.len) catch |err| {
            if (err == error.EndOfStream) break;
            std.debug.assert(err != error.StreamTooLong);
            return err;
        };

        const line = line_buf[0..fbs.pos];
        if (line.len == 0) break;

        var point_srcs = std.mem.tokenizeSequence(u8, line, " -> ");
        var start = parsePoint(point_srcs.next().?) - board_left_corner;
        while (point_srcs.next()) |v_end| {
            const end = parsePoint(v_end) - board_left_corner;
            const lt = start < end;
            const min = @select(usize, lt, start, end);
            const max = @select(usize, lt, end, start);
            // vertical
            if (start[0] == end[0]) {
                const x = start[0];

                for (min[1]..max[1] + 1) |y| {
                    board.set(index(.{ x, y }));
                }
            } else {
                std.debug.assert(start[1] == end[1]);
                // horizontal
                const y = start[1];
                for (min[0]..max[0] + 1) |x| {
                    board.set(index(.{ x, y }));
                }
            }
            start = end;
        }
    }
}

pub fn main() !void {
    var sim = if (is_part2) Part2Sim{} else Part1Sim{};

    try parseFile(&sim.blocked);

    if (will_paint_table) {
        // if (is_part2) {
        //     for (0..board_width) |x| {
        //         sim.blocked.set(index(.{ x, floor_position }));
        //     }
        // }
        drawBoard(&sim.blocked);
    }

    sim.drop(drop_point);

    if (will_paint_table) {
        finishBoard();
    }

    std.log.debug("sand count: {}", .{if (is_part2) sim.calcCount() else sim.settled_count});
}
