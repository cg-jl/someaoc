const std = @import("std");
const parse = @import("parse.zig");
const interactive = @import("interactive.zig");
// NOTE: I could do `usingnamespace @import`.. but the LSP brings 'undefined
// symbol' errors even if it can properly point to their definitions.
const constants = @import("constants.zig");

// After playing with different algorithms, I realized that if a grain of sand
// gets to a place, it will eventually settle. The propagation is always the
// same and the condition to propagate is that there was a grain there and, for
// each neighbor to propagate, that it is not blocked by a wall. With this
// premise we can construct just a single lane and update it on each iteration.
//
// Each "lane" is a bitmask saying whether there are sand grains on a
// particular y level or not. Its popcount is the amount of grains on said
// level. To propagate, we OR the lane with its two shifted versions, one to
// the left and one to the right. Then we AND this result with the available
// spaces that the sand can go through in the next y level.

comptime {
    std.debug.assert(constants.board_width <= @bitSizeOf(usize));
}

const Lane = std.meta.Int(.unsigned, constants.board_width);
const ShiftInt = std.math.Log2Int(Lane);

const AvailableTable = [constants.board_height]Lane;

const full_mask = std.math.boolMask(Lane, true);

const full_table: AvailableTable = [_]Lane{full_mask} ** constants.board_height;

fn drawTable(table: *const AvailableTable) void {
    std.debug.print("\x1b[H\x1b[J\x1b[?25l\x1b[38;5;15m", .{});

    for (table) |mask| {
        drawLane(~mask, "@");
        std.debug.print("\n", .{});
    }
}

fn drawLane(lane: Lane, comptime chs: []const u8) void {
    var rem_mask = lane;

    var last_x: usize = 0;

    while (rem_mask != 0) {
        const x = @ctz(rem_mask);
        rem_mask &= rem_mask - 1;
        if (x - last_x >= 2) {
            std.debug.print("\x1b[{}C", .{x - last_x - 1});
        }
        std.debug.print(chs, .{});
        last_x = x;
    }
}

fn maskBit(x: usize) Lane {
    return @as(Lane, 1) << @intCast(ShiftInt, x);
}

const Loader = struct {
    table: *AvailableTable,

    pub fn addHorizontal(l: Loader, y: usize, from_x: usize, to_x: usize) void {
        const keep_lt_from = maskBit(from_x) - 1;
        const keep_gt_to = full_mask << @intCast(ShiftInt, to_x + 1);
        const keep_mask = keep_lt_from | keep_gt_to;
        l.table.*[y] &= keep_mask;
    }

    pub fn addVertical(l: Loader, x: usize, from_y: usize, to_y: usize) void {
        const keep_mask = ~maskBit(x);
        for (l.table[from_y .. to_y + 1]) |*lane| {
            lane.* &= keep_mask;
        }
    }
};

fn drop(available: AvailableTable, pos: constants.Point) usize {
    std.debug.assert(pos[1] == 0);
    var lane: Lane = maskBit(pos[0]);

    var settled_count: usize = 1;
    var right_height: usize = 0;
    var left_height: usize = 0;

    if (constants.will_paint_table) {
        std.debug.print("\x1b[H", .{});
    }

    for (available[1..]) |next_level_mask| {
        if (constants.will_paint_table) {
            drawLane(lane, "\x1b[38;5;11m+");
            std.debug.print("\x1b[B\r", .{});
            std.time.sleep(10 * std.time.ns_per_ms);
        }

        const propagation_targets = lane | (lane << 1) | (lane >> 1);
        const next_lane = propagation_targets & next_level_mask;
        defer lane = next_lane;

        settled_count += @popCount(next_lane);
        right_height += @truncate(u1, lane >> (@bitSizeOf(Lane) - 1));
        left_height += @truncate(u1, lane);
    }

    return settled_count + missedFromBoundaryCut(left_height, right_height);
}

fn missedFromBoundaryCut(left_height: usize, right_height: usize) usize {

    // calculate the missed amount by the simulation.
    // The big triangle has base 2 * height.
    // Its peak is *not* centered over the box, although the box *contains* the
    // center.
    // We have something like this:
    //   /\
    // /|b|\
    //
    // Where 'b' represents the box bounded by the platforms, which is the
    // "unpredictable" part. We're calculating the areas of the triangles on
    // the left and right, which, since the peak is not centered over `b`, is
    // **not** symmetric. A symmetric formula will work on the test input
    // though, because of some weird property regarding the heights and the
    // distances to the center on that input. That made things confusing, so be
    // careful!

    // This (-1) looks arbitrary; what it does is exclude the peak from the
    // distances.
    const dist_start_to_right = constants.board_width - constants.drop_point[0] - 1;
    const dist_start_to_left = constants.drop_point[0];

    const half_base = constants.floor_position;

    const left_triangle_base = half_base - dist_start_to_left;
    const right_triangle_base = half_base - dist_start_to_right;

    // I'm pretty sure that `left_height` and `right_height` *can* be precomputed
    // but I'm only interested in doing enough geometry to cut the triangles.
    // The heights are _very_ cheap to compute.
    const left_triangle_area = left_triangle_base * left_height / 2;
    const right_triangle_area = right_triangle_base * right_height / 2;

    const missed = left_triangle_area + right_triangle_area;

    return missed;
}

fn isBench() bool {
    return std.os.argv.len == 2 and std.mem.eql(u8, std.mem.span(std.os.argv[1]), "bench");
}

const mode = @import("builtin").mode;

pub const std_options = struct {
    pub const log_level = if (mode == .Debug) .debug else .info;
};

pub fn main() !void {
    var table = full_table;
    try parse.loadFile(Loader{ .table = &table }, constants.input_file_name);

    if (isBench()) {
        if (mode == .Debug) {
            std.log.err("Use .ReleaseFast, .ReleaseSmall or .ReleaseSafe to benchmark!", .{});
            return;
        }

        const max_time = 10 * std.time.ns_per_s;
        const sample_count = 10_000;

        var sample_buf: [sample_count]usize = undefined;
        var accum_time: usize = 0;

        var timer = try std.time.Timer.start();
        const num_samples = for (&sample_buf, 0..) |*sample, sample_index| {
            if (accum_time >= max_time) break sample_index;

            timer.reset();
            std.mem.doNotOptimizeAway(drop(table, constants.drop_point));
            sample.* = timer.read();

            accum_time += sample.*;
        } else sample_buf.len;

        const avg: usize = accum_time / num_samples;
        var variance: usize = 0;
        const samples = sample_buf[0..num_samples];

        for (samples) |sample| {
            const dist = if (avg > sample) avg - sample else sample - avg;
            variance += dist * dist;
        }

        const stddev = std.math.sqrt(variance / num_samples);

        const median = samples[num_samples / 2];

        std.log.info(
            "avg: {} median: {} stddev: {} run count: {}",
            .{
                std.fmt.fmtDuration(avg),
                std.fmt.fmtDuration(median),
                std.fmt.fmtDuration(stddev),
                num_samples,
            },
        );
    } else {
        if (std.os.argv.len == 0) {
            if (constants.will_paint_table) {
                drawTable(&table);
            }

            const settled_count = drop(table, constants.drop_point);

            if (constants.will_paint_table) {
                interactive.finishBoard();
            }
            std.log.info("settled count: {}", .{settled_count});
        }
    }
}
