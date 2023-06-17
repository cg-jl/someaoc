const std = @import("std");

const is_test_file = false;

const side_length = if (is_test_file) 5 else 99;
const file_name = if (is_test_file) "day8/test.txt" else "day8/input.txt";

const trees_per_line = side_length;
const line_count = side_length;
const tree_count = trees_per_line * line_count;

/// Gives back an unsigned integer with the minimum number of bits
/// that can represent the maximum value.
fn MaxUInt(comptime maxv: usize) type {
    const log2 = std.math.log2_int_ceil(usize, maxv);
    const is_pow2 = std.math.isPowerOfTwo(maxv);
    return @Type(.{ .Int = .{
        .signedness = .unsigned,
        .bits = if (is_pow2) log2 + 1 else log2,
    } });
}

const ScoreInt = MaxUInt(std.math.powi(usize, side_length / 2, 4) catch unreachable);
const Distance = MaxUInt(side_length);

const TreeTable = [side_length][side_length]u8;
const ScoreTable = [side_length][side_length]usize;

const BitTable = std.DynamicBitSetUnmanaged;
// grabbed from ^^^
fn numMasks(bit_length: usize) usize {
    return (bit_length + (@bitSizeOf(BitTable.MaskInt) - 1)) / @bitSizeOf(BitTable.MaskInt);
}
fn setTable(tb: *BitTable, v: bool) void {
    const mask_count = numMasks(tree_count);
    @memset(tb.masks[0 .. mask_count - 1], std.math.boolMask(BitTable.MaskInt, v));
    const leftover_bits = tree_count & @as(comptime_int, std.math.boolMask(@Type(.{ .Int = .{
        .signedness = .unsigned,
        .bits = @bitSizeOf(BitTable.ShiftInt) - 1,
    } }), true));
    const final_mask = std.math.boolMask(@Type(.{ .Int = .{
        .signedness = .unsigned,
        .bits = leftover_bits,
    } }), true);
    tb.masks[mask_count - 1] = final_mask;
}

fn TableFmter(comptime T: type, comptime n: usize) type {
    if (n == 0) @compileLog("Trying to print a zero size array");
    return struct {
        to_print: *const [n][n]T,

        pub fn format(
            ctx: @This(),
            comptime fmt: []const u8,
            opts: std.fmt.FormatOptions,
            w: anytype,
        ) @TypeOf(w).Error!void {
            for (ctx.to_print) |line| {
                for (line[0 .. n - 1]) |elem| {
                    try std.fmt.formatType(elem, fmt, opts, w, std.options.fmt_max_depth);
                    try w.writeAll(" ");
                }
                try std.fmt.formatType(line[n - 1], fmt, opts, w, std.options.fmt_max_depth);
                try w.writeAll("\n");
            }
        }
    };
}

fn FullTableFmter(comptime T: type) type {
    return TableFmter(T, side_length);
}

fn parseTrees(comptime file: []const u8) ![trees_per_line][line_count]u8 {
    const inp = try std.fs.cwd().openFile(file, .{});
    const reader = inp.reader();
    var line: [trees_per_line + 1]u8 = undefined;
    _ = line;
    var result: [trees_per_line][line_count]u8 = undefined;

    for (0..line_count) |line_idx| {
        _ = try reader.read(&result[line_idx]);
        // consume '\n'
        _ = try reader.readByte();
        for (&result[line_idx]) |*x| x.* -= '0';
    }

    return result;
}

/// A map from height to the latest distance from the beginning that it was
/// recorded from.
///
/// Entries that haven't been filled yet should be zero, so that the
/// distance that is computed from each check results in the original distance
/// of the checked tree. All entries should be filled from minimum dsitance to
/// the beginning to latest distance to the beginning
const Distances = [10]usize;
const zero_distances = [_]usize{0} ** 10;

fn findViewDistance(distances: *const Distances, height: u8, edge_dist: usize) usize {
    // find the minimum of the recorded distances that are >= height.
    var min = edge_dist;
    for (distances[height..]) |other_dist_from_begin| {
        min = @min(min, edge_dist - other_dist_from_begin);
    }
    return min;
}

fn accumScoresFromRight(trees: *const TreeTable, scores: *ScoreTable) void {
    for (trees, scores) |line, *score_line| {
        var last_seen = zero_distances;
        var col = line.len - 1;
        while (col != 0) : (col -= 1) {
            const dist_to_edge = line.len - col - 1;
            const height = line[col];
            score_line[col] *= findViewDistance(&last_seen, height, dist_to_edge);
            last_seen[height] = dist_to_edge;
        }
    }
}

fn accumScoresFromLeft(trees: *const TreeTable, scores: *ScoreTable) void {
    for (trees, scores) |line, *score_line| {
        var last_seen = zero_distances;
        for (line, score_line, 0..) |height, *score, dist_to_edge| {
            score.* *= findViewDistance(&last_seen, height, dist_to_edge);
            last_seen[height] = dist_to_edge;
        }
    }
}

fn accumScoresFromTop(trees: *const TreeTable, scores: *ScoreTable) void {
    var last_seen_per_col = [_]Distances{zero_distances} ** trees_per_line;
    for (trees, scores, 0..) |line, *score_line, dist_to_edge| {
        for (line, score_line, &last_seen_per_col) |height, *score, *last_seen| {
            score.* *= findViewDistance(last_seen, height, dist_to_edge);
            last_seen[height] = dist_to_edge;
        }
    }
}

fn accumScoresFromBottom(trees: *const TreeTable, scores: *ScoreTable) void {
    var last_seen_per_col = [_]Distances{zero_distances} ** trees_per_line;
    var row = trees.len - 1;
    // NOTE: this misses both edges on purpose.
    while (row != 0) : (row -= 1) {
        const line = trees[row];
        const dist_to_edge = trees.len - row - 1;
        const score_line = &scores[row];
        for (line, score_line, &last_seen_per_col) |height, *score, *last_seen| {
            score.* *= findViewDistance(last_seen, height, dist_to_edge);
            last_seen[height] = dist_to_edge;
        }
    }
}

fn lookUp(
    trees: *const TreeTable,
    height: u8,
    row: usize,
    col: usize,
) usize {
    for (1..row) |distance| {
        if (trees[row - distance][col] >= height) return distance;
    }
    return row;
}

fn lookDown(
    trees: *const TreeTable,
    height: u8,
    row: usize,
    col: usize,
) usize {
    for (trees[row + 1 ..], 1..) |line, distance| {
        if (line[col] >= height) return distance;
    }
    return trees.len - row - 1;
}

fn lookLeft(
    line: *const [trees_per_line]u8,
    height: u8,
    col: usize,
) usize {
    for (1..col) |distance| {
        if (line[col - distance] >= height) return distance;
    }
    return col;
}

fn lookRight(
    line: *const [trees_per_line]u8,
    height: u8,
    col: usize,
) usize {
    for (line[col + 1 ..], 1..) |other, distance| {
        if (other >= height) return distance;
    }
    return line.len - col - 1;
}

fn scenicScore(
    trees: *const TreeTable,
    height: u8,
    row: usize,
    col: usize,
) usize {
    const top = lookUp(trees, height, row, col);
    const bot = lookDown(trees, height, row, col);
    const left = lookLeft(&trees[row], height, col);
    const right = lookRight(&trees[row], height, col);
    return top * bot * left * right;
}

fn partTwoNaive(trees: *const TreeTable) usize {
    var max_score: usize = 0;
    for (trees[1 .. trees.len - 1], 1..) |line, row| {
        for (line[1 .. line.len - 1], 1..) |height, col| {
            const score = scenicScore(trees, height, row, col);
            max_score = @max(max_score, score);
        }
    }
    return max_score;
}

fn partTwoAccum(trees: *const TreeTable) usize {
    var scores: ScoreTable = undefined;
    for (&scores) |*line| @memset(line, 1);

    accumScoresFromLeft(trees, &scores);
    accumScoresFromRight(trees, &scores);
    accumScoresFromTop(trees, &scores);
    accumScoresFromBottom(trees, &scores);

    var max: usize = 0;
    for (scores) |line| {
        for (line) |sc| max = @max(max, sc);
    }
    return max;
}

const partTwo = partTwoAccum;

fn partOne(trees: [trees_per_line][line_count]u8, ally: std.mem.Allocator) !usize {
    _ = ScoreInt;
    var seen_tmp = try std.DynamicBitSetUnmanaged.initFull(ally, tree_count);
    var seen_final = try std.DynamicBitSetUnmanaged.initEmpty(ally, tree_count);
    defer seen_tmp.deinit(ally);
    defer seen_final.deinit(ally);

    // left to right
    for (0..line_count) |line| {
        var max_height = trees[line][0];
        const start_index = line * trees_per_line;
        for (trees[line][1..], 1..) |curr_height, column| {
            if (curr_height > max_height) {
                max_height = curr_height;
            } else seen_tmp.unset(column + start_index);
        }
    }

    seen_final.setUnion(seen_tmp);

    setTable(&seen_tmp, true);

    // right to left
    for (0..line_count) |line| {
        var max_height = trees[line][trees[line].len - 1];
        const start_index = line * trees_per_line;
        var column_plus_one: usize = trees[line].len - 1;
        while (column_plus_one != 0) : (column_plus_one -= 1) {
            const column = column_plus_one - 1;
            const height = trees[line][column];
            const tree_index = column + start_index;
            if (height > max_height) {
                max_height = height;
            } else seen_tmp.unset(tree_index);
        }
    }

    seen_final.setUnion(seen_tmp);
    setTable(&seen_tmp, true);

    // top to bottom
    var max_heights: [line_count]u8 = trees[0];
    for (trees[1..], 1..) |line, line_idx| {
        const start_index = line_idx * trees_per_line;
        for (line, &max_heights, 0..) |height, *max_height, column| {
            const tree_index = column + start_index;
            if (height > max_height.*) {
                max_height.* = height;
            } else seen_tmp.unset(tree_index);
        }
    }

    seen_final.setUnion(seen_tmp);
    setTable(&seen_tmp, true);

    // bottom to top
    max_heights = trees[trees.len - 1];
    var line_plus_one = trees.len - 1;
    while (line_plus_one != 0) : (line_plus_one -= 1) {
        const line_idx = line_plus_one - 1;
        const line = trees[line_idx];
        const start_index = line_idx * trees_per_line;
        for (line, &max_heights, 0..) |height, *max_height, column| {
            const tree_index = column + start_index;
            if (height > max_height.*) {
                max_height.* = height;
            } else seen_tmp.unset(tree_index);
        }
    }

    seen_final.setUnion(seen_tmp);

    return seen_final.count();
}

test "partOne" {
    const trees = try parseTrees("day8/input.txt");
    try std.testing.expectEqual(try partOne(trees, std.testing.allocator), 1859);
}

test "partTwo.naive" {
    const trees = try parseTrees("day8/input.txt");
    try std.testing.expectEqual(partTwoNaive(&trees), 332640);
}

test "partTwo.accum" {
    const trees = try parseTrees("day8/input.txt");
    try std.testing.expectEqual(partTwoAccum(&trees), 332640);
}

pub fn main() !void {
    const trees = try parseTrees(file_name);
    var alloc = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer alloc.deinit();

    const part1 = try partOne(trees, alloc.allocator());
    const part2 = partTwo(&trees);
    std.log.info("Part 1: {}", .{part1});
    std.log.info("Part 2: {}", .{part2});
}
