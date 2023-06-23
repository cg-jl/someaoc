const std = @import("std");
const builtin = @import("builtin");
const is_real = true; // builtin.mode != .Debug;
const is_part1 = true;

const input_file = if (is_real) "input.txt" else "test.txt";

// grabbed from DynamicBitSetUnmanaged, it was private :(
fn numMasks(bit_length: usize) usize {
    return (bit_length + (@bitSizeOf(NodeSet.MaskInt) - 1)) / @bitSizeOf(NodeSet.MaskInt);
}

pub fn grabMasksConst(bs: *const NodeSet) []const NodeSet.MaskInt {
    return bs.masks[0..numMasks(bs.bit_length)];
}

pub fn grabMasks(bs: *NodeSet) []NodeSet.MaskInt {
    return bs.masks[0..numMasks(bs.bit_length)];
}

/// Min-Heap implementation to store node indices.
const OpenSet = struct {
    available: NodeSet,
    /// The internal buffer of the Open Set.
    /// The upper bound for the buffer size is the node count.
    buffer: []usize,
    len: usize = 0,

    pub fn has(s: *const OpenSet, index: usize) bool {
        return s.available.isSet(index);
    }

    pub fn slice(s: OpenSet) []const usize {
        return s.buffer[0..s.len];
    }

    pub fn init(node_count: usize, alloc: std.mem.Allocator) !OpenSet {
        return .{
            .buffer = try alloc.alloc(usize, node_count),
            .available = try NodeSet.initEmpty(alloc, node_count),
        };
    }

    pub fn deinit(s: OpenSet, alloc: std.mem.Allocator) void {
        alloc.free(s.buffer);
        var av = s.available;
        av.deinit(alloc);
    }

    fn bubbleNodeDown(set: *OpenSet, start_node: usize, f_scores: []const usize) void {
        var node: usize = start_node;
        while (true) {
            var child = 2 * node + 1;
            if (child >= set.len) break;

            // select the smallest child, so that the
            // min property always holds when swapping
            if (child + 1 != set.len and
                f_scores[set.buffer[child + 1]] < f_scores[set.buffer[child]])
            {
                child = child + 1;
            }

            if (f_scores[set.buffer[child]] < f_scores[set.buffer[node]]) {
                std.mem.swap(usize, &set.buffer[child], &set.buffer[node]);
                // since we've just swapped,
                // we have to make sure that it's still
                // less than the rest of the subtree.
                // Hence, we (tail) recurse on the new index.
                node = child;
            } else break;
        }
    }

    pub fn add(set: *OpenSet, value: usize, f_scores: []const usize) void {
        std.debug.assert(set.len < set.buffer.len);
        if (set.available.isSet(value)) return;
        set.available.set(value);

        set.buffer[set.len] = value;
        // 1. Bubble up the value
        var index = set.len;
        set.len += 1;
        while (true) {
            // This is the absolute minimum.
            if (index == 0) break;
            const parent = (index - 1) / 2;
            // The min-heap property holds.
            if (f_scores[set.buffer[parent]] < f_scores[set.buffer[index]]) break;
            std.mem.swap(usize, &set.buffer[parent], &set.buffer[index]);
            index = parent;
        }

        // 2. Bubble down the value
        set.bubbleNodeDown(index, f_scores);
    }

    pub fn remove(set: *OpenSet, f_scores: []const usize) ?usize {
        if (set.len == 0) return null;

        // 1. "swap" the removed value with the last value
        const removed = set.buffer[0];
        set.available.unset(removed);

        set.buffer[0] = set.buffer[set.len - 1];
        set.len -= 1;

        // 2. push down the last value, fixing the min-heap property
        set.bubbleNodeDown(0, f_scores);

        return removed;
    }
};

const NodeSet = std.DynamicBitSetUnmanaged;

pub const ListNeighbors = struct {
    index: usize,
    tbl: Table,
    current: ?enum { left, right, top, bot } = .left,

    pub fn next(l: *ListNeighbors) ?usize {
        while (l.nextAvailable()) |n| {
            if (l.tbl.canJump(l.index, n)) return n;
        }
        return null;
    }

    fn nextAvailable(l: *ListNeighbors) ?usize {
        while (l.current) |curr| {
            switch (curr) {
                .left => {
                    l.current = .right;
                    if (l.index % l.tbl.lane_size != 0) return l.index - 1;
                },
                .right => {
                    l.current = .top;
                    if (l.index % l.tbl.lane_size != l.tbl.lane_size - 1) return l.index + 1;
                },
                .top => {
                    l.current = .bot;
                    if (l.index >= l.tbl.lane_size) return l.index - l.tbl.lane_size;
                },
                .bot => {
                    l.current = null; // checked all.
                    if (l.index < l.tbl.heights.len - l.tbl.lane_size) return l.index + l.tbl.lane_size;
                },
            }
        }
        return null;
    }
};

const Table = struct {
    heights: []u8,
    h: []usize,
    lane_size: usize,

    pub fn format(
        tbl: Table,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        w: anytype,
    ) @TypeOf(w).Error!void {
        try w.writeAll("Table{\n");
        try w.print("\theights: '{s}'\n", .{std.fmt.fmtSliceEscapeLower(tbl.heights)});
        try w.print("\tlane size: {}\n", .{tbl.lane_size});
        try w.print("\tdistances: {any}\n", .{tbl.h});
        return w.writeAll("}");
    }

    fn deinit(tbl: Table, alloc: std.mem.Allocator) void {
        alloc.free(tbl.heights);
        alloc.free(tbl.h);
    }

    fn canJump(tbl: Table, from: usize, to: usize) bool {
        const from_ch = tbl.heights[from];
        const to_ch = tbl.heights[to];

        return from_ch >= to_ch or (to_ch - from_ch == 1);
    }
};

const AStarState = struct {
    open_set: OpenSet,
    came_from: []?usize,
    h: []const usize,
    /// For node `n`, g[n] + h[n]. Potential score if we decide to go through `n`.
    f: []usize,
    /// For node `n`, the cost of the cheapest path from `start` to `n`.
    g: []usize,

    pub fn init(tbl: Table, alloc: std.mem.Allocator) !AStarState {
        const node_count = tbl.h.len;
        var state = .{
            .open_set = try OpenSet.init(node_count, alloc),
            .h = tbl.h,
            .f = try alloc.alloc(usize, node_count),
            .g = try alloc.alloc(usize, node_count),
            .came_from = try alloc.alloc(?usize, node_count),
        };

        // "infinity", for these purposes.
        @memset(state.f, std.math.maxInt(usize));
        @memset(state.g, std.math.maxInt(usize));

        @memset(state.came_from, null);

        return state;
    }

    pub fn deinit(s: AStarState, alloc: std.mem.Allocator) void {
        alloc.free(s.came_from);
        alloc.free(s.g);
        alloc.free(s.f);
        s.open_set.deinit(alloc);
    }

    pub fn pathLength(s: *const AStarState, end: usize) usize {
        return s.f[end];
    }

    inline fn considerNeighbor(
        s: *AStarState,
        tentative_g: usize,
        current: usize,
        neighbor: usize,
    ) void {
        // This path to neighbor is better than any previous one. Record it!
        s.g[neighbor] = tentative_g;
        s.f[neighbor] = tentative_g + s.h[neighbor];
        s.came_from[neighbor] = current;

        s.open_set.add(neighbor, s.f);
    }

    // Wikipedia: https://en.wikipedia.org/wiki/A*_search_algorithm
    pub fn solve(s: *AStarState, start: usize, end: usize, tbl: Table) error{Unsolvable}!void {
        s.open_set.len = 0;

        s.g[start] = 0;
        s.f[start] = s.h[start];

        // 'S' cannot jump if it's higher (as a `u8`)!
        // So we will visit its neighbors without going through
        // the canJump filter.

        {
            var start_neighbors = ListNeighbors{ .tbl = tbl, .index = start };
            while (start_neighbors.nextAvailable()) |neighbor| {
                s.considerNeighbor(1, start, neighbor);
            }
        }

        while (s.open_set.remove(s.f)) |current| {
            if (current == end) return;

            var neighbors = ListNeighbors{
                .tbl = tbl,
                .index = current,
            };

            // d(current,neighbor) is the weight of the edge from current to neighbor
            // tentative_gScore is the distance from start to the neighbor through current
            // d(current,neighbor) is always 1.
            const tentative_g = s.g[current] + 1;
            while (neighbors.next()) |neighbor| {
                if (tentative_g <= s.g[neighbor]) {
                    s.considerNeighbor(tentative_g, current, neighbor);
                }
            }
        } else return error.Unsolvable;
    }
};

const BufReader = std.io.BufferedReader(4096, std.fs.File.Reader);

pub fn parseTable(r: BufReader.Reader, alloc: std.mem.Allocator) !struct { Table, usize, usize } {
    const buf = try r.readAllAlloc(alloc, std.math.maxInt(usize));
    defer alloc.free(buf);

    const lane_size = std.mem.indexOfScalar(u8, buf, '\n').?;

    const lane_count = buf.len / lane_size;
    const table_size = lane_size * lane_count;

    const heights = try alloc.alloc(u8, table_size);

    for (0..lane_count) |row_idx| {
        const removed_newline_count = row_idx;

        const target_row = heights[row_idx * lane_size ..][0..lane_size];

        const source_row = buf[row_idx * lane_size + removed_newline_count ..][0..lane_size];

        @memcpy(target_row, source_row);
    }

    const start = std.mem.indexOfScalar(u8, heights, 'S').?;
    const end = std.mem.indexOfScalar(u8, heights, 'E').?;

    heights[start] = 'a';
    heights[end] = 'z';

    const end_row = end / lane_size;
    const end_col = end % lane_size;

    const h = try alloc.alloc(usize, table_size);

    // fill the manhattan distance table.

    for (0..end_row) |row_idx| {
        const row = h[row_idx * lane_size ..][0..lane_size];
        // invariant: all of the rows in this loop are < end_row
        const row_distance = end_row - row_idx;

        for (0..end_col) |col| {
            const col_distance = end_col - col;
            row[col] = col_distance + row_distance;
        }

        for (end_col..lane_size) |col| {
            const col_distance = col - end_col;
            row[col] = col_distance + row_distance;
        }
    }

    for (end_row..lane_count) |row_idx| {
        const row = h[row_idx * lane_size ..][0..lane_size];
        // invariant: all of the rows in this loop are >= end_row
        const row_distance = row_idx - end_row;

        for (0..end_col) |col| {
            const col_distance = end_col - col;
            row[col] = col_distance + row_distance;
        }

        for (end_col..lane_size) |col| {
            const col_distance = col - end_col;
            row[col] = col_distance + row_distance;
        }
    }

    return .{
        Table{ .heights = heights, .lane_size = lane_size, .h = h },
        start,
        end,
    };
}

pub fn main() !void {
    const file = try std.fs.cwd().openFile(input_file, .{});
    defer file.close();
    var bufr = std.io.bufferedReader(file.reader());

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const res = try parseTable(bufr.reader(), gpa.allocator());
    const table: Table = res.@"0";
    const start: usize = res.@"1";
    const end: usize = res.@"2";
    defer table.deinit(gpa.allocator());

    var astar = try AStarState.init(table, gpa.allocator());
    defer astar.deinit(gpa.allocator());

    // hide cursor
    std.debug.print("\x1b[?25l", .{});
    // show cursor
    defer std.debug.print("\x1b[?25h", .{});

    astar.solve(start, end, table) catch {
        std.log.err("found the maze to be unsolvable!", .{});
        return;
    };

    std.debug.print("path length: {}\n", .{astar.pathLength(end)});
}
