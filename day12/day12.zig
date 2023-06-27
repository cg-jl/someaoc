const std = @import("std");
const builtin = @import("builtin");
const is_real = true; // builtin.mode != .Debug;
const is_part1 = false;

const input_file = if (is_real) "input.txt" else "test.txt";

/// Whether to output VTE codes to the terminal to draw the map with the ongoing
/// open set and the chosen path. Note that the 'animition' is not smooth and
/// will appear flickery.
const show_astar = true;

const vte_colors = .{
    .unvisited = 0,
    .visited_and_dismissed = 8,
    .current_path = 42,
    .target = 69,
    .open_set = 223,
    .final_path = 2,
};

/// Sleep between frames this value. Only used when `show_astar` is
/// `true`.
const ensure_ns_between_frames = if (is_part1)
    std.time.ns_per_ms / 4
else
    std.time.ns_per_ms;

const NodeSet = std.DynamicBitSetUnmanaged;

pub const ListNeighbors = struct {
    index: usize,
    tbl: MazeData,
    current: ?enum { left, right, top, bot } = .left,

    pub fn next(l: *ListNeighbors) ?usize {
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

const MazeData = struct {
    heights: []u8,
    lane_size: usize,

    pub fn format(
        tbl: MazeData,
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

    fn deinit(tbl: MazeData, alloc: std.mem.Allocator) void {
        alloc.free(tbl.heights);
    }

    inline fn canJump(tbl: MazeData, from: usize, to: usize) bool {
        const from_ch = tbl.heights[from];
        const to_ch = tbl.heights[to];

        return from_ch >= to_ch or (to_ch - from_ch == 1);
    }

    inline fn canJumpBackwards(tbl: MazeData, from: usize, to: usize) bool {
        return tbl.canJump(to, from);
    }
};

/// Collection of state management that is unique to the drawing steps for
/// drawing the A* animation.
const AStarDrawState = struct {
    io: std.fs.File.Writer,
    /// Map to track paths. The solutions to this problem don't
    /// require tracking the path, but the animation does.
    came_from: []?usize,
    maze: MazeData,
    last_checked: usize,

    pub fn init(maze: MazeData, start_node: usize, alloc: std.mem.Allocator) !AStarDrawState {
        // I know, i'm wasting space here. But this removes memory management
        // between frames, which makes the draw loop easier to understand.
        const came_from = try alloc.alloc(?usize, maze.heights.len);
        @memset(came_from, null);

        return .{
            .io = std.io.getStdOut().writer(),
            .came_from = came_from,
            .maze = maze,
            .last_checked = start_node,
        };
    }

    pub fn deinit(state: AStarDrawState, alloc: std.mem.Allocator) void {
        return alloc.free(state.came_from);
    }

    pub fn prepareAnimation(state: AStarDrawState) void {
        // - move the cursor to top left
        // - clear the screen
        // - hide the cursor
        return state.io.writeAll(
            "\x1b[H\x1b[J\x1b[?25l",
        ) catch {};
    }

    /// cleanup the cursor state and the cursor so any prints after this
    /// don't write in a weird place.
    pub fn endAnimation(state: AStarDrawState) void {
        // - move the cursor to the last row
        // - show the cursor
        // - reset coloring
        // - move the cursor to the next line
        return state.io.print(
            "\x1b[{};{}H\x1b[?25h\x1b[m\n",
            .{ state.maze.heights.len / state.maze.lane_size, state.maze.lane_size },
        ) catch {};
    }

    pub inline fn drawChAtNode(state: *AStarDrawState, index: usize, color: u8, ch: u8) void {
        return state.io.print(
            "\x1b[{};{}H\x1b[38;5;{}m{c}",
            .{
                index / state.maze.lane_size + 1,
                index % state.maze.lane_size + 1,
                color,
                ch,
            },
        ) catch {};
    }

    pub inline fn drawNode(state: *AStarDrawState, index: usize, color: u8) void {
        return state.drawChAtNode(index, color, state.maze.heights[index]);
    }

    pub fn updateCurrentPath(state: *AStarDrawState, new_current: usize) void {
        // "remove" the current path by drawing it on white.
        state.drawPathLeadingTo(state.last_checked, vte_colors.visited_and_dismissed);
        state.last_checked = new_current;
        return state.drawPathLeadingTo(new_current, vte_colors.current_path);
    }

    pub fn drawPathLeadingTo(state: *AStarDrawState, node: usize, color: u8) void {
        var curr = node;

        while (state.came_from[curr]) |old| : (curr = old) {
            state.drawNode(old, color);
        }
    }
};

fn astar(
    comptime validateNeighbor: fn (MazeData, usize, usize) callconv(.Inline) bool,
    hctx: anytype,
    win_ctx: anytype,
    start_node: usize,
    table: MazeData,
    alloc: std.mem.Allocator,
) error{ OutOfMemory, Unsolvable }!usize {
    if (win_ctx.didWin(start_node)) return 0;

    // Wikipedia: https://en.wikipedia.org/wiki/A*_search_algorithm
    const potential_scores: []usize = try alloc.alloc(usize, table.heights.len);
    defer alloc.free(potential_scores);

    const cheapest_path_cost: []usize = try alloc.alloc(usize, table.heights.len);
    defer alloc.free(cheapest_path_cost);

    var in_open_set = try NodeSet.initEmpty(alloc, table.heights.len);
    defer in_open_set.deinit(alloc);

    const OpenSet = std.PriorityQueue(usize, []const usize, struct {
        fn lessThan(ctx: []const usize, a: usize, b: usize) std.math.Order {
            return std.math.order(ctx[a], ctx[b]);
        }
    }.lessThan);

    var open_set = OpenSet.init(alloc, potential_scores);
    defer open_set.deinit();

    // "infinity" for these purposes.
    @memset(potential_scores, std.math.maxInt(usize));
    @memset(cheapest_path_cost, std.math.maxInt(usize));

    cheapest_path_cost[start_node] = 0;
    potential_scores[start_node] = hctx.heuristic(start_node);

    try open_set.add(start_node);

    var draw_state = if (show_astar)
        try AStarDrawState.init(table, start_node, alloc)
    else {};

    defer if (show_astar) draw_state.deinit(alloc);

    if (show_astar) draw_state.prepareAnimation();

    // draw the start node in green (open set)
    if (show_astar) {
        draw_state.drawNode(start_node, 2);

        // draw everything except the start node in grey
        for (0..table.heights.len) |node| {
            if (node == start_node) continue;
            draw_state.drawNode(node, vte_colors.unvisited);
        }

        if (is_part1) {
            draw_state.drawNode(hctx.end, vte_colors.target);
        } else {
            for (hctx.positions) |pos| {
                draw_state.drawNode(pos, vte_colors.target);
            }
        }
    }

    while (open_set.removeOrNull()) |current| {
        //std.io.getStdIn().reader().skipUntilDelimiterOrEof('\n') catch {};
        if (show_astar and ensure_ns_between_frames != 0) std.time.sleep(ensure_ns_between_frames);
        if (win_ctx.didWin(current)) {
            if (show_astar) {
                for (0..table.heights.len) |node| {
                    draw_state.drawNode(node, 0);
                }

                draw_state.drawChAtNode(current, vte_colors.target, if (is_part1) 'E' else 'S');

                draw_state.drawPathLeadingTo(current, 2);

                draw_state.drawChAtNode(start_node, vte_colors.target, if (is_part1) 'S' else 'E');

                draw_state.endAnimation();
            }

            return cheapest_path_cost[current];
        }

        if (show_astar) {
            draw_state.updateCurrentPath(current);
        }

        in_open_set.unset(current);

        var neighbors = ListNeighbors{ .tbl = table, .index = current };

        // d(current,neighbor) is the weight of the edge from current to neighbor
        // tentative_path_cost is the distance from start to the neighbor through current
        // d(current,neighbor) is always 1.
        const tentative_path_cost = cheapest_path_cost[current] + 1;

        while (neighbors.next()) |neighbor| {
            if (!validateNeighbor(table, current, neighbor)) continue;
            if (tentative_path_cost <= cheapest_path_cost[neighbor]) {
                // This path to neighbor is better than any previous one. Record it!
                cheapest_path_cost[neighbor] = tentative_path_cost;
                potential_scores[neighbor] = tentative_path_cost + hctx.heuristic(neighbor);
                if (show_astar) {
                    draw_state.came_from[neighbor] = current;
                }
                if (!in_open_set.isSet(neighbor)) {
                    in_open_set.set(neighbor);
                    try open_set.add(neighbor);
                    if (show_astar) draw_state.drawNode(neighbor, vte_colors.open_set);
                }
            }
        }
    }

    if (show_astar) draw_state.endAnimation();

    return error.Unsolvable;
}

const BufReader = std.io.BufferedReader(4096, std.fs.File.Reader);

pub fn parseTable(r: BufReader.Reader, alloc: std.mem.Allocator) !struct { MazeData, usize, usize } {
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

    // TODO: make this part of `part1`, and make `part2` bake a similar table
    // with a QuadTree (nearest 'a').

    return .{
        MazeData{ .heights = heights, .lane_size = lane_size },
        start,
        end,
    };
}

pub fn bakePositions(table: MazeData, alloc: std.mem.Allocator) !std.ArrayList(usize) {
    std.log.debug("baking positions", .{});
    var positions = std.ArrayList(usize).init(alloc);
    errdefer positions.deinit();

    for (table.heights, 0..) |height, i| {
        if (height == 'a') {

            // check if it has at least one neighbor that it can jump to
            // and is not an 'a' itself (since it would end earlier)
            var neighbors = ListNeighbors{ .index = i, .tbl = table };

            while (neighbors.next()) |n| {
                if (table.canJump(i, n) and table.heights[n] != 'a') {
                    try positions.append(i);
                    break;
                }
            }
        }
    }
    std.log.debug("baked positions", .{});

    return positions;
}

fn absDistance(a: usize, b: usize) usize {
    return if (a > b) a - b else b - a;
}

fn manhattanDistance(a: usize, b: usize, lane_size: usize) usize {
    return absDistance(a % lane_size, b % lane_size) + absDistance(a / lane_size, b / lane_size);
}

// this is supposed to be a... BST in 2 dimensions.
// so a merging of two BSTs:
// - horizontal BST: left means to the left, and right means to the right
// - vertical BST: left means up, right means bottom
// We always start with the center of the current dimensions and then divide (x dim or y dim)
// by 2 and add it/subtract it from the center point to find the next
//

pub fn main() !void {
    const file = try std.fs.cwd().openFile(input_file, .{});
    defer file.close();
    var bufr = std.io.bufferedReader(file.reader());

    var gpa = std.heap.GeneralPurposeAllocator(.{ .enable_memory_limit = true }){
        .requested_memory_limit = 2 << 20, // 2 MiB
    };
    defer _ = gpa.deinit();

    const res = try parseTable(bufr.reader(), gpa.allocator());
    const table: MazeData = res.@"0";
    const end: usize = res.@"2";
    defer table.deinit(gpa.allocator());

    const start: usize = res.@"1";

    if (is_part1) {
        const HCtx = struct {
            end: usize,
            lane_size: usize,

            pub inline fn heuristic(ctx: @This(), node: usize) usize {
                return manhattanDistance(node, ctx.end, ctx.lane_size);
            }
        };

        const WinCtx = struct {
            end: usize,

            pub inline fn didWin(ctx: @This(), node: usize) bool {
                return node == ctx.end;
            }
        };

        const result = try astar(
            MazeData.canJump,
            HCtx{ .end = end, .lane_size = table.lane_size },
            WinCtx{ .end = end },
            start,
            table,
            gpa.allocator(),
        );
        std.debug.print("path length: {}\n", .{result});
    } else {
        const positions = try bakePositions(table, gpa.allocator());
        defer positions.deinit();
        const HCtx = struct {
            positions: []const usize,
            lane_size: usize,

            /// Returns the minimum manhattan distance to any node.
            pub fn heuristic(ctx: @This(), node: usize) usize {
                // no manhattan distance can be >= 2 * lane_size, since that would
                // imply a (lane_size + 1)^2 area. Since `lane_size` is derived
                // from the input area, this can never hapen. This is infinity
                // without overflowing.
                var min: usize = 2 * ctx.lane_size;

                for (ctx.positions) |a_pos| {
                    min = @min(min, manhattanDistance(node, a_pos, ctx.lane_size));
                }

                return min;
            }
        };

        const WinCtx = struct {
            heights: []const u8,

            pub inline fn didWin(ctx: @This(), node: usize) bool {
                return ctx.heights[node] == 'a';
            }
        };

        const result = try astar(
            MazeData.canJumpBackwards,
            HCtx{ .positions = positions.items, .lane_size = table.lane_size },
            WinCtx{ .heights = table.heights },
            end,
            table,
            gpa.allocator(),
        );

        std.debug.print("minimum path length {}\n", .{result});
    }
}
