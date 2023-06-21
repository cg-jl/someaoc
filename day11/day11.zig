const std = @import("std");
const common = @import("common.zig");

const is_part1 = false;
const is_real = true;

const round_count = if (is_part1) 20 else 10_000;

const P1State = common.State(.part1);
const P2State = common.State(.part2);

fn part1(s: *P1State) void {
    for (0..20) |_| {
        for (s.metas.items, s.number_queues, s.counts) |meta, *q, *count| {
            while (q.readItem()) |next| {
                count.* += 1;
                const applied = switch (meta.op) {
                    .mul => |x| next * x / 3,
                    .add => |x| (next + x) / 3,
                    .square => next * next / 3,
                };

                const next_index = if (applied % meta.modulo == 0)
                    meta.if_true
                else
                    meta.if_false;

                s.number_queues[next_index].writeItemAssumeCapacity(applied);
            }
        }
    }
}

fn part2(s: *P2State) void {
    const monkey_count = s.metas.items.len;
    for (0..10_000) |_| {
        // ops / modulos could be their own arrays, makes for loops easier
        for (
            s.metas.items,
            s.number_queues,
            s.counts,
            0..,
        ) |meta, *q, *count, monkey_index| {
            while (q.readItem()) |num_index| {
                count.* += 1;
                const mod_numbers = s.mod_numbers[num_index * monkey_count ..][0..monkey_count];

                // NOTE: this can all be reduced, but I'm not a mathematicion.
                switch (meta.op) {
                    // proof:
                    // x === b (mod p) <=> x = pk + b
                    // x + a = pk + b + a => x + a === b + a (mod p)
                    .add => |a| for (mod_numbers, s.metas.items) |*m, other| {
                        m.* = (m.* + a) % other.modulo;
                    },

                    // proof:
                    // x === b (mod p) <=> x = pk + b
                    // x * a = pka + ba, since ka is an integer:
                    // => xa === ba (mod p)
                    .mul => |a| for (mod_numbers, s.metas.items) |*m, other| {
                        m.* = (m.* * a) % other.modulo;
                    },

                    // proof:
                    // x === b (mod p) <=> x = pk + b
                    // x^2 = p^2k^2 + 2pkb + b^2 = p(pk^2 + 2kb) + b^2
                    // Since pk^2 + 2kb is an integer:
                    // => x^2 === b^2 (mod p)
                    .square => for (mod_numbers, s.metas.items) |*m, other| {
                        m.* = (m.* * m.*) % other.modulo;
                    },
                }

                const div_test = mod_numbers[monkey_index];

                const index = if (div_test == 0) meta.if_true else meta.if_false;

                s.number_queues[index].writeItemAssumeCapacity(num_index);
            }
        }
    }
}

const filename = if (is_real) "input.txt" else "test.txt";

pub fn main() !void {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var bufr = std.io.bufferedReader(file.reader());

    var lines = common.LineReader{ .inner = bufr.reader() };

    var gpa = std.heap.GeneralPurposeAllocator(.{ .enable_memory_limit = true }){};
    defer _ = gpa.deinit();

    const result = if (is_part1) part1: {
        var state = try P1State.fromLines(&lines, gpa.allocator());
        defer state.deinit(gpa.allocator());
        part1(&state);
        break :part1 state.monkeynessScore();
    } else part2: {
        var state = try P2State.fromLines(&lines, gpa.allocator());
        defer state.deinit(gpa.allocator());
        part2(&state);
        break :part2 state.monkeynessScore();
    };

    std.log.info("Monkeyness score: {}", .{result});
}
