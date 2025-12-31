const std = @import("std");
const Allocator = std.mem.Allocator;
const Order = std.math.Order;
const ArrayList = std.ArrayList;

const State = struct {
    row: i32,
    col: i32,
    dir: i8, // -1 for initial state, 0-3 for directions
    consec: u8,
};

const HeapEntry = struct {
    heat: u32,
    state: State,

    fn compare(_: void, a: HeapEntry, b: HeapEntry) Order {
        return std.math.order(a.heat, b.heat);
    }
};

// Direction deltas: right, down, left, up
const DR = [4]i32{ 0, 1, 0, -1 };
const DC = [4]i32{ 1, 0, -1, 0 };

fn stateIndex(cols: usize, r: i32, c: i32, d: i8, consec: u8) usize {
    // State index: (row, col, dir+1, consec) where dir can be -1 to 3
    // dir+1 gives us 0-4, consec is 0-10
    const dir_offset: usize = @intCast(d + 1);
    return (@as(usize, @intCast(r)) * cols * 5 * 11) +
        (@as(usize, @intCast(c)) * 5 * 11) +
        (dir_offset * 11) +
        @as(usize, consec);
}

fn dijkstra(allocator: Allocator, grid: []const []const u8, min_straight: u8, max_straight: u8) !u32 {
    const rows = grid.len;
    const cols = grid[0].len;
    const rows_i: i32 = @intCast(rows);
    const cols_i: i32 = @intCast(cols);

    // Visited set: indexed by stateIndex
    const visited_size = rows * cols * 5 * 11;
    var visited = try allocator.alloc(bool, visited_size);
    defer allocator.free(visited);
    @memset(visited, false);

    // Priority queue (min-heap)
    var pq = std.PriorityQueue(HeapEntry, void, HeapEntry.compare).init(allocator, {});
    defer pq.deinit();

    // Start with initial state (no direction yet)
    try pq.add(HeapEntry{
        .heat = 0,
        .state = State{ .row = 0, .col = 0, .dir = -1, .consec = 0 },
    });

    while (pq.count() > 0) {
        const entry = pq.remove();
        const heat = entry.heat;
        const r = entry.state.row;
        const c = entry.state.col;
        const d = entry.state.dir;
        const consec = entry.state.consec;

        // Check if we reached the goal
        if (r == rows_i - 1 and c == cols_i - 1) {
            if (min_straight == 0 or consec >= min_straight) {
                return heat;
            }
        }

        // Check if already visited
        const idx = stateIndex(cols, r, c, d, consec);
        if (visited[idx]) {
            continue;
        }
        visited[idx] = true;

        // Try all four directions
        for (0..4) |nd_usize| {
            const nd: i8 = @intCast(nd_usize);

            // Can't reverse direction
            if (d != -1 and nd == @mod(d + 2, 4)) {
                continue;
            }

            const nr = r + DR[nd_usize];
            const nc = c + DC[nd_usize];

            // Bounds check
            if (nr < 0 or nr >= rows_i or nc < 0 or nc >= cols_i) {
                continue;
            }

            // Check consecutive constraints
            var new_consec: u8 = undefined;
            if (nd == d) {
                // Continuing in same direction
                new_consec = consec + 1;
                if (new_consec > max_straight) {
                    continue;
                }
            } else {
                // Turning - must have gone min_straight in previous direction first
                if (d != -1 and consec < min_straight) {
                    continue;
                }
                new_consec = 1;
            }

            const new_heat = heat + grid[@intCast(nr)][@intCast(nc)];
            const new_idx = stateIndex(cols, nr, nc, nd, new_consec);

            if (!visited[new_idx]) {
                try pq.add(HeapEntry{
                    .heat = new_heat,
                    .state = State{ .row = nr, .col = nc, .dir = nd, .consec = new_consec },
                });
            }
        }
    }

    return 0; // No path found
}

fn part1(allocator: Allocator, grid: []const []const u8) !u32 {
    return dijkstra(allocator, grid, 0, 3);
}

fn part2(allocator: Allocator, grid: []const []const u8) !u32 {
    return dijkstra(allocator, grid, 4, 10);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Parse into grid - convert ASCII digits to integer values
    var lines = try ArrayList([]u8).initCapacity(allocator, 200);
    defer {
        for (lines.items) |line| {
            allocator.free(line);
        }
        lines.deinit(allocator);
    }

    var line_iter = std.mem.splitScalar(u8, content, '\n');
    while (line_iter.next()) |line| {
        if (line.len > 0) {
            // Convert ASCII digits to values 0-9
            var row = try allocator.alloc(u8, line.len);
            for (line, 0..) |ch, i| {
                row[i] = ch - '0';
            }
            try lines.append(allocator, row);
        }
    }

    // Create const slice for the grid
    var const_lines = try allocator.alloc([]const u8, lines.items.len);
    defer allocator.free(const_lines);
    for (lines.items, 0..) |line, i| {
        const_lines[i] = line;
    }

    const result1 = try part1(allocator, const_lines);
    const result2 = try part2(allocator, const_lines);

    std.debug.print("Part 1: {d}\n", .{result1});
    std.debug.print("Part 2: {d}\n", .{result2});
}
