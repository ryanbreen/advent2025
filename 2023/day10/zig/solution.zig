const std = @import("std");

/// Represents a movement direction as row/column deltas.
const Direction = struct {
    dr: i32,
    dc: i32,
};

/// Cardinal direction constants for pipe navigation.
const north = Direction{ .dr = -1, .dc = 0 };
const south = Direction{ .dr = 1, .dc = 0 };
const west = Direction{ .dr = 0, .dc = -1 };
const east = Direction{ .dr = 0, .dc = 1 };

/// Returns the two directions a pipe character connects, or null for non-pipe characters.
fn getPipeConnections(ch: u8) ?[2]Direction {
    return switch (ch) {
        '|' => .{ north, south },
        '-' => .{ west, east },
        'L' => .{ north, east },
        'J' => .{ north, west },
        '7' => .{ south, west },
        'F' => .{ south, east },
        else => null,
    };
}

/// Checks if a pipe character has a north-facing connection (used for ray casting).
fn hasNorthConnection(ch: u8) bool {
    return switch (ch) {
        '|', 'L', 'J' => true,
        else => false,
    };
}

/// A position in the grid represented as row and column indices.
const Pos = struct {
    row: usize,
    col: usize,
};

/// Attempts to move from a position in the given direction, returning null if out of bounds.
fn moveInBounds(pos: Pos, dir: Direction, rows: usize, cols: usize) ?Pos {
    const nr = @as(i64, @intCast(pos.row)) + dir.dr;
    const nc = @as(i64, @intCast(pos.col)) + dir.dc;
    if (nr >= 0 and nr < rows and nc >= 0 and nc < cols) {
        return Pos{ .row = @intCast(nr), .col = @intCast(nc) };
    }
    return null;
}

/// Checks if a position connects back to an origin position through the pipe at that position.
fn connectsBackTo(grid: []const []const u8, neighbor: Pos, origin: Pos) bool {
    const adj_ch = grid[neighbor.row][neighbor.col];
    if (getPipeConnections(adj_ch)) |conns| {
        for (conns) |conn| {
            const back_r = @as(i64, @intCast(neighbor.row)) + conn.dr;
            const back_c = @as(i64, @intCast(neighbor.col)) + conn.dc;
            if (back_r == @as(i64, @intCast(origin.row)) and back_c == @as(i64, @intCast(origin.col))) {
                return true;
            }
        }
    }
    return false;
}

/// Finds the starting position 'S' in the grid.
fn findStart(grid: []const []const u8) ?Pos {
    for (grid, 0..) |row, r| {
        for (row, 0..) |ch, c| {
            if (ch == 'S') {
                return Pos{ .row = r, .col = c };
            }
        }
    }
    return null;
}

/// Gets valid neighboring positions connected to the given position via pipes.
fn getNeighbors(grid: []const []const u8, pos: Pos, neighbors: *[4]Pos) usize {
    const rows = grid.len;
    const cols = grid[0].len;
    const ch = grid[pos.row][pos.col];
    var count: usize = 0;

    if (ch == 'S') {
        // S can connect to any adjacent pipe that connects back to it
        const directions = [_]Direction{ north, south, west, east };
        for (directions) |dir| {
            if (moveInBounds(pos, dir, rows, cols)) |neighbor| {
                if (connectsBackTo(grid, neighbor, pos)) {
                    neighbors[count] = neighbor;
                    count += 1;
                }
            }
        }
    } else if (getPipeConnections(ch)) |conns| {
        for (conns) |dir| {
            if (moveInBounds(pos, dir, rows, cols)) |neighbor| {
                neighbors[count] = neighbor;
                count += 1;
            }
        }
    }

    return count;
}

/// HashMap storing positions (as keys) mapped to their distances from start.
const PosMap = std.AutoHashMap(u64, usize);

/// Converts a position to a unique key for use in the position map.
fn posToKey(pos: Pos, cols: usize) u64 {
    return @as(u64, pos.row) * @as(u64, cols) + @as(u64, pos.col);
}

/// Performs BFS to find all positions in the loop and their distances from start.
fn findLoop(allocator: std.mem.Allocator, grid: []const []const u8, start: Pos) !PosMap {
    const cols = grid[0].len;
    var distances = PosMap.init(allocator);

    var queue: std.ArrayListUnmanaged(Pos) = .empty;
    defer queue.deinit(allocator);

    try distances.put(posToKey(start, cols), 0);
    try queue.append(allocator, start);

    var head: usize = 0;
    while (head < queue.items.len) {
        const pos = queue.items[head];
        head += 1;

        const current_dist = distances.get(posToKey(pos, cols)).?;

        var neighbors: [4]Pos = undefined;
        const neighbor_count = getNeighbors(grid, pos, &neighbors);

        for (neighbors[0..neighbor_count]) |neighbor| {
            const key = posToKey(neighbor, cols);
            if (!distances.contains(key)) {
                try distances.put(key, current_dist + 1);
                try queue.append(allocator, neighbor);
            }
        }
    }

    return distances;
}

/// Determines what pipe character 'S' actually represents based on its connections.
fn determineStartPipe(grid: []const []const u8, start: Pos, loop_positions: *const PosMap) u8 {
    const rows = grid.len;
    const cols = grid[0].len;

    var has_north = false;
    var has_south = false;
    var has_west = false;
    var has_east = false;

    const directions = [_]struct { dir: Direction, set_flag: *bool }{
        .{ .dir = north, .set_flag = &has_north },
        .{ .dir = south, .set_flag = &has_south },
        .{ .dir = west, .set_flag = &has_west },
        .{ .dir = east, .set_flag = &has_east },
    };

    for (directions) |d| {
        if (moveInBounds(start, d.dir, rows, cols)) |neighbor| {
            const key = posToKey(neighbor, cols);
            if (loop_positions.contains(key)) {
                if (connectsBackTo(grid, neighbor, start)) {
                    d.set_flag.* = true;
                }
            }
        }
    }

    if (has_north and has_south) return '|';
    if (has_west and has_east) return '-';
    if (has_north and has_east) return 'L';
    if (has_north and has_west) return 'J';
    if (has_south and has_west) return '7';
    if (has_south and has_east) return 'F';

    return 'S';
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const input = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(input);

    // Parse grid
    var lines: std.ArrayListUnmanaged([]const u8) = .empty;
    defer lines.deinit(allocator);

    var line_iter = std.mem.splitScalar(u8, std.mem.trim(u8, input, &std.ascii.whitespace), '\n');
    while (line_iter.next()) |line| {
        if (line.len > 0) {
            try lines.append(allocator, line);
        }
    }

    const grid = lines.items;
    const cols = grid[0].len;

    // Find start position
    const start = findStart(grid) orelse {
        std.debug.print("Could not find start position\n", .{});
        return;
    };

    // Part 1: Find the loop and max distance
    var distances = try findLoop(allocator, grid, start);
    defer distances.deinit();

    var max_dist: usize = 0;
    var iter = distances.valueIterator();
    while (iter.next()) |dist| {
        max_dist = @max(max_dist, dist.*);
    }

    std.debug.print("Part 1: {}\n", .{max_dist});

    // Part 2: Count enclosed tiles using ray casting
    const start_pipe = determineStartPipe(grid, start, &distances);

    var enclosed: usize = 0;
    for (grid, 0..) |row, r| {
        var inside = false;
        for (row, 0..) |ch, c| {
            const key = posToKey(Pos{ .row = r, .col = c }, cols);
            if (distances.contains(key)) {
                // Get the actual pipe character (replace S with its real type)
                const actual_ch = if (r == start.row and c == start.col) start_pipe else ch;
                // Count pipes with north connection for ray casting
                if (hasNorthConnection(actual_ch)) {
                    inside = !inside;
                }
            } else {
                if (inside) {
                    enclosed += 1;
                }
            }
        }
    }

    std.debug.print("Part 2: {}\n", .{enclosed});
}
