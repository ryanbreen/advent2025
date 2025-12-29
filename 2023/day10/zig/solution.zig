const std = @import("std");

const Direction = struct {
    dr: i32,
    dc: i32,
};

const NORTH = Direction{ .dr = -1, .dc = 0 };
const SOUTH = Direction{ .dr = 1, .dc = 0 };
const WEST = Direction{ .dr = 0, .dc = -1 };
const EAST = Direction{ .dr = 0, .dc = 1 };

fn getPipeConnections(ch: u8) ?[2]Direction {
    return switch (ch) {
        '|' => .{ NORTH, SOUTH },
        '-' => .{ WEST, EAST },
        'L' => .{ NORTH, EAST },
        'J' => .{ NORTH, WEST },
        '7' => .{ SOUTH, WEST },
        'F' => .{ SOUTH, EAST },
        else => null,
    };
}

fn hasNorthConnection(ch: u8) bool {
    return switch (ch) {
        '|', 'L', 'J' => true,
        else => false,
    };
}

const Pos = struct {
    row: usize,
    col: usize,
};

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

fn getNeighbors(grid: []const []const u8, pos: Pos, neighbors: *[4]Pos) usize {
    const rows = grid.len;
    const cols = grid[0].len;
    const ch = grid[pos.row][pos.col];
    var count: usize = 0;

    if (ch == 'S') {
        // S can connect to any adjacent pipe that connects back to it
        const directions = [_]Direction{ NORTH, SOUTH, WEST, EAST };
        for (directions) |dir| {
            const nr_signed = @as(i64, @intCast(pos.row)) + dir.dr;
            const nc_signed = @as(i64, @intCast(pos.col)) + dir.dc;

            if (nr_signed >= 0 and nr_signed < @as(i64, @intCast(rows)) and
                nc_signed >= 0 and nc_signed < @as(i64, @intCast(cols)))
            {
                const nr: usize = @intCast(nr_signed);
                const nc: usize = @intCast(nc_signed);
                const adj_ch = grid[nr][nc];

                if (getPipeConnections(adj_ch)) |conns| {
                    for (conns) |conn| {
                        const back_r = @as(i64, @intCast(nr)) + conn.dr;
                        const back_c = @as(i64, @intCast(nc)) + conn.dc;
                        if (back_r == @as(i64, @intCast(pos.row)) and back_c == @as(i64, @intCast(pos.col))) {
                            neighbors[count] = Pos{ .row = nr, .col = nc };
                            count += 1;
                            break;
                        }
                    }
                }
            }
        }
    } else if (getPipeConnections(ch)) |conns| {
        for (conns) |dir| {
            const nr_signed = @as(i64, @intCast(pos.row)) + dir.dr;
            const nc_signed = @as(i64, @intCast(pos.col)) + dir.dc;

            if (nr_signed >= 0 and nr_signed < @as(i64, @intCast(rows)) and
                nc_signed >= 0 and nc_signed < @as(i64, @intCast(cols)))
            {
                const nr: usize = @intCast(nr_signed);
                const nc: usize = @intCast(nc_signed);
                neighbors[count] = Pos{ .row = nr, .col = nc };
                count += 1;
            }
        }
    }

    return count;
}

const PosMap = std.AutoHashMap(u64, usize);

fn posToKey(pos: Pos, cols: usize) u64 {
    return @as(u64, pos.row) * @as(u64, cols) + @as(u64, pos.col);
}

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

fn determineStartPipe(grid: []const []const u8, start: Pos, loop_positions: *const PosMap) u8 {
    const rows = grid.len;
    const cols = grid[0].len;

    var has_north = false;
    var has_south = false;
    var has_west = false;
    var has_east = false;

    const directions = [_]struct { dir: Direction, set_flag: *bool }{
        .{ .dir = NORTH, .set_flag = &has_north },
        .{ .dir = SOUTH, .set_flag = &has_south },
        .{ .dir = WEST, .set_flag = &has_west },
        .{ .dir = EAST, .set_flag = &has_east },
    };

    for (directions) |d| {
        const nr_signed = @as(i64, @intCast(start.row)) + d.dir.dr;
        const nc_signed = @as(i64, @intCast(start.col)) + d.dir.dc;

        if (nr_signed >= 0 and nr_signed < @as(i64, @intCast(rows)) and
            nc_signed >= 0 and nc_signed < @as(i64, @intCast(cols)))
        {
            const nr: usize = @intCast(nr_signed);
            const nc: usize = @intCast(nc_signed);
            const key = @as(u64, nr) * @as(u64, cols) + @as(u64, nc);

            if (loop_positions.contains(key)) {
                const adj_ch = grid[nr][nc];
                if (getPipeConnections(adj_ch)) |conns| {
                    for (conns) |conn| {
                        const back_r = @as(i64, @intCast(nr)) + conn.dr;
                        const back_c = @as(i64, @intCast(nc)) + conn.dc;
                        if (back_r == @as(i64, @intCast(start.row)) and back_c == @as(i64, @intCast(start.col))) {
                            d.set_flag.* = true;
                            break;
                        }
                    }
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
        if (dist.* > max_dist) {
            max_dist = dist.*;
        }
    }

    std.debug.print("Part 1: {}\n", .{max_dist});

    // Part 2: Count enclosed tiles using ray casting
    const start_pipe = determineStartPipe(grid, start, &distances);

    var enclosed: usize = 0;
    for (grid, 0..) |row, r| {
        var inside = false;
        for (row, 0..) |ch, c| {
            const key = @as(u64, r) * @as(u64, cols) + @as(u64, c);
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
