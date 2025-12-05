const std = @import("std");

fn part1(allocator: std.mem.Allocator, grid: [][]const u8) !i32 {
    const rows = grid.len;
    const cols = grid[0].len;
    const target = "XMAS";
    var count: i32 = 0;

    // 8 directions: right, left, down, up, and 4 diagonals
    const directions = [_][2]i32{
        .{ 0, 1 },   // right
        .{ 0, -1 },  // left
        .{ 1, 0 },   // down
        .{ -1, 0 },  // up
        .{ 1, 1 },   // down-right
        .{ 1, -1 },  // down-left
        .{ -1, 1 },  // up-right
        .{ -1, -1 }, // up-left
    };

    var r: usize = 0;
    while (r < rows) : (r += 1) {
        var c: usize = 0;
        while (c < cols) : (c += 1) {
            // Try each direction from this position
            for (directions) |dir| {
                const dr = dir[0];
                const dc = dir[1];
                var found = true;

                // Check if XMAS fits in this direction
                var i: usize = 0;
                while (i < target.len) : (i += 1) {
                    const nr = @as(i32, @intCast(r)) + dr * @as(i32, @intCast(i));
                    const nc = @as(i32, @intCast(c)) + dc * @as(i32, @intCast(i));

                    if (nr < 0 or nr >= @as(i32, @intCast(rows)) or nc < 0 or nc >= @as(i32, @intCast(cols))) {
                        found = false;
                        break;
                    }

                    if (grid[@intCast(nr)][@intCast(nc)] != target[i]) {
                        found = false;
                        break;
                    }
                }

                if (found) {
                    count += 1;
                }
            }
        }
    }

    _ = allocator;
    return count;
}

fn part2(allocator: std.mem.Allocator, grid: [][]const u8) !i32 {
    const rows = grid.len;
    const cols = grid[0].len;
    var count: i32 = 0;

    // Check each possible center point (A must be in the middle)
    var r: usize = 1;
    while (r < rows - 1) : (r += 1) {
        var c: usize = 1;
        while (c < cols - 1) : (c += 1) {
            if (grid[r][c] != 'A') {
                continue;
            }

            // Get the four corners
            const top_left = grid[r - 1][c - 1];
            const top_right = grid[r - 1][c + 1];
            const bottom_left = grid[r + 1][c - 1];
            const bottom_right = grid[r + 1][c + 1];

            // Check diagonal 1 (top-left to bottom-right): MAS or SAM
            const diag1_ok = (top_left == 'M' and bottom_right == 'S') or (top_left == 'S' and bottom_right == 'M');

            // Check diagonal 2 (top-right to bottom-left): MAS or SAM
            const diag2_ok = (top_right == 'M' and bottom_left == 'S') or (top_right == 'S' and bottom_left == 'M');

            if (diag1_ok and diag2_ok) {
                count += 1;
            }
        }
    }

    _ = allocator;
    return count;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const data = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(data);

    // Parse grid - split by newlines
    var lines: std.ArrayList([]const u8) = .{};
    defer lines.deinit(allocator);

    var line_iter = std.mem.tokenizeScalar(u8, data, '\n');
    while (line_iter.next()) |line| {
        if (line.len > 0) {
            try lines.append(allocator, line);
        }
    }

    // Convert to array of arrays for easier indexing
    const grid = try allocator.alloc([]const u8, lines.items.len);
    defer allocator.free(grid);

    for (lines.items, 0..) |line, i| {
        grid[i] = line;
    }

    const p1 = try part1(allocator, grid);
    const p2 = try part2(allocator, grid);

    std.debug.print("Part 1: {d}\n", .{p1});
    std.debug.print("Part 2: {d}\n", .{p2});
}
