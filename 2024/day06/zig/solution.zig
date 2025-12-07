const std = @import("std");

const MAX_SIZE = 200;

// Direction vectors: up, right, down, left (matching C implementation)
const dx = [4]i32{ -1, 0, 1, 0 };
const dy = [4]i32{ 0, 1, 0, -1 };

// Simulate guard movement
// Returns true if guard gets stuck in a loop, false if they leave the map
// If count_visited is true, also counts distinct positions visited
fn simulate(grid: *const [MAX_SIZE][MAX_SIZE]u8, rows: usize, cols: usize, start_x: usize, start_y: usize, start_dir: usize, obstruction_x: ?usize, obstruction_y: ?usize, count_visited: bool, visited_count: *usize) bool {
    var visited: [MAX_SIZE][MAX_SIZE]bool = undefined;
    var state_visited: [MAX_SIZE][MAX_SIZE][4]bool = undefined;

    // Zero-initialize arrays
    @memset(&visited, [_]bool{false} ** MAX_SIZE);
    for (&state_visited) |*row| {
        @memset(row, [_]bool{false} ** 4);
    }

    var x = start_x;
    var y = start_y;
    var dir = start_dir;

    visited[x][y] = true;
    var count: usize = 1;

    while (true) {
        // Check if we've seen this state before (loop detection)
        if (state_visited[x][y][dir]) {
            return true; // Loop detected
        }
        state_visited[x][y][dir] = true;

        // Try to move forward
        const next_x_i: i32 = @as(i32, @intCast(x)) + dx[dir];
        const next_y_i: i32 = @as(i32, @intCast(y)) + dy[dir];

        // Check if we're leaving the map
        if (next_x_i < 0 or next_x_i >= @as(i32, @intCast(rows)) or next_y_i < 0 or next_y_i >= @as(i32, @intCast(cols))) {
            // Guard leaves the map
            if (count_visited) {
                visited_count.* = count;
            }
            return false;
        }

        const next_x: usize = @intCast(next_x_i);
        const next_y: usize = @intCast(next_y_i);

        // Check if there's an obstacle
        const is_obstacle = (grid[next_x][next_y] == '#') or
            (obstruction_x != null and obstruction_y != null and next_x == obstruction_x.? and next_y == obstruction_y.?);

        if (is_obstacle) {
            // Turn right
            dir = (dir + 1) % 4;
        } else {
            // Move forward
            x = next_x;
            y = next_y;

            if (count_visited and !visited[x][y]) {
                visited[x][y] = true;
                count += 1;
            }
        }
    }
}

pub fn main() !void {
    // Read the input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var buf: [1024 * 1024]u8 = undefined;
    const bytes_read = try file.readAll(&buf);
    const content = buf[0..bytes_read];

    // Parse the grid into fixed-size array
    var grid: [MAX_SIZE][MAX_SIZE]u8 = undefined;
    @memset(&grid, [_]u8{'.'} ** MAX_SIZE);

    var rows: usize = 0;
    var cols: usize = 0;
    var start_x: usize = 0;
    var start_y: usize = 0;
    var start_dir: usize = 0;

    var lines = std.mem.splitSequence(u8, content, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        if (cols == 0) cols = line.len;

        for (line, 0..) |char, j| {
            grid[rows][j] = char;

            // Find starting position and direction
            switch (char) {
                '^' => {
                    start_x = rows;
                    start_y = j;
                    start_dir = 0; // up
                    grid[rows][j] = '.'; // Replace with empty space
                },
                '>' => {
                    start_x = rows;
                    start_y = j;
                    start_dir = 1; // right
                    grid[rows][j] = '.';
                },
                'v' => {
                    start_x = rows;
                    start_y = j;
                    start_dir = 2; // down
                    grid[rows][j] = '.';
                },
                '<' => {
                    start_x = rows;
                    start_y = j;
                    start_dir = 3; // left
                    grid[rows][j] = '.';
                },
                else => {},
            }
        }
        rows += 1;
    }

    // Part 1: Count visited positions
    var visited_count: usize = 0;
    _ = simulate(&grid, rows, cols, start_x, start_y, start_dir, null, null, true, &visited_count);
    std.debug.print("Part 1: {d}\n", .{visited_count});

    // Part 2: Try placing an obstruction at each empty position (except start)
    var loop_positions: usize = 0;
    var dummy: usize = 0;

    for (0..rows) |r| {
        for (0..cols) |c| {
            // Skip if not empty or is starting position
            if (grid[r][c] != '.' or (r == start_x and c == start_y)) {
                continue;
            }

            // Simulate with obstruction at (r, c)
            if (simulate(&grid, rows, cols, start_x, start_y, start_dir, r, c, false, &dummy)) {
                loop_positions += 1;
            }
        }
    }

    std.debug.print("Part 2: {d}\n", .{loop_positions});
}
