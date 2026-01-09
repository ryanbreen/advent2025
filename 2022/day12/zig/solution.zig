const std = @import("std");

const MAX_SIZE = 200;
const MAX_QUEUE = MAX_SIZE * MAX_SIZE;

const Point = struct {
    row: usize,
    col: usize,
};

const QueueItem = struct {
    pos: Point,
    dist: usize,
};

fn parseGrid(data: []const u8, grid: *[MAX_SIZE][MAX_SIZE]u8, rows: *usize, cols: *usize, start: *Point, end: *Point) void {
    var row: usize = 0;
    var col: usize = 0;
    var max_col: usize = 0;

    for (data) |c| {
        if (c == '\n') {
            if (col > 0) {
                max_col = @max(max_col, col);
                row += 1;
                col = 0;
            }
        } else if (c >= 'a' and c <= 'z') {
            grid[row][col] = c;
            col += 1;
        } else if (c == 'S') {
            start.* = Point{ .row = row, .col = col };
            grid[row][col] = 'a';
            col += 1;
        } else if (c == 'E') {
            end.* = Point{ .row = row, .col = col };
            grid[row][col] = 'z';
            col += 1;
        }
    }

    // Handle last row if no trailing newline
    if (col > 0) {
        max_col = @max(max_col, col);
        row += 1;
    }

    rows.* = row;
    cols.* = max_col;
}

fn bfs(grid: *const [MAX_SIZE][MAX_SIZE]u8, rows: usize, cols: usize, starts: []const Point, end: Point) usize {
    var visited: [MAX_SIZE][MAX_SIZE]bool = undefined;
    for (0..MAX_SIZE) |r| {
        for (0..MAX_SIZE) |c| {
            visited[r][c] = false;
        }
    }

    var queue: [MAX_QUEUE]QueueItem = undefined;
    var queue_head: usize = 0;
    var queue_tail: usize = 0;

    // Add all starting positions to queue
    for (starts) |start| {
        queue[queue_tail] = QueueItem{ .pos = start, .dist = 0 };
        queue_tail += 1;
        visited[start.row][start.col] = true;
    }

    const directions = [_][2]i32{
        .{ -1, 0 }, // up
        .{ 1, 0 }, // down
        .{ 0, -1 }, // left
        .{ 0, 1 }, // right
    };

    while (queue_head < queue_tail) {
        const current = queue[queue_head];
        queue_head += 1;

        // Check if we reached the end
        if (current.pos.row == end.row and current.pos.col == end.col) {
            return current.dist;
        }

        const current_height = grid[current.pos.row][current.pos.col];

        // Try all four directions
        for (directions) |dir| {
            const dr = dir[0];
            const dc = dir[1];

            // Calculate new position (with bounds checking)
            const nr_signed: i32 = @as(i32, @intCast(current.pos.row)) + dr;
            const nc_signed: i32 = @as(i32, @intCast(current.pos.col)) + dc;

            if (nr_signed < 0 or nc_signed < 0) continue;

            const nr: usize = @intCast(nr_signed);
            const nc: usize = @intCast(nc_signed);

            if (nr >= rows or nc >= cols) continue;
            if (visited[nr][nc]) continue;

            const next_height = grid[nr][nc];

            // Can move if destination is at most 1 higher
            if (next_height <= current_height + 1) {
                visited[nr][nc] = true;
                queue[queue_tail] = QueueItem{
                    .pos = Point{ .row = nr, .col = nc },
                    .dist = current.dist + 1,
                };
                queue_tail += 1;
            }
        }
    }

    return std.math.maxInt(usize); // No path found
}

fn part1(grid: *const [MAX_SIZE][MAX_SIZE]u8, rows: usize, cols: usize, start: Point, end: Point) usize {
    const starts = [_]Point{start};
    return bfs(grid, rows, cols, &starts, end);
}

fn part2(grid: *const [MAX_SIZE][MAX_SIZE]u8, rows: usize, cols: usize, end: Point) usize {
    // Find all cells with elevation 'a'
    var starts: [MAX_QUEUE]Point = undefined;
    var start_count: usize = 0;

    for (0..rows) |r| {
        for (0..cols) |c| {
            if (grid[r][c] == 'a') {
                starts[start_count] = Point{ .row = r, .col = c };
                start_count += 1;
            }
        }
    }

    return bfs(grid, rows, cols, starts[0..start_count], end);
}

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const data = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(data);

    var grid: [MAX_SIZE][MAX_SIZE]u8 = undefined;
    var rows: usize = 0;
    var cols: usize = 0;
    var start: Point = undefined;
    var end: Point = undefined;

    parseGrid(data, &grid, &rows, &cols, &start, &end);

    const answer1 = part1(&grid, rows, cols, start, end);
    const answer2 = part2(&grid, rows, cols, end);

    std.debug.print("Part 1: {d}\n", .{answer1});
    std.debug.print("Part 2: {d}\n", .{answer2});
}
