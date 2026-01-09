const std = @import("std");

const MAX_SIZE = 200;

fn parseGrid(data: []const u8, grid: *[MAX_SIZE][MAX_SIZE]u8, rows: *usize, cols: *usize) void {
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
        } else if (c >= '0' and c <= '9') {
            grid[row][col] = c - '0';
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

fn isVisible(grid: *const [MAX_SIZE][MAX_SIZE]u8, rows: usize, cols: usize, row: usize, col: usize) bool {
    const height = grid[row][col];

    // Check from left
    var visible_left = true;
    var c: usize = 0;
    while (c < col) : (c += 1) {
        if (grid[row][c] >= height) {
            visible_left = false;
            break;
        }
    }
    if (visible_left) return true;

    // Check from right
    var visible_right = true;
    c = col + 1;
    while (c < cols) : (c += 1) {
        if (grid[row][c] >= height) {
            visible_right = false;
            break;
        }
    }
    if (visible_right) return true;

    // Check from top
    var visible_top = true;
    var r: usize = 0;
    while (r < row) : (r += 1) {
        if (grid[r][col] >= height) {
            visible_top = false;
            break;
        }
    }
    if (visible_top) return true;

    // Check from bottom
    var visible_bottom = true;
    r = row + 1;
    while (r < rows) : (r += 1) {
        if (grid[r][col] >= height) {
            visible_bottom = false;
            break;
        }
    }
    return visible_bottom;
}

fn scenicScore(grid: *const [MAX_SIZE][MAX_SIZE]u8, rows: usize, cols: usize, row: usize, col: usize) u64 {
    const height = grid[row][col];

    // Count trees visible left
    var left: u64 = 0;
    if (col > 0) {
        var c: usize = col - 1;
        while (true) {
            left += 1;
            if (grid[row][c] >= height) break;
            if (c == 0) break;
            c -= 1;
        }
    }

    // Count trees visible right
    var right: u64 = 0;
    var c: usize = col + 1;
    while (c < cols) : (c += 1) {
        right += 1;
        if (grid[row][c] >= height) break;
    }

    // Count trees visible up
    var up: u64 = 0;
    if (row > 0) {
        var r: usize = row - 1;
        while (true) {
            up += 1;
            if (grid[r][col] >= height) break;
            if (r == 0) break;
            r -= 1;
        }
    }

    // Count trees visible down
    var down: u64 = 0;
    var r: usize = row + 1;
    while (r < rows) : (r += 1) {
        down += 1;
        if (grid[r][col] >= height) break;
    }

    return left * right * up * down;
}

fn part1(grid: *const [MAX_SIZE][MAX_SIZE]u8, rows: usize, cols: usize) u64 {
    var count: u64 = 0;
    var r: usize = 0;
    while (r < rows) : (r += 1) {
        var c: usize = 0;
        while (c < cols) : (c += 1) {
            if (isVisible(grid, rows, cols, r, c)) {
                count += 1;
            }
        }
    }
    return count;
}

fn part2(grid: *const [MAX_SIZE][MAX_SIZE]u8, rows: usize, cols: usize) u64 {
    var max_score: u64 = 0;
    var r: usize = 0;
    while (r < rows) : (r += 1) {
        var c: usize = 0;
        while (c < cols) : (c += 1) {
            const score = scenicScore(grid, rows, cols, r, c);
            max_score = @max(max_score, score);
        }
    }
    return max_score;
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

    parseGrid(data, &grid, &rows, &cols);

    const answer1 = part1(&grid, rows, cols);
    const answer2 = part2(&grid, rows, cols);

    std.debug.print("Part 1: {d}\n", .{answer1});
    std.debug.print("Part 2: {d}\n", .{answer2});
}
