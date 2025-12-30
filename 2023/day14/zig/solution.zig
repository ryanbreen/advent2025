const std = @import("std");

const Cell = enum { Empty, Round, Cube };

const Grid = struct {
    data: []Cell,
    rows: usize,
    cols: usize,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, rows: usize, cols: usize) !Grid {
        const data = try allocator.alloc(Cell, rows * cols);
        return Grid{
            .data = data,
            .rows = rows,
            .cols = cols,
            .allocator = allocator,
        };
    }

    fn deinit(self: *Grid) void {
        self.allocator.free(self.data);
    }

    fn clone(self: *const Grid) !Grid {
        const new_grid = try Grid.init(self.allocator, self.rows, self.cols);
        @memcpy(new_grid.data, self.data);
        return new_grid;
    }

    fn get(self: *const Grid, row: usize, col: usize) Cell {
        return self.data[row * self.cols + col];
    }

    fn set(self: *Grid, row: usize, col: usize, value: Cell) void {
        self.data[row * self.cols + col] = value;
    }

    fn tiltNorth(self: *Grid) void {
        for (0..self.cols) |col| {
            var write_pos: usize = 0;
            for (0..self.rows) |row| {
                switch (self.get(row, col)) {
                    .Cube => write_pos = row + 1,
                    .Round => {
                        self.set(row, col, .Empty);
                        self.set(write_pos, col, .Round);
                        write_pos += 1;
                    },
                    .Empty => {},
                }
            }
        }
    }

    fn tiltSouth(self: *Grid) void {
        for (0..self.cols) |col| {
            var write_pos: usize = self.rows - 1;
            var row: usize = self.rows;
            while (row > 0) {
                row -= 1;
                switch (self.get(row, col)) {
                    .Cube => write_pos = if (row > 0) row - 1 else 0,
                    .Round => {
                        self.set(row, col, .Empty);
                        self.set(write_pos, col, .Round);
                        write_pos = if (write_pos > 0) write_pos - 1 else 0;
                    },
                    .Empty => {},
                }
            }
        }
    }

    fn tiltWest(self: *Grid) void {
        for (0..self.rows) |row| {
            var write_pos: usize = 0;
            for (0..self.cols) |col| {
                switch (self.get(row, col)) {
                    .Cube => write_pos = col + 1,
                    .Round => {
                        self.set(row, col, .Empty);
                        self.set(row, write_pos, .Round);
                        write_pos += 1;
                    },
                    .Empty => {},
                }
            }
        }
    }

    fn tiltEast(self: *Grid) void {
        for (0..self.rows) |row| {
            var write_pos: usize = self.cols - 1;
            var col: usize = self.cols;
            while (col > 0) {
                col -= 1;
                switch (self.get(row, col)) {
                    .Cube => write_pos = if (col > 0) col - 1 else 0,
                    .Round => {
                        self.set(row, col, .Empty);
                        self.set(row, write_pos, .Round);
                        write_pos = if (write_pos > 0) write_pos - 1 else 0;
                    },
                    .Empty => {},
                }
            }
        }
    }

    fn spinCycle(self: *Grid) void {
        self.tiltNorth();
        self.tiltWest();
        self.tiltSouth();
        self.tiltEast();
    }

    fn calculateLoad(self: *const Grid) u64 {
        var total: u64 = 0;
        for (0..self.rows) |row| {
            for (0..self.cols) |col| {
                if (self.get(row, col) == .Round) {
                    total += @intCast(self.rows - row);
                }
            }
        }
        return total;
    }

    fn toKey(self: *const Grid, allocator: std.mem.Allocator) ![]u8 {
        const key = try allocator.alloc(u8, self.data.len);
        for (self.data, 0..) |cell, i| {
            key[i] = switch (cell) {
                .Empty => '.',
                .Round => 'O',
                .Cube => '#',
            };
        }
        return key;
    }
};

fn parseInput(allocator: std.mem.Allocator, input: []const u8) !Grid {
    // Count rows and columns
    var lines = std.mem.tokenizeScalar(u8, input, '\n');
    var rows: usize = 0;
    var cols: usize = 0;

    while (lines.next()) |line| {
        if (line.len > 0) {
            if (cols == 0) cols = line.len;
            rows += 1;
        }
    }

    var grid = try Grid.init(allocator, rows, cols);

    // Reset iterator and populate grid
    lines = std.mem.tokenizeScalar(u8, input, '\n');
    var row: usize = 0;
    while (lines.next()) |line| {
        if (line.len > 0) {
            for (line, 0..) |c, col| {
                const cell: Cell = switch (c) {
                    '.' => .Empty,
                    'O' => .Round,
                    '#' => .Cube,
                    else => .Empty,
                };
                grid.set(row, col, cell);
            }
            row += 1;
        }
    }

    return grid;
}

fn part1(original_grid: *const Grid) !u64 {
    var grid = try original_grid.clone();
    defer grid.deinit();
    grid.tiltNorth();
    return grid.calculateLoad();
}

fn part2(allocator: std.mem.Allocator, original_grid: *const Grid) !u64 {
    var grid = try original_grid.clone();
    defer grid.deinit();

    const target: u64 = 1_000_000_000;

    var seen = std.StringHashMap(u64).init(allocator);
    defer {
        var it = seen.keyIterator();
        while (it.next()) |key| {
            allocator.free(key.*);
        }
        seen.deinit();
    }

    var cycle_num: u64 = 0;

    while (cycle_num < target) {
        const key = try grid.toKey(allocator);

        if (seen.get(key)) |cycle_start| {
            allocator.free(key);
            const cycle_length = cycle_num - cycle_start;
            const remaining = (target - cycle_num) % cycle_length;
            for (0..remaining) |_| {
                grid.spinCycle();
            }
            return grid.calculateLoad();
        }

        try seen.put(key, cycle_num);
        grid.spinCycle();
        cycle_num += 1;
    }

    return grid.calculateLoad();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const input = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(input);

    var grid = try parseInput(allocator, input);
    defer grid.deinit();

    var buffer: [256]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&buffer);

    const answer1 = try part1(&grid);
    try stdout.interface.print("Part 1: {d}\n", .{answer1});

    const answer2 = try part2(allocator, &grid);
    try stdout.interface.print("Part 2: {d}\n", .{answer2});

    try stdout.interface.flush();
}
