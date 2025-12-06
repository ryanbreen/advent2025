const std = @import("std");

const Direction = enum {
    North,
    East,
    South,
    West,

    fn turnRight(self: Direction) Direction {
        return switch (self) {
            .North => .East,
            .East => .South,
            .South => .West,
            .West => .North,
        };
    }

    fn getDelta(self: Direction) struct { dx: i32, dy: i32 } {
        return switch (self) {
            .North => .{ .dx = 0, .dy = -1 },
            .East => .{ .dx = 1, .dy = 0 },
            .South => .{ .dx = 0, .dy = 1 },
            .West => .{ .dx = -1, .dy = 0 },
        };
    }
};

const Position = struct {
    x: i32,
    y: i32,
};

const State = struct {
    x: i32,
    y: i32,
    dir: Direction,
};

fn simulatePatrol(allocator: std.mem.Allocator, grid: [][]u8, start_pos: Position, start_dir: Direction) !std.AutoHashMap(Position, void) {
    const height: i32 = @intCast(grid.len);
    const width: i32 = @intCast(grid[0].len);

    var visited = std.AutoHashMap(Position, void).init(allocator);
    try visited.put(start_pos, {});

    var pos = start_pos;
    var dir = start_dir;

    while (true) {
        const delta = dir.getDelta();
        const next_x = pos.x + delta.dx;
        const next_y = pos.y + delta.dy;

        // Check if the guard has left the map
        if (next_x < 0 or next_x >= width or next_y < 0 or next_y >= height) {
            break;
        }

        // Check if there's an obstacle ahead
        const next_cell = grid[@intCast(next_y)][@intCast(next_x)];
        if (next_cell == '#') {
            // Turn right
            dir = dir.turnRight();
        } else {
            // Move forward
            pos.x = next_x;
            pos.y = next_y;
            try visited.put(pos, {});
        }
    }

    return visited;
}

fn detectLoop(allocator: std.mem.Allocator, grid: [][]u8, start_pos: Position, start_dir: Direction) !bool {
    const height: i32 = @intCast(grid.len);
    const width: i32 = @intCast(grid[0].len);

    var states = std.AutoHashMap(State, void).init(allocator);
    defer states.deinit();

    var pos = start_pos;
    var dir = start_dir;

    while (true) {
        const state = State{ .x = pos.x, .y = pos.y, .dir = dir };
        if (states.contains(state)) {
            return true; // Loop detected
        }
        try states.put(state, {});

        const delta = dir.getDelta();
        const next_x = pos.x + delta.dx;
        const next_y = pos.y + delta.dy;

        // Check if the guard has left the map
        if (next_x < 0 or next_x >= width or next_y < 0 or next_y >= height) {
            return false; // Exited the map
        }

        // Check if there's an obstacle ahead
        const next_cell = grid[@intCast(next_y)][@intCast(next_x)];
        if (next_cell == '#') {
            // Turn right
            dir = dir.turnRight();
        } else {
            // Move forward
            pos.x = next_x;
            pos.y = next_y;
        }
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read the input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Parse the map - count lines first
    var line_count: usize = 0;
    var lines_iter = std.mem.splitSequence(u8, content, "\n");
    while (lines_iter.next()) |line| {
        if (line.len > 0) {
            line_count += 1;
        }
    }

    // Allocate array for grid (mutable for part 2)
    const grid = try allocator.alloc([]u8, line_count);
    defer {
        for (grid) |row| {
            allocator.free(row);
        }
        allocator.free(grid);
    }

    // Fill the grid array with mutable copies
    var line_idx: usize = 0;
    var lines = std.mem.splitSequence(u8, content, "\n");
    while (lines.next()) |line| {
        if (line.len > 0) {
            grid[line_idx] = try allocator.dupe(u8, line);
            line_idx += 1;
        }
    }

    // Find the guard's starting position and direction
    var guard_pos: Position = undefined;
    var guard_dir: Direction = undefined;
    var found = false;

    for (grid, 0..) |row, y| {
        for (row, 0..) |cell, x| {
            switch (cell) {
                '^' => {
                    guard_pos = .{ .x = @intCast(x), .y = @intCast(y) };
                    guard_dir = .North;
                    found = true;
                },
                'v' => {
                    guard_pos = .{ .x = @intCast(x), .y = @intCast(y) };
                    guard_dir = .South;
                    found = true;
                },
                '<' => {
                    guard_pos = .{ .x = @intCast(x), .y = @intCast(y) };
                    guard_dir = .West;
                    found = true;
                },
                '>' => {
                    guard_pos = .{ .x = @intCast(x), .y = @intCast(y) };
                    guard_dir = .East;
                    found = true;
                },
                else => {},
            }
            if (found) break;
        }
        if (found) break;
    }

    // Part 1: Simulate normal patrol
    var visited = try simulatePatrol(allocator, grid, guard_pos, guard_dir);
    defer visited.deinit();

    const part1 = visited.count();
    std.debug.print("Part 1: {d}\n", .{part1});

    // Part 2: Count positions where adding an obstruction creates a loop
    var loop_count: usize = 0;

    var iter = visited.keyIterator();
    while (iter.next()) |pos| {
        // Skip the starting position
        if (pos.x == guard_pos.x and pos.y == guard_pos.y) {
            continue;
        }

        // Place obstruction
        const y_idx: usize = @intCast(pos.y);
        const x_idx: usize = @intCast(pos.x);
        const original = grid[y_idx][x_idx];
        grid[y_idx][x_idx] = '#';

        // Check if this creates a loop
        if (try detectLoop(allocator, grid, guard_pos, guard_dir)) {
            loop_count += 1;
        }

        // Remove obstruction
        grid[y_idx][x_idx] = original;
    }

    std.debug.print("Part 2: {d}\n", .{loop_count});
}
