const std = @import("std");
const ArrayList = std.ArrayList;

const SIZE: usize = 71;
const NUM_BYTES: usize = 1024;

const Point = struct {
    x: i32,
    y: i32,

    fn toIndex(self: Point) usize {
        return @as(usize, @intCast(self.y)) * SIZE + @as(usize, @intCast(self.x));
    }

    fn isValid(self: Point) bool {
        return self.x >= 0 and self.x < SIZE and self.y >= 0 and self.y < SIZE;
    }
};

const QueueItem = struct {
    point: Point,
    steps: i32,
};

const directions = [_]Point{
    .{ .x = 0, .y = 1 },
    .{ .x = 0, .y = -1 },
    .{ .x = 1, .y = 0 },
    .{ .x = -1, .y = 0 },
};

fn bfs(corrupted: []const bool, allocator: std.mem.Allocator) !i32 {
    const start = Point{ .x = 0, .y = 0 };
    const goal = Point{ .x = SIZE - 1, .y = SIZE - 1 };

    if (corrupted[start.toIndex()] or corrupted[goal.toIndex()]) {
        return -1;
    }

    var visited: [SIZE * SIZE]bool = [_]bool{false} ** (SIZE * SIZE);
    visited[start.toIndex()] = true;

    var queue = try ArrayList(QueueItem).initCapacity(allocator, SIZE * SIZE);
    defer queue.deinit(allocator);

    try queue.append(allocator, .{ .point = start, .steps = 0 });

    var read_idx: usize = 0;
    while (read_idx < queue.items.len) {
        const item = queue.items[read_idx];
        read_idx += 1;

        if (item.point.x == goal.x and item.point.y == goal.y) {
            return item.steps;
        }

        for (directions) |dir| {
            const nx = item.point.x + dir.x;
            const ny = item.point.y + dir.y;
            const next = Point{ .x = nx, .y = ny };

            if (next.isValid()) {
                const idx = next.toIndex();
                if (!visited[idx] and !corrupted[idx]) {
                    visited[idx] = true;
                    try queue.append(allocator, .{ .point = next, .steps = item.steps + 1 });
                }
            }
        }
    }

    return -1;
}

fn part1(positions: []const Point, allocator: std.mem.Allocator) !i32 {
    var corrupted: [SIZE * SIZE]bool = [_]bool{false} ** (SIZE * SIZE);

    const limit = @min(NUM_BYTES, positions.len);
    for (positions[0..limit]) |pos| {
        if (pos.isValid()) {
            corrupted[pos.toIndex()] = true;
        }
    }

    return try bfs(&corrupted, allocator);
}

fn part2(positions: []const Point, allocator: std.mem.Allocator) !Point {
    var left: usize = 0;
    var right: usize = positions.len;

    while (left < right) {
        const mid = (left + right) / 2;

        var corrupted: [SIZE * SIZE]bool = [_]bool{false} ** (SIZE * SIZE);
        for (positions[0 .. mid + 1]) |pos| {
            if (pos.isValid()) {
                corrupted[pos.toIndex()] = true;
            }
        }

        const result = try bfs(&corrupted, allocator);
        if (result == -1) {
            right = mid;
        } else {
            left = mid + 1;
        }
    }

    return positions[left];
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

    // Parse positions
    var positions = try ArrayList(Point).initCapacity(allocator, 4096);
    defer positions.deinit(allocator);

    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, &[_]u8{ ' ', '\r', '\t' });
        if (trimmed.len == 0) continue;

        var parts = std.mem.splitScalar(u8, trimmed, ',');
        const x_str = parts.next() orelse continue;
        const y_str = parts.next() orelse continue;

        const x = std.fmt.parseInt(i32, x_str, 10) catch continue;
        const y = std.fmt.parseInt(i32, y_str, 10) catch continue;

        try positions.append(allocator, .{ .x = x, .y = y });
    }

    // Part 1
    const p1_result = try part1(positions.items, allocator);
    std.debug.print("Part 1: {d}\n", .{p1_result});

    // Part 2
    const p2_result = try part2(positions.items, allocator);
    std.debug.print("Part 2: {d},{d}\n", .{ p2_result.x, p2_result.y });
}
