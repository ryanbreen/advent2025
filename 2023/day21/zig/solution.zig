const std = @import("std");
const Allocator = std.mem.Allocator;

const TARGET_STEPS: i64 = 26501365;

const Point = struct {
    r: i32,
    c: i32,
};

const QueueItem = struct {
    r: i32,
    c: i32,
    dist: i32,
};

fn mod(a: i32, b: i32) i32 {
    const result = @mod(a, b);
    return result;
}

fn countReachable(allocator: Allocator, grid: []const []const u8, start: Point, steps: i32) i64 {
    const rows: i32 = @intCast(grid.len);
    const cols: i32 = @intCast(grid[0].len);

    // For Part 1 bounded grid, use a simple distance array
    var visited = allocator.alloc(i32, @intCast(rows * cols)) catch unreachable;
    defer allocator.free(visited);
    for (visited) |*v| {
        v.* = -1;
    }

    var queue = std.ArrayListUnmanaged(QueueItem){};
    defer queue.deinit(allocator);

    const startIdx: usize = @intCast(start.r * cols + start.c);
    visited[startIdx] = 0;
    queue.append(allocator, .{ .r = start.r, .c = start.c, .dist = 0 }) catch unreachable;

    const dirs = [_][2]i32{ .{ -1, 0 }, .{ 1, 0 }, .{ 0, -1 }, .{ 0, 1 } };

    var head: usize = 0;
    while (head < queue.items.len) {
        const item = queue.items[head];
        head += 1;

        if (item.dist >= steps) continue;

        for (dirs) |dir| {
            const nr = item.r + dir[0];
            const nc = item.c + dir[1];

            if (nr >= 0 and nr < rows and nc >= 0 and nc < cols) {
                const idx: usize = @intCast(nr * cols + nc);
                if (grid[@intCast(nr)][@intCast(nc)] != '#' and visited[idx] == -1) {
                    visited[idx] = item.dist + 1;
                    queue.append(allocator, .{ .r = nr, .c = nc, .dist = item.dist + 1 }) catch unreachable;
                }
            }
        }
    }

    const target_parity: i32 = @mod(steps, 2);
    var count: i64 = 0;
    for (visited) |d| {
        if (d >= 0 and d <= steps and @mod(d, 2) == target_parity) {
            count += 1;
        }
    }
    return count;
}

fn coordToKey(r: i32, c: i32) i64 {
    // Combine r and c into a single i64 key
    const r64: i64 = @as(i64, r);
    const c64: i64 = @as(i64, c);
    // Use a larger shift to handle negative coordinates better
    return ((r64 + 100000) << 20) | ((c64 + 100000) & 0xFFFFF);
}

fn countReachableInfiniteBFS(allocator: Allocator, grid: []const []const u8, start: Point, steps: i32) i64 {
    const rows: i32 = @intCast(grid.len);
    const cols: i32 = @intCast(grid[0].len);

    // Use a hash map for infinite grid coordinates
    var visited = std.AutoHashMapUnmanaged(i64, i32){};
    defer visited.deinit(allocator);

    var queue = std.ArrayListUnmanaged(QueueItem){};
    defer queue.deinit(allocator);

    const startKey = coordToKey(start.r, start.c);
    visited.put(allocator, startKey, 0) catch unreachable;
    queue.append(allocator, .{ .r = start.r, .c = start.c, .dist = 0 }) catch unreachable;

    const dirs = [_][2]i32{ .{ -1, 0 }, .{ 1, 0 }, .{ 0, -1 }, .{ 0, 1 } };

    var head: usize = 0;
    while (head < queue.items.len) {
        const item = queue.items[head];
        head += 1;

        if (item.dist >= steps) continue;

        for (dirs) |dir| {
            const nr = item.r + dir[0];
            const nc = item.c + dir[1];

            // Map to grid coordinates (infinite tiling)
            const gr = mod(nr, rows);
            const gc = mod(nc, cols);

            const key = coordToKey(nr, nc);
            if (grid[@intCast(gr)][@intCast(gc)] != '#' and !visited.contains(key)) {
                visited.put(allocator, key, item.dist + 1) catch unreachable;
                queue.append(allocator, .{ .r = nr, .c = nc, .dist = item.dist + 1 }) catch unreachable;
            }
        }
    }

    const target_parity: i32 = @mod(steps, 2);
    var count: i64 = 0;
    var it = visited.valueIterator();
    while (it.next()) |d| {
        if (d.* >= 0 and d.* <= steps and @mod(d.*, 2) == target_parity) {
            count += 1;
        }
    }
    return count;
}

fn countReachableInfinite(allocator: Allocator, grid: []const []const u8, start: Point, steps: i64) i64 {
    const size: i64 = @intCast(grid.len);
    const half: i64 = @divTrunc(size, 2);

    if (steps <= size * 2) {
        return countReachableInfiniteBFS(allocator, grid, start, @intCast(steps));
    }

    // n = (steps - half) / size
    const n: i64 = @divTrunc(steps - half, size);

    // Calculate y0, y1, y2 for the quadratic interpolation
    const y0 = countReachableInfiniteBFS(allocator, grid, start, @intCast(half));
    const y1 = countReachableInfiniteBFS(allocator, grid, start, @intCast(half + size));
    const y2 = countReachableInfiniteBFS(allocator, grid, start, @intCast(half + 2 * size));

    // Solve for a, b, c using finite differences
    // f(n) = an^2 + bn + c
    const a = @divTrunc(y2 - 2 * y1 + y0, 2);
    const b = y1 - y0 - a;
    const c = y0;

    return a * n * n + b * n + c;
}

fn parseInput(allocator: Allocator, content: []const u8) !struct { grid: [][]const u8, start: Point } {
    var lines = std.ArrayListUnmanaged([]const u8){};
    defer lines.deinit(allocator);

    var it = std.mem.splitScalar(u8, content, '\n');
    while (it.next()) |line| {
        if (line.len > 0) {
            try lines.append(allocator, line);
        }
    }

    var start = Point{ .r = 0, .c = 0 };

    // Find start position
    for (lines.items, 0..) |line, r| {
        for (line, 0..) |ch, c| {
            if (ch == 'S') {
                start.r = @intCast(r);
                start.c = @intCast(c);
                break;
            }
        }
    }

    return .{
        .grid = try lines.toOwnedSlice(allocator),
        .start = start,
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    const result = try parseInput(allocator, content);
    defer allocator.free(result.grid);

    const part1 = countReachable(allocator, result.grid, result.start, 64);
    const part2 = countReachableInfinite(allocator, result.grid, result.start, TARGET_STEPS);

    std.debug.print("Part 1: {d}\n", .{part1});
    std.debug.print("Part 2: {d}\n", .{part2});
}
