const std = @import("std");

const Point = struct {
    x: i32,
    y: i32,
};

// Use a hash set for blocked positions
const PointSet = std.AutoHashMap(Point, void);

fn parseNumber(s: []const u8) !i32 {
    return std.fmt.parseInt(i32, s, 10);
}

fn min(a: i32, b: i32) i32 {
    return if (a < b) a else b;
}

fn max(a: i32, b: i32) i32 {
    return if (a > b) a else b;
}

fn parsePaths(allocator: std.mem.Allocator, text: []const u8) !struct { rocks: PointSet, max_y: i32 } {
    var rocks = PointSet.init(allocator);
    var max_y: i32 = 0;

    var lines = std.mem.tokenizeScalar(u8, text, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        // Parse all points in this path
        var points: std.ArrayListUnmanaged(Point) = .empty;
        defer points.deinit(allocator);

        var segments = std.mem.splitSequence(u8, line, " -> ");
        while (segments.next()) |segment| {
            var coords = std.mem.splitScalar(u8, segment, ',');
            const x_str = coords.next() orelse continue;
            const y_str = coords.next() orelse continue;
            const x = try parseNumber(x_str);
            const y = try parseNumber(y_str);
            try points.append(allocator, Point{ .x = x, .y = y });
            max_y = max(max_y, y);
        }

        // Draw lines between consecutive points
        const pts = points.items;
        for (0..pts.len - 1) |i| {
            const p1 = pts[i];
            const p2 = pts[i + 1];

            if (p1.x == p2.x) {
                // Vertical line
                var y = min(p1.y, p2.y);
                while (y <= max(p1.y, p2.y)) : (y += 1) {
                    try rocks.put(Point{ .x = p1.x, .y = y }, {});
                }
            } else {
                // Horizontal line
                var x = min(p1.x, p2.x);
                while (x <= max(p1.x, p2.x)) : (x += 1) {
                    try rocks.put(Point{ .x = x, .y = p1.y }, {});
                }
            }
        }
    }

    return .{ .rocks = rocks, .max_y = max_y };
}

fn simulateSand(blocked: *PointSet, max_y: i32, floor: bool) !?Point {
    var x: i32 = 500;
    var y: i32 = 0;

    while (true) {
        // Check if sand has fallen below all rocks (into abyss)
        if (!floor and y > max_y) {
            return null;
        }

        // Try to move down
        if (floor and y + 1 == max_y + 2) {
            // Hit the floor
            return Point{ .x = x, .y = y };
        } else if (!blocked.contains(Point{ .x = x, .y = y + 1 })) {
            y += 1;
        }
        // Try to move down-left
        else if (!blocked.contains(Point{ .x = x - 1, .y = y + 1 })) {
            x -= 1;
            y += 1;
        }
        // Try to move down-right
        else if (!blocked.contains(Point{ .x = x + 1, .y = y + 1 })) {
            x += 1;
            y += 1;
        }
        // Sand comes to rest
        else {
            return Point{ .x = x, .y = y };
        }
    }
}

fn part1(allocator: std.mem.Allocator, text: []const u8) !i32 {
    const result = try parsePaths(allocator, text);
    var blocked = result.rocks;
    defer blocked.deinit();
    const max_y = result.max_y;

    var count: i32 = 0;
    while (true) {
        const pos = try simulateSand(&blocked, max_y, false);
        if (pos == null) {
            break;
        }
        try blocked.put(pos.?, {});
        count += 1;
    }

    return count;
}

fn part2(allocator: std.mem.Allocator, text: []const u8) !i32 {
    const result = try parsePaths(allocator, text);
    var blocked = result.rocks;
    defer blocked.deinit();
    const max_y = result.max_y;

    var count: i32 = 0;
    while (true) {
        const pos = try simulateSand(&blocked, max_y, true);
        try blocked.put(pos.?, {});
        count += 1;
        if (pos.?.x == 500 and pos.?.y == 0) {
            break;
        }
    }

    return count;
}

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const input = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(input);

    const trimmed = std.mem.trim(u8, input, "\n\r ");

    const answer1 = try part1(allocator, trimmed);
    const answer2 = try part2(allocator, trimmed);

    std.debug.print("Part 1: {d}\n", .{answer1});
    std.debug.print("Part 2: {d}\n", .{answer2});
}
