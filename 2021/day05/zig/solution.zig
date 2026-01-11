const std = @import("std");

const Line = struct {
    x1: i32,
    y1: i32,
    x2: i32,
    y2: i32,
};

fn sign(x: i32) i32 {
    if (x > 0) return 1;
    if (x < 0) return -1;
    return 0;
}

fn countOverlaps(lines: []const Line, include_diagonals: bool, allocator: std.mem.Allocator) !u32 {
    var grid = std.AutoHashMap(u64, u16).init(allocator);
    defer grid.deinit();

    for (lines) |line| {
        const dx = sign(line.x2 - line.x1);
        const dy = sign(line.y2 - line.y1);

        // Skip diagonals in part 1
        if (!include_diagonals and dx != 0 and dy != 0) {
            continue;
        }

        var x = line.x1;
        var y = line.y1;
        while (true) {
            const key = (@as(u64, @intCast(@as(u32, @bitCast(x)))) << 32) | @as(u64, @intCast(@as(u32, @bitCast(y))));
            const entry = try grid.getOrPut(key);
            if (!entry.found_existing) {
                entry.value_ptr.* = 0;
            }
            entry.value_ptr.* += 1;

            if (x == line.x2 and y == line.y2) break;
            x += dx;
            y += dy;
        }
    }

    var count: u32 = 0;
    var iter = grid.valueIterator();
    while (iter.next()) |value| {
        if (value.* >= 2) {
            count += 1;
        }
    }
    return count;
}

fn parseInt(str: []const u8) !i32 {
    const trimmed = std.mem.trim(u8, str, " \t\r\n");
    return try std.fmt.parseInt(i32, trimmed, 10);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const file_size = (try file.stat()).size;
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    _ = try file.readAll(buffer);

    const content = std.mem.trim(u8, buffer, &std.ascii.whitespace);

    // Parse lines
    var lines = std.ArrayListUnmanaged(Line){};
    defer lines.deinit(allocator);

    var line_iter = std.mem.splitScalar(u8, content, '\n');
    while (line_iter.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r\n");
        if (trimmed.len == 0) continue;

        // Parse "x1,y1 -> x2,y2"
        var parts = std.mem.splitSequence(u8, trimmed, " -> ");
        const start = parts.next() orelse continue;
        const end = parts.next() orelse continue;

        var start_parts = std.mem.splitScalar(u8, start, ',');
        const x1 = try parseInt(start_parts.next() orelse continue);
        const y1 = try parseInt(start_parts.next() orelse continue);

        var end_parts = std.mem.splitScalar(u8, end, ',');
        const x2 = try parseInt(end_parts.next() orelse continue);
        const y2 = try parseInt(end_parts.next() orelse continue);

        try lines.append(allocator, Line{ .x1 = x1, .y1 = y1, .x2 = x2, .y2 = y2 });
    }

    const part1 = try countOverlaps(lines.items, false, allocator);
    const part2 = try countOverlaps(lines.items, true, allocator);

    std.debug.print("Part 1: {d}\n", .{part1});
    std.debug.print("Part 2: {d}\n", .{part2});
}
