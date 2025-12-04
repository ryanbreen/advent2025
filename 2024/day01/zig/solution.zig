const std = @import("std");

fn compareInts(_: void, a: i32, b: i32) bool {
    return a < b;
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

    var left_list: std.ArrayList(i32) = .{};
    defer left_list.deinit(allocator);
    var right_list: std.ArrayList(i32) = .{};
    defer right_list.deinit(allocator);

    // Parse input
    var lines = std.mem.splitScalar(u8, buffer, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        var parts = std.mem.splitScalar(u8, line, ' ');
        var nums: std.ArrayList(i32) = .{};
        defer nums.deinit(allocator);

        while (parts.next()) |part| {
            if (part.len == 0) continue;
            const num = try std.fmt.parseInt(i32, part, 10);
            try nums.append(allocator, num);
        }

        if (nums.items.len == 2) {
            try left_list.append(allocator, nums.items[0]);
            try right_list.append(allocator, nums.items[1]);
        }
    }

    // Part 1: Sort both lists and calculate total distance
    const left_sorted = try allocator.dupe(i32, left_list.items);
    defer allocator.free(left_sorted);
    const right_sorted = try allocator.dupe(i32, right_list.items);
    defer allocator.free(right_sorted);

    std.mem.sort(i32, left_sorted, {}, compareInts);
    std.mem.sort(i32, right_sorted, {}, compareInts);

    var part1_sum: i64 = 0;
    for (left_sorted, right_sorted) |l, r| {
        const diff = l - r;
        part1_sum += if (diff > 0) diff else -diff;
    }

    // Part 2: Calculate similarity score
    var part2_sum: i64 = 0;
    for (left_list.items) |num| {
        var count: i32 = 0;
        for (right_list.items) |r| {
            if (r == num) count += 1;
        }
        part2_sum += @as(i64, num) * @as(i64, count);
    }

    std.debug.print("Part 1: {d}\n", .{part1_sum});
    std.debug.print("Part 2: {d}\n", .{part2_sum});
}
