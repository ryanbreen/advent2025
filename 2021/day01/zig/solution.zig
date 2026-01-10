const std = @import("std");

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

    // Trim trailing whitespace
    const input = std.mem.trim(u8, buffer, &std.ascii.whitespace);

    // Parse depths into array
    var depths = std.ArrayListUnmanaged(i32){};
    defer depths.deinit(allocator);

    var lines = std.mem.splitScalar(u8, input, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, &[_]u8{ '\r', ' ' });
        if (trimmed.len == 0) continue;
        const depth = try std.fmt.parseInt(i32, trimmed, 10);
        try depths.append(allocator, depth);
    }

    const items = depths.items;

    // Part 1: Count increases from previous measurement
    var part1: u32 = 0;
    for (1..items.len) |i| {
        if (items[i] > items[i - 1]) {
            part1 += 1;
        }
    }

    // Part 2: Count increases in 3-measurement sliding window sums
    // Comparing sum(i-3, i-2, i-1) with sum(i-2, i-1, i):
    // The middle two elements cancel out, so we just compare items[i] > items[i-3]
    var part2: u32 = 0;
    if (items.len >= 4) {
        for (3..items.len) |i| {
            if (items[i] > items[i - 3]) {
                part2 += 1;
            }
        }
    }

    std.debug.print("Part 1: {d}\n", .{part1});
    std.debug.print("Part 2: {d}\n", .{part2});
}
