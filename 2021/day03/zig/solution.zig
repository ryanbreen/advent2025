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

    // Parse binary strings into array
    var lines = std.ArrayListUnmanaged([]const u8){};
    defer lines.deinit(allocator);

    var line_iter = std.mem.splitScalar(u8, input, '\n');
    while (line_iter.next()) |line| {
        const trimmed = std.mem.trim(u8, line, &[_]u8{ '\r', ' ' });
        if (trimmed.len == 0) continue;
        try lines.append(allocator, trimmed);
    }

    const numbers = lines.items;
    const num_bits = numbers[0].len;

    // Part 1: Calculate gamma and epsilon
    var gamma: u32 = 0;
    for (0..num_bits) |pos| {
        var ones: usize = 0;
        for (numbers) |line| {
            if (line[pos] == '1') ones += 1;
        }
        const zeros = numbers.len - ones;
        if (ones >= zeros) {
            gamma |= @as(u32, 1) << @intCast(num_bits - 1 - pos);
        }
    }
    const epsilon = gamma ^ ((@as(u32, 1) << @intCast(num_bits)) - 1);
    const part1_answer = gamma * epsilon;

    // Part 2: Find oxygen and CO2 ratings
    const oxygen = try findRating(allocator, numbers, num_bits, true);
    const co2 = try findRating(allocator, numbers, num_bits, false);
    const part2_answer = oxygen * co2;

    std.debug.print("Part 1: {d}\n", .{part1_answer});
    std.debug.print("Part 2: {d}\n", .{part2_answer});
}

fn findRating(allocator: std.mem.Allocator, numbers: []const []const u8, num_bits: usize, use_most_common: bool) !u32 {
    var candidates = std.ArrayListUnmanaged([]const u8){};
    defer candidates.deinit(allocator);

    // Copy initial numbers
    for (numbers) |n| {
        try candidates.append(allocator, n);
    }

    for (0..num_bits) |pos| {
        if (candidates.items.len == 1) break;

        // Count ones at this position
        var ones: usize = 0;
        for (candidates.items) |line| {
            if (line[pos] == '1') ones += 1;
        }
        const zeros = candidates.items.len - ones;

        // Determine target bit
        const target: u8 = if (use_most_common)
            (if (ones >= zeros) '1' else '0')
        else
            (if (zeros <= ones) '0' else '1');

        // Filter candidates in-place
        var write_idx: usize = 0;
        for (candidates.items) |line| {
            if (line[pos] == target) {
                candidates.items[write_idx] = line;
                write_idx += 1;
            }
        }
        candidates.shrinkRetainingCapacity(write_idx);
    }

    // Parse the remaining binary string
    var result: u32 = 0;
    for (candidates.items[0]) |c| {
        result = (result << 1) | @as(u32, if (c == '1') 1 else 0);
    }
    return result;
}
