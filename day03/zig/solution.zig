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

    // Part 1
    const part1_result = try part1(allocator, input);
    std.debug.print("Part 1: {d}\n", .{part1_result});

    // Part 2
    const part2_result = try part2(allocator, input);
    std.debug.print("Part 2: {d}\n", .{part2_result});
}

fn part1(allocator: std.mem.Allocator, input: []const u8) !u64 {
    var total: u64 = 0;

    var lines = std.mem.splitScalar(u8, input, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        const n = line.len;

        // Precompute max suffix: max_suffix[i] = max digit from position i to end
        var max_suffix = try allocator.alloc(u8, n);
        defer allocator.free(max_suffix);

        max_suffix[n - 1] = line[n - 1] - '0';
        var i: usize = n - 1;
        while (i > 0) {
            i -= 1;
            const digit = line[i] - '0';
            max_suffix[i] = @max(digit, max_suffix[i + 1]);
        }

        var max_joltage: u64 = 0;

        // For each possible first battery position
        for (0..n - 1) |pos| {
            const first_digit = line[pos] - '0';
            // The maximum second digit is the max from position pos+1 onwards
            const max_second = max_suffix[pos + 1];
            const joltage = @as(u64, first_digit) * 10 + max_second;
            max_joltage = @max(max_joltage, joltage);
        }

        total += max_joltage;
    }

    return total;
}

fn part2(allocator: std.mem.Allocator, input: []const u8) !u64 {
    var total: u64 = 0;

    var lines = std.mem.splitScalar(u8, input, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        const n = line.len;
        const k = 12; // Select exactly 12 batteries

        // Greedy algorithm to select k digits that form the maximum number
        var result = try allocator.alloc(u8, k);
        defer allocator.free(result);

        var current_pos: usize = 0;

        for (0..k) |i| {
            // How many digits we still need to select after this one
            const remaining_needed = k - i - 1;
            // Latest position we can start searching from
            const search_end = n - remaining_needed;

            // Find the maximum digit in the valid range
            var max_digit: u8 = 0;
            var max_pos: usize = current_pos;

            for (current_pos..search_end) |j| {
                const digit = line[j] - '0';
                if (digit > max_digit) {
                    max_digit = digit;
                    max_pos = j;
                }
            }

            result[i] = max_digit;
            current_pos = max_pos + 1;
        }

        // Convert result to u64
        var joltage: u64 = 0;
        for (result) |digit| {
            joltage = joltage * 10 + digit;
        }

        total += joltage;
    }

    return total;
}
