const std = @import("std");
const ArrayList = std.ArrayList;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    const input = std.mem.trim(u8, content, "\n\r ");

    // Parse input - split by double newline
    var sections = std.mem.splitSequence(u8, input, "\n\n");
    const patterns_line = sections.next() orelse return error.InvalidInput;
    const designs_section = sections.next() orelse return error.InvalidInput;

    // Parse patterns
    var patterns_list: ArrayList([]const u8) = .{};
    defer patterns_list.deinit(allocator);

    var pattern_iter = std.mem.splitSequence(u8, patterns_line, ", ");
    while (pattern_iter.next()) |p| {
        const trimmed = std.mem.trim(u8, p, " \t\r");
        if (trimmed.len > 0) {
            try patterns_list.append(allocator, trimmed);
        }
    }
    const patterns = patterns_list.items;

    // Parse designs
    var designs_list: ArrayList([]const u8) = .{};
    defer designs_list.deinit(allocator);

    var design_iter = std.mem.splitScalar(u8, designs_section, '\n');
    while (design_iter.next()) |d| {
        const trimmed = std.mem.trim(u8, d, " \t\r");
        if (trimmed.len > 0) {
            try designs_list.append(allocator, trimmed);
        }
    }
    const designs = designs_list.items;

    // Part 1: Count designs that can be formed
    // Part 2: Sum the number of ways
    var part1: u64 = 0;
    var part2: u64 = 0;

    for (designs) |design| {
        const ways = try countWays(allocator, design, patterns);
        if (ways > 0) {
            part1 += 1;
        }
        part2 += ways;
    }

    const stdout = std.fs.File.stdout();
    var buf: [64]u8 = undefined;
    const output = std.fmt.bufPrint(&buf, "Part 1: {d}\nPart 2: {d}\n", .{ part1, part2 }) catch unreachable;
    _ = stdout.write(output) catch {};
}

/// Counts the number of ways to form the given design using the available patterns.
/// Uses bottom-up dynamic programming where dp[i] represents the number of ways
/// to form design[i..]. Returns 0 if the design cannot be formed.
fn countWays(allocator: std.mem.Allocator, design: []const u8, patterns: []const []const u8) !u64 {
    // dp[i] = number of ways to form design[i..]
    const dp = try allocator.alloc(u64, design.len + 1);
    defer allocator.free(dp);
    @memset(dp, 0);

    // Base case: empty suffix has exactly one way (use no patterns)
    dp[design.len] = 1;

    // Fill from right to left
    var i: usize = design.len;
    while (i > 0) {
        i -= 1;
        for (patterns) |pattern| {
            if (i + pattern.len <= design.len and
                std.mem.eql(u8, design[i..][0..pattern.len], pattern))
            {
                dp[i] += dp[i + pattern.len];
            }
        }
    }

    return dp[0];
}
