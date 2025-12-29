const std = @import("std");

const State = struct {
    pos: u16,
    group_idx: u8,
    current_run: u8,
};

fn countArrangements(pattern: []const u8, groups: []const u8, allocator: std.mem.Allocator) !u64 {
    var memo = std.AutoHashMap(State, u64).init(allocator);
    defer memo.deinit();

    return try dp(pattern, groups, 0, 0, 0, &memo);
}

fn dp(
    pattern: []const u8,
    groups: []const u8,
    pos: u16,
    group_idx: u8,
    current_run: u8,
    memo: *std.AutoHashMap(State, u64),
) !u64 {
    const state = State{ .pos = pos, .group_idx = group_idx, .current_run = current_run };

    // Check memo
    if (memo.get(state)) |cached| {
        return cached;
    }

    // Base case: reached end of pattern
    if (pos == pattern.len) {
        var result: u64 = 0;
        // Valid if we've matched all groups and no partial run
        if (group_idx == groups.len and current_run == 0) {
            result = 1;
        }
        // Or if we're on the last group and the run matches
        else if (group_idx == groups.len - 1 and groups[group_idx] == current_run) {
            result = 1;
        }
        try memo.put(state, result);
        return result;
    }

    var result: u64 = 0;
    const char = pattern[pos];

    // Option 1: Place operational spring (.)
    if (char == '.' or char == '?') {
        if (current_run == 0) {
            // No active run, just move forward
            result += try dp(pattern, groups, pos + 1, group_idx, 0, memo);
        } else if (group_idx < groups.len and groups[group_idx] == current_run) {
            // End current run if it matches expected group size
            result += try dp(pattern, groups, pos + 1, group_idx + 1, 0, memo);
        }
        // Otherwise invalid (run doesn't match group)
    }

    // Option 2: Place damaged spring (#)
    if (char == '#' or char == '?') {
        if (group_idx < groups.len and current_run < groups[group_idx]) {
            // Can extend current run
            result += try dp(pattern, groups, pos + 1, group_idx, current_run + 1, memo);
        }
        // Otherwise invalid (exceeds group size or no more groups)
    }

    try memo.put(state, result);
    return result;
}

fn parseLine(line: []const u8, groups_buf: []u8) !struct { pattern: []const u8, groups: []const u8 } {
    // Find the space separating pattern from groups
    const space_idx = std.mem.indexOfScalar(u8, line, ' ') orelse return error.InvalidFormat;

    const pattern = line[0..space_idx];
    const groups_str = line[space_idx + 1 ..];

    // Parse comma-separated numbers
    var group_count: usize = 0;
    var num: u8 = 0;
    var has_digit = false;

    for (groups_str) |c| {
        if (c >= '0' and c <= '9') {
            num = num * 10 + (c - '0');
            has_digit = true;
        } else if (c == ',') {
            if (has_digit) {
                groups_buf[group_count] = num;
                group_count += 1;
                num = 0;
                has_digit = false;
            }
        }
    }
    if (has_digit) {
        groups_buf[group_count] = num;
        group_count += 1;
    }

    return .{ .pattern = pattern, .groups = groups_buf[0..group_count] };
}

fn unfold(
    pattern: []const u8,
    groups: []const u8,
    unfolded_pattern_buf: []u8,
    unfolded_groups_buf: []u8,
) struct { pattern: []const u8, groups: []const u8 } {
    // Build unfolded pattern: pattern?pattern?pattern?pattern?pattern
    var pattern_len: usize = 0;
    for (0..5) |i| {
        if (i > 0) {
            unfolded_pattern_buf[pattern_len] = '?';
            pattern_len += 1;
        }
        @memcpy(unfolded_pattern_buf[pattern_len .. pattern_len + pattern.len], pattern);
        pattern_len += pattern.len;
    }

    // Build unfolded groups: groups repeated 5 times
    var groups_len: usize = 0;
    for (0..5) |_| {
        @memcpy(unfolded_groups_buf[groups_len .. groups_len + groups.len], groups);
        groups_len += groups.len;
    }

    return .{
        .pattern = unfolded_pattern_buf[0..pattern_len],
        .groups = unfolded_groups_buf[0..groups_len],
    };
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

    var part1_total: u64 = 0;
    var part2_total: u64 = 0;

    // Buffers for parsing
    var groups_buf: [64]u8 = undefined;
    var unfolded_pattern_buf: [1024]u8 = undefined;
    var unfolded_groups_buf: [320]u8 = undefined;

    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        // Remove potential carriage return
        const clean_line = if (line.len > 0 and line[line.len - 1] == '\r')
            line[0 .. line.len - 1]
        else
            line;

        if (clean_line.len == 0) continue;

        const parsed = try parseLine(clean_line, &groups_buf);

        // Part 1
        const count1 = try countArrangements(parsed.pattern, parsed.groups, allocator);
        part1_total += count1;

        // Part 2: unfold and count
        const unfolded = unfold(parsed.pattern, parsed.groups, &unfolded_pattern_buf, &unfolded_groups_buf);
        const count2 = try countArrangements(unfolded.pattern, unfolded.groups, allocator);
        part2_total += count2;
    }

    std.debug.print("Part 1: {d}\n", .{part1_total});
    std.debug.print("Part 2: {d}\n", .{part2_total});
}
