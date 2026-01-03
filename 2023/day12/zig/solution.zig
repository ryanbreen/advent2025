const std = @import("std");

const MAX_PATTERN_LEN = 128;
const MAX_GROUPS = 64;
const MAX_RUN_LEN = 32;

// Pre-allocated memoization table as a flat array
// Using a sentinel value (max u64) to indicate "not computed"
const NOT_COMPUTED: u64 = std.math.maxInt(u64);

// Thread-local (in practice, global) state for the current line being processed
var g_pattern: []const u8 = undefined;
var g_groups: []const u8 = undefined;
var g_memo: [MAX_PATTERN_LEN * MAX_GROUPS * MAX_RUN_LEN]u64 = undefined;

fn resetMemo() void {
    @memset(&g_memo, NOT_COMPUTED);
}

inline fn memoIndex(pos: usize, group_idx: usize, current_run: usize) usize {
    return pos * (MAX_GROUPS * MAX_RUN_LEN) + group_idx * MAX_RUN_LEN + current_run;
}

fn countArrangements(pattern: []const u8, groups: []const u8) u64 {
    g_pattern = pattern;
    g_groups = groups;
    resetMemo();
    return dp(0, 0, 0);
}

fn dp(pos: usize, group_idx: usize, current_run: usize) u64 {
    // Base case: reached end of pattern
    if (pos == g_pattern.len) {
        // Valid if we've matched all groups and no partial run
        if (group_idx == g_groups.len and current_run == 0) {
            return 1;
        }
        // Or if we're on the last group and the run matches
        if (group_idx == g_groups.len - 1 and g_groups[group_idx] == current_run) {
            return 1;
        }
        return 0;
    }

    // Check memo
    const idx = memoIndex(pos, group_idx, current_run);
    if (g_memo[idx] != NOT_COMPUTED) {
        return g_memo[idx];
    }

    var result: u64 = 0;
    const char = g_pattern[pos];

    // Option 1: Place operational spring (.)
    if (char == '.' or char == '?') {
        if (current_run == 0) {
            // No active run, just move forward
            result += dp(pos + 1, group_idx, 0);
        } else if (group_idx < g_groups.len and g_groups[group_idx] == current_run) {
            // End current run if it matches expected group size
            result += dp(pos + 1, group_idx + 1, 0);
        }
        // Otherwise invalid (run doesn't match group)
    }

    // Option 2: Place damaged spring (#)
    if (char == '#' or char == '?') {
        if (group_idx < g_groups.len and current_run < g_groups[group_idx]) {
            // Can extend current run
            result += dp(pos + 1, group_idx, current_run + 1);
        }
        // Otherwise invalid (exceeds group size or no more groups)
    }

    g_memo[idx] = result;
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
        const count1 = countArrangements(parsed.pattern, parsed.groups);
        part1_total += count1;

        // Part 2: unfold and count
        const unfolded = unfold(parsed.pattern, parsed.groups, &unfolded_pattern_buf, &unfolded_groups_buf);
        const count2 = countArrangements(unfolded.pattern, unfolded.groups);
        part2_total += count2;
    }

    std.debug.print("Part 1: {d}\n", .{part1_total});
    std.debug.print("Part 2: {d}\n", .{part2_total});
}
