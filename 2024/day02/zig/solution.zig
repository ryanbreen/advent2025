const std = @import("std");

const Report = struct {
    levels: [100]i32,
    count: usize,
};

fn isSafeReport(levels: []const i32) bool {
    if (levels.len < 2) return true;

    const increasing = levels[1] > levels[0];

    var i: usize = 0;
    while (i < levels.len - 1) : (i += 1) {
        const diff = levels[i + 1] - levels[i];
        const abs_diff = @abs(diff);

        // Check if difference is in valid range [1, 3]
        if (abs_diff < 1 or abs_diff > 3) {
            return false;
        }

        // Check if direction is consistent
        if ((increasing and diff <= 0) or (!increasing and diff >= 0)) {
            return false;
        }
    }

    return true;
}

fn isSafeWithDampener(levels: []const i32, allocator: std.mem.Allocator) !bool {
    // First check if already safe
    if (isSafeReport(levels)) {
        return true;
    }

    // Try removing each level one at a time
    var skip: usize = 0;
    while (skip < levels.len) : (skip += 1) {
        var temp = try allocator.alloc(i32, levels.len - 1);
        defer allocator.free(temp);

        var temp_idx: usize = 0;
        var i: usize = 0;
        while (i < levels.len) : (i += 1) {
            if (i != skip) {
                temp[temp_idx] = levels[i];
                temp_idx += 1;
            }
        }

        if (isSafeReport(temp)) {
            return true;
        }
    }

    return false;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var reports: std.ArrayList(Report) = .{};
    defer reports.deinit(allocator);

    var lines = std.mem.tokenizeScalar(u8, content, '\n');
    while (lines.next()) |line| {
        var report = Report{
            .levels = undefined,
            .count = 0,
        };

        var numbers = std.mem.tokenizeScalar(u8, line, ' ');
        while (numbers.next()) |num_str| {
            const num = try std.fmt.parseInt(i32, num_str, 10);
            report.levels[report.count] = num;
            report.count += 1;
        }

        if (report.count > 0) {
            try reports.append(allocator, report);
        }
    }

    // Part 1: Count safe reports
    var safe_count: usize = 0;
    for (reports.items) |report| {
        const levels = report.levels[0..report.count];
        if (isSafeReport(levels)) {
            safe_count += 1;
        }
    }

    std.debug.print("Part 1: {d}\n", .{safe_count});

    // Part 2: Count safe reports with dampener
    var safe_with_dampener: usize = 0;
    for (reports.items) |report| {
        const levels = report.levels[0..report.count];
        if (try isSafeWithDampener(levels, allocator)) {
            safe_with_dampener += 1;
        }
    }

    std.debug.print("Part 2: {d}\n", .{safe_with_dampener});
}
