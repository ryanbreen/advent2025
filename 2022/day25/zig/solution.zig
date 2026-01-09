const std = @import("std");

fn snafuToDecimal(s: []const u8) i64 {
    var result: i64 = 0;
    for (s) |c| {
        const digit_value: i64 = switch (c) {
            '2' => 2,
            '1' => 1,
            '0' => 0,
            '-' => -1,
            '=' => -2,
            else => 0,
        };
        result = result * 5 + digit_value;
    }
    return result;
}

fn decimalToSnafu(n: i64, buffer: []u8) []const u8 {
    if (n == 0) {
        buffer[0] = '0';
        return buffer[0..1];
    }

    var digits: [32]u8 = undefined;
    var digit_count: usize = 0;
    var num = n;

    while (num != 0) {
        const remainder: i64 = @mod(num, 5);
        if (remainder <= 2) {
            digits[digit_count] = @intCast('0' + @as(u8, @intCast(remainder)));
            num = @divFloor(num, 5);
        } else if (remainder == 3) {
            digits[digit_count] = '=';
            num = @divFloor(num, 5) + 1;
        } else { // remainder == 4
            digits[digit_count] = '-';
            num = @divFloor(num, 5) + 1;
        }
        digit_count += 1;
    }

    // Reverse the digits into the output buffer
    for (0..digit_count) |i| {
        buffer[i] = digits[digit_count - 1 - i];
    }

    return buffer[0..digit_count];
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    // Read input file
    const file = std.fs.cwd().openFile("../input.txt", .{}) catch |err| {
        std.debug.print("Failed to open ../input.txt: {}\n", .{err});
        return err;
    };
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Parse and sum all SNAFU numbers
    var total: i64 = 0;
    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, &[_]u8{ '\r', ' ' });
        if (trimmed.len > 0) {
            total += snafuToDecimal(trimmed);
        }
    }

    // Convert back to SNAFU
    var snafu_buffer: [32]u8 = undefined;
    const result = decimalToSnafu(total, &snafu_buffer);

    // Use posix write to stdout
    const stdout_fd = std.posix.STDOUT_FILENO;
    var buf1: [64]u8 = undefined;
    const msg1 = std.fmt.bufPrint(&buf1, "Part 1: {s}\n", .{result}) catch unreachable;
    _ = std.posix.write(stdout_fd, msg1) catch {};

    var buf2: [64]u8 = undefined;
    const msg2 = std.fmt.bufPrint(&buf2, "Part 2: No Part 2 on Day 25!\n", .{}) catch unreachable;
    _ = std.posix.write(stdout_fd, msg2) catch {};
}
