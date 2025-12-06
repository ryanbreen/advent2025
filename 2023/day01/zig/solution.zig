//! Advent of Code 2023 - Day 1: Trebuchet?!
//! Calibration value extraction from lines containing digits and spelled-out numbers.
//! Assumes ASCII input.

const std = @import("std");

/// Mapping of spelled-out digit words to their numeric values
const DigitWord = struct {
    word: []const u8,
    value: u8,
};

/// Comptime array of digit word mappings for Part 2
const digit_words = [_]DigitWord{
    .{ .word = "one", .value = 1 },
    .{ .word = "two", .value = 2 },
    .{ .word = "three", .value = 3 },
    .{ .word = "four", .value = 4 },
    .{ .word = "five", .value = 5 },
    .{ .word = "six", .value = 6 },
    .{ .word = "seven", .value = 7 },
    .{ .word = "eight", .value = 8 },
    .{ .word = "nine", .value = 9 },
};

/// Check if a byte is an ASCII digit ('0'-'9')
inline fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

/// Convert ASCII digit character to its numeric value
inline fn digitValue(c: u8) u8 {
    return c - '0';
}

/// Part 1: Extract calibration values using only numeric digits
fn part1(input: []const u8) u32 {
    var total: u32 = 0;
    var lines = std.mem.splitScalar(u8, input, '\n');

    while (lines.next()) |line| {
        if (line.len == 0) continue;

        var first_digit: ?u8 = null;
        var last_digit: u8 = 0;

        // Scan line for ASCII digits
        for (line) |c| {
            if (isDigit(c)) {
                const digit = digitValue(c);
                if (first_digit == null) {
                    first_digit = digit;
                }
                last_digit = digit;
            }
        }

        // Combine first and last digits to form calibration value
        if (first_digit) |first| {
            total += first * 10 + last_digit;
        }
    }

    return total;
}

/// Part 2: Extract calibration values using both numeric digits and spelled-out digit words
fn part2(input: []const u8) u32 {
    var total: u32 = 0;
    var lines = std.mem.splitScalar(u8, input, '\n');

    while (lines.next()) |line| {
        if (line.len == 0) continue;

        var first_digit: ?u8 = null;
        var last_digit: u8 = 0;

        // Scan each position in the line
        var i: usize = 0;
        while (i < line.len) : (i += 1) {
            // Try to extract a digit at current position (numeric or spelled-out)
            const digit: ?u8 = blk: {
                // Check for ASCII digit first (more common, faster path)
                if (isDigit(line[i])) {
                    break :blk digitValue(line[i]);
                }

                // Check for spelled-out digit words
                inline for (digit_words) |entry| {
                    if (std.mem.startsWith(u8, line[i..], entry.word)) {
                        break :blk entry.value;
                    }
                }

                break :blk null;
            };

            // Track first and last digit found
            if (digit) |d| {
                if (first_digit == null) {
                    first_digit = d;
                }
                last_digit = d;
            }
        }

        // Combine first and last digits to form calibration value
        if (first_digit) |first| {
            total += first * 10 + last_digit;
        }
    }

    return total;
}

pub fn main() !void {
    // Use ArenaAllocator for simple one-shot allocation pattern
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const file_stat = try file.stat();
    const file_size = file_stat.size;

    // Handle empty file case
    if (file_size == 0) {
        return error.EmptyInputFile;
    }

    const buffer = try allocator.alloc(u8, file_size);
    const bytes_read = try file.readAll(buffer);
    const input = buffer[0..bytes_read];

    // Calculate and print results
    const result1 = part1(input);
    const result2 = part2(input);

    // In Zig 0.15.2, std.debug.print is the idiomatic way to print to stderr
    // For production code, use std.fmt.format with explicit stdout file handle
    std.debug.print("Part 1: {d}\n", .{result1});
    std.debug.print("Part 2: {d}\n", .{result2});
}
