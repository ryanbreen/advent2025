const std = @import("std");

fn priority(char: u8) u32 {
    if (char >= 'a' and char <= 'z') {
        return char - 'a' + 1;
    } else {
        return char - 'A' + 27;
    }
}

fn charToIndex(char: u8) usize {
    if (char >= 'a' and char <= 'z') {
        return char - 'a';
    } else {
        return char - 'A' + 26;
    }
}

fn part1(lines: []const []const u8) u32 {
    var total: u32 = 0;

    for (lines) |line| {
        const mid = line.len / 2;
        const first = line[0..mid];
        const second = line[mid..];

        // Build set of chars in first half
        var first_set: [52]bool = [_]bool{false} ** 52;
        for (first) |c| {
            first_set[charToIndex(c)] = true;
        }

        // Find common char in second half
        for (second) |c| {
            if (first_set[charToIndex(c)]) {
                total += priority(c);
                break;
            }
        }
    }

    return total;
}

fn part2(lines: []const []const u8) u32 {
    var total: u32 = 0;
    var i: usize = 0;

    while (i + 3 <= lines.len) : (i += 3) {
        const line1 = lines[i];
        const line2 = lines[i + 1];
        const line3 = lines[i + 2];

        // Build sets for first two lines
        var set1: [52]bool = [_]bool{false} ** 52;
        var set2: [52]bool = [_]bool{false} ** 52;

        for (line1) |c| {
            set1[charToIndex(c)] = true;
        }
        for (line2) |c| {
            set2[charToIndex(c)] = true;
        }

        // Find char common to all three
        for (line3) |c| {
            const idx = charToIndex(c);
            if (set1[idx] and set2[idx]) {
                total += priority(c);
                break;
            }
        }
    }

    return total;
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

    // Parse lines
    var lines: std.ArrayList([]const u8) = .{};
    defer lines.deinit(allocator);

    var line_iter = std.mem.splitScalar(u8, buffer, '\n');
    while (line_iter.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \r");
        if (trimmed.len > 0) {
            try lines.append(allocator, trimmed);
        }
    }

    const lines_slice = lines.items;

    std.debug.print("Part 1: {d}\n", .{part1(lines_slice)});
    std.debug.print("Part 2: {d}\n", .{part2(lines_slice)});
}
