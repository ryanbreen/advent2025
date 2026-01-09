const std = @import("std");

const Range = struct {
    start: u32,
    end: u32,
};

const Pair = struct {
    first: Range,
    second: Range,
};

fn fullyContains(pair: Pair) bool {
    // Check if one range fully contains the other
    const first_contains_second = pair.first.start <= pair.second.start and pair.first.end >= pair.second.end;
    const second_contains_first = pair.second.start <= pair.first.start and pair.second.end >= pair.first.end;
    return first_contains_second or second_contains_first;
}

fn overlaps(pair: Pair) bool {
    // Check if ranges overlap at all
    return pair.first.start <= pair.second.end and pair.second.start <= pair.first.end;
}

fn parseRange(s: []const u8) !Range {
    var iter = std.mem.splitScalar(u8, s, '-');
    const start_str = iter.next() orelse return error.InvalidFormat;
    const end_str = iter.next() orelse return error.InvalidFormat;
    return Range{
        .start = try std.fmt.parseInt(u32, start_str, 10),
        .end = try std.fmt.parseInt(u32, end_str, 10),
    };
}

fn parseLine(line: []const u8) !Pair {
    var iter = std.mem.splitScalar(u8, line, ',');
    const first_str = iter.next() orelse return error.InvalidFormat;
    const second_str = iter.next() orelse return error.InvalidFormat;
    return Pair{
        .first = try parseRange(first_str),
        .second = try parseRange(second_str),
    };
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Parse pairs and count results
    var part1_count: u32 = 0;
    var part2_count: u32 = 0;

    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        const pair = try parseLine(line);

        if (fullyContains(pair)) {
            part1_count += 1;
        }
        if (overlaps(pair)) {
            part2_count += 1;
        }
    }

    const stdout = std.fs.File.stdout();
    var buffer: [256]u8 = undefined;
    var file_writer = stdout.writer(&buffer);
    const writer = &file_writer.interface;
    try writer.print("Part 1: {d}\n", .{part1_count});
    try writer.print("Part 2: {d}\n", .{part2_count});
    try writer.flush();
}
