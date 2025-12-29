const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const input = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(input);

    var part1_sum: i64 = 0;
    var part2_sum: i64 = 0;

    var lines = std.mem.splitScalar(u8, std.mem.trim(u8, input, &std.ascii.whitespace), '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        var numbers: std.ArrayListUnmanaged(i64) = .empty;
        defer numbers.deinit(allocator);

        var tokens = std.mem.tokenizeAny(u8, line, " \t");
        while (tokens.next()) |token| {
            try numbers.append(allocator, try std.fmt.parseInt(i64, token, 10));
        }

        // Part 1: extrapolate next value
        part1_sum += try extrapolate(allocator, numbers.items);

        // Part 2: extrapolate previous value by reversing the sequence
        std.mem.reverse(i64, numbers.items);
        part2_sum += try extrapolate(allocator, numbers.items);
    }

    std.debug.print("Part 1: {d}\n", .{part1_sum});
    std.debug.print("Part 2: {d}\n", .{part2_sum});
}

fn extrapolate(allocator: std.mem.Allocator, seq: []const i64) !i64 {
    var sequences: std.ArrayListUnmanaged(std.ArrayListUnmanaged(i64)) = .empty;
    defer {
        for (sequences.items) |*s| s.deinit(allocator);
        sequences.deinit(allocator);
    }

    // Start with a copy of the input sequence
    var initial: std.ArrayListUnmanaged(i64) = .empty;
    try initial.appendSlice(allocator, seq);
    try sequences.append(allocator, initial);

    // Build difference sequences until all zeros
    while (!allZeros(lastSeq(&sequences))) {
        const current = lastSeq(&sequences);
        var diff: std.ArrayListUnmanaged(i64) = .empty;
        for (current[0 .. current.len - 1], current[1..]) |a, b| {
            try diff.append(allocator, b - a);
        }
        try sequences.append(allocator, diff);
    }

    // Extrapolate by summing last elements from bottom up
    var extrapolated: i64 = 0;
    var i = sequences.items.len;
    while (i > 0) {
        i -= 1;
        const items = sequences.items[i].items;
        extrapolated += items[items.len - 1];
    }

    return extrapolated;
}

fn lastSeq(sequences: *std.ArrayListUnmanaged(std.ArrayListUnmanaged(i64))) []const i64 {
    return sequences.items[sequences.items.len - 1].items;
}

fn allZeros(seq: []const i64) bool {
    for (seq) |val| {
        if (val != 0) return false;
    }
    return true;
}
