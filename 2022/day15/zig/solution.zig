const std = @import("std");

const Sensor = struct {
    sx: i64,
    sy: i64,
    bx: i64,
    by: i64,
    dist: i64,
};

const Range = struct {
    start: i64,
    end: i64,

    fn lessThan(_: void, a: Range, b: Range) bool {
        return a.start < b.start;
    }
};

fn abs(x: i64) i64 {
    return if (x < 0) -x else x;
}

fn parseSensors(allocator: std.mem.Allocator, text: []const u8) !std.ArrayListUnmanaged(Sensor) {
    var sensors: std.ArrayListUnmanaged(Sensor) = .empty;

    var lines = std.mem.splitScalar(u8, text, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        // Parse: "Sensor at x=3772068, y=2853720: closest beacon is at x=4068389, y=2345925"
        var sx: i64 = 0;
        var sy: i64 = 0;
        var bx: i64 = 0;
        var by: i64 = 0;

        var idx: usize = 0;
        var num_count: usize = 0;

        while (idx < line.len and num_count < 4) {
            // Find start of number (digit or minus sign)
            while (idx < line.len and line[idx] != '-' and !std.ascii.isDigit(line[idx])) {
                idx += 1;
            }
            if (idx >= line.len) break;

            // Parse the number
            var end = idx;
            if (line[end] == '-') end += 1;
            while (end < line.len and std.ascii.isDigit(line[end])) {
                end += 1;
            }

            const num = std.fmt.parseInt(i64, line[idx..end], 10) catch break;
            switch (num_count) {
                0 => sx = num,
                1 => sy = num,
                2 => bx = num,
                3 => by = num,
                else => {},
            }
            num_count += 1;
            idx = end;
        }

        if (num_count == 4) {
            const dist = abs(sx - bx) + abs(sy - by);
            try sensors.append(allocator, .{ .sx = sx, .sy = sy, .bx = bx, .by = by, .dist = dist });
        }
    }

    return sensors;
}

fn mergeRanges(allocator: std.mem.Allocator, ranges: []Range) !std.ArrayListUnmanaged(Range) {
    var merged: std.ArrayListUnmanaged(Range) = .empty;

    if (ranges.len == 0) return merged;

    std.mem.sort(Range, ranges, {}, Range.lessThan);

    try merged.append(allocator, ranges[0]);

    for (ranges[1..]) |r| {
        const last = &merged.items[merged.items.len - 1];
        if (r.start <= last.end + 1) {
            last.end = @max(last.end, r.end);
        } else {
            try merged.append(allocator, r);
        }
    }

    return merged;
}

fn getCoverageAtRow(allocator: std.mem.Allocator, sensors: []const Sensor, row: i64) !std.ArrayListUnmanaged(Range) {
    var ranges: std.ArrayListUnmanaged(Range) = .empty;
    defer ranges.deinit(allocator);

    for (sensors) |s| {
        const row_dist = abs(s.sy - row);
        if (row_dist > s.dist) continue;

        const x_spread = s.dist - row_dist;
        try ranges.append(allocator, .{ .start = s.sx - x_spread, .end = s.sx + x_spread });
    }

    return mergeRanges(allocator, ranges.items);
}

fn part1(allocator: std.mem.Allocator, sensors: []const Sensor) !i64 {
    const target_row: i64 = 2000000;

    var merged = try getCoverageAtRow(allocator, sensors, target_row);
    defer merged.deinit(allocator);

    var total: i64 = 0;
    for (merged.items) |r| {
        total += r.end - r.start + 1;
    }

    // Subtract beacons on this row
    var beacon_set = std.AutoHashMap(i64, void).init(allocator);
    defer beacon_set.deinit();

    for (sensors) |s| {
        if (s.by == target_row) {
            try beacon_set.put(s.bx, {});
        }
    }

    return total - @as(i64, @intCast(beacon_set.count()));
}

fn part2(allocator: std.mem.Allocator, sensors: []const Sensor) !i64 {
    const max_coord: i64 = 4000000;

    var row: i64 = 0;
    while (row <= max_coord) : (row += 1) {
        var merged = try getCoverageAtRow(allocator, sensors, row);
        defer merged.deinit(allocator);

        // Clip ranges to search area
        var clipped: std.ArrayListUnmanaged(Range) = .empty;
        defer clipped.deinit(allocator);

        for (merged.items) |r| {
            if (r.end < 0 or r.start > max_coord) continue;
            try clipped.append(allocator, .{
                .start = @max(0, r.start),
                .end = @min(max_coord, r.end),
            });
        }

        var clipped_merged = try mergeRanges(allocator, clipped.items);
        defer clipped_merged.deinit(allocator);

        // Check if full row is covered
        if (clipped_merged.items.len == 1 and
            clipped_merged.items[0].start == 0 and
            clipped_merged.items[0].end == max_coord)
        {
            continue;
        }

        // Found a gap
        var x: i64 = undefined;
        if (clipped_merged.items.len > 1) {
            x = clipped_merged.items[0].end + 1;
        } else if (clipped_merged.items[0].start > 0) {
            x = 0;
        } else {
            x = clipped_merged.items[0].end + 1;
        }

        return x * 4000000 + row;
    }

    return 0;
}

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const input = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(input);

    const text = std.mem.trim(u8, input, "\n\r ");

    var sensors = try parseSensors(allocator, text);
    defer sensors.deinit(allocator);

    const p1 = try part1(allocator, sensors.items);
    const p2 = try part2(allocator, sensors.items);

    std.debug.print("Part 1: {d}\n", .{p1});
    std.debug.print("Part 2: {d}\n", .{p2});
}
