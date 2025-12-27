const std = @import("std");

/// Represents a mapping range from source to destination in the almanac.
/// Values in [src_start, src_start + length) map to [dst_start, dst_start + length).
const Range = struct {
    dst_start: u64,
    src_start: u64,
    length: u64,

    /// Returns the end of the source range (exclusive).
    pub inline fn srcEnd(self: Range) u64 {
        return self.src_start + self.length;
    }

    /// Attempts to map a value through this range.
    /// Returns the mapped value if within range, null otherwise.
    pub inline fn mapValue(self: Range, value: u64) ?u64 {
        if (value >= self.src_start and value < self.srcEnd()) {
            return self.dst_start + (value - self.src_start);
        }
        return null;
    }
};

/// Represents a contiguous range of seed values [start, end).
const SeedRange = struct {
    start: u64,
    end: u64,

    /// Creates a new seed range from start and length.
    pub inline fn fromStartAndLength(start: u64, length: u64) SeedRange {
        return .{ .start = start, .end = start + length };
    }

    /// Returns true if this range is non-empty.
    pub inline fn isValid(self: SeedRange) bool {
        return self.start < self.end;
    }
};

/// Parsed input data containing seeds and mapping chains.
const ParsedInput = struct {
    seeds: []u64,
    maps: [][]Range,
    allocator: std.mem.Allocator,

    /// Frees all allocated memory.
    pub fn deinit(self: *ParsedInput) void {
        self.allocator.free(self.seeds);
        for (self.maps) |m| {
            self.allocator.free(m);
        }
        self.allocator.free(self.maps);
    }
};

/// Parses the almanac input text into seeds and mapping chains.
fn parseInput(allocator: std.mem.Allocator, text: []const u8) !ParsedInput {
    var sections = std.mem.splitSequence(u8, text, "\n\n");

    // Parse seeds line: "seeds: 79 14 55 13"
    const seeds_section = sections.next() orelse return error.InvalidInput;
    const colon_pos = std.mem.indexOf(u8, seeds_section, ": ") orelse return error.InvalidInput;
    const seeds_str = seeds_section[colon_pos + 2 ..];

    var seeds_list: std.ArrayListUnmanaged(u64) = .empty;
    defer seeds_list.deinit(allocator);

    var seeds_iter = std.mem.tokenizeAny(u8, seeds_str, " \n");
    while (seeds_iter.next()) |seed_str| {
        try seeds_list.append(allocator, try std.fmt.parseInt(u64, seed_str, 10));
    }

    // Parse mapping sections
    var maps_list: std.ArrayListUnmanaged([]Range) = .empty;
    errdefer {
        for (maps_list.items) |m| allocator.free(m);
        maps_list.deinit(allocator);
    }

    while (sections.next()) |section| {
        if (section.len == 0) continue;

        var lines = std.mem.splitScalar(u8, section, '\n');
        _ = lines.next(); // Skip header line (e.g., "seed-to-soil map:")

        var ranges_list: std.ArrayListUnmanaged(Range) = .empty;
        defer ranges_list.deinit(allocator);

        while (lines.next()) |line| {
            if (line.len == 0) continue;

            var nums = std.mem.tokenizeScalar(u8, line, ' ');
            const dst_start = try std.fmt.parseInt(u64, nums.next() orelse continue, 10);
            const src_start = try std.fmt.parseInt(u64, nums.next() orelse continue, 10);
            const length = try std.fmt.parseInt(u64, nums.next() orelse continue, 10);

            try ranges_list.append(allocator, .{
                .dst_start = dst_start,
                .src_start = src_start,
                .length = length,
            });
        }

        try maps_list.append(allocator, try ranges_list.toOwnedSlice(allocator));
    }

    return .{
        .seeds = try seeds_list.toOwnedSlice(allocator),
        .maps = try maps_list.toOwnedSlice(allocator),
        .allocator = allocator,
    };
}

/// Applies a single mapping layer to a value.
/// Returns the mapped value, or the original if no range matches.
fn applyMap(value: u64, ranges: []const Range) u64 {
    for (ranges) |r| {
        if (r.mapValue(value)) |mapped| {
            return mapped;
        }
    }
    return value;
}

/// Traces a seed through all mapping layers to get its final location.
fn seedToLocation(seed: u64, maps: []const []Range) u64 {
    var value = seed;
    for (maps) |map_ranges| {
        value = applyMap(value, map_ranges);
    }
    return value;
}

/// Part 1: Find the lowest location for individual seed values.
fn part1(seeds: []const u64, maps: []const []Range) u64 {
    var min_loc: u64 = std.math.maxInt(u64);
    for (seeds) |seed| {
        min_loc = @min(min_loc, seedToLocation(seed, maps));
    }
    return min_loc;
}

/// Applies a mapping layer to a set of seed ranges, splitting and transforming as needed.
fn applyMapToRanges(
    allocator: std.mem.Allocator,
    input_ranges: []const SeedRange,
    map_ranges: []const Range,
) ![]SeedRange {
    var result: std.ArrayListUnmanaged(SeedRange) = .empty;
    errdefer result.deinit(allocator);

    for (input_ranges) |in_range| {
        var remaining: std.ArrayListUnmanaged(SeedRange) = .empty;
        defer remaining.deinit(allocator);
        try remaining.append(allocator, in_range);

        for (map_ranges) |r| {
            var new_remaining: std.ArrayListUnmanaged(SeedRange) = .empty;
            defer new_remaining.deinit(allocator);

            for (remaining.items) |rem| {
                // Part before the map range (stays unmapped for now)
                if (rem.start < r.src_start) {
                    const before = SeedRange{
                        .start = rem.start,
                        .end = @min(rem.end, r.src_start),
                    };
                    if (before.isValid()) try new_remaining.append(allocator, before);
                }

                // Overlapping part (gets mapped)
                const overlap_start = @max(rem.start, r.src_start);
                const overlap_end = @min(rem.end, r.srcEnd());
                if (overlap_start < overlap_end) {
                    const offset = r.dst_start -% r.src_start;
                    try result.append(allocator, .{
                        .start = overlap_start +% offset,
                        .end = overlap_end +% offset,
                    });
                }

                // Part after the map range (stays unmapped for now)
                if (rem.end > r.srcEnd()) {
                    const after = SeedRange{
                        .start = @max(rem.start, r.srcEnd()),
                        .end = rem.end,
                    };
                    if (after.isValid()) try new_remaining.append(allocator, after);
                }
            }

            remaining.clearRetainingCapacity();
            try remaining.appendSlice(allocator, new_remaining.items);
        }

        // Any remaining parts pass through unmapped (identity)
        try result.appendSlice(allocator, remaining.items);
    }

    return try result.toOwnedSlice(allocator);
}

/// Part 2: Find the lowest location treating seed pairs as ranges.
fn part2(allocator: std.mem.Allocator, seeds: []const u64, maps: []const []Range) !u64 {
    // Convert seed pairs to ranges
    var initial_ranges: std.ArrayListUnmanaged(SeedRange) = .empty;
    defer initial_ranges.deinit(allocator);

    var i: usize = 0;
    while (i + 1 < seeds.len) : (i += 2) {
        try initial_ranges.append(allocator, SeedRange.fromStartAndLength(seeds[i], seeds[i + 1]));
    }

    // Apply each mapping layer successively
    var current_ranges = try initial_ranges.toOwnedSlice(allocator);
    defer allocator.free(current_ranges);

    for (maps) |map_ranges| {
        const new_ranges = try applyMapToRanges(allocator, current_ranges, map_ranges);
        allocator.free(current_ranges);
        current_ranges = new_ranges;
    }

    // Find minimum start across all resulting ranges
    var min_loc: u64 = std.math.maxInt(u64);
    for (current_ranges) |r| {
        min_loc = @min(min_loc, r.start);
    }

    return min_loc;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read and parse input
    const input = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(input);

    var parsed = try parseInput(allocator, input);
    defer parsed.deinit();

    // Solve and output results
    const result1 = part1(parsed.seeds, parsed.maps);
    const result2 = try part2(allocator, parsed.seeds, parsed.maps);

    std.debug.print("Part 1: {d}\n", .{result1});
    std.debug.print("Part 2: {d}\n", .{result2});
}
