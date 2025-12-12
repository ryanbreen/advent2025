const std = @import("std");

const Region = struct {
    width: usize,
    height: usize,
    counts: []usize,

    fn deinit(self: Region, allocator: std.mem.Allocator) void {
        allocator.free(self.counts);
    }
};

const ParseResult = struct {
    shape_sizes: []usize,
    regions: []Region,

    fn deinit(self: ParseResult, allocator: std.mem.Allocator) void {
        allocator.free(self.shape_sizes);
        for (self.regions) |region| {
            region.deinit(allocator);
        }
        allocator.free(self.regions);
    }
};

fn parseInput(allocator: std.mem.Allocator, text: []const u8) !ParseResult {
    var shape_sizes = std.ArrayList(usize){};
    try shape_sizes.ensureTotalCapacity(allocator, 10);
    defer shape_sizes.deinit(allocator);

    var regions = std.ArrayList(Region){};
    errdefer {
        for (regions.items) |region| {
            region.deinit(allocator);
        }
        regions.deinit(allocator);
    }

    var lines = std.mem.splitScalar(u8, text, '\n');
    var in_shape = false;
    var current_shape_idx: usize = 0;
    var current_shape_cells: usize = 0;

    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \r\t");

        // Empty line - end of shape definition
        if (trimmed.len == 0) {
            if (in_shape) {
                // Ensure array is large enough
                while (shape_sizes.items.len <= current_shape_idx) {
                    try shape_sizes.append(allocator, 0);
                }
                shape_sizes.items[current_shape_idx] = current_shape_cells;
                in_shape = false;
            }
            continue;
        }

        // Check if it's a shape definition (has ':' but no 'x')
        if (std.mem.indexOf(u8, trimmed, ":")) |colon_idx| {
            if (std.mem.indexOf(u8, trimmed, "x") == null) {
                // Shape header: "N:"
                const idx_str = trimmed[0..colon_idx];
                current_shape_idx = try std.fmt.parseInt(usize, idx_str, 10);
                current_shape_cells = 0;
                in_shape = true;
            } else {
                // Region definition: "WxH: c0 c1 c2 ..."
                const dims_str = trimmed[0..colon_idx];
                const x_idx = std.mem.indexOf(u8, dims_str, "x") orelse continue;

                const width = try std.fmt.parseInt(usize, dims_str[0..x_idx], 10);
                const height = try std.fmt.parseInt(usize, dims_str[x_idx + 1..], 10);

                const counts_str = std.mem.trim(u8, trimmed[colon_idx + 1..], " \r\t");
                var counts = std.ArrayList(usize){};
                errdefer counts.deinit(allocator);

                var tokens = std.mem.tokenizeScalar(u8, counts_str, ' ');
                while (tokens.next()) |token| {
                    const count = try std.fmt.parseInt(usize, token, 10);
                    try counts.append(allocator, count);
                }

                try regions.append(allocator, Region{
                    .width = width,
                    .height = height,
                    .counts = try counts.toOwnedSlice(allocator),
                });
            }
        } else if (in_shape) {
            // Shape body - count '#' characters
            for (trimmed) |c| {
                if (c == '#') current_shape_cells += 1;
            }
        }
    }

    // Handle last shape if file doesn't end with empty line
    if (in_shape) {
        while (shape_sizes.items.len <= current_shape_idx) {
            try shape_sizes.append(allocator, 0);
        }
        shape_sizes.items[current_shape_idx] = current_shape_cells;
    }

    return ParseResult{
        .shape_sizes = try shape_sizes.toOwnedSlice(allocator),
        .regions = try regions.toOwnedSlice(allocator),
    };
}

fn canFitRegion(region: Region, shape_sizes: []const usize) bool {
    var total_cells_needed: usize = 0;
    for (region.counts, 0..) |count, i| {
        if (i < shape_sizes.len) {
            total_cells_needed += count * shape_sizes[i];
        }
    }
    const available = region.width * region.height;
    return total_cells_needed <= available;
}

fn part1(shape_sizes: []const usize, regions: []const Region) usize {
    var count: usize = 0;
    for (regions) |region| {
        if (canFitRegion(region, shape_sizes)) {
            count += 1;
        }
    }
    return count;
}

fn part2() usize {
    return 0;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa.deinit();
        if (leaked == .leak) {
            std.debug.print("Memory leak detected!\n", .{});
        }
    }
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    const bytes_read = try file.readAll(buffer);

    const parsed = try parseInput(allocator, buffer[0..bytes_read]);
    defer parsed.deinit(allocator);

    std.debug.print("Part 1: {d}\n", .{part1(parsed.shape_sizes, parsed.regions)});
    std.debug.print("Part 2: {d}\n", .{part2()});
}
