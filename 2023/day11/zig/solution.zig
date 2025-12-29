const std = @import("std");

/// Represents a galaxy position in the grid.
const Galaxy = struct {
    row: usize,
    col: usize,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const input = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(input);

    // Parse lines
    var lines: std.ArrayListUnmanaged([]const u8) = .empty;
    defer lines.deinit(allocator);

    var line_iter = std.mem.splitScalar(u8, std.mem.trim(u8, input, &std.ascii.whitespace), '\n');
    while (line_iter.next()) |line| {
        if (line.len > 0) {
            try lines.append(allocator, line);
        }
    }

    const grid = lines.items;
    const rows = grid.len;
    const cols = if (rows > 0) grid[0].len else 0;

    // Find all galaxies
    var galaxies: std.ArrayListUnmanaged(Galaxy) = .empty;
    defer galaxies.deinit(allocator);

    for (grid, 0..) |line, r| {
        for (line, 0..) |ch, c| {
            if (ch == '#') {
                try galaxies.append(allocator, .{ .row = r, .col = c });
            }
        }
    }

    // Find empty rows using a boolean slice (more memory-efficient than HashMap)
    const empty_rows = try allocator.alloc(bool, rows);
    defer allocator.free(empty_rows);

    for (0..rows) |r| {
        // Use indexOfScalar to check if '#' exists in the row
        empty_rows[r] = std.mem.indexOfScalar(u8, grid[r], '#') == null;
    }

    // Find empty columns using a boolean slice
    const empty_cols = try allocator.alloc(bool, cols);
    defer allocator.free(empty_cols);

    for (0..cols) |c| {
        var has_galaxy = false;
        for (grid) |line| {
            if (line[c] == '#') {
                has_galaxy = true;
                break;
            }
        }
        empty_cols[c] = !has_galaxy;
    }

    // Calculate distances for Part 1 (expansion factor = 2)
    const part1 = calculateDistances(galaxies.items, empty_rows, empty_cols, 2);

    // Calculate distances for Part 2 (expansion factor = 1,000,000)
    const part2 = calculateDistances(galaxies.items, empty_rows, empty_cols, 1_000_000);

    std.debug.print("Part 1: {}\n", .{part1});
    std.debug.print("Part 2: {}\n", .{part2});
}

fn calculateDistances(
    galaxies: []const Galaxy,
    empty_rows: []const bool,
    empty_cols: []const bool,
    expansion_factor: u64,
) u64 {
    var total: u64 = 0;

    // Iterate over all pairs of galaxies
    for (0..galaxies.len) |i| {
        for (i + 1..galaxies.len) |j| {
            const g1 = galaxies[i];
            const g2 = galaxies[j];

            // Calculate row distance with expansion
            const min_r = @min(g1.row, g2.row);
            const max_r = @max(g1.row, g2.row);
            var row_dist: u64 = max_r - min_r;

            for (min_r..max_r) |r| {
                if (empty_rows[r]) {
                    // Each empty row/col expands by expansion_factor. We already counted it
                    // once in the base distance (max_r - min_r), so we add (expansion_factor - 1)
                    // additional steps to account for the expansion.
                    row_dist += expansion_factor - 1;
                }
            }

            // Calculate column distance with expansion
            const min_c = @min(g1.col, g2.col);
            const max_c = @max(g1.col, g2.col);
            var col_dist: u64 = max_c - min_c;

            for (min_c..max_c) |c| {
                if (empty_cols[c]) {
                    // Same logic: add (expansion_factor - 1) for each empty column crossed
                    col_dist += expansion_factor - 1;
                }
            }

            total += row_dist + col_dist;
        }
    }

    return total;
}
