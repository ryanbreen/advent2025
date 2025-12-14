const std = @import("std");

const Point = struct {
    r: i32,
    c: i32,

    pub fn eql(self: Point, other: Point) bool {
        return self.r == other.r and self.c == other.c;
    }
};

const PointSet = std.AutoHashMap(Point, void);

const Region = struct {
    cells: PointSet,

    fn deinit(self: *Region) void {
        self.cells.deinit();
    }
};

fn findRegions(allocator: std.mem.Allocator, grid: [][]const u8, rows: usize, cols: usize) ![]Region {
    var visited = PointSet.init(allocator);
    defer visited.deinit();

    var regions_list: std.ArrayList(Region) = .{};
    defer regions_list.deinit(allocator);

    var r: i32 = 0;
    while (r < rows) : (r += 1) {
        var c: i32 = 0;
        while (c < cols) : (c += 1) {
            const pos = Point{ .r = r, .c = c };
            if (visited.contains(pos)) continue;

            // BFS to find all cells in this region
            const plant = grid[@intCast(r)][@intCast(c)];
            var region_cells = PointSet.init(allocator);
            var queue: std.ArrayList(Point) = .{};
            defer queue.deinit(allocator);

            try queue.append(allocator, pos);

            while (queue.items.len > 0) {
                const curr = queue.orderedRemove(0);
                if (visited.contains(curr)) continue;
                if (curr.r < 0 or curr.r >= rows or curr.c < 0 or curr.c >= cols) continue;
                if (grid[@intCast(curr.r)][@intCast(curr.c)] != plant) continue;

                try visited.put(curr, {});
                try region_cells.put(curr, {});

                const directions = [_]Point{
                    Point{ .r = 0, .c = 1 },
                    Point{ .r = 0, .c = -1 },
                    Point{ .r = 1, .c = 0 },
                    Point{ .r = -1, .c = 0 },
                };

                for (directions) |dir| {
                    const next = Point{ .r = curr.r + dir.r, .c = curr.c + dir.c };
                    if (!visited.contains(next)) {
                        try queue.append(allocator, next);
                    }
                }
            }

            try regions_list.append(allocator, Region{ .cells = region_cells });
        }
    }

    return regions_list.toOwnedSlice(allocator);
}

fn calculatePerimeter(region: *const PointSet) u32 {
    var perimeter: u32 = 0;

    var iter = region.keyIterator();
    while (iter.next()) |pos| {
        const directions = [_]Point{
            Point{ .r = 0, .c = 1 },
            Point{ .r = 0, .c = -1 },
            Point{ .r = 1, .c = 0 },
            Point{ .r = -1, .c = 0 },
        };

        for (directions) |dir| {
            const next = Point{ .r = pos.r + dir.r, .c = pos.c + dir.c };
            if (!region.contains(next)) {
                perimeter += 1;
            }
        }
    }

    return perimeter;
}

fn countSides(region: *const PointSet) u32 {
    var corners: u32 = 0;

    var iter = region.keyIterator();
    while (iter.next()) |pos| {
        const r = pos.r;
        const c = pos.c;

        // Check all 4 corners of this cell
        const up = region.contains(Point{ .r = r - 1, .c = c });
        const down = region.contains(Point{ .r = r + 1, .c = c });
        const left = region.contains(Point{ .r = r, .c = c - 1 });
        const right = region.contains(Point{ .r = r, .c = c + 1 });
        const up_left = region.contains(Point{ .r = r - 1, .c = c - 1 });
        const up_right = region.contains(Point{ .r = r - 1, .c = c + 1 });
        const down_left = region.contains(Point{ .r = r + 1, .c = c - 1 });
        const down_right = region.contains(Point{ .r = r + 1, .c = c + 1 });

        // Top-left corner
        if (!up and !left) { // convex
            corners += 1;
        } else if (up and left and !up_left) { // concave
            corners += 1;
        }

        // Top-right corner
        if (!up and !right) { // convex
            corners += 1;
        } else if (up and right and !up_right) { // concave
            corners += 1;
        }

        // Bottom-left corner
        if (!down and !left) { // convex
            corners += 1;
        } else if (down and left and !down_left) { // concave
            corners += 1;
        }

        // Bottom-right corner
        if (!down and !right) { // convex
            corners += 1;
        } else if (down and right and !down_right) { // concave
            corners += 1;
        }
    }

    return corners;
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

    // Trim whitespace
    const input = std.mem.trim(u8, buffer, &std.ascii.whitespace);

    // Count lines
    var line_count: usize = 0;
    var line_iter1 = std.mem.splitScalar(u8, input, '\n');
    while (line_iter1.next()) |_| {
        line_count += 1;
    }

    if (line_count == 0) return;

    // Parse grid
    const grid = try allocator.alloc([]const u8, line_count);
    defer allocator.free(grid);

    var idx: usize = 0;
    var line_iter2 = std.mem.splitScalar(u8, input, '\n');
    while (line_iter2.next()) |line| {
        grid[idx] = line;
        idx += 1;
    }

    const rows = grid.len;
    const cols = if (rows > 0) grid[0].len else 0;

    // Part 1
    const regions = try findRegions(allocator, grid, rows, cols);
    defer {
        for (regions) |*region| {
            region.deinit();
        }
        allocator.free(regions);
    }

    var total1: u64 = 0;
    for (regions) |*region| {
        const area: u64 = region.cells.count();
        const perimeter = calculatePerimeter(&region.cells);
        total1 += area * perimeter;
    }

    // Part 2
    const regions2 = try findRegions(allocator, grid, rows, cols);
    defer {
        for (regions2) |*region| {
            region.deinit();
        }
        allocator.free(regions2);
    }

    var total2: u64 = 0;
    for (regions2) |*region| {
        const area: u64 = region.cells.count();
        const sides = countSides(&region.cells);
        total2 += area * sides;
    }

    std.debug.print("Part 1: {d}\n", .{total1});
    std.debug.print("Part 2: {d}\n", .{total2});
}
