const std = @import("std");
const Allocator = std.mem.Allocator;

const Point = struct {
    x: i32,
    y: i32,
    z: i32,
};

// 6 directions: +x, -x, +y, -y, +z, -z
const directions = [6]Point{
    .{ .x = 1, .y = 0, .z = 0 },
    .{ .x = -1, .y = 0, .z = 0 },
    .{ .x = 0, .y = 1, .z = 0 },
    .{ .x = 0, .y = -1, .z = 0 },
    .{ .x = 0, .y = 0, .z = 1 },
    .{ .x = 0, .y = 0, .z = -1 },
};

fn parseInput(allocator: Allocator, text: []const u8) !std.AutoHashMap(Point, void) {
    var cubes = std.AutoHashMap(Point, void).init(allocator);

    var lines = std.mem.splitScalar(u8, text, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        var parts = std.mem.splitScalar(u8, line, ',');
        const x = try std.fmt.parseInt(i32, parts.next().?, 10);
        const y = try std.fmt.parseInt(i32, parts.next().?, 10);
        const z = try std.fmt.parseInt(i32, parts.next().?, 10);

        try cubes.put(.{ .x = x, .y = y, .z = z }, {});
    }

    return cubes;
}

fn part1(cubes: *const std.AutoHashMap(Point, void)) u32 {
    var surface_area: u32 = 0;

    var it = cubes.keyIterator();
    while (it.next()) |cube| {
        for (directions) |dir| {
            const neighbor = Point{
                .x = cube.x + dir.x,
                .y = cube.y + dir.y,
                .z = cube.z + dir.z,
            };
            if (!cubes.contains(neighbor)) {
                surface_area += 1;
            }
        }
    }

    return surface_area;
}

fn part2(allocator: Allocator, cubes: *const std.AutoHashMap(Point, void)) !u32 {
    // Find bounding box with 1 unit padding
    var min_x: i32 = std.math.maxInt(i32);
    var max_x: i32 = std.math.minInt(i32);
    var min_y: i32 = std.math.maxInt(i32);
    var max_y: i32 = std.math.minInt(i32);
    var min_z: i32 = std.math.maxInt(i32);
    var max_z: i32 = std.math.minInt(i32);

    var it = cubes.keyIterator();
    while (it.next()) |cube| {
        min_x = @min(min_x, cube.x);
        max_x = @max(max_x, cube.x);
        min_y = @min(min_y, cube.y);
        max_y = @max(max_y, cube.y);
        min_z = @min(min_z, cube.z);
        max_z = @max(max_z, cube.z);
    }

    min_x -= 1;
    max_x += 1;
    min_y -= 1;
    max_y += 1;
    min_z -= 1;
    max_z += 1;

    // BFS to find all exterior air cells
    var exterior = std.AutoHashMap(Point, void).init(allocator);
    defer exterior.deinit();

    var queue: std.ArrayList(Point) = .{};
    defer queue.deinit(allocator);

    const start = Point{ .x = min_x, .y = min_y, .z = min_z };
    try exterior.put(start, {});
    try queue.append(allocator, start);

    var idx: usize = 0;
    while (idx < queue.items.len) {
        const current = queue.items[idx];
        idx += 1;

        for (directions) |dir| {
            const nx = current.x + dir.x;
            const ny = current.y + dir.y;
            const nz = current.z + dir.z;

            // Stay within bounds
            if (nx < min_x or nx > max_x or
                ny < min_y or ny > max_y or
                nz < min_z or nz > max_z)
            {
                continue;
            }

            const neighbor = Point{ .x = nx, .y = ny, .z = nz };

            // Skip cubes and already visited
            if (cubes.contains(neighbor) or exterior.contains(neighbor)) {
                continue;
            }

            try exterior.put(neighbor, {});
            try queue.append(allocator, neighbor);
        }
    }

    // Count faces touching exterior air
    var surface_area: u32 = 0;
    it = cubes.keyIterator();
    while (it.next()) |cube| {
        for (directions) |dir| {
            const neighbor = Point{
                .x = cube.x + dir.x,
                .y = cube.y + dir.y,
                .z = cube.z + dir.z,
            };
            if (exterior.contains(neighbor)) {
                surface_area += 1;
            }
        }
    }

    return surface_area;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file - relative path from zig directory
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const file_size = (try file.stat()).size;
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    _ = try file.readAll(buffer);

    var cubes = try parseInput(allocator, buffer);
    defer cubes.deinit();

    const answer1 = part1(&cubes);
    const answer2 = try part2(allocator, &cubes);

    std.debug.print("Part 1: {d}\n", .{answer1});
    std.debug.print("Part 2: {d}\n", .{answer2});
}
