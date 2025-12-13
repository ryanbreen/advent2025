const std = @import("std");

const WIDTH = 101;
const HEIGHT = 103;

const Robot = struct {
    px: i32,
    py: i32,
    vx: i32,
    vy: i32,
};

fn parseRobots(allocator: std.mem.Allocator, input: []const u8) !std.ArrayList(Robot) {
    var robots = std.ArrayList(Robot){};
    var lines = std.mem.splitSequence(u8, input, "\n");

    while (lines.next()) |line| {
        if (line.len == 0) continue;

        // Parse "p=px,py v=vx,vy"
        var parts = std.mem.splitSequence(u8, line, " ");
        const p_part = parts.next() orelse continue;
        const v_part = parts.next() orelse continue;

        // Parse p=px,py
        if (!std.mem.startsWith(u8, p_part, "p=")) continue;
        const p_coords = p_part[2..];
        var p_split = std.mem.splitSequence(u8, p_coords, ",");
        const px_str = p_split.next() orelse continue;
        const py_str = p_split.next() orelse continue;

        // Parse v=vx,vy
        if (!std.mem.startsWith(u8, v_part, "v=")) continue;
        const v_coords = v_part[2..];
        var v_split = std.mem.splitSequence(u8, v_coords, ",");
        const vx_str = v_split.next() orelse continue;
        const vy_str = v_split.next() orelse continue;

        const px = try std.fmt.parseInt(i32, px_str, 10);
        const py = try std.fmt.parseInt(i32, py_str, 10);
        const vx = try std.fmt.parseInt(i32, vx_str, 10);
        const vy = try std.fmt.parseInt(i32, vy_str, 10);

        try robots.append(allocator, Robot{ .px = px, .py = py, .vx = vx, .vy = vy });
    }

    return robots;
}

const Position = struct {
    x: i32,
    y: i32,
};

fn simulate(allocator: std.mem.Allocator, robots: []const Robot, seconds: i32) !std.ArrayList(Position) {
    var positions = std.ArrayList(Position){};

    for (robots) |robot| {
        // Calculate new position with wrapping
        const new_x = @mod(robot.px + robot.vx * seconds, WIDTH);
        const new_y = @mod(robot.py + robot.vy * seconds, HEIGHT);

        try positions.append(allocator, Position{ .x = new_x, .y = new_y });
    }

    return positions;
}

fn countQuadrants(positions: []const Position) [4]i32 {
    const mid_x: i32 = WIDTH / 2; // 50
    const mid_y: i32 = HEIGHT / 2; // 51

    var quadrants = [_]i32{0} ** 4;

    for (positions) |pos| {
        if (pos.x == mid_x or pos.y == mid_y) {
            continue; // Skip robots on middle lines
        }

        if (pos.x < mid_x and pos.y < mid_y) {
            quadrants[0] += 1; // Top-left
        } else if (pos.x > mid_x and pos.y < mid_y) {
            quadrants[1] += 1; // Top-right
        } else if (pos.x < mid_x and pos.y > mid_y) {
            quadrants[2] += 1; // Bottom-left
        } else {
            quadrants[3] += 1; // Bottom-right
        }
    }

    return quadrants;
}

fn part1(allocator: std.mem.Allocator, robots: []const Robot) !i64 {
    var positions = try simulate(allocator, robots, 100);
    defer positions.deinit(allocator);

    const quadrants = countQuadrants(positions.items);
    const safety_factor: i64 = @as(i64, quadrants[0]) * @as(i64, quadrants[1]) * @as(i64, quadrants[2]) * @as(i64, quadrants[3]);

    return safety_factor;
}

fn part2(allocator: std.mem.Allocator, robots: []const Robot) !i32 {
    // Look for a frame with a long horizontal line of robots (tree pattern)
    var seconds: i32 = 1;
    while (seconds <= WIDTH * HEIGHT) : (seconds += 1) {
        var positions = try simulate(allocator, robots, seconds);
        defer positions.deinit(allocator);

        // Create a set for O(1) lookup
        var pos_set = std.AutoHashMap(Position, void).init(allocator);
        defer pos_set.deinit();

        for (positions.items) |pos| {
            try pos_set.put(pos, {});
        }

        // Look for a horizontal line of at least 20 consecutive robots
        var y: i32 = 0;
        while (y < HEIGHT) : (y += 1) {
            var max_consecutive: i32 = 0;
            var consecutive: i32 = 0;

            var x: i32 = 0;
            while (x < WIDTH) : (x += 1) {
                if (pos_set.contains(Position{ .x = x, .y = y })) {
                    consecutive += 1;
                    if (consecutive > max_consecutive) {
                        max_consecutive = consecutive;
                    }
                } else {
                    consecutive = 0;
                }
            }

            if (max_consecutive >= 20) {
                return seconds;
            }
        }
    }

    return -1;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const input = try allocator.alloc(u8, file_size);
    defer allocator.free(input);

    _ = try file.readAll(input);

    // Trim trailing newline/whitespace
    const trimmed_input = std.mem.trim(u8, input, "\n\r ");

    // Parse robots
    var robots = try parseRobots(allocator, trimmed_input);
    defer robots.deinit(allocator);

    // Solve parts
    const p1 = try part1(allocator, robots.items);
    const p2 = try part2(allocator, robots.items);

    std.debug.print("Part 1: {d}\n", .{p1});
    std.debug.print("Part 2: {d}\n", .{p2});
}
