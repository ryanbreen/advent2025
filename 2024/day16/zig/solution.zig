const std = @import("std");
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

const State = struct {
    x: i32,
    y: i32,
    dir: u8,
};

const Position = struct {
    x: i32,
    y: i32,
};

const QueueItem = struct {
    cost: u32,
    x: i32,
    y: i32,
    dir: u8,
};

fn lessThan(_: void, a: QueueItem, b: QueueItem) std.math.Order {
    return std.math.order(a.cost, b.cost);
}

const DX = [4]i32{ 1, 0, -1, 0 }; // East, South, West, North
const DY = [4]i32{ 0, 1, 0, -1 };

fn parseInput(allocator: std.mem.Allocator, text: []const u8) !struct {
    grid: ArrayList([]const u8),
    start: Position,
    end: Position,
} {
    var grid = try ArrayList([]const u8).initCapacity(allocator, 200);
    var start: ?Position = null;
    var end: ?Position = null;

    var lines = std.mem.splitScalar(u8, text, '\n');
    var y: i32 = 0;
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        try grid.append(allocator, line);

        for (line, 0..) |cell, x| {
            if (cell == 'S') {
                start = .{ .x = @intCast(x), .y = y };
            } else if (cell == 'E') {
                end = .{ .x = @intCast(x), .y = y };
            }
        }
        y += 1;
    }

    return .{
        .grid = grid,
        .start = start.?,
        .end = end.?,
    };
}

fn dijkstraForward(
    allocator: std.mem.Allocator,
    grid: ArrayList([]const u8),
    start: Position,
) !AutoHashMap(State, u32) {
    var dist = AutoHashMap(State, u32).init(allocator);
    var pq = std.PriorityQueue(QueueItem, void, lessThan).init(allocator, {});
    defer pq.deinit();

    // Start facing East (direction 0)
    try pq.add(.{ .cost = 0, .x = start.x, .y = start.y, .dir = 0 });

    while (pq.removeOrNull()) |item| {
        const state = State{ .x = item.x, .y = item.y, .dir = item.dir };

        if (dist.contains(state)) continue;

        try dist.put(state, item.cost);

        // Move forward
        const nx = item.x + DX[item.dir];
        const ny = item.y + DY[item.dir];

        if (ny >= 0 and ny < grid.items.len and
            nx >= 0 and nx < grid.items[@intCast(ny)].len and
            grid.items[@intCast(ny)][@intCast(nx)] != '#')
        {
            try pq.add(.{ .cost = item.cost + 1, .x = nx, .y = ny, .dir = item.dir });
        }

        // Turn left
        try pq.add(.{
            .cost = item.cost + 1000,
            .x = item.x,
            .y = item.y,
            .dir = @intCast((item.dir + 3) % 4),
        });

        // Turn right
        try pq.add(.{
            .cost = item.cost + 1000,
            .x = item.x,
            .y = item.y,
            .dir = @intCast((item.dir + 1) % 4),
        });
    }

    return dist;
}

fn dijkstraBackward(
    allocator: std.mem.Allocator,
    grid: ArrayList([]const u8),
    end: Position,
) !AutoHashMap(State, u32) {
    var dist = AutoHashMap(State, u32).init(allocator);
    var pq = std.PriorityQueue(QueueItem, void, lessThan).init(allocator, {});
    defer pq.deinit();

    // Can arrive at end from any direction
    var d: u8 = 0;
    while (d < 4) : (d += 1) {
        try pq.add(.{ .cost = 0, .x = end.x, .y = end.y, .dir = d });
    }

    while (pq.removeOrNull()) |item| {
        const state = State{ .x = item.x, .y = item.y, .dir = item.dir };

        if (dist.contains(state)) continue;

        try dist.put(state, item.cost);

        // Reverse of "move forward": come from behind
        const px = item.x - DX[item.dir];
        const py = item.y - DY[item.dir];

        if (py >= 0 and py < grid.items.len and
            px >= 0 and px < grid.items[@intCast(py)].len and
            grid.items[@intCast(py)][@intCast(px)] != '#')
        {
            try pq.add(.{ .cost = item.cost + 1, .x = px, .y = py, .dir = item.dir });
        }

        // Reverse of turn: same position, different direction
        try pq.add(.{
            .cost = item.cost + 1000,
            .x = item.x,
            .y = item.y,
            .dir = @intCast((item.dir + 3) % 4),
        });

        try pq.add(.{
            .cost = item.cost + 1000,
            .x = item.x,
            .y = item.y,
            .dir = @intCast((item.dir + 1) % 4),
        });
    }

    return dist;
}

fn part1(dist: AutoHashMap(State, u32), end: Position) u32 {
    var min_cost: u32 = std.math.maxInt(u32);

    var d: u8 = 0;
    while (d < 4) : (d += 1) {
        const state = State{ .x = end.x, .y = end.y, .dir = d };
        if (dist.get(state)) |cost| {
            if (cost < min_cost) {
                min_cost = cost;
            }
        }
    }

    return min_cost;
}

fn part2(
    allocator: std.mem.Allocator,
    grid: ArrayList([]const u8),
    dist_from_start: AutoHashMap(State, u32),
    dist_to_end: AutoHashMap(State, u32),
    best_score: u32,
) !u32 {
    var tiles = AutoHashMap(Position, void).init(allocator);
    defer tiles.deinit();

    var y: i32 = 0;
    while (y < grid.items.len) : (y += 1) {
        var x: i32 = 0;
        while (x < grid.items[@intCast(y)].len) : (x += 1) {
            if (grid.items[@intCast(y)][@intCast(x)] == '#') continue;

            var d: u8 = 0;
            while (d < 4) : (d += 1) {
                const state = State{ .x = x, .y = y, .dir = d };
                const from_start = dist_from_start.get(state) orelse std.math.maxInt(u32);
                const to_end = dist_to_end.get(state) orelse std.math.maxInt(u32);

                if (from_start != std.math.maxInt(u32) and
                    to_end != std.math.maxInt(u32) and
                    from_start + to_end == best_score)
                {
                    try tiles.put(.{ .x = x, .y = y }, {});
                    break;
                }
            }
        }
    }

    return tiles.count();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const text = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(text);

    var parsed = try parseInput(allocator, text);
    defer parsed.grid.deinit(allocator);

    var dist_from_start = try dijkstraForward(allocator, parsed.grid, parsed.start);
    defer dist_from_start.deinit();

    const answer1 = part1(dist_from_start, parsed.end);
    std.debug.print("Part 1: {d}\n", .{answer1});

    var dist_to_end = try dijkstraBackward(allocator, parsed.grid, parsed.end);
    defer dist_to_end.deinit();

    const answer2 = try part2(allocator, parsed.grid, dist_from_start, dist_to_end, answer1);
    std.debug.print("Part 2: {d}\n", .{answer2});
}
