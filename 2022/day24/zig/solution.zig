const std = @import("std");

const Blizzard = struct {
    r: i32,
    c: i32,
    dir: u8,
};

const Coord = struct {
    r: i32,
    c: i32,

    fn hash(self: Coord) u64 {
        const r_u: u64 = @bitCast(@as(i64, self.r));
        const c_u: u64 = @bitCast(@as(i64, self.c));
        return r_u *% 0x9e3779b97f4a7c15 +% c_u;
    }

    fn eql(a: Coord, b: Coord) bool {
        return a.r == b.r and a.c == b.c;
    }
};

const CoordSet = std.HashMap(Coord, void, struct {
    pub fn hash(_: @This(), key: Coord) u64 {
        return key.hash();
    }
    pub fn eql(_: @This(), a: Coord, b: Coord) bool {
        return Coord.eql(a, b);
    }
}, std.hash_map.default_max_load_percentage);

const State = struct {
    time_mod: u32,
    r: i32,
    c: i32,

    fn hash(self: State) u64 {
        const t_u: u64 = self.time_mod;
        const r_u: u64 = @bitCast(@as(i64, self.r));
        const c_u: u64 = @bitCast(@as(i64, self.c));
        return t_u *% 0x9e3779b97f4a7c15 +% r_u *% 0x517cc1b727220a95 +% c_u;
    }

    fn eql(a: State, b: State) bool {
        return a.time_mod == b.time_mod and a.r == b.r and a.c == b.c;
    }
};

const StateSet = std.HashMap(State, void, struct {
    pub fn hash(_: @This(), key: State) u64 {
        return key.hash();
    }
    pub fn eql(_: @This(), a: State, b: State) bool {
        return State.eql(a, b);
    }
}, std.hash_map.default_max_load_percentage);

const ParseResult = struct {
    blizzards: []Blizzard,
    height: i32,
    width: i32,
    inner_h: u32,
    inner_w: u32,
    start: Coord,
    end: Coord,
};

fn gcd(a: u32, b: u32) u32 {
    var x = a;
    var y = b;
    while (y != 0) {
        const t = y;
        y = x % y;
        x = t;
    }
    return x;
}

fn lcm(a: u32, b: u32) u32 {
    return a * b / gcd(a, b);
}

fn parseInput(allocator: std.mem.Allocator, text: []const u8) !ParseResult {
    // Count lines and find dimensions
    var lines_iter = std.mem.splitScalar(u8, text, '\n');
    var line_count: usize = 0;
    var line_width: usize = 0;
    var first_line: ?[]const u8 = null;
    var last_line: []const u8 = undefined;

    while (lines_iter.next()) |line| {
        if (line.len == 0) continue;
        if (first_line == null) {
            first_line = line;
            line_width = line.len;
        }
        last_line = line;
        line_count += 1;
    }

    const height: i32 = @intCast(line_count);
    const width: i32 = @intCast(line_width);
    const inner_h: u32 = @intCast(height - 2);
    const inner_w: u32 = @intCast(width - 2);

    // Count blizzards first
    var blizzard_count: usize = 0;
    lines_iter = std.mem.splitScalar(u8, text, '\n');
    while (lines_iter.next()) |line| {
        for (line) |ch| {
            if (ch == '^' or ch == 'v' or ch == '<' or ch == '>') {
                blizzard_count += 1;
            }
        }
    }

    // Allocate and populate blizzards
    var blizzards = try allocator.alloc(Blizzard, blizzard_count);
    var bliz_idx: usize = 0;

    lines_iter = std.mem.splitScalar(u8, text, '\n');
    var r: i32 = 0;
    while (lines_iter.next()) |line| {
        if (line.len == 0) continue;
        for (line, 0..) |ch, c_usize| {
            const c: i32 = @intCast(c_usize);
            if (ch == '^' or ch == 'v' or ch == '<' or ch == '>') {
                blizzards[bliz_idx] = .{ .r = r, .c = c, .dir = ch };
                bliz_idx += 1;
            }
        }
        r += 1;
    }

    // Find start position (first '.' in first row)
    var start_c: i32 = 0;
    for (first_line.?, 0..) |ch, c| {
        if (ch == '.') {
            start_c = @intCast(c);
            break;
        }
    }

    // Find end position (first '.' in last row)
    var end_c: i32 = 0;
    for (last_line, 0..) |ch, c| {
        if (ch == '.') {
            end_c = @intCast(c);
            break;
        }
    }

    return .{
        .blizzards = blizzards,
        .height = height,
        .width = width,
        .inner_h = inner_h,
        .inner_w = inner_w,
        .start = .{ .r = 0, .c = start_c },
        .end = .{ .r = height - 1, .c = end_c },
    };
}

fn getBlizzardPositions(allocator: std.mem.Allocator, blizzards: []const Blizzard, inner_h: u32, inner_w: u32, time: u32) !CoordSet {
    var positions = CoordSet.init(allocator);

    for (blizzards) |b| {
        // Adjust to inner coordinates (subtract 1 for wall)
        const ir = b.r - 1;
        const ic = b.c - 1;
        var nr: i32 = undefined;
        var nc: i32 = undefined;

        const inner_h_i: i32 = @intCast(inner_h);
        const inner_w_i: i32 = @intCast(inner_w);
        const time_i: i32 = @intCast(time);

        switch (b.dir) {
            '^' => {
                nr = @mod(ir - time_i, inner_h_i);
                nc = ic;
            },
            'v' => {
                nr = @mod(ir + time_i, inner_h_i);
                nc = ic;
            },
            '<' => {
                nr = ir;
                nc = @mod(ic - time_i, inner_w_i);
            },
            '>' => {
                nr = ir;
                nc = @mod(ic + time_i, inner_w_i);
            },
            else => unreachable,
        }

        // Convert back to full coordinates
        const final_r: i32 = nr + 1;
        const final_c: i32 = nc + 1;
        try positions.put(.{ .r = final_r, .c = final_c }, {});
    }

    return positions;
}

fn bfs(
    allocator: std.mem.Allocator,
    blizzards: []const Blizzard,
    height: i32,
    width: i32,
    inner_h: u32,
    inner_w: u32,
    start: Coord,
    end: Coord,
    start_time: u32,
) !u32 {
    const period = lcm(inner_h, inner_w);

    // Precompute blizzard positions for all times in one period
    var blizzard_cache = try allocator.alloc(CoordSet, period);
    defer {
        for (blizzard_cache) |*cache| {
            cache.deinit();
        }
        allocator.free(blizzard_cache);
    }

    for (0..period) |t| {
        blizzard_cache[t] = try getBlizzardPositions(allocator, blizzards, inner_h, inner_w, @intCast(t));
    }

    // BFS queue
    const QueueItem = struct { time: u32, r: i32, c: i32 };
    var queue_data = try allocator.alloc(QueueItem, 1024 * 1024);
    defer allocator.free(queue_data);

    var visited = StateSet.init(allocator);
    defer visited.deinit();

    queue_data[0] = .{ .time = start_time, .r = start.r, .c = start.c };
    var queue_head: usize = 0;
    var queue_tail: usize = 1;

    try visited.put(.{ .time_mod = start_time % period, .r = start.r, .c = start.c }, {});

    // Directions: wait, up, down, left, right
    const directions = [_][2]i32{ .{ 0, 0 }, .{ -1, 0 }, .{ 1, 0 }, .{ 0, -1 }, .{ 0, 1 } };

    while (queue_head < queue_tail) {
        const item = queue_data[queue_head];
        queue_head += 1;

        if (item.r == end.r and item.c == end.c) {
            return item.time;
        }

        const next_time = item.time + 1;
        const next_blizzards = &blizzard_cache[next_time % period];

        for (directions) |dir| {
            const nr = item.r + dir[0];
            const nc = item.c + dir[1];

            // Check bounds
            const is_start = (nr == start.r and nc == start.c);
            const is_end = (nr == end.r and nc == end.c);

            if (!is_start and !is_end) {
                if (nr <= 0 or nr >= height - 1 or nc <= 0 or nc >= width - 1) {
                    continue;
                }
            }

            // Check blizzards
            if (next_blizzards.contains(.{ .r = nr, .c = nc })) {
                continue;
            }

            const state = State{ .time_mod = next_time % period, .r = nr, .c = nc };
            if (!visited.contains(state)) {
                try visited.put(state, {});
                queue_data[queue_tail] = .{ .time = next_time, .r = nr, .c = nc };
                queue_tail += 1;
            }
        }
    }

    return 0; // No path found
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = std.fs.cwd().openFile("../input.txt", .{}) catch |err| {
        std.debug.print("Failed to open ../input.txt: {}\n", .{err});
        return err;
    };
    defer file.close();

    const text = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(text);

    const parsed = try parseInput(allocator, text);
    defer allocator.free(parsed.blizzards);

    // Part 1: start to end
    const part1 = try bfs(
        allocator,
        parsed.blizzards,
        parsed.height,
        parsed.width,
        parsed.inner_h,
        parsed.inner_w,
        parsed.start,
        parsed.end,
        0,
    );

    // Part 2: start -> end -> start -> end
    const t1 = part1;
    const t2 = try bfs(
        allocator,
        parsed.blizzards,
        parsed.height,
        parsed.width,
        parsed.inner_h,
        parsed.inner_w,
        parsed.end,
        parsed.start,
        t1,
    );
    const t3 = try bfs(
        allocator,
        parsed.blizzards,
        parsed.height,
        parsed.width,
        parsed.inner_h,
        parsed.inner_w,
        parsed.start,
        parsed.end,
        t2,
    );

    // Use posix write to stdout
    const stdout_fd = std.posix.STDOUT_FILENO;
    var buf1: [64]u8 = undefined;
    const msg1 = std.fmt.bufPrint(&buf1, "Part 1: {d}\n", .{part1}) catch unreachable;
    _ = std.posix.write(stdout_fd, msg1) catch {};

    var buf2: [64]u8 = undefined;
    const msg2 = std.fmt.bufPrint(&buf2, "Part 2: {d}\n", .{t3}) catch unreachable;
    _ = std.posix.write(stdout_fd, msg2) catch {};
}
