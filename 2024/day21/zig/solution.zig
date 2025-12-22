const std = @import("std");

const Position = struct {
    row: i8,
    col: i8,
};

// Keypad positions
const numeric_positions = [_]Position{
    .{ .row = 0, .col = 0 }, // '7'
    .{ .row = 0, .col = 1 }, // '8'
    .{ .row = 0, .col = 2 }, // '9'
    .{ .row = 1, .col = 0 }, // '4'
    .{ .row = 1, .col = 1 }, // '5'
    .{ .row = 1, .col = 2 }, // '6'
    .{ .row = 2, .col = 0 }, // '1'
    .{ .row = 2, .col = 1 }, // '2'
    .{ .row = 2, .col = 2 }, // '3'
    .{ .row = 3, .col = 1 }, // '0'
    .{ .row = 3, .col = 2 }, // 'A'
};

const directional_positions = [_]Position{
    .{ .row = 0, .col = 1 }, // '^'
    .{ .row = 0, .col = 2 }, // 'A'
    .{ .row = 1, .col = 0 }, // '<'
    .{ .row = 1, .col = 1 }, // 'v'
    .{ .row = 1, .col = 2 }, // '>'
};

const NUMERIC_GAP = Position{ .row = 3, .col = 0 };
const DIRECTIONAL_GAP = Position{ .row = 0, .col = 0 };

fn getNumericPos(c: u8) Position {
    return switch (c) {
        '7' => numeric_positions[0],
        '8' => numeric_positions[1],
        '9' => numeric_positions[2],
        '4' => numeric_positions[3],
        '5' => numeric_positions[4],
        '6' => numeric_positions[5],
        '1' => numeric_positions[6],
        '2' => numeric_positions[7],
        '3' => numeric_positions[8],
        '0' => numeric_positions[9],
        'A' => numeric_positions[10],
        else => unreachable,
    };
}

fn getDirectionalPos(c: u8) Position {
    return switch (c) {
        '^' => directional_positions[0],
        'A' => directional_positions[1],
        '<' => directional_positions[2],
        'v' => directional_positions[3],
        '>' => directional_positions[4],
        else => unreachable,
    };
}

const Path = struct {
    moves: [10]u8,
    len: u8,
};

fn findShortestPaths(allocator: std.mem.Allocator, start: Position, end: Position, gap: Position) !std.ArrayList(Path) {
    var paths: std.ArrayList(Path) = .{};

    const dr = end.row - start.row;
    const dc = end.col - start.col;

    // If start == end, return empty path
    if (dr == 0 and dc == 0) {
        try paths.append(allocator, Path{ .moves = undefined, .len = 0 });
        return paths;
    }

    var path = Path{ .moves = undefined, .len = 0 };
    try dfs(allocator, &paths, start, end, gap, &path);

    return paths;
}

fn dfs(allocator: std.mem.Allocator, paths: *std.ArrayList(Path), current: Position, target: Position, gap: Position, path: *Path) !void {
    if (current.row == gap.row and current.col == gap.col) {
        return;
    }

    if (current.row == target.row and current.col == target.col) {
        try paths.append(allocator, path.*);
        return;
    }

    // Move vertically toward target
    if (current.row < target.row) {
        var new_path = path.*;
        new_path.moves[new_path.len] = 'v';
        new_path.len += 1;
        try dfs(allocator, paths, .{ .row = current.row + 1, .col = current.col }, target, gap, &new_path);
    } else if (current.row > target.row) {
        var new_path = path.*;
        new_path.moves[new_path.len] = '^';
        new_path.len += 1;
        try dfs(allocator, paths, .{ .row = current.row - 1, .col = current.col }, target, gap, &new_path);
    }

    // Move horizontally toward target
    if (current.col < target.col) {
        var new_path = path.*;
        new_path.moves[new_path.len] = '>';
        new_path.len += 1;
        try dfs(allocator, paths, .{ .row = current.row, .col = current.col + 1 }, target, gap, &new_path);
    } else if (current.col > target.col) {
        var new_path = path.*;
        new_path.moves[new_path.len] = '<';
        new_path.len += 1;
        try dfs(allocator, paths, .{ .row = current.row, .col = current.col - 1 }, target, gap, &new_path);
    }
}

const CacheKey = struct {
    from: u8,
    to: u8,
    depth: u8,
    is_numeric: bool,

    pub fn hash(self: CacheKey) u64 {
        var h: u64 = 0;
        h = h * 31 + @as(u64, self.from);
        h = h * 31 + @as(u64, self.to);
        h = h * 31 + @as(u64, self.depth);
        h = h * 31 + @as(u64, if (self.is_numeric) 1 else 0);
        return h;
    }

    pub fn eql(self: CacheKey, other: CacheKey) bool {
        return self.from == other.from and
               self.to == other.to and
               self.depth == other.depth and
               self.is_numeric == other.is_numeric;
    }
};

const CacheContext = struct {
    pub fn hash(_: CacheContext, key: CacheKey) u64 {
        return key.hash();
    }

    pub fn eql(_: CacheContext, a: CacheKey, b: CacheKey) bool {
        return a.eql(b);
    }
};

const Cache = std.HashMap(CacheKey, u64, CacheContext, std.hash_map.default_max_load_percentage);

fn minPressesForMove(
    allocator: std.mem.Allocator,
    cache: *Cache,
    from: u8,
    to: u8,
    depth: u8,
    is_numeric: bool,
) !u64 {
    const key = CacheKey{
        .from = from,
        .to = to,
        .depth = depth,
        .is_numeric = is_numeric,
    };

    if (cache.get(key)) |cached| {
        return cached;
    }

    const start_pos = if (is_numeric) getNumericPos(from) else getDirectionalPos(from);
    const end_pos = if (is_numeric) getNumericPos(to) else getDirectionalPos(to);
    const gap = if (is_numeric) NUMERIC_GAP else DIRECTIONAL_GAP;

    var paths = try findShortestPaths(allocator, start_pos, end_pos, gap);
    defer paths.deinit(allocator);

    var result: u64 = 0;

    if (depth == 0) {
        // At human level, just return path length + 1 for 'A' press
        var min_len: u64 = std.math.maxInt(u64);
        for (paths.items) |path| {
            const len: u64 = @as(u64, path.len) + 1;
            if (len < min_len) {
                min_len = len;
            }
        }
        result = min_len;
    } else {
        var best: u64 = std.math.maxInt(u64);

        for (paths.items) |path| {
            // Need to type path + 'A' on the directional keypad above
            var cost: u64 = 0;
            var current: u8 = 'A';

            var i: usize = 0;
            while (i < path.len) : (i += 1) {
                const next = path.moves[i];
                cost += try minPressesForMove(allocator, cache, current, next, depth - 1, false);
                current = next;
            }

            // Press 'A' at the end
            cost += try minPressesForMove(allocator, cache, current, 'A', depth - 1, false);

            if (cost < best) {
                best = cost;
            }
        }

        result = best;
    }

    try cache.put(key, result);
    return result;
}

fn solveCode(allocator: std.mem.Allocator, cache: *Cache, code: []const u8, depth: u8) !u64 {
    var total: u64 = 0;
    var current: u8 = 'A';

    for (code) |char| {
        total += try minPressesForMove(allocator, cache, current, char, depth, true);
        current = char;
    }

    return total;
}

fn complexity(code: []const u8, length: u64) !u64 {
    // Parse numeric part (ignoring leading zeros and 'A')
    var numeric_part: u64 = 0;
    for (code) |c| {
        if (c >= '0' and c <= '9') {
            numeric_part = numeric_part * 10 + (c - '0');
        }
    }
    return length * numeric_part;
}

fn part1(allocator: std.mem.Allocator, codes: [][]const u8) !u64 {
    var cache = Cache.init(allocator);
    defer cache.deinit();

    var total: u64 = 0;
    for (codes) |code| {
        const length = try solveCode(allocator, &cache, code, 2);
        total += try complexity(code, length);
    }
    return total;
}

fn part2(allocator: std.mem.Allocator, codes: [][]const u8) !u64 {
    var cache = Cache.init(allocator);
    defer cache.deinit();

    var total: u64 = 0;
    for (codes) |code| {
        const length = try solveCode(allocator, &cache, code, 25);
        total += try complexity(code, length);
    }
    return total;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Parse codes
    var codes: std.ArrayList([]const u8) = .{};
    defer codes.deinit(allocator);

    var lines = std.mem.tokenizeScalar(u8, content, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \r\t");
        if (trimmed.len > 0) {
            try codes.append(allocator, trimmed);
        }
    }

    const p1 = try part1(allocator, codes.items);
    const p2 = try part2(allocator, codes.items);

    const stdout = std.fs.File.stdout();
    var buf: [256]u8 = undefined;
    const out1 = try std.fmt.bufPrint(&buf, "Part 1: {d}\n", .{p1});
    _ = try stdout.write(out1);
    const out2 = try std.fmt.bufPrint(&buf, "Part 2: {d}\n", .{p2});
    _ = try stdout.write(out2);
}
