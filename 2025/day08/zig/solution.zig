const std = @import("std");

const Point = struct {
    x: i32,
    y: i32,
    z: i32,
};

const Pair = struct {
    dist_sq: i64,
    i: usize,
    j: usize,
};

fn lessThanPair(_: void, a: Pair, b: Pair) bool {
    return a.dist_sq < b.dist_sq;
}

const UnionFind = struct {
    parent: []usize,
    rank: []usize,
    size: []usize,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, n: usize) !UnionFind {
        const parent = try allocator.alloc(usize, n);
        const rank = try allocator.alloc(usize, n);
        const size = try allocator.alloc(usize, n);

        for (0..n) |i| {
            parent[i] = i;
            rank[i] = 0;
            size[i] = 1;
        }

        return UnionFind{
            .parent = parent,
            .rank = rank,
            .size = size,
            .allocator = allocator,
        };
    }

    fn deinit(self: *UnionFind) void {
        self.allocator.free(self.parent);
        self.allocator.free(self.rank);
        self.allocator.free(self.size);
    }

    fn find(self: *UnionFind, x: usize) usize {
        if (self.parent[x] != x) {
            self.parent[x] = self.find(self.parent[x]); // Path compression
        }
        return self.parent[x];
    }

    fn unionSets(self: *UnionFind, x: usize, y: usize) bool {
        var px = self.find(x);
        var py = self.find(y);

        if (px == py) {
            return false; // Already in same set
        }

        // Union by rank
        if (self.rank[px] < self.rank[py]) {
            const temp = px;
            px = py;
            py = temp;
        }

        self.parent[py] = px;
        self.size[px] += self.size[py];

        if (self.rank[px] == self.rank[py]) {
            self.rank[px] += 1;
        }

        return true;
    }

    fn getComponentSizes(self: *UnionFind, allocator: std.mem.Allocator) ![]usize {
        // Count components first
        var count: usize = 0;
        for (0..self.parent.len) |i| {
            if (self.parent[i] == i) {
                count += 1;
            }
        }

        const sizes = try allocator.alloc(usize, count);
        var idx: usize = 0;
        for (0..self.parent.len) |i| {
            if (self.parent[i] == i) {
                sizes[idx] = self.size[i];
                idx += 1;
            }
        }

        return sizes;
    }
};

fn euclideanDistanceSq(p1: Point, p2: Point) i64 {
    const dx: i64 = @as(i64, p1.x) - @as(i64, p2.x);
    const dy: i64 = @as(i64, p1.y) - @as(i64, p2.y);
    const dz: i64 = @as(i64, p1.z) - @as(i64, p2.z);
    return dx * dx + dy * dy + dz * dz;
}

fn parseInput(allocator: std.mem.Allocator, filename: []const u8) ![]Point {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const file_size = (try file.stat()).size;
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    _ = try file.readAll(buffer);

    // Count lines
    var line_count: usize = 0;
    var line_iter1 = std.mem.splitScalar(u8, buffer, '\n');
    while (line_iter1.next()) |line| {
        if (line.len > 0) {
            line_count += 1;
        }
    }

    // Allocate points array
    const points = try allocator.alloc(Point, line_count);
    var idx: usize = 0;

    // Parse points
    var line_iter2 = std.mem.splitScalar(u8, buffer, '\n');
    while (line_iter2.next()) |line| {
        if (line.len == 0) continue;

        var it = std.mem.splitScalar(u8, line, ',');
        const x_str = it.next() orelse continue;
        const y_str = it.next() orelse continue;
        const z_str = it.next() orelse continue;

        const x = try std.fmt.parseInt(i32, x_str, 10);
        const y = try std.fmt.parseInt(i32, y_str, 10);
        const z = try std.fmt.parseInt(i32, z_str, 10);

        points[idx] = Point{ .x = x, .y = y, .z = z };
        idx += 1;
    }

    return points;
}

fn part1(allocator: std.mem.Allocator, points: []const Point, num_connections: usize) !i64 {
    const n = points.len;

    // Calculate number of pairs
    const num_pairs = (n * (n - 1)) / 2;

    // Generate all pairs with distances
    const pairs = try allocator.alloc(Pair, num_pairs);
    defer allocator.free(pairs);

    var pair_idx: usize = 0;
    for (0..n) |i| {
        for (i + 1..n) |j| {
            const dist_sq = euclideanDistanceSq(points[i], points[j]);
            pairs[pair_idx] = Pair{ .dist_sq = dist_sq, .i = i, .j = j };
            pair_idx += 1;
        }
    }

    // Sort by distance
    std.mem.sort(Pair, pairs, {}, lessThanPair);

    // Union-Find to connect closest pairs
    var uf = try UnionFind.init(allocator, n);
    defer uf.deinit();

    var connections: usize = 0;
    for (pairs) |pair| {
        _ = uf.unionSets(pair.i, pair.j);
        connections += 1;
        if (connections == num_connections) {
            break;
        }
    }

    // Get component sizes and find the 3 largest
    const sizes = try uf.getComponentSizes(allocator);
    defer allocator.free(sizes);

    // Sort sizes in descending order
    std.mem.sort(usize, sizes, {}, comptime std.sort.desc(usize));

    // Multiply the 3 largest
    const s0: i64 = @intCast(sizes[0]);
    const s1: i64 = @intCast(sizes[1]);
    const s2: i64 = @intCast(sizes[2]);
    return s0 * s1 * s2;
}

fn part2(allocator: std.mem.Allocator, points: []const Point) !i64 {
    const n = points.len;

    // Calculate number of pairs
    const num_pairs = (n * (n - 1)) / 2;

    // Generate all pairs with distances
    const pairs = try allocator.alloc(Pair, num_pairs);
    defer allocator.free(pairs);

    var pair_idx: usize = 0;
    for (0..n) |i| {
        for (i + 1..n) |j| {
            const dist_sq = euclideanDistanceSq(points[i], points[j]);
            pairs[pair_idx] = Pair{ .dist_sq = dist_sq, .i = i, .j = j };
            pair_idx += 1;
        }
    }

    // Sort by distance
    std.mem.sort(Pair, pairs, {}, lessThanPair);

    // Union-Find to connect until all in one circuit
    var uf = try UnionFind.init(allocator, n);
    defer uf.deinit();

    var num_components: usize = n;

    for (pairs) |pair| {
        if (uf.unionSets(pair.i, pair.j)) {
            num_components -= 1;
            if (num_components == 1) {
                // This was the last connection - all in one circuit now
                return @as(i64, points[pair.i].x) * @as(i64, points[pair.j].x);
            }
        }
    }

    return 0;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const input_file = if (args.len > 1) args[1] else "../input.txt";

    const points = try parseInput(allocator, input_file);
    defer allocator.free(points);

    const part1_result = try part1(allocator, points, 1000);
    const part2_result = try part2(allocator, points);

    std.debug.print("Part 1: {d}\n", .{part1_result});
    std.debug.print("Part 2: {d}\n", .{part2_result});
}
