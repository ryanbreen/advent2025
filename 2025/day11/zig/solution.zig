const std = @import("std");

const Graph = std.StringHashMap(std.ArrayList([]const u8));
const MemoCache = std.StringHashMap(u128);

fn parseInput(allocator: std.mem.Allocator, filename: []const u8) !Graph {
    var graph = Graph.init(allocator);

    const file_content = try std.fs.cwd().readFileAlloc(allocator, filename, 1024 * 1024);
    defer allocator.free(file_content);

    var lines = std.mem.splitScalar(u8, file_content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        var parts = std.mem.splitSequence(u8, line, ": ");
        const node_str = parts.next() orelse continue;
        const node = try allocator.dupe(u8, node_str);

        var neighbors: std.ArrayList([]const u8) = .{};

        if (parts.next()) |rest| {
            var neighbor_it = std.mem.splitScalar(u8, rest, ' ');
            while (neighbor_it.next()) |neighbor_str| {
                if (neighbor_str.len > 0) {
                    const neighbor = try allocator.dupe(u8, neighbor_str);
                    try neighbors.append(allocator, neighbor);
                }
            }
        }

        try graph.put(node, neighbors);
    }

    return graph;
}

fn countPathsToTarget(
    allocator: std.mem.Allocator,
    graph: *const Graph,
    target: []const u8,
) !*MemoCache {
    const cache = try allocator.create(MemoCache);
    cache.* = MemoCache.init(allocator);

    const CountPathsFn = struct {
        fn count(
            g: *const Graph,
            c: *MemoCache,
            tgt: []const u8,
            node: []const u8,
        ) anyerror!u128 {
            // Check if we've already computed this
            if (c.get(node)) |cached| {
                return cached;
            }

            // Base case: reached target
            if (std.mem.eql(u8, node, tgt)) {
                try c.put(node, 1);
                return 1;
            }

            // No outgoing edges from this node
            if (!g.contains(node)) {
                try c.put(node, 0);
                return 0;
            }

            // Sum paths through all neighbors
            const neighbors = g.get(node).?;
            var total: u128 = 0;
            for (neighbors.items) |neighbor| {
                total += try count(g, c, tgt, neighbor);
            }

            try c.put(node, total);
            return total;
        }
    };

    // Pre-fill cache for all nodes
    var it = graph.keyIterator();
    while (it.next()) |key| {
        _ = try CountPathsFn.count(graph, cache, target, key.*);
    }

    return cache;
}

fn part1(allocator: std.mem.Allocator, graph: *const Graph) !u128 {
    var cache = MemoCache.init(allocator);
    defer cache.deinit();

    const CountPathsFn = struct {
        fn count(
            g: *const Graph,
            c: *MemoCache,
            node: []const u8,
        ) anyerror!u128 {
            if (c.get(node)) |cached| {
                return cached;
            }

            if (std.mem.eql(u8, node, "out")) {
                try c.put(node, 1);
                return 1;
            }

            if (!g.contains(node)) {
                try c.put(node, 0);
                return 0;
            }

            const neighbors = g.get(node).?;
            var total: u128 = 0;
            for (neighbors.items) |neighbor| {
                total += try count(g, c, neighbor);
            }

            try c.put(node, total);
            return total;
        }
    };

    return try CountPathsFn.count(graph, &cache, "you");
}

fn part2(allocator: std.mem.Allocator, graph: *const Graph) !u128 {
    // Count paths to 'out'
    const paths_to_out = try countPathsToTarget(allocator, graph, "out");
    defer {
        paths_to_out.deinit();
        allocator.destroy(paths_to_out);
    }

    // Count paths to 'dac'
    const paths_to_dac = try countPathsToTarget(allocator, graph, "dac");
    defer {
        paths_to_dac.deinit();
        allocator.destroy(paths_to_dac);
    }

    // Count paths to 'fft'
    const paths_to_fft = try countPathsToTarget(allocator, graph, "fft");
    defer {
        paths_to_fft.deinit();
        allocator.destroy(paths_to_fft);
    }

    // Paths from svr to dac
    const svr_to_dac = paths_to_dac.get("svr") orelse 0;

    // Paths from dac to fft
    const dac_to_fft = paths_to_fft.get("dac") orelse 0;

    // Paths from fft to out
    const fft_to_out = paths_to_out.get("fft") orelse 0;

    // Paths from svr to fft
    const svr_to_fft = paths_to_fft.get("svr") orelse 0;

    // Paths from fft to dac
    const fft_to_dac = paths_to_dac.get("fft") orelse 0;

    // Paths from dac to out
    const dac_to_out = paths_to_out.get("dac") orelse 0;

    // Paths that visit dac before fft: svr -> dac -> fft -> out
    const dac_before_fft = svr_to_dac * dac_to_fft * fft_to_out;

    // Paths that visit fft before dac: svr -> fft -> dac -> out
    const fft_before_dac = svr_to_fft * fft_to_dac * dac_to_out;

    return dac_before_fft + fft_before_dac;
}

fn freeGraph(allocator: std.mem.Allocator, graph: *Graph) void {
    var it = graph.iterator();
    while (it.next()) |entry| {
        entry.value_ptr.deinit(allocator);
    }
    graph.deinit();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const input_file = if (args.len > 1) args[1] else "../input.txt";

    var graph = try parseInput(allocator, input_file);
    defer freeGraph(allocator, &graph);

    const p1 = try part1(allocator, &graph);
    const p2 = try part2(allocator, &graph);

    std.debug.print("Part 1: {d}\n", .{p1});
    std.debug.print("Part 2: {d}\n", .{p2});
}
