const std = @import("std");

const Graph = std.StringHashMap(std.StringHashMap(void));
const StringSet = std.StringHashMap(void);

const Edge = struct {
    a: []const u8,
    b: []const u8,

    fn init(x: []const u8, y: []const u8) Edge {
        // Normalize edge: lexicographically smaller node first
        if (std.mem.order(u8, x, y) == .lt) {
            return .{ .a = x, .b = y };
        } else {
            return .{ .a = y, .b = x };
        }
    }
};

const EdgeContext = struct {
    pub fn hash(_: EdgeContext, edge: Edge) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(edge.a);
        hasher.update(edge.b);
        return hasher.final();
    }

    pub fn eql(_: EdgeContext, a: Edge, b: Edge) bool {
        return std.mem.eql(u8, a.a, b.a) and std.mem.eql(u8, a.b, b.b);
    }
};

const EdgeCount = std.HashMap(Edge, f64, EdgeContext, std.hash_map.default_max_load_percentage);
const EdgeSet = std.HashMap(Edge, void, EdgeContext, std.hash_map.default_max_load_percentage);

fn parseInput(allocator: std.mem.Allocator, filename: []const u8) !Graph {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const file_size = (try file.stat()).size;
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    _ = try file.readAll(buffer);

    var graph = Graph.init(allocator);

    // Track all unique node strings to avoid duplicate allocations
    var node_strings = StringSet.init(allocator);
    defer node_strings.deinit();

    var lines = std.mem.splitScalar(u8, buffer, '\n');

    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, &std.ascii.whitespace);
        if (trimmed.len == 0) continue;

        var parts = std.mem.splitSequence(u8, trimmed, ": ");
        const left = parts.next() orelse continue;
        const right = parts.next() orelse continue;

        // Get or create left node string
        var left_str: []const u8 = undefined;
        if (node_strings.getKey(left)) |existing| {
            left_str = existing;
        } else {
            left_str = try allocator.dupe(u8, left);
            try node_strings.put(left_str, {});
        }

        // Ensure left node is in graph
        const left_result = try graph.getOrPut(left_str);
        if (!left_result.found_existing) {
            left_result.value_ptr.* = StringSet.init(allocator);
        }

        var neighbors = std.mem.splitScalar(u8, right, ' ');
        while (neighbors.next()) |neighbor| {
            if (neighbor.len == 0) continue;

            // Get or create neighbor node string
            var neighbor_str: []const u8 = undefined;
            if (node_strings.getKey(neighbor)) |existing| {
                neighbor_str = existing;
            } else {
                neighbor_str = try allocator.dupe(u8, neighbor);
                try node_strings.put(neighbor_str, {});
            }

            // Add edge: left -> neighbor
            try left_result.value_ptr.put(neighbor_str, {});

            // Add edge: neighbor -> left (undirected)
            const neighbor_result = try graph.getOrPut(neighbor_str);
            if (!neighbor_result.found_existing) {
                neighbor_result.value_ptr.* = StringSet.init(allocator);
            }
            try neighbor_result.value_ptr.put(left_str, {});
        }
    }

    return graph;
}

fn bfsComponentSize(allocator: std.mem.Allocator, graph: Graph, start: []const u8, excluded_edges: EdgeSet) !usize {
    var visited = StringSet.init(allocator);
    defer visited.deinit();

    var queue: std.ArrayList([]const u8) = .{};
    defer queue.deinit(allocator);

    try visited.put(start, {});
    try queue.append(allocator, start);

    var read_idx: usize = 0;
    while (read_idx < queue.items.len) {
        const node = queue.items[read_idx];
        read_idx += 1;

        if (graph.get(node)) |neighbors| {
            var it = neighbors.keyIterator();
            while (it.next()) |neighbor_ptr| {
                const neighbor = neighbor_ptr.*;
                const edge = Edge.init(node, neighbor);

                if (!visited.contains(neighbor) and !excluded_edges.contains(edge)) {
                    try visited.put(neighbor, {});
                    try queue.append(allocator, neighbor);
                }
            }
        }
    }

    return visited.count();
}

fn computeEdgeBetweenness(allocator: std.mem.Allocator, graph: Graph, sample_size: usize) !EdgeCount {
    var edge_count = EdgeCount.init(allocator);

    var nodes: std.ArrayList([]const u8) = .{};
    defer nodes.deinit(allocator);

    var graph_it = graph.keyIterator();
    while (graph_it.next()) |key| {
        try nodes.append(allocator, key.*);
    }

    // Sample nodes for efficiency
    var sampled_nodes: std.ArrayList([]const u8) = .{};
    defer sampled_nodes.deinit(allocator);

    if (nodes.items.len > sample_size) {
        // Use a simple seeded PRNG for reproducibility
        var prng = std.Random.DefaultPrng.init(42);
        const random = prng.random();

        var indices: std.ArrayList(usize) = .{};
        defer indices.deinit(allocator);
        for (0..nodes.items.len) |i| {
            try indices.append(allocator, i);
        }

        // Fisher-Yates shuffle, then take first sample_size
        for (0..sample_size) |i| {
            const j = random.intRangeAtMost(usize, i, indices.items.len - 1);
            const tmp = indices.items[i];
            indices.items[i] = indices.items[j];
            indices.items[j] = tmp;
        }

        for (0..sample_size) |i| {
            try sampled_nodes.append(allocator, nodes.items[indices.items[i]]);
        }
    } else {
        try sampled_nodes.appendSlice(allocator, nodes.items);
    }

    // For each sampled source node
    for (sampled_nodes.items) |source| {
        var dist = std.StringHashMap(usize).init(allocator);
        defer dist.deinit();

        var pred = std.StringHashMap(std.ArrayList([]const u8)).init(allocator);
        defer {
            var pred_it = pred.valueIterator();
            while (pred_it.next()) |list| {
                list.deinit(allocator);
            }
            pred.deinit();
        }

        var queue: std.ArrayList([]const u8) = .{};
        defer queue.deinit(allocator);

        try dist.put(source, 0);
        try queue.append(allocator, source);

        var read_idx: usize = 0;
        while (read_idx < queue.items.len) {
            const node = queue.items[read_idx];
            read_idx += 1;

            const node_dist = dist.get(node).?;

            if (graph.get(node)) |neighbors| {
                var neighbor_it = neighbors.keyIterator();
                while (neighbor_it.next()) |neighbor_ptr| {
                    const neighbor = neighbor_ptr.*;

                    if (!dist.contains(neighbor)) {
                        try dist.put(neighbor, node_dist + 1);
                        const pred_result = try pred.getOrPut(neighbor);
                        if (!pred_result.found_existing) {
                            pred_result.value_ptr.* = .{};
                        }
                        try pred_result.value_ptr.append(allocator, node);
                        try queue.append(allocator, neighbor);
                    } else if (dist.get(neighbor).? == node_dist + 1) {
                        const pred_result = try pred.getOrPut(neighbor);
                        if (!pred_result.found_existing) {
                            pred_result.value_ptr.* = .{};
                        }
                        try pred_result.value_ptr.append(allocator, node);
                    }
                }
            }
        }

        // Compute num_paths
        var num_paths = std.StringHashMap(f64).init(allocator);
        defer num_paths.deinit();

        try num_paths.put(source, 1.0);

        // Sort nodes by distance
        var nodes_by_dist: std.ArrayList([]const u8) = .{};
        defer nodes_by_dist.deinit(allocator);

        var dist_it = dist.keyIterator();
        while (dist_it.next()) |key| {
            try nodes_by_dist.append(allocator, key.*);
        }

        const DistContext = struct {
            distances: *std.StringHashMap(usize),

            pub fn lessThan(ctx: @This(), a: []const u8, b: []const u8) bool {
                const dist_a = ctx.distances.get(a).?;
                const dist_b = ctx.distances.get(b).?;
                return dist_a < dist_b;
            }
        };

        std.mem.sort([]const u8, nodes_by_dist.items, DistContext{ .distances = &dist }, DistContext.lessThan);

        for (nodes_by_dist.items) |node| {
            if (pred.get(node)) |predecessors| {
                for (predecessors.items) |p| {
                    const np = num_paths.get(p) orelse 0.0;
                    const nn = num_paths.get(node) orelse 0.0;
                    try num_paths.put(node, nn + np);
                }
            }
        }

        // Accumulate edge betweenness (reverse order)
        var dependency = std.StringHashMap(f64).init(allocator);
        defer dependency.deinit();

        var reverse_idx: usize = nodes_by_dist.items.len;
        while (reverse_idx > 0) {
            reverse_idx -= 1;
            const node = nodes_by_dist.items[reverse_idx];

            if (pred.get(node)) |predecessors| {
                for (predecessors.items) |p| {
                    const edge = Edge.init(p, node);
                    const np = num_paths.get(p) orelse 0.0;
                    const nn = num_paths.get(node) orelse 1.0;
                    const dep = dependency.get(node) orelse 0.0;
                    const frac = np / nn;
                    const contrib = frac * (1.0 + dep);

                    const current = edge_count.get(edge) orelse 0.0;
                    try edge_count.put(edge, current + contrib);

                    const dep_p = dependency.get(p) orelse 0.0;
                    try dependency.put(p, dep_p + contrib);
                }
            }
        }
    }

    return edge_count;
}

fn findCutEdges(allocator: std.mem.Allocator, graph: Graph) !usize {
    // Compute edge betweenness
    var edge_betweenness = try computeEdgeBetweenness(allocator, graph, 100);
    defer edge_betweenness.deinit();

    // Sort edges by betweenness
    const EdgeBetweenness = struct {
        edge: Edge,
        score: f64,
    };

    var sorted_edges: std.ArrayList(EdgeBetweenness) = .{};
    defer sorted_edges.deinit(allocator);

    var eb_it = edge_betweenness.iterator();
    while (eb_it.next()) |entry| {
        try sorted_edges.append(allocator, .{ .edge = entry.key_ptr.*, .score = entry.value_ptr.* });
    }

    const SortContext = struct {
        pub fn lessThan(_: @This(), a: EdgeBetweenness, b: EdgeBetweenness) bool {
            return a.score > b.score; // Descending order
        }
    };

    std.mem.sort(EdgeBetweenness, sorted_edges.items, SortContext{}, SortContext.lessThan);

    const total_nodes = graph.count();

    // Try top 20 candidates
    const top_count = @min(20, sorted_edges.items.len);

    var i: usize = 0;
    while (i < top_count) : (i += 1) {
        var j: usize = i + 1;
        while (j < top_count) : (j += 1) {
            var k: usize = j + 1;
            while (k < top_count) : (k += 1) {
                var excluded = EdgeSet.init(allocator);
                defer excluded.deinit();

                try excluded.put(sorted_edges.items[i].edge, {});
                try excluded.put(sorted_edges.items[j].edge, {});
                try excluded.put(sorted_edges.items[k].edge, {});

                // Get any starting node
                var graph_it = graph.keyIterator();
                const start = graph_it.next().?.*;

                const size1 = try bfsComponentSize(allocator, graph, start, excluded);

                if (size1 < total_nodes) {
                    // Graph is disconnected!
                    const size2 = total_nodes - size1;
                    return size1 * size2;
                }
            }
        }
    }

    return 0;
}

fn part1(allocator: std.mem.Allocator, filename: []const u8) !usize {
    var graph = try parseInput(allocator, filename);
    defer {
        // Clean up neighbor sets
        var graph_it = graph.valueIterator();
        while (graph_it.next()) |neighbors| {
            neighbors.deinit();
        }
        // Free node strings (each only once since we deduplicated)
        var key_it = graph.keyIterator();
        while (key_it.next()) |key| {
            allocator.free(key.*);
        }
        graph.deinit();
    }

    return try findCutEdges(allocator, graph);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const input_path = "../input.txt";

    const result1 = try part1(allocator, input_path);
    std.debug.print("Part 1: {d}\n", .{result1});
    std.debug.print("Part 2: Push the big red button!\n", .{});
}
