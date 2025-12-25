const std = @import("std");

const Graph = std.StringHashMap(std.StringHashMap(void));

fn parseInput(allocator: std.mem.Allocator, filename: []const u8) !Graph {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const file_size = (try file.stat()).size;
    const text = try allocator.alloc(u8, file_size);
    defer allocator.free(text);

    _ = try file.readAll(text);

    var graph = Graph.init(allocator);

    var lines_it = std.mem.splitScalar(u8, text, '\n');
    while (lines_it.next()) |line| {
        if (line.len == 0) continue;

        var it = std.mem.splitScalar(u8, line, '-');
        const a = it.next() orelse continue;
        const b = it.next() orelse continue;

        // Allocate permanent copies of the strings
        const a_copy = try allocator.dupe(u8, a);
        const b_copy = try allocator.dupe(u8, b);

        // Add edge a -> b
        const gop_a = try graph.getOrPut(a_copy);
        if (!gop_a.found_existing) {
            gop_a.value_ptr.* = std.StringHashMap(void).init(allocator);
        }
        try gop_a.value_ptr.put(b_copy, {});

        // Add edge b -> a
        const gop_b = try graph.getOrPut(b_copy);
        if (!gop_b.found_existing) {
            gop_b.value_ptr.* = std.StringHashMap(void).init(allocator);
        }
        try gop_b.value_ptr.put(a_copy, {});
    }

    return graph;
}

const Triangle = struct {
    a: []const u8,
    b: []const u8,
    c: []const u8,

    fn lessThan(_: void, lhs: Triangle, rhs: Triangle) bool {
        const cmp_a = std.mem.order(u8, lhs.a, rhs.a);
        if (cmp_a != .eq) return cmp_a == .lt;
        const cmp_b = std.mem.order(u8, lhs.b, rhs.b);
        if (cmp_b != .eq) return cmp_b == .lt;
        return std.mem.order(u8, lhs.c, rhs.c) == .lt;
    }

    fn eql(self: Triangle, other: Triangle) bool {
        return std.mem.eql(u8, self.a, other.a) and
               std.mem.eql(u8, self.b, other.b) and
               std.mem.eql(u8, self.c, other.c);
    }

    fn hash(self: Triangle) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(self.a);
        hasher.update(self.b);
        hasher.update(self.c);
        return hasher.final();
    }
};

const TriangleContext = struct {
    pub fn hash(_: TriangleContext, key: Triangle) u64 {
        return key.hash();
    }

    pub fn eql(_: TriangleContext, a: Triangle, b: Triangle) bool {
        return a.eql(b);
    }
};

fn findTriangles(allocator: std.mem.Allocator, graph: Graph) !std.ArrayList(Triangle) {
    var triangles: std.ArrayList(Triangle) = .{};
    var seen = std.HashMap(Triangle, void, TriangleContext, std.hash_map.default_max_load_percentage).init(allocator);
    defer seen.deinit();

    var graph_iter = graph.iterator();
    while (graph_iter.next()) |entry_a| {
        const a = entry_a.key_ptr.*;
        const neighbors_a = entry_a.value_ptr.*;

        var neighbors_a_iter = neighbors_a.iterator();
        while (neighbors_a_iter.next()) |entry_b| {
            const b = entry_b.key_ptr.*;

            // Only process each edge once (a < b)
            if (std.mem.order(u8, a, b) != .lt) continue;

            // Find common neighbors
            const neighbors_b = graph.get(b) orelse continue;

            var neighbors_b_iter = neighbors_b.iterator();
            while (neighbors_b_iter.next()) |entry_c| {
                const c = entry_c.key_ptr.*;

                // Check if c is also a neighbor of a
                if (neighbors_a.contains(c)) {
                    // Create sorted triangle
                    var nodes = [_][]const u8{a, b, c};
                    std.mem.sort([]const u8, &nodes, {}, struct {
                        fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
                            return std.mem.order(u8, lhs, rhs) == .lt;
                        }
                    }.lessThan);

                    const tri = Triangle{ .a = nodes[0], .b = nodes[1], .c = nodes[2] };

                    const gop = try seen.getOrPut(tri);
                    if (!gop.found_existing) {
                        try triangles.append(allocator, tri);
                    }
                }
            }
        }
    }

    return triangles;
}

fn part1(allocator: std.mem.Allocator, graph: Graph) !usize {
    var triangles = try findTriangles(allocator, graph);
    defer triangles.deinit(allocator);

    var count: usize = 0;
    for (triangles.items) |tri| {
        if (tri.a[0] == 't' or tri.b[0] == 't' or tri.c[0] == 't') {
            count += 1;
        }
    }

    return count;
}

const NodeSet = std.StringHashMap(void);

fn bronKerbosch(
    allocator: std.mem.Allocator,
    graph: Graph,
    r: NodeSet,
    p: NodeSet,
    x: NodeSet,
    cliques: *std.ArrayList(NodeSet)
) !void {
    var p_mut = try p.clone();
    defer p_mut.deinit();
    var x_mut = try x.clone();
    defer x_mut.deinit();

    if (p_mut.count() == 0 and x_mut.count() == 0) {
        try cliques.append(allocator, try r.clone());
        return;
    }

    // Find pivot with maximum connections in P
    var pivot: ?[]const u8 = null;
    var max_connections: usize = 0;

    var p_iter = p_mut.iterator();
    while (p_iter.next()) |entry| {
        const v = entry.key_ptr.*;
        const neighbors = graph.get(v) orelse continue;
        var conn_count: usize = 0;
        var neighbors_iter = neighbors.iterator();
        while (neighbors_iter.next()) |neighbor_entry| {
            if (p_mut.contains(neighbor_entry.key_ptr.*)) {
                conn_count += 1;
            }
        }
        if (conn_count > max_connections) {
            max_connections = conn_count;
            pivot = v;
        }
    }

    var x_iter = x_mut.iterator();
    while (x_iter.next()) |entry| {
        const v = entry.key_ptr.*;
        const neighbors = graph.get(v) orelse continue;
        var conn_count: usize = 0;
        var neighbors_iter = neighbors.iterator();
        while (neighbors_iter.next()) |neighbor_entry| {
            if (p_mut.contains(neighbor_entry.key_ptr.*)) {
                conn_count += 1;
            }
        }
        if (conn_count > max_connections) {
            max_connections = conn_count;
            pivot = v;
        }
    }

    // Get pivot neighbors in P
    var pivot_neighbors = NodeSet.init(allocator);
    defer pivot_neighbors.deinit();

    if (pivot) |piv| {
        if (graph.get(piv)) |neighbors| {
            var neighbors_iter = neighbors.iterator();
            while (neighbors_iter.next()) |entry| {
                if (p_mut.contains(entry.key_ptr.*)) {
                    try pivot_neighbors.put(entry.key_ptr.*, {});
                }
            }
        }
    }

    // Collect nodes to iterate over (P - N(pivot))
    var to_process: std.ArrayList([]const u8) = .{};
    defer to_process.deinit(allocator);

    var p_iter2 = p_mut.iterator();
    while (p_iter2.next()) |entry| {
        if (!pivot_neighbors.contains(entry.key_ptr.*)) {
            try to_process.append(allocator, entry.key_ptr.*);
        }
    }

    for (to_process.items) |v| {
        var r_new = try r.clone();
        defer r_new.deinit();
        try r_new.put(v, {});

        // P ∩ N(v)
        var p_new = NodeSet.init(allocator);
        defer p_new.deinit();

        const neighbors = graph.get(v) orelse continue;
        var p_iter3 = p_mut.iterator();
        while (p_iter3.next()) |entry| {
            if (neighbors.contains(entry.key_ptr.*)) {
                try p_new.put(entry.key_ptr.*, {});
            }
        }

        // X ∩ N(v)
        var x_new = NodeSet.init(allocator);
        defer x_new.deinit();

        var x_iter2 = x_mut.iterator();
        while (x_iter2.next()) |entry| {
            if (neighbors.contains(entry.key_ptr.*)) {
                try x_new.put(entry.key_ptr.*, {});
            }
        }

        try bronKerbosch(allocator, graph, r_new, p_new, x_new, cliques);

        _ = p_mut.remove(v);
        try x_mut.put(v, {});
    }
}

fn part2(allocator: std.mem.Allocator, graph: Graph) ![]u8 {
    var cliques: std.ArrayList(NodeSet) = .{};
    defer {
        for (cliques.items) |clique| {
            var mut_clique = clique;
            mut_clique.deinit();
        }
        cliques.deinit(allocator);
    }

    var all_nodes = NodeSet.init(allocator);
    defer all_nodes.deinit();

    var graph_iter = graph.iterator();
    while (graph_iter.next()) |entry| {
        try all_nodes.put(entry.key_ptr.*, {});
    }

    var r = NodeSet.init(allocator);
    defer r.deinit();
    var x = NodeSet.init(allocator);
    defer x.deinit();

    try bronKerbosch(allocator, graph, r, all_nodes, x, &cliques);

    // Find largest clique
    var largest: ?NodeSet = null;
    var largest_size: usize = 0;

    for (cliques.items) |clique| {
        if (clique.count() > largest_size) {
            largest_size = clique.count();
            largest = clique;
        }
    }

    // Sort and join
    var nodes: std.ArrayList([]const u8) = .{};
    defer nodes.deinit(allocator);

    if (largest) |clique| {
        var iter = clique.iterator();
        while (iter.next()) |entry| {
            try nodes.append(allocator, entry.key_ptr.*);
        }
    }

    std.mem.sort([]const u8, nodes.items, {}, struct {
        fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
            return std.mem.order(u8, lhs, rhs) == .lt;
        }
    }.lessThan);

    // Join with commas
    var result: std.ArrayList(u8) = .{};
    for (nodes.items, 0..) |node, i| {
        if (i > 0) {
            try result.append(allocator, ',');
        }
        try result.appendSlice(allocator, node);
    }

    return result.toOwnedSlice(allocator);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var graph = try parseInput(allocator, "../input.txt");
    defer {
        var graph_iter = graph.iterator();
        while (graph_iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            var neighbors = entry.value_ptr.*;
            neighbors.deinit();
        }
        graph.deinit();
    }

    const p1 = try part1(allocator, graph);
    const p2 = try part2(allocator, graph);
    defer allocator.free(p2);

    std.debug.print("Part 1: {d}\n", .{p1});
    std.debug.print("Part 2: {s}\n", .{p2});
}
