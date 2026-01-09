const std = @import("std");
const Allocator = std.mem.Allocator;

const MAX_VALVES = 64;
const MAX_VALUABLE = 16;

const Valve = struct {
    name: [2]u8,
    flow: u32,
    neighbors: [8]u8, // indices of neighbors
    neighbor_count: u8,
};

const MemoKey = struct {
    pos: u8,
    time: u8,
    mask: u16,
};

const MemoMap = std.AutoHashMap(MemoKey, u32);

const Graph = struct {
    valves: [MAX_VALVES]Valve,
    valve_count: u8,
    name_to_idx: std.AutoHashMap([2]u8, u8),
    distances: [MAX_VALUABLE + 1][MAX_VALUABLE + 1]u8,
    valuable_indices: [MAX_VALUABLE]u8, // original valve indices of valuable valves
    valuable_flows: [MAX_VALUABLE]u32,
    valuable_count: u8,
    aa_idx: u8,

    fn init(allocator: Allocator) Graph {
        return Graph{
            .valves = undefined,
            .valve_count = 0,
            .name_to_idx = std.AutoHashMap([2]u8, u8).init(allocator),
            .distances = undefined,
            .valuable_indices = undefined,
            .valuable_flows = undefined,
            .valuable_count = 0,
            .aa_idx = 0,
        };
    }

    fn deinit(self: *Graph) void {
        self.name_to_idx.deinit();
    }
};

fn parseName(s: []const u8) [2]u8 {
    return [2]u8{ s[0], s[1] };
}

fn parseInput(allocator: Allocator, text: []const u8) !Graph {
    var graph = Graph.init(allocator);

    // First pass: collect all valve names
    var lines = std.mem.splitScalar(u8, text, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        // Parse: "Valve XX has flow rate=N; tunnels lead to valves ..."
        const valve_start = std.mem.indexOf(u8, line, "Valve ") orelse continue;
        const name = parseName(line[valve_start + 6 ..]);

        try graph.name_to_idx.put(name, graph.valve_count);
        graph.valves[graph.valve_count].name = name;
        graph.valves[graph.valve_count].neighbor_count = 0;
        graph.valve_count += 1;
    }

    // Second pass: parse flow rates and neighbors
    lines = std.mem.splitScalar(u8, text, '\n');
    var idx: u8 = 0;
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        // Find flow rate
        const rate_start = (std.mem.indexOf(u8, line, "rate=") orelse continue) + 5;
        const rate_end = std.mem.indexOfScalarPos(u8, line, rate_start, ';') orelse continue;
        const flow = try std.fmt.parseInt(u32, line[rate_start..rate_end], 10);
        graph.valves[idx].flow = flow;

        // Find neighbors - look for "valve " or "valves "
        var neighbors_start: usize = 0;
        if (std.mem.indexOf(u8, line, "valves ")) |pos| {
            neighbors_start = pos + 7;
        } else if (std.mem.indexOf(u8, line, "valve ")) |pos| {
            neighbors_start = pos + 6;
        } else {
            continue;
        }

        var neighbor_iter = std.mem.splitSequence(u8, line[neighbors_start..], ", ");
        while (neighbor_iter.next()) |neighbor| {
            if (neighbor.len < 2) continue;
            const neighbor_name = parseName(neighbor);
            if (graph.name_to_idx.get(neighbor_name)) |neighbor_idx| {
                graph.valves[idx].neighbors[graph.valves[idx].neighbor_count] = neighbor_idx;
                graph.valves[idx].neighbor_count += 1;
            }
        }

        idx += 1;
    }

    // Find AA and valuable valves
    const aa_name = [2]u8{ 'A', 'A' };
    graph.aa_idx = graph.name_to_idx.get(aa_name) orelse return error.NoAA;

    for (0..graph.valve_count) |i| {
        if (graph.valves[i].flow > 0) {
            graph.valuable_indices[graph.valuable_count] = @intCast(i);
            graph.valuable_flows[graph.valuable_count] = graph.valves[i].flow;
            graph.valuable_count += 1;
        }
    }

    // Compute distances using BFS
    // Index 0 = AA, indices 1..valuable_count = valuable valves
    computeDistances(&graph);

    return graph;
}

fn computeDistances(graph: *Graph) void {
    // Sources: AA (index 0) and all valuable valves (indices 1..)
    var sources: [MAX_VALUABLE + 1]u8 = undefined;
    sources[0] = graph.aa_idx;
    for (0..graph.valuable_count) |i| {
        sources[i + 1] = graph.valuable_indices[i];
    }
    const source_count = graph.valuable_count + 1;

    for (0..source_count) |src_idx| {
        const start = sources[src_idx];

        // BFS
        var dist: [MAX_VALVES]u8 = undefined;
        @memset(&dist, 255);
        dist[start] = 0;

        var queue: [MAX_VALVES]u8 = undefined;
        var head: usize = 0;
        var tail: usize = 0;
        queue[tail] = start;
        tail += 1;

        while (head < tail) {
            const curr = queue[head];
            head += 1;
            const curr_dist = dist[curr];

            for (0..graph.valves[curr].neighbor_count) |n| {
                const neighbor = graph.valves[curr].neighbors[n];
                if (dist[neighbor] == 255) {
                    dist[neighbor] = curr_dist + 1;
                    queue[tail] = neighbor;
                    tail += 1;
                }
            }
        }

        // Store distances to all destinations
        for (0..source_count) |dst_idx| {
            graph.distances[src_idx][dst_idx] = dist[sources[dst_idx]];
        }
    }
}

fn dfs1(graph: *const Graph, memo: *MemoMap, pos: u8, time_left: u8, opened: u16, target_mask: u16) u32 {
    if (time_left <= 1) return 0;

    const key = MemoKey{ .pos = pos, .time = time_left, .mask = opened };
    if (memo.get(key)) |cached| {
        return cached;
    }

    var best: u32 = 0;
    const valuable_count = graph.valuable_count;

    for (0..valuable_count) |i| {
        const bit: u16 = @as(u16, 1) << @intCast(i);
        if ((target_mask & bit) == 0) continue; // Not in target set
        if ((opened & bit) != 0) continue; // Already opened

        // Distance from current pos to valuable valve i
        // pos is in distance matrix coordinates (0=AA, 1..=valuable)
        const dest_idx: u8 = @intCast(i + 1);
        const dist = graph.distances[pos][dest_idx];
        const time_cost = dist + 1; // +1 to open

        if (time_cost < time_left) {
            const new_time = time_left - time_cost;
            const pressure = graph.valuable_flows[i] * new_time;
            const sub = dfs1(graph, memo, dest_idx, new_time, opened | bit, target_mask);
            best = @max(best, pressure + sub);
        }
    }

    memo.put(key, best) catch {};
    return best;
}

fn part1(graph: *const Graph) u32 {
    const valuable_count = graph.valuable_count;
    const all_mask: u16 = (@as(u16, 1) << @intCast(valuable_count)) - 1;

    var memo = MemoMap.init(graph.name_to_idx.allocator);
    defer memo.deinit();

    const result = dfs1(graph, &memo, 0, 30, 0, all_mask);
    return result;
}

fn part2(graph: *const Graph) u32 {
    const valuable_count = graph.valuable_count;
    const n_subsets: u32 = @as(u32, 1) << @intCast(valuable_count);

    // Compute max pressure for each subset
    var max_scores: [1 << MAX_VALUABLE]u32 = undefined;

    for (0..n_subsets) |mask| {
        var memo = MemoMap.init(graph.name_to_idx.allocator);
        defer memo.deinit();
        max_scores[mask] = dfs1(graph, &memo, 0, 26, 0, @intCast(mask));
    }

    // Find best partition
    var best: u32 = 0;
    const full_mask: u32 = n_subsets - 1;
    for (0..n_subsets) |mask| {
        const complement = full_mask ^ mask;
        if (mask <= complement) {
            const combined = max_scores[mask] + max_scores[complement];
            best = @max(best, combined);
        }
    }

    return best;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Get path to input file
    const exe_path = try std.fs.selfExeDirPathAlloc(allocator);
    defer allocator.free(exe_path);

    const input_path = try std.fs.path.join(allocator, &[_][]const u8{ exe_path, "..", "input.txt" });
    defer allocator.free(input_path);

    const file = try std.fs.cwd().openFile(input_path, .{});
    defer file.close();

    const text = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(text);

    var graph = try parseInput(allocator, text);
    defer graph.deinit();

    const p1 = part1(&graph);
    const p2 = part2(&graph);

    std.debug.print("Part 1: {d}\n", .{p1});
    std.debug.print("Part 2: {d}\n", .{p2});
}
