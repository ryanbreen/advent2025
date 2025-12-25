const std = @import("std");

const GateOp = enum {
    AND,
    OR,
    XOR,
};

const Gate = struct {
    in1: []const u8,
    in2: []const u8,
    op: GateOp,
    out: []const u8,
};

fn parseInput(allocator: std.mem.Allocator, filename: []const u8) !struct {
    wires: std.StringHashMap(u1),
    gates: std.ArrayList(Gate),
} {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(content);

    var wires = std.StringHashMap(u1).init(allocator);
    var gates: std.ArrayList(Gate) = .{};

    var sections = std.mem.splitSequence(u8, content, "\n\n");
    const initial_section = sections.next() orelse return error.InvalidInput;
    const gates_section = sections.next() orelse return error.InvalidInput;

    // Parse initial wire values
    var lines = std.mem.splitScalar(u8, initial_section, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        var parts = std.mem.splitSequence(u8, line, ": ");
        const name = parts.next() orelse continue;
        const val_str = parts.next() orelse continue;

        const name_copy = try allocator.dupe(u8, name);
        const val: u1 = if (std.mem.eql(u8, val_str, "1")) 1 else 0;
        try wires.put(name_copy, val);
    }

    // Parse gates
    var gate_lines = std.mem.splitScalar(u8, gates_section, '\n');
    while (gate_lines.next()) |line| {
        if (line.len == 0) continue;

        // Format: "x00 AND y00 -> z00"
        var parts = std.mem.tokenizeScalar(u8, line, ' ');
        const in1 = parts.next() orelse continue;
        const op_str = parts.next() orelse continue;
        const in2 = parts.next() orelse continue;
        _ = parts.next(); // skip "->"
        const out = parts.next() orelse continue;

        const op = if (std.mem.eql(u8, op_str, "AND"))
            GateOp.AND
        else if (std.mem.eql(u8, op_str, "OR"))
            GateOp.OR
        else
            GateOp.XOR;

        try gates.append(allocator, Gate{
            .in1 = try allocator.dupe(u8, in1),
            .in2 = try allocator.dupe(u8, in2),
            .op = op,
            .out = try allocator.dupe(u8, out),
        });
    }

    return .{ .wires = wires, .gates = gates };
}

fn simulate(allocator: std.mem.Allocator, initial_wires: std.StringHashMap(u1), gates: std.ArrayList(Gate)) !std.StringHashMap(u1) {
    var wires = std.StringHashMap(u1).init(allocator);

    // Copy initial wires
    var it = initial_wires.iterator();
    while (it.next()) |entry| {
        const key_copy = try allocator.dupe(u8, entry.key_ptr.*);
        try wires.put(key_copy, entry.value_ptr.*);
    }

    var remaining: std.ArrayList(Gate) = .{};
    defer remaining.deinit(allocator);

    for (gates.items) |gate| {
        try remaining.append(allocator, gate);
    }

    while (remaining.items.len > 0) {
        var made_progress = false;
        var new_remaining: std.ArrayList(Gate) = .{};

        for (remaining.items) |gate| {
            const v1_opt = wires.get(gate.in1);
            const v2_opt = wires.get(gate.in2);

            if (v1_opt != null and v2_opt != null) {
                const v1 = v1_opt.?;
                const v2 = v2_opt.?;

                const result: u1 = switch (gate.op) {
                    .AND => v1 & v2,
                    .OR => v1 | v2,
                    .XOR => v1 ^ v2,
                };

                const out_copy = try allocator.dupe(u8, gate.out);
                try wires.put(out_copy, result);
                made_progress = true;
            } else {
                try new_remaining.append(allocator, gate);
            }
        }

        remaining.deinit(allocator);
        remaining = new_remaining;
    }

    return wires;
}

fn getZValue(wires: std.StringHashMap(u1), allocator: std.mem.Allocator) !u64 {
    var z_wires: std.ArrayList([]const u8) = .{};
    defer z_wires.deinit(allocator);

    var it = wires.iterator();
    while (it.next()) |entry| {
        if (entry.key_ptr.*[0] == 'z') {
            try z_wires.append(allocator, entry.key_ptr.*);
        }
    }

    // Sort in reverse order (z45, z44, ..., z00)
    std.mem.sort([]const u8, z_wires.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.order(u8, a, b) == .gt;
        }
    }.lessThan);

    var result: u64 = 0;
    for (z_wires.items) |z| {
        result = (result << 1) | wires.get(z).?;
    }

    return result;
}

fn part1(allocator: std.mem.Allocator, initial_wires: std.StringHashMap(u1), gates: std.ArrayList(Gate)) !u64 {
    var final_wires = try simulate(allocator, initial_wires, gates);
    defer {
        var it = final_wires.keyIterator();
        while (it.next()) |key| {
            allocator.free(key.*);
        }
        final_wires.deinit();
    }

    return try getZValue(final_wires, allocator);
}

fn part2(allocator: std.mem.Allocator, gates: std.ArrayList(Gate)) ![]const u8 {
    var swapped = std.StringHashMap(void).init(allocator);
    defer {
        var it = swapped.keyIterator();
        while (it.next()) |key| {
            allocator.free(key.*);
        }
        swapped.deinit();
    }

    // Build lookup maps
    var gate_by_output = std.StringHashMap(Gate).init(allocator);
    defer gate_by_output.deinit();

    for (gates.items) |gate| {
        try gate_by_output.put(gate.out, gate);
    }

    // Find max bit
    var max_bit: u8 = 0;
    for (gates.items) |gate| {
        if (gate.out[0] == 'z' and gate.out.len == 3) {
            const bit = try std.fmt.parseInt(u8, gate.out[1..], 10);
            if (bit > max_bit) max_bit = bit;
        }
    }

    const max_z = try std.fmt.allocPrint(allocator, "z{d:0>2}", .{max_bit});
    defer allocator.free(max_z);

    for (gates.items) |gate| {
        // Rule: XOR gates that don't take x,y as input should output to z
        if (gate.op == .XOR) {
            const is_xy_xor = (gate.in1[0] == 'x' or gate.in1[0] == 'y') and
                (gate.in2[0] == 'x' or gate.in2[0] == 'y');

            if (!is_xy_xor) {
                // Second-level XOR should output to z
                if (gate.out[0] != 'z') {
                    const out_copy = try allocator.dupe(u8, gate.out);
                    try swapped.put(out_copy, {});
                }
            }
        }

        // Rule: z outputs (except last) should come from XOR
        if (gate.out[0] == 'z' and !std.mem.eql(u8, gate.out, max_z)) {
            if (gate.op != .XOR) {
                const out_copy = try allocator.dupe(u8, gate.out);
                try swapped.put(out_copy, {});
            }
        }

        // Rule: AND gates (except x00 AND y00) should feed into OR
        if (gate.op == .AND) {
            const is_first_bit = (std.mem.eql(u8, gate.in1, "x00") and std.mem.eql(u8, gate.in2, "y00")) or
                (std.mem.eql(u8, gate.in1, "y00") and std.mem.eql(u8, gate.in2, "x00"));

            if (!is_first_bit) {
                var used_by_or = false;
                for (gates.items) |g2| {
                    if (g2.op == .OR) {
                        if (std.mem.eql(u8, g2.in1, gate.out) or std.mem.eql(u8, g2.in2, gate.out)) {
                            used_by_or = true;
                            break;
                        }
                    }
                }
                if (!used_by_or) {
                    const out_copy = try allocator.dupe(u8, gate.out);
                    try swapped.put(out_copy, {});
                }
            }
        }

        // Rule: XOR of x,y should feed into XOR and AND
        if (gate.op == .XOR) {
            const is_xy_xor = (gate.in1[0] == 'x' or gate.in1[0] == 'y') and
                (gate.in2[0] == 'x' or gate.in2[0] == 'y');
            const is_z00 = (std.mem.eql(u8, gate.in1, "x00") and std.mem.eql(u8, gate.in2, "y00")) or
                (std.mem.eql(u8, gate.in1, "y00") and std.mem.eql(u8, gate.in2, "x00"));

            if (is_xy_xor and !is_z00) {
                var used_by_xor = false;
                var used_by_and = false;
                for (gates.items) |g2| {
                    if (std.mem.eql(u8, g2.in1, gate.out) or std.mem.eql(u8, g2.in2, gate.out)) {
                        if (g2.op == .XOR) used_by_xor = true;
                        if (g2.op == .AND) used_by_and = true;
                    }
                }
                if (!used_by_xor or !used_by_and) {
                    const out_copy = try allocator.dupe(u8, gate.out);
                    try swapped.put(out_copy, {});
                }
            }
        }
    }

    // Collect and sort swapped wires
    var swapped_list: std.ArrayList([]const u8) = .{};
    defer swapped_list.deinit(allocator);

    var it = swapped.keyIterator();
    while (it.next()) |key| {
        try swapped_list.append(allocator, key.*);
    }

    std.mem.sort([]const u8, swapped_list.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.order(u8, a, b) == .lt;
        }
    }.lessThan);

    // Join with commas
    var result: std.ArrayList(u8) = .{};
    defer result.deinit(allocator);

    for (swapped_list.items, 0..) |wire, i| {
        if (i > 0) try result.append(allocator, ',');
        try result.appendSlice(allocator, wire);
    }

    return try result.toOwnedSlice(allocator);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const parsed = try parseInput(allocator, "../input.txt");
    var wires = parsed.wires;
    defer {
        var it = wires.keyIterator();
        while (it.next()) |key| {
            allocator.free(key.*);
        }
        wires.deinit();
    }

    var gates = parsed.gates;
    defer {
        for (gates.items) |gate| {
            allocator.free(gate.in1);
            allocator.free(gate.in2);
            allocator.free(gate.out);
        }
        gates.deinit(allocator);
    }

    const p1 = try part1(allocator, wires, gates);
    std.debug.print("Part 1: {d}\n", .{p1});

    const p2 = try part2(allocator, gates);
    defer allocator.free(p2);
    std.debug.print("Part 2: {s}\n", .{p2});
}
