const std = @import("std");

const MODULO: u32 = 16777216; // 2^24

fn nextSecret(secret: u32) u32 {
    var s = secret;

    // Step 1: multiply by 64, mix, prune
    s ^= (s << 6);
    s &= 0xFFFFFF;

    // Step 2: divide by 32, mix, prune
    s ^= (s >> 5);
    s &= 0xFFFFFF;

    // Step 3: multiply by 2048, mix, prune
    s ^= (s << 11);
    s &= 0xFFFFFF;

    return s;
}

fn part1(allocator: std.mem.Allocator, initial_secrets: []const u32) !u64 {
    _ = allocator;
    var total: u64 = 0;

    for (initial_secrets) |initial| {
        var secret = initial;
        var i: usize = 0;
        while (i < 2000) : (i += 1) {
            secret = nextSecret(secret);
        }
        total += secret;
    }

    return total;
}

const Sequence = struct {
    changes: [4]i8,

    pub fn hash(self: Sequence) u64 {
        // Simple hash function for the 4-change sequence
        var h: u64 = 0;
        inline for (0..4) |i| {
            h = h * 31 + @as(u64, @intCast(@as(i32, self.changes[i]) + 128));
        }
        return h;
    }

    pub fn eql(self: Sequence, other: Sequence) bool {
        return std.mem.eql(i8, &self.changes, &other.changes);
    }
};

const SequenceContext = struct {
    pub fn hash(_: SequenceContext, key: Sequence) u64 {
        return key.hash();
    }

    pub fn eql(_: SequenceContext, a: Sequence, b: Sequence) bool {
        return a.eql(b);
    }
};

const SequenceMap = std.HashMap(Sequence, u32, SequenceContext, std.hash_map.default_max_load_percentage);
const SequenceSet = std.HashMap(Sequence, void, SequenceContext, std.hash_map.default_max_load_percentage);

fn part2(allocator: std.mem.Allocator, initial_secrets: []const u32) !u32 {
    // Map from sequence to total bananas
    var sequence_totals = SequenceMap.init(allocator);
    defer sequence_totals.deinit();

    for (initial_secrets) |initial| {
        // Generate 2001 secrets (initial + 2000 new)
        var secrets: [2001]u32 = undefined;
        secrets[0] = initial;

        var i: usize = 0;
        while (i < 2000) : (i += 1) {
            secrets[i + 1] = nextSecret(secrets[i]);
        }

        // Calculate prices (last digit)
        var prices: [2001]u8 = undefined;
        for (secrets, 0..) |secret, idx| {
            prices[idx] = @intCast(secret % 10);
        }

        // Calculate changes
        var changes: [2000]i8 = undefined;
        i = 0;
        while (i < 2000) : (i += 1) {
            changes[i] = @as(i8, @intCast(prices[i + 1])) - @as(i8, @intCast(prices[i]));
        }

        // Track first occurrence of each 4-change sequence for this buyer
        var seen = SequenceSet.init(allocator);
        defer seen.deinit();

        i = 0;
        while (i < changes.len - 3) : (i += 1) {
            const seq = Sequence{
                .changes = .{
                    changes[i],
                    changes[i + 1],
                    changes[i + 2],
                    changes[i + 3],
                },
            };

            if (!seen.contains(seq)) {
                try seen.put(seq, {});
                // Price we get is after these 4 changes
                const price = prices[i + 4];
                const entry = try sequence_totals.getOrPut(seq);
                if (!entry.found_existing) {
                    entry.value_ptr.* = 0;
                }
                entry.value_ptr.* += price;
            }
        }
    }

    // Find maximum total
    var max_bananas: u32 = 0;
    var iter = sequence_totals.valueIterator();
    while (iter.next()) |total| {
        if (total.* > max_bananas) {
            max_bananas = total.*;
        }
    }

    return max_bananas;
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

    // Parse initial secrets
    var initial_secrets: std.ArrayList(u32) = .{};
    defer initial_secrets.deinit(allocator);

    var lines = std.mem.tokenizeAny(u8, content, "\n\r");
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t");
        if (trimmed.len > 0) {
            const num = try std.fmt.parseInt(u32, trimmed, 10);
            try initial_secrets.append(allocator, num);
        }
    }

    const secrets = try initial_secrets.toOwnedSlice(allocator);
    defer allocator.free(secrets);

    const p1 = try part1(allocator, secrets);
    const p2 = try part2(allocator, secrets);

    std.debug.print("Part 1: {d}\n", .{p1});
    std.debug.print("Part 2: {d}\n", .{p2});
}
