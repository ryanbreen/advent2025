const std = @import("std");

const MAX_MONKEYS = 10;
const MAX_ITEMS = 64;

const Operation = enum {
    add,
    mul,
};

const Monkey = struct {
    items: [MAX_ITEMS]u64,
    item_count: usize,
    op: Operation,
    operand: ?u64, // null means "old"
    divisor: u64,
    if_true: usize,
    if_false: usize,
    inspections: u64,

    fn init() Monkey {
        return Monkey{
            .items = undefined,
            .item_count = 0,
            .op = .add,
            .operand = 0,
            .divisor = 1,
            .if_true = 0,
            .if_false = 0,
            .inspections = 0,
        };
    }

    fn addItem(self: *Monkey, item: u64) void {
        self.items[self.item_count] = item;
        self.item_count += 1;
    }

    fn clearItems(self: *Monkey) void {
        self.item_count = 0;
    }

    fn applyOp(self: *const Monkey, old: u64) u64 {
        const val = self.operand orelse old;
        return switch (self.op) {
            .add => old + val,
            .mul => old * val,
        };
    }
};

fn parseNumber(line: []const u8, start: usize) struct { val: u64, end: usize } {
    var i = start;
    // Skip non-digits
    while (i < line.len and (line[i] < '0' or line[i] > '9')) : (i += 1) {}
    if (i >= line.len) return .{ .val = 0, .end = i };

    var val: u64 = 0;
    while (i < line.len and line[i] >= '0' and line[i] <= '9') : (i += 1) {
        val = val * 10 + (line[i] - '0');
    }
    return .{ .val = val, .end = i };
}

fn parseMonkeys(input: []const u8, monkeys: *[MAX_MONKEYS]Monkey) usize {
    var count: usize = 0;
    var i: usize = 0;

    while (i < input.len) {
        // Skip to "Monkey X:"
        while (i < input.len and !std.mem.startsWith(u8, input[i..], "Monkey ")) : (i += 1) {}
        if (i >= input.len) break;

        var monkey = Monkey.init();

        // Skip to next line (Starting items)
        while (i < input.len and input[i] != '\n') : (i += 1) {}
        i += 1;

        // Parse starting items
        while (i < input.len and input[i] != '\n') {
            const res = parseNumber(input, i);
            if (res.end > i) {
                monkey.addItem(res.val);
                i = res.end;
            } else {
                i += 1;
            }
        }
        i += 1;

        // Parse operation line
        while (i < input.len and input[i] != '\n') {
            if (std.mem.startsWith(u8, input[i..], "old * old")) {
                monkey.op = .mul;
                monkey.operand = null;
                break;
            } else if (std.mem.startsWith(u8, input[i..], "old + ")) {
                monkey.op = .add;
                const res = parseNumber(input, i + 6);
                monkey.operand = res.val;
                break;
            } else if (std.mem.startsWith(u8, input[i..], "old * ")) {
                monkey.op = .mul;
                const res = parseNumber(input, i + 6);
                monkey.operand = res.val;
                break;
            }
            i += 1;
        }
        while (i < input.len and input[i] != '\n') : (i += 1) {}
        i += 1;

        // Parse divisor
        const div_res = parseNumber(input, i);
        monkey.divisor = div_res.val;
        i = div_res.end;
        while (i < input.len and input[i] != '\n') : (i += 1) {}
        i += 1;

        // Parse if_true target
        const true_res = parseNumber(input, i);
        monkey.if_true = @intCast(true_res.val);
        i = true_res.end;
        while (i < input.len and input[i] != '\n') : (i += 1) {}
        i += 1;

        // Parse if_false target
        const false_res = parseNumber(input, i);
        monkey.if_false = @intCast(false_res.val);
        i = false_res.end;

        monkeys[count] = monkey;
        count += 1;
    }

    return count;
}

fn simulate(monkeys: *[MAX_MONKEYS]Monkey, count: usize, rounds: usize, relief_divisor: u64, mod_value: u64) void {
    for (0..rounds) |_| {
        for (0..count) |m| {
            const monkey = &monkeys[m];
            for (0..monkey.item_count) |item_idx| {
                var worry = monkey.items[item_idx];
                monkey.inspections += 1;

                // Apply operation
                worry = monkey.applyOp(worry);

                // Apply relief
                if (relief_divisor > 1) {
                    worry = worry / relief_divisor;
                }

                // Apply modulo to prevent overflow
                if (mod_value > 0) {
                    worry = worry % mod_value;
                }

                // Test and throw
                const target = if (worry % monkey.divisor == 0) monkey.if_true else monkey.if_false;
                monkeys[target].addItem(worry);
            }
            monkey.clearItems();
        }
    }
}

fn monkeyBusiness(monkeys: *const [MAX_MONKEYS]Monkey, count: usize) u64 {
    var inspections: [MAX_MONKEYS]u64 = undefined;
    for (0..count) |i| {
        inspections[i] = monkeys[i].inspections;
    }

    // Sort descending (simple bubble sort for small array)
    for (0..count) |i| {
        for (i + 1..count) |j| {
            if (inspections[j] > inspections[i]) {
                const tmp = inspections[i];
                inspections[i] = inspections[j];
                inspections[j] = tmp;
            }
        }
    }

    return inspections[0] * inspections[1];
}

fn copyMonkeys(src: *const [MAX_MONKEYS]Monkey, dst: *[MAX_MONKEYS]Monkey, count: usize) void {
    for (0..count) |i| {
        dst[i] = src[i];
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const file_size = (try file.stat()).size;
    const input = try allocator.alloc(u8, file_size);
    defer allocator.free(input);

    _ = try file.readAll(input);

    // Parse monkeys
    var base_monkeys: [MAX_MONKEYS]Monkey = undefined;
    const count = parseMonkeys(input, &base_monkeys);

    // Calculate product of all divisors for part 2
    var mod_value: u64 = 1;
    for (0..count) |i| {
        mod_value *= base_monkeys[i].divisor;
    }

    // Part 1: 20 rounds with relief (divide by 3)
    var monkeys1: [MAX_MONKEYS]Monkey = undefined;
    copyMonkeys(&base_monkeys, &monkeys1, count);
    simulate(&monkeys1, count, 20, 3, 0);
    const part1 = monkeyBusiness(&monkeys1, count);

    // Part 2: 10000 rounds without relief
    var monkeys2: [MAX_MONKEYS]Monkey = undefined;
    copyMonkeys(&base_monkeys, &monkeys2, count);
    simulate(&monkeys2, count, 10000, 1, mod_value);
    const part2 = monkeyBusiness(&monkeys2, count);

    std.debug.print("Part 1: {d}\n", .{part1});
    std.debug.print("Part 2: {d}\n", .{part2});
}
