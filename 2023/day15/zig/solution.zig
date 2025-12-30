const std = @import("std");

const Lens = struct {
    label: []const u8,
    focal: u8,
};

fn hash(s: []const u8) u8 {
    var current: u32 = 0;
    for (s) |c| {
        current = ((current + c) * 17) % 256;
    }
    return @intCast(current);
}

fn part1(input: []const u8) u64 {
    var total: u64 = 0;
    var iter = std.mem.splitScalar(u8, input, ',');
    while (iter.next()) |step| {
        total += hash(step);
    }
    return total;
}

fn part2(allocator: std.mem.Allocator, input: []const u8) !u64 {
    // 256 boxes, each containing a dynamic list of lenses
    var boxes: [256]std.ArrayList(Lens) = undefined;
    for (&boxes) |*box| {
        box.* = .{};
    }
    defer {
        for (&boxes) |*box| {
            box.deinit(allocator);
        }
    }

    var iter = std.mem.splitScalar(u8, input, ',');
    while (iter.next()) |step| {
        if (std.mem.indexOfScalar(u8, step, '=')) |eq_pos| {
            // Add or replace lens
            const label = step[0..eq_pos];
            const focal = step[eq_pos + 1] - '0';
            const box_num = hash(label);

            // Look for existing lens with same label
            var found = false;
            for (boxes[box_num].items) |*lens| {
                if (std.mem.eql(u8, lens.label, label)) {
                    lens.focal = focal;
                    found = true;
                    break;
                }
            }
            if (!found) {
                try boxes[box_num].append(allocator, .{ .label = label, .focal = focal });
            }
        } else {
            // Remove lens (step ends with '-')
            const label = step[0 .. step.len - 1];
            const box_num = hash(label);

            // Find and remove lens with matching label
            var i: usize = 0;
            while (i < boxes[box_num].items.len) {
                if (std.mem.eql(u8, boxes[box_num].items[i].label, label)) {
                    _ = boxes[box_num].orderedRemove(i);
                    break;
                }
                i += 1;
            }
        }
    }

    // Calculate focusing power
    var total: u64 = 0;
    for (boxes, 0..) |box, box_num| {
        for (box.items, 0..) |lens, slot| {
            total += (@as(u64, box_num) + 1) * (slot + 1) * lens.focal;
        }
    }

    return total;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const input = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(input);

    // Trim whitespace
    const trimmed = std.mem.trim(u8, input, &[_]u8{ '\n', '\r', ' ' });

    std.debug.print("Part 1: {d}\n", .{part1(trimmed)});
    std.debug.print("Part 2: {d}\n", .{try part2(allocator, trimmed)});
}
