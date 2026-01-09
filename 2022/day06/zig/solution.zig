const std = @import("std");

fn allUnique(slice: []const u8) bool {
    var seen: [256]bool = [_]bool{false} ** 256;
    for (slice) |c| {
        if (seen[c]) return false;
        seen[c] = true;
    }
    return true;
}

fn findMarker(data: []const u8, window_size: usize) usize {
    if (data.len < window_size) return 0;

    var i: usize = window_size;
    while (i <= data.len) : (i += 1) {
        const window = data[i - window_size .. i];
        if (allUnique(window)) {
            return i;
        }
    }
    return 0;
}

fn part1(data: []const u8) usize {
    return findMarker(data, 4);
}

fn part2(data: []const u8) usize {
    return findMarker(data, 14);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const file_size = (try file.stat()).size;
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    _ = try file.readAll(buffer);

    // Trim whitespace
    const data = std.mem.trim(u8, buffer, &std.ascii.whitespace);

    std.debug.print("Part 1: {d}\n", .{part1(data)});
    std.debug.print("Part 2: {d}\n", .{part2(data)});
}
