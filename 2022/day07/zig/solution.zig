const std = @import("std");

const MAX_PATH_DEPTH = 32;
const MAX_PATH_LEN = 256;
const MAX_DIRS = 512;

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const content = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(content);

    // Use HashMap to store directory sizes
    var dir_sizes = std.StringHashMap(u64).init(allocator);
    defer {
        var it = dir_sizes.keyIterator();
        while (it.next()) |key| {
            allocator.free(key.*);
        }
        dir_sizes.deinit();
    }

    // Track current path components
    var path_components: [MAX_PATH_DEPTH][128]u8 = undefined;
    var path_lengths: [MAX_PATH_DEPTH]usize = undefined;
    var path_depth: usize = 0;

    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        if (std.mem.startsWith(u8, line, "$ cd")) {
            const target = line[5..];
            if (std.mem.eql(u8, target, "/")) {
                path_depth = 1;
                path_components[0][0] = '/';
                path_lengths[0] = 1;
            } else if (std.mem.eql(u8, target, "..")) {
                if (path_depth > 1) {
                    path_depth -= 1;
                }
            } else {
                @memcpy(path_components[path_depth][0..target.len], target);
                path_lengths[path_depth] = target.len;
                path_depth += 1;
            }
        } else if (std.mem.startsWith(u8, line, "$ ls") or std.mem.startsWith(u8, line, "dir ")) {
            continue;
        } else {
            // It's a file with size
            var parts = std.mem.splitScalar(u8, line, ' ');
            const size_str = parts.next() orelse continue;
            const size = std.fmt.parseInt(u64, size_str, 10) catch continue;

            // Add size to current directory and all parent directories
            for (1..path_depth + 1) |depth| {
                const dir_path = try buildPathFromComponents(allocator, &path_components, &path_lengths, depth);

                const result = try dir_sizes.getOrPut(dir_path);
                if (result.found_existing) {
                    result.value_ptr.* += size;
                    allocator.free(dir_path);
                } else {
                    result.value_ptr.* = size;
                }
            }
        }
    }

    // Part 1: Sum of sizes of directories with total size <= 100000
    var part1: u64 = 0;
    var value_iter = dir_sizes.valueIterator();
    while (value_iter.next()) |size| {
        if (size.* <= 100000) {
            part1 += size.*;
        }
    }

    // Part 2: Find smallest directory to delete
    const total_space: u64 = 70000000;
    const needed_space: u64 = 30000000;
    const used_space = dir_sizes.get("/") orelse 0;
    const free_space = total_space - used_space;
    const need_to_free = needed_space - free_space;

    var part2: u64 = std.math.maxInt(u64);
    var value_iter2 = dir_sizes.valueIterator();
    while (value_iter2.next()) |size| {
        if (size.* >= need_to_free and size.* < part2) {
            part2 = size.*;
        }
    }

    std.debug.print("Part 1: {d}\n", .{part1});
    std.debug.print("Part 2: {d}\n", .{part2});
}

fn buildPathFromComponents(
    allocator: std.mem.Allocator,
    components: *const [MAX_PATH_DEPTH][128]u8,
    lengths: *const [MAX_PATH_DEPTH]usize,
    depth: usize,
) ![]u8 {
    if (depth == 0) {
        return try allocator.dupe(u8, "/");
    }

    // Calculate total length needed
    var total_len: usize = 0;
    for (0..depth) |i| {
        total_len += lengths[i];
        // Add separator if not root and previous wasn't root
        if (i > 0 and !(lengths[i - 1] == 1 and components[i - 1][0] == '/')) {
            total_len += 1;
        }
    }

    var result = try allocator.alloc(u8, total_len);
    var pos: usize = 0;

    for (0..depth) |i| {
        // Add separator if needed
        if (i > 0 and !(lengths[i - 1] == 1 and components[i - 1][0] == '/')) {
            result[pos] = '/';
            pos += 1;
        }
        @memcpy(result[pos .. pos + lengths[i]], components[i][0..lengths[i]]);
        pos += lengths[i];
    }

    return result[0..pos];
}
