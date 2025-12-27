const std = @import("std");

fn countWaysToWin(time: u64, record: u64) u64 {
    // If we hold the button for t ms, we travel t * (time - t) mm.
    // We need: t * (time - t) > record
    // Solving: -t^2 + time*t - record > 0
    // Roots: t = (time +/- sqrt(time^2 - 4*record)) / 2

    const time_f: f64 = @floatFromInt(time);
    const record_f: f64 = @floatFromInt(record);

    const discriminant = time_f * time_f - 4.0 * record_f;
    if (discriminant <= 0) {
        return 0;
    }

    const sqrt_d = @sqrt(discriminant);
    const t_low = (time_f - sqrt_d) / 2.0;
    const t_high = (time_f + sqrt_d) / 2.0;

    // We need integer values strictly between the roots
    const first = @as(u64, @intFromFloat(@floor(t_low))) + 1;
    const last_float = @ceil(t_high) - 1.0;
    if (last_float < 0) {
        return 0;
    }
    const last = @as(u64, @intFromFloat(last_float));

    if (last < first) {
        return 0;
    }
    return last - first + 1;
}

fn parseNumbers(line: []const u8, buffer: []u64) usize {
    // Skip until after the colon
    var i: usize = 0;
    while (i < line.len and line[i] != ':') : (i += 1) {}
    i += 1; // Skip the colon

    var count: usize = 0;
    while (i < line.len) {
        // Skip whitespace
        while (i < line.len and line[i] == ' ') : (i += 1) {}
        if (i >= line.len) break;

        // Parse number
        var num: u64 = 0;
        while (i < line.len and line[i] >= '0' and line[i] <= '9') : (i += 1) {
            num = num * 10 + (line[i] - '0');
        }
        buffer[count] = num;
        count += 1;
    }
    return count;
}

fn concatenateNumbers(numbers: []const u64, count: usize) u64 {
    var result: u64 = 0;
    for (0..count) |i| {
        const num = numbers[i];
        // Find the number of digits
        var digits: u64 = 1;
        var temp = num;
        while (temp >= 10) : (temp /= 10) {
            digits += 1;
        }
        // Shift result and add the number
        var j: u64 = 0;
        while (j < digits) : (j += 1) {
            result *= 10;
        }
        result += num;
    }
    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const input = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(input);

    // Split into lines
    var lines_iter = std.mem.splitScalar(u8, input, '\n');
    const time_line = lines_iter.next() orelse return;
    const distance_line = lines_iter.next() orelse return;

    // Parse times and distances
    var times: [10]u64 = undefined;
    var distances: [10]u64 = undefined;
    const time_count = parseNumbers(time_line, &times);
    const distance_count = parseNumbers(distance_line, &distances);

    _ = distance_count;

    // Part 1: Multiply counts for all races
    var part1: u64 = 1;
    for (0..time_count) |i| {
        const ways = countWaysToWin(times[i], distances[i]);
        part1 *= ways;
    }

    // Part 2: Concatenate all times and distances into single numbers
    const big_time = concatenateNumbers(&times, time_count);
    const big_distance = concatenateNumbers(&distances, time_count);
    const part2 = countWaysToWin(big_time, big_distance);

    std.debug.print("Part 1: {d}\n", .{part1});
    std.debug.print("Part 2: {d}\n", .{part2});
}
