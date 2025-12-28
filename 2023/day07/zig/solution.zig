const std = @import("std");

const Hand = struct {
    cards: [5]u8,
    bid: u32,
    hand_type: u8,
    card_values: [5]u8,
};

fn cardStrength(card: u8, with_jokers: bool) u8 {
    if (with_jokers) {
        return switch (card) {
            'J' => 0,
            '2' => 1,
            '3' => 2,
            '4' => 3,
            '5' => 4,
            '6' => 5,
            '7' => 6,
            '8' => 7,
            '9' => 8,
            'T' => 9,
            'Q' => 10,
            'K' => 11,
            'A' => 12,
            else => 0,
        };
    } else {
        return switch (card) {
            '2' => 0,
            '3' => 1,
            '4' => 2,
            '5' => 3,
            '6' => 4,
            '7' => 5,
            '8' => 6,
            '9' => 7,
            'T' => 8,
            'J' => 9,
            'Q' => 10,
            'K' => 11,
            'A' => 12,
            else => 0,
        };
    }
}

fn getHandType(cards: [5]u8, with_jokers: bool) u8 {
    var counts: [13]u8 = .{0} ** 13;
    var joker_count: u8 = 0;

    for (cards) |card| {
        if (with_jokers and card == 'J') {
            joker_count += 1;
        } else {
            counts[cardStrength(card, false)] += 1;
        }
    }

    // Sort counts descending
    std.mem.sort(u8, &counts, {}, std.sort.desc(u8));

    // Add jokers to the highest count
    if (with_jokers) {
        counts[0] += joker_count;
    }

    // Determine hand type
    if (counts[0] == 5) {
        return 6; // Five of a kind
    } else if (counts[0] == 4) {
        return 5; // Four of a kind
    } else if (counts[0] == 3 and counts[1] == 2) {
        return 4; // Full house
    } else if (counts[0] == 3) {
        return 3; // Three of a kind
    } else if (counts[0] == 2 and counts[1] == 2) {
        return 2; // Two pair
    } else if (counts[0] == 2) {
        return 1; // One pair
    } else {
        return 0; // High card
    }
}

fn compareHands(context: void, a: Hand, b: Hand) bool {
    _ = context;
    if (a.hand_type != b.hand_type) {
        return a.hand_type < b.hand_type;
    }
    for (0..5) |i| {
        if (a.card_values[i] != b.card_values[i]) {
            return a.card_values[i] < b.card_values[i];
        }
    }
    return false;
}

fn parseHands(allocator: std.mem.Allocator, input: []const u8, with_jokers: bool) ![]Hand {
    var hands: std.ArrayListUnmanaged(Hand) = .empty;
    errdefer hands.deinit(allocator);

    var lines = std.mem.splitScalar(u8, input, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        var parts = std.mem.splitScalar(u8, line, ' ');
        const hand_str = parts.next() orelse continue;
        const bid_str = parts.next() orelse continue;

        if (hand_str.len != 5) continue;

        var hand: Hand = undefined;
        hand.bid = std.fmt.parseInt(u32, bid_str, 10) catch continue;

        for (0..5) |i| {
            hand.cards[i] = hand_str[i];
            hand.card_values[i] = cardStrength(hand_str[i], with_jokers);
        }

        hand.hand_type = getHandType(hand.cards, with_jokers);
        try hands.append(allocator, hand);
    }

    return hands.toOwnedSlice(allocator);
}

fn calculateWinnings(hands: []Hand) u64 {
    std.mem.sort(Hand, hands, {}, compareHands);

    var total: u64 = 0;
    for (hands, 1..) |hand, rank| {
        total += rank * hand.bid;
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

    // Part 1
    const hands1 = try parseHands(allocator, input, false);
    defer allocator.free(hands1);
    const part1 = calculateWinnings(hands1);

    // Part 2
    const hands2 = try parseHands(allocator, input, true);
    defer allocator.free(hands2);
    const part2 = calculateWinnings(hands2);

    std.debug.print("Part 1: {d}\n", .{part1});
    std.debug.print("Part 2: {d}\n", .{part2});
}
