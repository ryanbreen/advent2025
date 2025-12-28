use std::cmp::Ordering;
use std::collections::HashMap;
use std::fs;

fn get_hand_type(hand: &str) -> u8 {
    let mut counts: HashMap<char, u8> = HashMap::new();
    for c in hand.chars() {
        *counts.entry(c).or_insert(0) += 1;
    }

    let mut count_values: Vec<u8> = counts.values().cloned().collect();
    count_values.sort_by(|a, b| b.cmp(a));

    match count_values.as_slice() {
        [5] => 6,           // Five of a kind
        [4, 1] => 5,        // Four of a kind
        [3, 2] => 4,        // Full house
        [3, 1, 1] => 3,     // Three of a kind
        [2, 2, 1] => 2,     // Two pair
        [2, 1, 1, 1] => 1,  // One pair
        _ => 0,             // High card
    }
}

fn get_hand_type_with_jokers(hand: &str) -> u8 {
    let joker_count = hand.chars().filter(|&c| c == 'J').count() as u8;

    if joker_count == 0 {
        return get_hand_type(hand);
    }
    if joker_count == 5 {
        return 6; // Five of a kind
    }

    // Count non-joker cards
    let mut counts: HashMap<char, u8> = HashMap::new();
    for c in hand.chars() {
        if c != 'J' {
            *counts.entry(c).or_insert(0) += 1;
        }
    }

    let mut count_values: Vec<u8> = counts.values().cloned().collect();
    count_values.sort_by(|a, b| b.cmp(a));

    // Add jokers to the highest count
    count_values[0] += joker_count;

    match count_values.as_slice() {
        [5] => 6,           // Five of a kind
        [4, 1] => 5,        // Four of a kind
        [3, 2] => 4,        // Full house
        [3, 1, 1] => 3,     // Three of a kind
        [2, 2, 1] => 2,     // Two pair
        [2, 1, 1, 1] => 1,  // One pair
        _ => 0,             // High card
    }
}

fn card_strength(c: char) -> u8 {
    const STRENGTH: &str = "23456789TJQKA";
    STRENGTH.find(c).unwrap() as u8
}

fn card_strength_joker(c: char) -> u8 {
    const STRENGTH: &str = "J23456789TQKA";
    STRENGTH.find(c).unwrap() as u8
}

fn compare_hands(a: &str, b: &str) -> Ordering {
    let type_a = get_hand_type(a);
    let type_b = get_hand_type(b);

    match type_a.cmp(&type_b) {
        Ordering::Equal => {
            // Compare card by card
            for (ca, cb) in a.chars().zip(b.chars()) {
                let strength_cmp = card_strength(ca).cmp(&card_strength(cb));
                if strength_cmp != Ordering::Equal {
                    return strength_cmp;
                }
            }
            Ordering::Equal
        }
        other => other,
    }
}

fn compare_hands_joker(a: &str, b: &str) -> Ordering {
    let type_a = get_hand_type_with_jokers(a);
    let type_b = get_hand_type_with_jokers(b);

    match type_a.cmp(&type_b) {
        Ordering::Equal => {
            // Compare card by card
            for (ca, cb) in a.chars().zip(b.chars()) {
                let strength_cmp = card_strength_joker(ca).cmp(&card_strength_joker(cb));
                if strength_cmp != Ordering::Equal {
                    return strength_cmp;
                }
            }
            Ordering::Equal
        }
        other => other,
    }
}

fn part1(hands: &[(String, u64)]) -> u64 {
    let mut sorted_hands: Vec<_> = hands.iter().collect();
    sorted_hands.sort_by(|a, b| compare_hands(&a.0, &b.0));

    sorted_hands
        .iter()
        .enumerate()
        .map(|(i, (_, bid))| (i as u64 + 1) * bid)
        .sum()
}

fn part2(hands: &[(String, u64)]) -> u64 {
    let mut sorted_hands: Vec<_> = hands.iter().collect();
    sorted_hands.sort_by(|a, b| compare_hands_joker(&a.0, &b.0));

    sorted_hands
        .iter()
        .enumerate()
        .map(|(i, (_, bid))| (i as u64 + 1) * bid)
        .sum()
}

fn main() {
    let input = fs::read_to_string("../input.txt").expect("Failed to read input file");

    let hands: Vec<(String, u64)> = input
        .trim()
        .lines()
        .map(|line| {
            let parts: Vec<&str> = line.split_whitespace().collect();
            (parts[0].to_string(), parts[1].parse().unwrap())
        })
        .collect();

    println!("Part 1: {}", part1(&hands));
    println!("Part 2: {}", part2(&hands));
}
