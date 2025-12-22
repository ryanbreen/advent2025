use std::collections::{HashMap, HashSet};
use std::fs;

fn next_secret(mut secret: u32) -> u32 {
    // Step 1: multiply by 64, mix, prune
    secret ^= secret << 6;
    secret &= 0xFFFFFF; // % 16777216 = & (2^24 - 1)

    // Step 2: divide by 32, mix, prune
    secret ^= secret >> 5;
    secret &= 0xFFFFFF;

    // Step 3: multiply by 2048, mix, prune
    secret ^= secret << 11;
    secret &= 0xFFFFFF;

    secret
}

fn part1(initial_secrets: &[u32]) -> u64 {
    let mut total = 0u64;
    for &initial in initial_secrets {
        let mut secret = initial;
        for _ in 0..2000 {
            secret = next_secret(secret);
        }
        total += secret as u64;
    }
    total
}

fn part2(initial_secrets: &[u32]) -> u32 {
    // Map from (change1, change2, change3, change4) -> total bananas
    let mut sequence_totals: HashMap<(i8, i8, i8, i8), u32> = HashMap::new();

    for &initial in initial_secrets {
        // Generate 2001 secrets (initial + 2000 new)
        let mut secrets = Vec::with_capacity(2001);
        secrets.push(initial);
        let mut secret = initial;
        for _ in 0..2000 {
            secret = next_secret(secret);
            secrets.push(secret);
        }

        // Calculate prices (last digit)
        let prices: Vec<u8> = secrets.iter().map(|&s| (s % 10) as u8).collect();

        // Calculate changes
        let changes: Vec<i8> = prices
            .windows(2)
            .map(|w| w[1] as i8 - w[0] as i8)
            .collect();

        // Track first occurrence of each 4-change sequence for this buyer
        let mut seen: HashSet<(i8, i8, i8, i8)> = HashSet::new();
        for i in 0..changes.len().saturating_sub(3) {
            let seq = (changes[i], changes[i + 1], changes[i + 2], changes[i + 3]);
            if !seen.contains(&seq) {
                seen.insert(seq);
                // Price we get is after these 4 changes
                *sequence_totals.entry(seq).or_insert(0) += prices[i + 4] as u32;
            }
        }
    }

    *sequence_totals.values().max().unwrap_or(&0)
}

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    let initial_secrets: Vec<u32> = input
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| line.parse().expect("Invalid number"))
        .collect();

    println!("Part 1: {}", part1(&initial_secrets));
    println!("Part 2: {}", part2(&initial_secrets));
}
