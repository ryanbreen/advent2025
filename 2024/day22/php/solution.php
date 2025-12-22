#!/usr/bin/env php
<?php
/**
 * Day 22: Monkey Market - Pseudorandom number generation for market prices
 */

/**
 * Generate the next secret number using mix and prune operations.
 */
function next_secret(int $secret): int {
    // Step 1: multiply by 64, mix, prune
    $secret ^= ($secret << 6);  // * 64 = << 6
    $secret &= 0xFFFFFF;        // % 16777216 = & (2^24 - 1)

    // Step 2: divide by 32, mix, prune
    $secret ^= ($secret >> 5);  // // 32 = >> 5
    $secret &= 0xFFFFFF;

    // Step 3: multiply by 2048, mix, prune
    $secret ^= ($secret << 11); // * 2048 = << 11
    $secret &= 0xFFFFFF;

    return $secret;
}

/**
 * Generate a sequence of secret numbers.
 */
function generate_secrets(int $initial, int $count): array {
    $secrets = [$initial];
    $secret = $initial;
    for ($i = 0; $i < $count; $i++) {
        $secret = next_secret($secret);
        $secrets[] = $secret;
    }
    return $secrets;
}

/**
 * Part 1: Sum of the 2000th secret number for each buyer.
 */
function part1(array $initial_secrets): int {
    $total = 0;
    foreach ($initial_secrets as $initial) {
        $secret = $initial;
        for ($i = 0; $i < 2000; $i++) {
            $secret = next_secret($secret);
        }
        $total += $secret;
    }
    return $total;
}

/**
 * Part 2: Find the best sequence of 4 price changes to maximize bananas.
 */
function part2(array $initial_secrets): int {
    // Map from sequence key -> total bananas
    $sequence_totals = [];

    foreach ($initial_secrets as $initial) {
        // Generate 2001 secrets (initial + 2000 new)
        $secrets = generate_secrets($initial, 2000);

        // Calculate prices (last digit)
        $prices = array_map(fn($s) => $s % 10, $secrets);

        // Calculate changes
        $changes = [];
        for ($i = 0; $i < count($prices) - 1; $i++) {
            $changes[] = $prices[$i + 1] - $prices[$i];
        }

        // Track first occurrence of each 4-change sequence for this buyer
        $seen = [];
        for ($i = 0; $i <= count($changes) - 4; $i++) {
            // Create sequence key
            $seq_key = $changes[$i] . ',' . $changes[$i + 1] . ',' .
                       $changes[$i + 2] . ',' . $changes[$i + 3];

            if (!isset($seen[$seq_key])) {
                $seen[$seq_key] = true;
                // Price we get is after these 4 changes
                if (!isset($sequence_totals[$seq_key])) {
                    $sequence_totals[$seq_key] = 0;
                }
                $sequence_totals[$seq_key] += $prices[$i + 4];
            }
        }
    }

    return max($sequence_totals);
}

function main() {
    $input_text = file_get_contents(__DIR__ . '/../input.txt');
    $lines = array_filter(array_map('trim', explode("\n", trim($input_text))));
    $initial_secrets = array_map('intval', $lines);

    echo "Part 1: " . part1($initial_secrets) . "\n";
    echo "Part 2: " . part2($initial_secrets) . "\n";
}

main();
