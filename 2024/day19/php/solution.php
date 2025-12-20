<?php
declare(strict_types=1);

/**
 * Advent of Code 2024 - Day 19: Linen Layout
 *
 * Part 1: Count designs that CAN be formed by concatenating towel patterns
 * Part 2: Sum the NUMBER OF WAYS each design can be formed
 * Uses dynamic programming with memoization
 */

/**
 * Count the number of ways to form design from patterns.
 */
function countWays(string $design, array $patterns): int {
    $len = strlen($design);
    $dp = array_fill(0, $len + 1, 0);
    $dp[0] = 1;

    for ($i = 0; $i < $len; $i++) {
        if ($dp[$i] === 0) {
            continue;
        }
        foreach ($patterns as $pattern) {
            $plen = strlen($pattern);
            if ($i + $plen <= $len && substr_compare($design, $pattern, $i, $plen) === 0) {
                $dp[$i + $plen] += $dp[$i];
            }
        }
    }

    return $dp[$len];
}

(function(): void {
    $input = trim(file_get_contents(__DIR__ . '/../input.txt'));
    [$patternSection, $designSection] = explode("\n\n", $input);

    $patterns = array_map(trim(...), explode(',', $patternSection));
    $designs = explode("\n", trim($designSection));

    $part1 = count(array_filter($designs, fn(string $d): bool => countWays($d, $patterns) > 0));
    $part2 = array_sum(array_map(fn(string $d): int => countWays($d, $patterns), $designs));

    echo "Part 1: {$part1}\n";
    echo "Part 2: {$part2}\n";
})();
