<?php
declare(strict_types=1);

/**
 * Count how many stones result from a single stone after N blinks.
 * Uses memoization to avoid redundant calculations.
 *
 * @param int $value The stone value
 * @param int $blinks Number of blinks remaining
 * @param array<string, int> $cache Memoization cache passed by reference
 * @return int Number of stones after blinks
 */
function countStones(int $value, int $blinks, array &$cache): int {
    // Base case: no more blinks
    if ($blinks === 0) {
        return 1;
    }

    // Check cache
    $cacheKey = "$value,$blinks";
    if (isset($cache[$cacheKey])) {
        return $cache[$cacheKey];
    }

    $result = 0;

    // Rule 1: 0 becomes 1
    if ($value === 0) {
        $result = countStones(1, $blinks - 1, $cache);
    }
    // Rule 2: Even number of digits -> split
    else {
        $s = (string)$value;
        $len = strlen($s);

        if ($len % 2 === 0) {
            $mid = (int)($len / 2);
            $left = (int)substr($s, 0, $mid);
            $right = (int)substr($s, $mid);
            $result = countStones($left, $blinks - 1, $cache) + countStones($right, $blinks - 1, $cache);
        }
        // Rule 3: Multiply by 2024
        else {
            $result = countStones($value * 2024, $blinks - 1, $cache);
        }
    }

    // Store in cache
    $cache[$cacheKey] = $result;
    return $result;
}

/**
 * @param array<int> $stones Initial stone values
 * @return int Total stones after 25 blinks
 */
function part1(array $stones): int {
    $cache = [];
    $total = 0;
    foreach ($stones as $stone) {
        $total += countStones($stone, 25, $cache);
    }
    return $total;
}

/**
 * @param array<int> $stones Initial stone values
 * @return int Total stones after 75 blinks
 */
function part2(array $stones): int {
    $cache = [];
    $total = 0;
    foreach ($stones as $stone) {
        $total += countStones($stone, 75, $cache);
    }
    return $total;
}

// Read and parse input
$inputFile = __DIR__ . '/../input.txt';
$input = @file_get_contents($inputFile);
if ($input === false) {
    fwrite(STDERR, "Error: Unable to read input file: $inputFile\n");
    exit(1);
}

$input = trim($input);
if (empty($input)) {
    fwrite(STDERR, "Error: Input file is empty\n");
    exit(1);
}

$stones = array_map('intval', explode(' ', $input));

// Run solutions
echo "Part 1: " . part1($stones) . "\n";
echo "Part 2: " . part2($stones) . "\n";
