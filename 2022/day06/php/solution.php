<?php
/**
 * Advent of Code 2022 - Day 6: Tuning Trouble
 *
 * Find the first position where the last N characters are all unique.
 * Part 1: N=4 (start-of-packet marker)
 * Part 2: N=14 (start-of-message marker)
 */

function findMarker(string $data, int $windowSize): int {
    $len = strlen($data);

    for ($i = $windowSize; $i <= $len; $i++) {
        $window = substr($data, $i - $windowSize, $windowSize);
        $chars = str_split($window);

        if (count(array_unique($chars)) === $windowSize) {
            return $i;
        }
    }

    return -1;
}

function part1(string $data): int {
    return findMarker($data, 4);
}

function part2(string $data): int {
    return findMarker($data, 14);
}

// Main
$inputFile = __DIR__ . '/../input.txt';
$data = trim(file_get_contents($inputFile));

echo 'Part 1: ' . part1($data) . PHP_EOL;
echo 'Part 2: ' . part2($data) . PHP_EOL;
