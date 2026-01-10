<?php

$input = trim(file_get_contents(__DIR__ . '/../input.txt'));
$depths = array_map('intval', explode("\n", $input));

function part1(array $depths): int {
    $count = 0;
    for ($i = 1; $i < count($depths); $i++) {
        if ($depths[$i] > $depths[$i - 1]) {
            $count++;
        }
    }
    return $count;
}

function part2(array $depths): int {
    // Create sliding window sums of 3 consecutive measurements
    $windowSums = [];
    for ($i = 0; $i < count($depths) - 2; $i++) {
        $windowSums[] = $depths[$i] + $depths[$i + 1] + $depths[$i + 2];
    }

    // Count how many times the sum increases
    $count = 0;
    for ($i = 1; $i < count($windowSums); $i++) {
        if ($windowSums[$i] > $windowSums[$i - 1]) {
            $count++;
        }
    }
    return $count;
}

echo "Part 1: " . part1($depths) . "\n";
echo "Part 2: " . part2($depths) . "\n";
