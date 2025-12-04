#!/usr/bin/env php
<?php

$inputPath = __DIR__ . '/../input.txt';
$input = trim(file_get_contents($inputPath));
$lines = explode("\n", $input);

// Parse reports
$reports = [];
foreach ($lines as $line) {
    $reports[] = array_map('intval', explode(' ', $line));
}

function isSafe($levels) {
    if (count($levels) < 2) {
        return false;
    }

    $diffs = [];
    for ($i = 0; $i < count($levels) - 1; $i++) {
        $diffs[] = $levels[$i + 1] - $levels[$i];
    }

    // All increasing or all decreasing
    $allIncreasing = true;
    $allDecreasing = true;
    foreach ($diffs as $d) {
        if ($d <= 0) $allIncreasing = false;
        if ($d >= 0) $allDecreasing = false;
    }

    if (!$allIncreasing && !$allDecreasing) {
        return false;
    }

    // All diffs must be 1-3 in absolute value
    foreach ($diffs as $d) {
        if (abs($d) < 1 || abs($d) > 3) {
            return false;
        }
    }

    return true;
}

function isSafeWithDampener($levels) {
    // Already safe without removing anything
    if (isSafe($levels)) {
        return true;
    }

    // Try removing each level one at a time
    for ($i = 0; $i < count($levels); $i++) {
        $modified = array_merge(
            array_slice($levels, 0, $i),
            array_slice($levels, $i + 1)
        );
        if (isSafe($modified)) {
            return true;
        }
    }

    return false;
}

function part1($reports) {
    $count = 0;
    foreach ($reports as $report) {
        if (isSafe($report)) {
            $count++;
        }
    }
    return $count;
}

function part2($reports) {
    $count = 0;
    foreach ($reports as $report) {
        if (isSafeWithDampener($report)) {
            $count++;
        }
    }
    return $count;
}

echo "Part 1: " . part1($reports) . "\n";
echo "Part 2: " . part2($reports) . "\n";
