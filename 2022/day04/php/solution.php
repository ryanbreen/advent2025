#!/usr/bin/env php
<?php

function parseInput(string $filename): array {
    $pairs = [];
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);

    foreach ($lines as $line) {
        $parts = explode(',', $line);
        $left = explode('-', $parts[0]);
        $right = explode('-', $parts[1]);
        $pairs[] = [
            (int)$left[0],
            (int)$left[1],
            (int)$right[0],
            (int)$right[1]
        ];
    }

    return $pairs;
}

function fullyContains(int $a1, int $b1, int $a2, int $b2): bool {
    // Check if one range fully contains the other
    return ($a1 <= $a2 && $b1 >= $b2) || ($a2 <= $a1 && $b2 >= $b1);
}

function overlaps(int $a1, int $b1, int $a2, int $b2): bool {
    // Check if ranges overlap at all
    return $a1 <= $b2 && $a2 <= $b1;
}

function part1(array $pairs): int {
    $count = 0;
    foreach ($pairs as $pair) {
        if (fullyContains($pair[0], $pair[1], $pair[2], $pair[3])) {
            $count++;
        }
    }
    return $count;
}

function part2(array $pairs): int {
    $count = 0;
    foreach ($pairs as $pair) {
        if (overlaps($pair[0], $pair[1], $pair[2], $pair[3])) {
            $count++;
        }
    }
    return $count;
}

function main(): void {
    $inputFile = __DIR__ . '/../input.txt';
    $pairs = parseInput($inputFile);

    echo 'Part 1: ' . part1($pairs) . PHP_EOL;
    echo 'Part 2: ' . part2($pairs) . PHP_EOL;
}

main();
