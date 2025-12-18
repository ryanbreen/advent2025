#!/usr/bin/env php
<?php

/**
 * Advent of Code 2024 - Day 18: RAM Run
 *
 * Part 1: Find shortest path after 1024 bytes have fallen using BFS
 * Part 2: Binary search to find the first byte that blocks all paths
 */

function parseInput(string $filename): array {
    $positions = [];
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    foreach ($lines as $line) {
        $parts = explode(',', trim($line));
        $positions[] = [(int)$parts[0], (int)$parts[1]];
    }
    return $positions;
}

function bfs(array $corrupted, int $size = 71): int {
    $start = [0, 0];
    $goal = [$size - 1, $size - 1];

    $startKey = "0,0";
    $goalKey = ($size - 1) . "," . ($size - 1);

    if (isset($corrupted[$startKey]) || isset($corrupted[$goalKey])) {
        return -1;
    }

    $queue = new SplQueue();
    $queue->enqueue([$start, 0]);
    $visited = [$startKey => true];

    $directions = [[0, 1], [0, -1], [1, 0], [-1, 0]];

    while (!$queue->isEmpty()) {
        [$pos, $steps] = $queue->dequeue();
        [$x, $y] = $pos;

        if ($x === $goal[0] && $y === $goal[1]) {
            return $steps;
        }

        foreach ($directions as $dir) {
            $nx = $x + $dir[0];
            $ny = $y + $dir[1];
            $key = "$nx,$ny";

            if ($nx >= 0 && $nx < $size && $ny >= 0 && $ny < $size
                && !isset($visited[$key]) && !isset($corrupted[$key])) {
                $visited[$key] = true;
                $queue->enqueue([[$nx, $ny], $steps + 1]);
            }
        }
    }

    return -1;
}

function part1(array $positions, int $numBytes = 1024, int $size = 71): int {
    $corrupted = [];
    for ($i = 0; $i < $numBytes && $i < count($positions); $i++) {
        $key = $positions[$i][0] . "," . $positions[$i][1];
        $corrupted[$key] = true;
    }
    return bfs($corrupted, $size);
}

function part2(array $positions, int $size = 71): string {
    $left = 0;
    $right = count($positions);

    while ($left < $right) {
        $mid = intdiv($left + $right, 2);

        $corrupted = [];
        for ($i = 0; $i <= $mid; $i++) {
            $key = $positions[$i][0] . "," . $positions[$i][1];
            $corrupted[$key] = true;
        }

        if (bfs($corrupted, $size) === -1) {
            $right = $mid;
        } else {
            $left = $mid + 1;
        }
    }

    $blockingPos = $positions[$left];
    return $blockingPos[0] . "," . $blockingPos[1];
}

$positions = parseInput(__DIR__ . "/../input.txt");

echo "Part 1: " . part1($positions) . "\n";
echo "Part 2: " . part2($positions) . "\n";
