<?php
/**
 * Advent of Code 2022 - Day 8: Treetop Tree House
 */

function parseGrid(array $lines): array {
    $grid = [];
    foreach ($lines as $line) {
        $grid[] = array_map('intval', str_split($line));
    }
    return $grid;
}

function isVisible(array $grid, int $row, int $col): bool {
    $rows = count($grid);
    $cols = count($grid[0]);
    $height = $grid[$row][$col];

    // Check from left
    $visibleLeft = true;
    for ($c = 0; $c < $col; $c++) {
        if ($grid[$row][$c] >= $height) {
            $visibleLeft = false;
            break;
        }
    }
    if ($visibleLeft) return true;

    // Check from right
    $visibleRight = true;
    for ($c = $col + 1; $c < $cols; $c++) {
        if ($grid[$row][$c] >= $height) {
            $visibleRight = false;
            break;
        }
    }
    if ($visibleRight) return true;

    // Check from top
    $visibleTop = true;
    for ($r = 0; $r < $row; $r++) {
        if ($grid[$r][$col] >= $height) {
            $visibleTop = false;
            break;
        }
    }
    if ($visibleTop) return true;

    // Check from bottom
    $visibleBottom = true;
    for ($r = $row + 1; $r < $rows; $r++) {
        if ($grid[$r][$col] >= $height) {
            $visibleBottom = false;
            break;
        }
    }

    return $visibleBottom;
}

function scenicScore(array $grid, int $row, int $col): int {
    $rows = count($grid);
    $cols = count($grid[0]);
    $height = $grid[$row][$col];

    // Count trees visible to the left
    $left = 0;
    for ($c = $col - 1; $c >= 0; $c--) {
        $left++;
        if ($grid[$row][$c] >= $height) {
            break;
        }
    }

    // Count trees visible to the right
    $right = 0;
    for ($c = $col + 1; $c < $cols; $c++) {
        $right++;
        if ($grid[$row][$c] >= $height) {
            break;
        }
    }

    // Count trees visible upward
    $up = 0;
    for ($r = $row - 1; $r >= 0; $r--) {
        $up++;
        if ($grid[$r][$col] >= $height) {
            break;
        }
    }

    // Count trees visible downward
    $down = 0;
    for ($r = $row + 1; $r < $rows; $r++) {
        $down++;
        if ($grid[$r][$col] >= $height) {
            break;
        }
    }

    return $left * $right * $up * $down;
}

function part1(array $grid): int {
    $rows = count($grid);
    $cols = count($grid[0]);
    $count = 0;

    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            if (isVisible($grid, $r, $c)) {
                $count++;
            }
        }
    }

    return $count;
}

function part2(array $grid): int {
    $rows = count($grid);
    $cols = count($grid[0]);
    $maxScore = 0;

    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            $score = scenicScore($grid, $r, $c);
            $maxScore = max($maxScore, $score);
        }
    }

    return $maxScore;
}

function main(): void {
    $inputFile = __DIR__ . '/../input.txt';
    $content = trim(file_get_contents($inputFile));
    $lines = explode("\n", $content);

    $grid = parseGrid($lines);

    echo 'Part 1: ' . part1($grid) . "\n";
    echo 'Part 2: ' . part2($grid) . "\n";
}

main();
