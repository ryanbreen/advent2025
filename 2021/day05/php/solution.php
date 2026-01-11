<?php

function parseInput(): array {
    $inputPath = __DIR__ . '/../input.txt';
    $content = file_get_contents($inputPath);
    $lines = [];

    foreach (explode("\n", $content) as $line) {
        $line = trim($line);
        if (empty($line)) {
            continue;
        }
        $parts = explode(' -> ', $line);
        list($x1, $y1) = array_map('intval', explode(',', $parts[0]));
        list($x2, $y2) = array_map('intval', explode(',', $parts[1]));
        $lines[] = [$x1, $y1, $x2, $y2];
    }

    return $lines;
}

function sign(int $x): int {
    if ($x > 0) return 1;
    if ($x < 0) return -1;
    return 0;
}

function countOverlaps(array $lines, bool $includeDiagonals = false): int {
    $grid = [];

    foreach ($lines as $line) {
        list($x1, $y1, $x2, $y2) = $line;
        $dx = sign($x2 - $x1);
        $dy = sign($y2 - $y1);

        // Skip diagonals in part 1
        if (!$includeDiagonals && $dx !== 0 && $dy !== 0) {
            continue;
        }

        $x = $x1;
        $y = $y1;
        while (true) {
            $key = "$x,$y";
            if (!isset($grid[$key])) {
                $grid[$key] = 0;
            }
            $grid[$key]++;

            if ($x === $x2 && $y === $y2) {
                break;
            }
            $x += $dx;
            $y += $dy;
        }
    }

    $count = 0;
    foreach ($grid as $value) {
        if ($value >= 2) {
            $count++;
        }
    }
    return $count;
}

function part1(array $lines): int {
    return countOverlaps($lines, false);
}

function part2(array $lines): int {
    return countOverlaps($lines, true);
}

$lines = parseInput();
echo "Part 1: " . part1($lines) . "\n";
echo "Part 2: " . part2($lines) . "\n";
