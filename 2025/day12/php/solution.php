#!/usr/bin/env php
<?php
/**
 * Day 12: Christmas Tree Farm - Polyomino Packing
 *
 * The solution checks if presents (polyominoes) can fit into rectangular regions.
 * For this problem, the constraint is simply: total cells needed <= available cells.
 */

declare(strict_types=1);

function parseInput(string $text): array {
    $sections = array_filter(array_map(trim(...), explode("\n\n", trim($text))));

    $shapes = [];
    $regions = [];

    foreach ($sections as $section) {
        $lines = array_filter(array_map(trim(...), explode("\n", $section)));

        if (empty($lines)) {
            continue;
        }

        $firstLine = $lines[0];

        if (str_contains($firstLine, ':') && !str_contains($firstLine, 'x')) {
            // Shape definition
            $idx = (int) rtrim($firstLine, ':');
            $shapeLines = array_slice($lines, 1);

            // Count total '#' cells in the shape using array functions
            $cellCount = array_sum(
                array_map(fn($line) => substr_count($line, '#'), $shapeLines)
            );

            $shapes[$idx] = $cellCount;
        } else {
            // Region definitions
            foreach ($lines as $line) {
                if (str_contains($line, 'x')) {
                    [$dims, $countsStr] = explode(':', $line, 2);
                    [$w, $h] = array_map('intval', explode('x', trim($dims)));
                    $counts = array_map('intval', preg_split('/\s+/', trim($countsStr)));
                    $regions[] = [$w, $h, $counts];
                }
            }
        }
    }

    return [$shapes, $regions];
}

function canFitRegion(int $width, int $height, array $counts, array $shapeSizes): bool {
    // Calculate total cells needed using array_sum with array_map
    $totalCellsNeeded = array_sum(
        array_map(
            fn($i) => $counts[$i] * $shapeSizes[$i],
            array_keys($counts)
        )
    );

    $available = $width * $height;
    return $totalCellsNeeded <= $available;
}

function part1(array $shapes, array $regions): int {
    // Count regions that can fit using array_filter
    return count(
        array_filter(
            $regions,
            fn($region) => canFitRegion($region[0], $region[1], $region[2], $shapes)
        )
    );
}

function part2(): int {
    // Part 2 is just a button click to finish - no computation needed
    return 0;
}

// Main execution
$text = file_get_contents('../input.txt');
[$shapes, $regions] = parseInput($text);

echo "Part 1: " . part1($shapes, $regions) . "\n";
echo "Part 2: " . part2() . "\n";
