#!/usr/bin/env php
<?php
declare(strict_types=1);

/**
 * Advent of Code 2023 - Day 11: Cosmic Expansion
 *
 * Calculates the sum of Manhattan distances between all pairs of galaxies,
 * accounting for cosmic expansion in empty rows and columns.
 */

/**
 * Parse the grid and return all galaxy positions.
 *
 * @param array<int, string> $lines
 * @return array<int, array{0: int, 1: int}>
 */
function parseGrid(array $lines): array {
    $galaxies = [];
    foreach ($lines as $r => $line) {
        $len = strlen($line);
        for ($c = 0; $c < $len; $c++) {
            if ($line[$c] === '#') {
                $galaxies[] = [$r, $c];
            }
        }
    }
    return $galaxies;
}

/**
 * Find all empty rows and columns in the grid.
 *
 * @param array<int, string> $lines
 * @return array{0: array<int, bool>, 1: array<int, bool>}
 */
function findEmptyRowsAndCols(array $lines): array {
    $rows = count($lines);
    $cols = $rows > 0 ? strlen($lines[0]) : 0;

    $emptyRows = [];
    $emptyCols = [];

    // Find empty rows
    foreach ($lines as $r => $line) {
        if (!str_contains($line, '#')) {
            $emptyRows[$r] = true;
        }
    }

    // Find empty columns
    for ($c = 0; $c < $cols; $c++) {
        $hasGalaxy = false;
        for ($r = 0; $r < $rows; $r++) {
            if (isset($lines[$r][$c]) && $lines[$r][$c] === '#') {
                $hasGalaxy = true;
                break;
            }
        }
        if (!$hasGalaxy) {
            $emptyCols[$c] = true;
        }
    }

    return [$emptyRows, $emptyCols];
}

/**
 * Calculate total Manhattan distances between all galaxy pairs with expansion.
 *
 * @param array<int, array{0: int, 1: int}> $galaxies
 * @param array<int, bool> $emptyRows
 * @param array<int, bool> $emptyCols
 * @param int $expansionFactor
 * @return int
 */
function calculateDistances(array $galaxies, array $emptyRows, array $emptyCols, int $expansionFactor): int {
    $total = 0;
    $n = count($galaxies);

    // Iterate over all pairs
    for ($i = 0; $i < $n - 1; $i++) {
        for ($j = $i + 1; $j < $n; $j++) {
            [$r1, $c1] = $galaxies[$i];
            [$r2, $c2] = $galaxies[$j];

            // Calculate row distance with expansion
            $minR = min($r1, $r2);
            $maxR = max($r1, $r2);
            $rowDist = $maxR - $minR;
            for ($r = $minR; $r < $maxR; $r++) {
                if (isset($emptyRows[$r])) {
                    $rowDist += $expansionFactor - 1;
                }
            }

            // Calculate column distance with expansion
            $minC = min($c1, $c2);
            $maxC = max($c1, $c2);
            $colDist = $maxC - $minC;
            for ($c = $minC; $c < $maxC; $c++) {
                if (isset($emptyCols[$c])) {
                    $colDist += $expansionFactor - 1;
                }
            }

            $total += $rowDist + $colDist;
        }
    }

    return $total;
}

/**
 * Part 1: Calculate distances with expansion factor of 2.
 *
 * @param array<int, array{0: int, 1: int}> $galaxies
 * @param array<int, bool> $emptyRows
 * @param array<int, bool> $emptyCols
 * @return int
 */
function part1(array $galaxies, array $emptyRows, array $emptyCols): int {
    return calculateDistances($galaxies, $emptyRows, $emptyCols, 2);
}

/**
 * Part 2: Calculate distances with expansion factor of 1,000,000.
 *
 * @param array<int, array{0: int, 1: int}> $galaxies
 * @param array<int, bool> $emptyRows
 * @param array<int, bool> $emptyCols
 * @return int
 */
function part2(array $galaxies, array $emptyRows, array $emptyCols): int {
    return calculateDistances($galaxies, $emptyRows, $emptyCols, 1000000);
}

function main(): void {
    global $argv;

    $inputFile = $argv[1] ?? __DIR__ . '/../input.txt';
    $content = file_get_contents($inputFile);

    if ($content === false) {
        throw new RuntimeException("Failed to read input file: $inputFile");
    }

    // Split lines and filter out empty ones
    $lines = array_values(array_filter(
        explode("\n", $content),
        fn(string $line): bool => $line !== ''
    ));

    // Parse grid and find empty rows/cols once
    $galaxies = parseGrid($lines);
    [$emptyRows, $emptyCols] = findEmptyRowsAndCols($lines);

    echo "Part 1: " . part1($galaxies, $emptyRows, $emptyCols) . "\n";
    echo "Part 2: " . part2($galaxies, $emptyRows, $emptyCols) . "\n";
}

main();
