#!/usr/bin/env php
<?php
declare(strict_types=1);

/**
 * Creates a coordinate key string from row and column indices.
 *
 * @param int $r Row index
 * @param int $c Column index
 * @return string Coordinate key in format "r,c"
 */
function coordKey(int $r, int $c): string
{
    return $r . ',' . $c;
}

/**
 * Parses the input file and extracts grid dimensions and antenna positions.
 *
 * @param string $filename Path to the input file
 * @return array Array containing [rows, cols, antennas]
 *               where antennas is a map of frequency => positions
 */
function parseInput(string $filename): array
{
    $grid = array_map('rtrim', file($filename));

    $rows = count($grid);
    $cols = $rows > 0 ? strlen($grid[0]) : 0;

    // Group antenna positions by frequency
    $antennas = [];
    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            $freq = $grid[$r][$c];
            if ($freq !== '.') {
                if (!isset($antennas[$freq])) {
                    $antennas[$freq] = [];
                }
                $antennas[$freq][] = [$r, $c];
            }
        }
    }

    return [$rows, $cols, $antennas];
}

/**
 * Calculates the number of unique antinode positions for Part 1.
 * Antinodes occur at positions where one antenna is twice as far from another.
 *
 * @param array $data Parsed input data [rows, cols, antennas]
 * @return int Count of unique antinode positions
 */
function part1(array $data): int
{
    [$rows, $cols, $antennas] = $data;

    $antinodes = [];

    foreach ($antennas as $freq => $positions) {
        $count = count($positions);

        // For each pair of antennas with same frequency
        for ($i = 0; $i < $count; $i++) {
            for ($j = $i + 1; $j < $count; $j++) {
                [$r1, $c1] = $positions[$i];
                [$r2, $c2] = $positions[$j];

                // Calculate the two antinodes
                // Antinode beyond antenna 1 (away from antenna 2)
                $ar1 = 2 * $r1 - $r2;
                $ac1 = 2 * $c1 - $c2;

                // Antinode beyond antenna 2 (away from antenna 1)
                $ar2 = 2 * $r2 - $r1;
                $ac2 = 2 * $c2 - $c1;

                // Add if within bounds
                if ($ar1 >= 0 && $ar1 < $rows && $ac1 >= 0 && $ac1 < $cols) {
                    $antinodes[coordKey($ar1, $ac1)] = true;
                }
                if ($ar2 >= 0 && $ar2 < $rows && $ac2 >= 0 && $ac2 < $cols) {
                    $antinodes[coordKey($ar2, $ac2)] = true;
                }
            }
        }
    }

    return count($antinodes);
}

/**
 * Calculates the number of unique antinode positions for Part 2.
 * Antinodes occur at all positions along the line between any two antennas.
 *
 * @param array $data Parsed input data [rows, cols, antennas]
 * @return int Count of unique antinode positions
 */
function part2(array $data): int
{
    [$rows, $cols, $antennas] = $data;

    $antinodes = [];

    foreach ($antennas as $freq => $positions) {
        $count = count($positions);

        // For each pair of antennas with same frequency
        for ($i = 0; $i < $count; $i++) {
            for ($j = $i + 1; $j < $count; $j++) {
                [$r1, $c1] = $positions[$i];
                [$r2, $c2] = $positions[$j];

                $dr = $r2 - $r1;
                $dc = $c2 - $c1;

                // Extend in both directions along the line
                // Direction 1: from antenna 1 towards and beyond antenna 2
                $r = $r1;
                $c = $c1;
                while ($r >= 0 && $r < $rows && $c >= 0 && $c < $cols) {
                    $antinodes[coordKey($r, $c)] = true;
                    $r += $dr;
                    $c += $dc;
                }

                // Direction 2: from antenna 1 away from antenna 2
                $r = $r1 - $dr;
                $c = $c1 - $dc;
                while ($r >= 0 && $r < $rows && $c >= 0 && $c < $cols) {
                    $antinodes[coordKey($r, $c)] = true;
                    $r -= $dr;
                    $c -= $dc;
                }
            }
        }
    }

    return count($antinodes);
}

// Parse input once and pass to both functions
$data = parseInput('../input.txt');
echo "Part 1: " . part1($data) . "\n";
echo "Part 2: " . part2($data) . "\n";
