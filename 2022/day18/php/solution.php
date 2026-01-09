<?php
/**
 * Advent of Code 2022 - Day 18: Boiling Boulders
 *
 * Part 1: Count all exposed cube faces (faces not touching another cube)
 * Part 2: Count only exterior surface area using BFS flood fill
 */

$directions = [
    [1, 0, 0], [-1, 0, 0],
    [0, 1, 0], [0, -1, 0],
    [0, 0, 1], [0, 0, -1]
];

/**
 * Parse input into set of cube coordinates.
 * Returns associative array with "x,y,z" keys for O(1) lookup.
 */
function parseInput(string $text): array {
    $cubes = [];
    foreach (explode("\n", trim($text)) as $line) {
        $coords = explode(',', trim($line));
        $key = implode(',', $coords);
        $cubes[$key] = [(int)$coords[0], (int)$coords[1], (int)$coords[2]];
    }
    return $cubes;
}

/**
 * Part 1: Count total surface area (all exposed faces).
 */
function part1(string $text): int {
    global $directions;

    $cubes = parseInput($text);
    $surfaceArea = 0;

    foreach ($cubes as $coords) {
        [$x, $y, $z] = $coords;
        foreach ($directions as $dir) {
            [$dx, $dy, $dz] = $dir;
            $neighborKey = ($x + $dx) . ',' . ($y + $dy) . ',' . ($z + $dz);
            if (!isset($cubes[$neighborKey])) {
                $surfaceArea++;
            }
        }
    }

    return $surfaceArea;
}

/**
 * Part 2: Count only exterior surface area (excluding trapped air pockets).
 * Uses BFS flood fill from outside the bounding box.
 */
function part2(string $text): int {
    global $directions;

    $cubes = parseInput($text);

    // Find bounding box with 1 unit padding
    $minX = $maxX = $minY = $maxY = $minZ = $maxZ = null;

    foreach ($cubes as $coords) {
        [$x, $y, $z] = $coords;
        if ($minX === null) {
            $minX = $maxX = $x;
            $minY = $maxY = $y;
            $minZ = $maxZ = $z;
        } else {
            $minX = min($minX, $x);
            $maxX = max($maxX, $x);
            $minY = min($minY, $y);
            $maxY = max($maxY, $y);
            $minZ = min($minZ, $z);
            $maxZ = max($maxZ, $z);
        }
    }

    // Add padding
    $minX--; $maxX++;
    $minY--; $maxY++;
    $minZ--; $maxZ++;

    // BFS to find all exterior air cells
    $exterior = [];
    $queue = new SplQueue();

    $startKey = "$minX,$minY,$minZ";
    $queue->enqueue([$minX, $minY, $minZ]);
    $exterior[$startKey] = true;

    while (!$queue->isEmpty()) {
        [$x, $y, $z] = $queue->dequeue();

        foreach ($directions as $dir) {
            [$dx, $dy, $dz] = $dir;
            $nx = $x + $dx;
            $ny = $y + $dy;
            $nz = $z + $dz;

            // Stay within bounds
            if ($nx < $minX || $nx > $maxX ||
                $ny < $minY || $ny > $maxY ||
                $nz < $minZ || $nz > $maxZ) {
                continue;
            }

            $neighborKey = "$nx,$ny,$nz";

            // Skip cubes and already visited
            if (isset($cubes[$neighborKey]) || isset($exterior[$neighborKey])) {
                continue;
            }

            $exterior[$neighborKey] = true;
            $queue->enqueue([$nx, $ny, $nz]);
        }
    }

    // Count faces touching exterior air
    $surfaceArea = 0;
    foreach ($cubes as $coords) {
        [$x, $y, $z] = $coords;
        foreach ($directions as $dir) {
            [$dx, $dy, $dz] = $dir;
            $neighborKey = ($x + $dx) . ',' . ($y + $dy) . ',' . ($z + $dz);
            if (isset($exterior[$neighborKey])) {
                $surfaceArea++;
            }
        }
    }

    return $surfaceArea;
}

// Main execution
$scriptDir = dirname(__FILE__);
$inputFile = $scriptDir . '/../input.txt';
$text = file_get_contents($inputFile);

echo "Part 1: " . part1($text) . "\n";
echo "Part 2: " . part2($text) . "\n";
