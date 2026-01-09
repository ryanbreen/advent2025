#!/usr/bin/env php
<?php

/**
 * Day 14: Regolith Reservoir - Falling sand simulation
 */

/**
 * Parse rock paths and return array of rock positions.
 */
function parsePaths(string $text): array {
    $rocks = [];
    $lines = explode("\n", trim($text));

    foreach ($lines as $line) {
        $points = explode(' -> ', $line);
        for ($i = 0; $i < count($points) - 1; $i++) {
            list($x1, $y1) = array_map('intval', explode(',', $points[$i]));
            list($x2, $y2) = array_map('intval', explode(',', $points[$i + 1]));

            // Draw line from (x1, y1) to (x2, y2)
            if ($x1 === $x2) {
                // Vertical line
                for ($y = min($y1, $y2); $y <= max($y1, $y2); $y++) {
                    $rocks["$x1,$y"] = true;
                }
            } else {
                // Horizontal line
                for ($x = min($x1, $x2); $x <= max($x1, $x2); $x++) {
                    $rocks["$x,$y1"] = true;
                }
            }
        }
    }

    return $rocks;
}

/**
 * Simulate one unit of sand falling.
 * Returns the resting position as "x,y", or null if sand falls into abyss.
 */
function simulateSand(array &$blocked, int $maxY, bool $floor = false): ?string {
    $x = 500;
    $y = 0;

    while (true) {
        // Check if sand has fallen below all rocks (into abyss)
        if (!$floor && $y > $maxY) {
            return null;
        }

        // Try to move down
        if ($floor && $y + 1 === $maxY + 2) {
            // Hit the floor
            return "$x,$y";
        } elseif (!isset($blocked["$x," . ($y + 1)])) {
            $y++;
        }
        // Try to move down-left
        elseif (!isset($blocked[($x - 1) . "," . ($y + 1)])) {
            $x--;
            $y++;
        }
        // Try to move down-right
        elseif (!isset($blocked[($x + 1) . "," . ($y + 1)])) {
            $x++;
            $y++;
        }
        // Sand comes to rest
        else {
            return "$x,$y";
        }
    }
}

/**
 * Part 1: Count sand units that come to rest before sand falls into abyss.
 */
function part1(string $text): int {
    $rocks = parsePaths($text);

    // Find max Y coordinate
    $maxY = 0;
    foreach (array_keys($rocks) as $key) {
        list($x, $y) = explode(',', $key);
        $maxY = max($maxY, (int)$y);
    }

    $blocked = $rocks;
    $count = 0;

    while (true) {
        $pos = simulateSand($blocked, $maxY);
        if ($pos === null) {
            break;
        }
        $blocked[$pos] = true;
        $count++;
    }

    return $count;
}

/**
 * Part 2: Count sand units until source is blocked (with floor).
 */
function part2(string $text): int {
    $rocks = parsePaths($text);

    // Find max Y coordinate
    $maxY = 0;
    foreach (array_keys($rocks) as $key) {
        list($x, $y) = explode(',', $key);
        $maxY = max($maxY, (int)$y);
    }

    $blocked = $rocks;
    $count = 0;

    while (true) {
        $pos = simulateSand($blocked, $maxY, true);
        $blocked[$pos] = true;
        $count++;
        if ($pos === "500,0") {
            break;
        }
    }

    return $count;
}

// Main execution
$scriptDir = dirname(__FILE__);
$inputFile = $scriptDir . '/../input.txt';
$text = file_get_contents($inputFile);

echo "Part 1: " . part1($text) . "\n";
echo "Part 2: " . part2($text) . "\n";
