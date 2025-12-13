<?php

$inputFile = __DIR__ . '/../input.txt';
$inputText = trim(file_get_contents($inputFile));

const WIDTH = 101;
const HEIGHT = 103;

/**
 * Parse robot positions and velocities from input.
 * @return array Array of [px, py, vx, vy] for each robot
 */
function parseRobots(string $text): array {
    $robots = [];
    $lines = explode("\n", $text);

    foreach ($lines as $line) {
        if (preg_match('/p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/', $line, $matches)) {
            $robots[] = [
                (int)$matches[1], // px
                (int)$matches[2], // py
                (int)$matches[3], // vx
                (int)$matches[4]  // vy
            ];
        }
    }

    return $robots;
}

/**
 * Simulate robot movement for given seconds.
 * @param array $robots Array of robots [px, py, vx, vy]
 * @param int $seconds Number of seconds to simulate
 * @return array Array of [x, y] positions
 */
function simulate(array $robots, int $seconds): array {
    $positions = [];

    foreach ($robots as [$px, $py, $vx, $vy]) {
        // Position after 'seconds' time, with wrapping
        // Handle negative modulo correctly
        $newX = (($px + $vx * $seconds) % WIDTH + WIDTH) % WIDTH;
        $newY = (($py + $vy * $seconds) % HEIGHT + HEIGHT) % HEIGHT;
        $positions[] = [$newX, $newY];
    }

    return $positions;
}

/**
 * Count robots in each quadrant, excluding middle row/column.
 * @param array $positions Array of [x, y] positions
 * @return array [q1, q2, q3, q4] quadrant counts
 */
function countQuadrants(array $positions): array {
    $midX = intdiv(WIDTH, 2);   // 50
    $midY = intdiv(HEIGHT, 2);  // 51

    $q1 = $q2 = $q3 = $q4 = 0;

    foreach ($positions as [$x, $y]) {
        if ($x === $midX || $y === $midY) {
            continue; // Skip robots on middle lines
        }

        if ($x < $midX && $y < $midY) {
            $q1++; // Top-left
        } elseif ($x > $midX && $y < $midY) {
            $q2++; // Top-right
        } elseif ($x < $midX && $y > $midY) {
            $q3++; // Bottom-left
        } else {
            $q4++; // Bottom-right
        }
    }

    return [$q1, $q2, $q3, $q4];
}

/**
 * Part 1: Safety factor after 100 seconds.
 */
function part1(): int {
    global $inputText;

    $robots = parseRobots($inputText);
    $positions = simulate($robots, 100);
    [$q1, $q2, $q3, $q4] = countQuadrants($positions);

    return $q1 * $q2 * $q3 * $q4;
}

/**
 * Part 2: Find when robots form a Christmas tree pattern.
 */
function part2(): int {
    global $inputText;

    $robots = parseRobots($inputText);

    // The Christmas tree appears when robots cluster together
    // Look for a frame with a long horizontal line of robots (tree base/border)
    for ($seconds = 1; $seconds <= WIDTH * HEIGHT; $seconds++) {
        $positions = simulate($robots, $seconds);

        // Convert positions to set for O(1) lookup
        $posSet = [];
        foreach ($positions as [$x, $y]) {
            $posSet["$x,$y"] = true;
        }

        // Look for a horizontal line of at least 20 consecutive robots
        for ($y = 0; $y < HEIGHT; $y++) {
            $maxConsecutive = 0;
            $consecutive = 0;

            for ($x = 0; $x < WIDTH; $x++) {
                if (isset($posSet["$x,$y"])) {
                    $consecutive++;
                    $maxConsecutive = max($maxConsecutive, $consecutive);
                } else {
                    $consecutive = 0;
                }
            }

            if ($maxConsecutive >= 20) {
                return $seconds;
            }
        }
    }

    return -1;
}

echo "Part 1: " . part1() . "\n";
echo "Part 2: " . part2() . "\n";
