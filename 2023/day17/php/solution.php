#!/usr/bin/env php
<?php
/**
 * Day 17: Clumsy Crucible - Dijkstra's shortest path with movement constraints.
 */

function parseInput(string $filename): array {
    $lines = array_filter(explode("\n", trim(file_get_contents($filename))));
    return array_map(fn($line) => array_map('intval', str_split($line)), $lines);
}

/**
 * Find minimum heat loss path using Dijkstra's algorithm.
 *
 * State: (row, col, direction, consecutive_steps)
 * Directions: 0=right, 1=down, 2=left, 3=up
 */
function dijkstra(array $grid, int $minStraight, int $maxStraight): int {
    $rows = count($grid);
    $cols = count($grid[0]);
    $dr = [0, 1, 0, -1];
    $dc = [1, 0, -1, 0];

    // Priority queue: (heat_loss, row, col, direction, consecutive)
    // PHP's SplPriorityQueue is a max-heap, so we negate the priority
    $pq = new SplPriorityQueue();
    $pq->setExtractFlags(SplPriorityQueue::EXTR_BOTH);

    // Start with no direction (-1)
    $pq->insert([0, 0, -1, 0], 0); // [row, col, direction, consecutive], negated priority
    $visited = [];

    while (!$pq->isEmpty()) {
        $item = $pq->extract();
        [$r, $c, $d, $consec] = $item['data'];
        $heat = -$item['priority'];

        // Check if we reached the goal
        if ($r === $rows - 1 && $c === $cols - 1) {
            if ($minStraight === 0 || $consec >= $minStraight) {
                return $heat;
            }
        }

        $stateKey = "$r,$c,$d,$consec";
        if (isset($visited[$stateKey])) {
            continue;
        }
        $visited[$stateKey] = true;

        // Try all four directions
        for ($nd = 0; $nd < 4; $nd++) {
            // Can't reverse direction
            if ($d !== -1 && $nd === ($d + 2) % 4) {
                continue;
            }

            $nr = $r + $dr[$nd];
            $nc = $c + $dc[$nd];

            // Bounds check
            if ($nr < 0 || $nr >= $rows || $nc < 0 || $nc >= $cols) {
                continue;
            }

            // Check consecutive constraints
            if ($nd === $d) {
                // Continuing in same direction
                $newConsec = $consec + 1;
                if ($newConsec > $maxStraight) {
                    continue;
                }
            } else {
                // Turning - must have gone minStraight in previous direction first
                if ($d !== -1 && $consec < $minStraight) {
                    continue;
                }
                $newConsec = 1;
            }

            $newHeat = $heat + $grid[$nr][$nc];
            $newStateKey = "$nr,$nc,$nd,$newConsec";

            if (!isset($visited[$newStateKey])) {
                $pq->insert([$nr, $nc, $nd, $newConsec], -$newHeat);
            }
        }
    }

    return -1; // No path found
}

function part1(array $grid): int {
    return dijkstra($grid, 0, 3);
}

function part2(array $grid): int {
    return dijkstra($grid, 4, 10);
}

$grid = parseInput(__DIR__ . '/../input.txt');
echo "Part 1: " . part1($grid) . "\n";
echo "Part 2: " . part2($grid) . "\n";
