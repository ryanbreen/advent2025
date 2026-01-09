<?php
/**
 * Advent of Code 2022 - Day 12: Hill Climbing Algorithm
 *
 * BFS shortest path in a height-constrained grid.
 * Part 1: Find shortest path from S to E
 * Part 2: Find shortest path from any 'a' elevation to E
 */

/**
 * Parse the grid and find start/end positions.
 *
 * @param string $text Raw input text
 * @return array [grid, start, end]
 */
function parseGrid(string $text): array {
    $lines = explode("\n", trim($text));
    $grid = [];
    $start = null;
    $end = null;

    foreach ($lines as $r => $line) {
        $row = str_split($line);
        foreach ($row as $c => $ch) {
            if ($ch === 'S') {
                $start = [$r, $c];
                $row[$c] = 'a';
            } elseif ($ch === 'E') {
                $end = [$r, $c];
                $row[$c] = 'z';
            }
        }
        $grid[] = $row;
    }

    return [$grid, $start, $end];
}

/**
 * BFS to find shortest path from any start position to end.
 *
 * @param array $grid The height grid
 * @param array $starts Array of starting positions
 * @param array $end End position
 * @return int Shortest distance, or -1 if no path
 */
function bfs(array $grid, array $starts, array $end): int {
    $rows = count($grid);
    $cols = count($grid[0]);
    $visited = [];
    $queue = new SplQueue();

    // Initialize with all start positions
    foreach ($starts as $start) {
        $key = $start[0] . ',' . $start[1];
        $queue->enqueue([$start[0], $start[1], 0]);
        $visited[$key] = true;
    }

    $directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];

    while (!$queue->isEmpty()) {
        [$r, $c, $dist] = $queue->dequeue();

        // Check if we reached the end
        if ($r === $end[0] && $c === $end[1]) {
            return $dist;
        }

        $currentHeight = ord($grid[$r][$c]);

        // Explore neighbors
        foreach ($directions as [$dr, $dc]) {
            $nr = $r + $dr;
            $nc = $c + $dc;
            $key = $nr . ',' . $nc;

            // Check bounds and if not visited
            if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols && !isset($visited[$key])) {
                $nextHeight = ord($grid[$nr][$nc]);

                // Can move if destination is at most 1 higher
                if ($nextHeight <= $currentHeight + 1) {
                    $visited[$key] = true;
                    $queue->enqueue([$nr, $nc, $dist + 1]);
                }
            }
        }
    }

    return -1; // No path found
}

/**
 * Part 1: Find shortest path from S to E.
 */
function part1(string $text): int {
    [$grid, $start, $end] = parseGrid($text);
    return bfs($grid, [$start], $end);
}

/**
 * Part 2: Find shortest path from any 'a' elevation to E.
 */
function part2(string $text): int {
    [$grid, $_, $end] = parseGrid($text);

    // Find all cells with elevation 'a'
    $starts = [];
    foreach ($grid as $r => $row) {
        foreach ($row as $c => $ch) {
            if ($ch === 'a') {
                $starts[] = [$r, $c];
            }
        }
    }

    return bfs($grid, $starts, $end);
}

// Main execution
$scriptDir = dirname(__FILE__);
$inputFile = $scriptDir . '/../input.txt';
$text = file_get_contents($inputFile);

echo "Part 1: " . part1($text) . "\n";
echo "Part 2: " . part2($text) . "\n";
