<?php
/**
 * Day 16: Reindeer Maze - Weighted shortest path with turn costs
 *
 * Uses Dijkstra's algorithm with bidirectional search to find:
 * - Part 1: Minimum cost path (forward costs 1, turns cost 1000)
 * - Part 2: All tiles on any optimal path
 */

/**
 * Parse the maze and find start/end positions.
 *
 * @param string $text The input maze text
 * @return array [$grid, $start, $end]
 */
function parseInput($text) {
    $grid = array_map('str_split', explode("\n", trim($text)));
    $start = null;
    $end = null;

    foreach ($grid as $y => $row) {
        foreach ($row as $x => $cell) {
            if ($cell === 'S') {
                $start = [$x, $y];
            } elseif ($cell === 'E') {
                $end = [$x, $y];
            }
        }
    }

    return [$grid, $start, $end];
}

// Directions: 0=East, 1=South, 2=West, 3=North
const DX = [1, 0, -1, 0];
const DY = [0, 1, 0, -1];

/**
 * Run Dijkstra from start facing East.
 *
 * @param array $grid The maze grid
 * @param array $start Starting position [x, y]
 * @return array Map of "x,y,dir" => minimum cost to reach that state
 */
function dijkstraForward($grid, $start) {
    // Priority queue: [cost, x, y, direction]
    $pq = new SplMinHeap();
    $pq->insert([0, $start[0], $start[1], 0]); // Start facing East
    $dist = [];

    while (!$pq->isEmpty()) {
        [$cost, $x, $y, $d] = $pq->extract();

        $stateKey = "$x,$y,$d";
        if (isset($dist[$stateKey])) {
            continue;
        }
        $dist[$stateKey] = $cost;

        // Move forward
        $nx = $x + DX[$d];
        $ny = $y + DY[$d];
        if ($ny >= 0 && $ny < count($grid) &&
            $nx >= 0 && $nx < count($grid[0]) &&
            $grid[$ny][$nx] !== '#') {
            $pq->insert([$cost + 1, $nx, $ny, $d]);
        }

        // Turn left/right
        $pq->insert([$cost + 1000, $x, $y, ($d - 1 + 4) % 4]);
        $pq->insert([$cost + 1000, $x, $y, ($d + 1) % 4]);
    }

    return $dist;
}

/**
 * Run Dijkstra backward from end (all directions at end have cost 0).
 *
 * @param array $grid The maze grid
 * @param array $end End position [x, y]
 * @return array Map of "x,y,dir" => minimum cost from that state to reach end
 */
function dijkstraBackward($grid, $end) {
    // At end, we can arrive facing any direction
    $pq = new SplMinHeap();
    for ($d = 0; $d < 4; $d++) {
        $pq->insert([0, $end[0], $end[1], $d]);
    }
    $dist = [];

    while (!$pq->isEmpty()) {
        [$cost, $x, $y, $d] = $pq->extract();

        $stateKey = "$x,$y,$d";
        if (isset($dist[$stateKey])) {
            continue;
        }
        $dist[$stateKey] = $cost;

        // Reverse of "move forward": come from behind
        $px = $x - DX[$d];
        $py = $y - DY[$d];
        if ($py >= 0 && $py < count($grid) &&
            $px >= 0 && $px < count($grid[0]) &&
            $grid[$py][$px] !== '#') {
            $pq->insert([$cost + 1, $px, $py, $d]);
        }

        // Reverse of turn: came from same position with different direction
        $pq->insert([$cost + 1000, $x, $y, ($d - 1 + 4) % 4]);
        $pq->insert([$cost + 1000, $x, $y, ($d + 1) % 4]);
    }

    return $dist;
}

/**
 * Find the lowest score path from start to end.
 *
 * @param array $grid The maze grid
 * @param array $start Starting position
 * @param array $end End position
 * @return int Minimum cost
 */
function part1($grid, $start, $end) {
    $dist = dijkstraForward($grid, $start);
    $minCost = PHP_INT_MAX;

    for ($d = 0; $d < 4; $d++) {
        $key = "{$end[0]},{$end[1]},$d";
        if (isset($dist[$key])) {
            $minCost = min($minCost, $dist[$key]);
        }
    }

    return $minCost;
}

/**
 * Count tiles that are part of any optimal path.
 *
 * @param array $grid The maze grid
 * @param array $start Starting position
 * @param array $end End position
 * @param int $bestScore The optimal score from part 1
 * @return int Number of tiles on optimal paths
 */
function part2($grid, $start, $end, $bestScore) {
    $distFromStart = dijkstraForward($grid, $start);
    $distToEnd = dijkstraBackward($grid, $end);

    $tilesOnBestPath = [];

    for ($y = 0; $y < count($grid); $y++) {
        for ($x = 0; $x < count($grid[0]); $x++) {
            if ($grid[$y][$x] === '#') {
                continue;
            }

            // Check if this tile is on any optimal path
            for ($d = 0; $d < 4; $d++) {
                $stateKey = "$x,$y,$d";
                $fromStart = $distFromStart[$stateKey] ?? PHP_INT_MAX;
                $toEnd = $distToEnd[$stateKey] ?? PHP_INT_MAX;

                if ($fromStart + $toEnd === $bestScore) {
                    $tilesOnBestPath["$x,$y"] = true;
                    break;
                }
            }
        }
    }

    return count($tilesOnBestPath);
}

/**
 * Main execution
 */
function main() {
    $inputPath = dirname(__FILE__) . "/../input.txt";
    $text = file_get_contents($inputPath);
    [$grid, $start, $end] = parseInput($text);

    $answer1 = part1($grid, $start, $end);
    echo "Part 1: $answer1\n";

    $answer2 = part2($grid, $start, $end, $answer1);
    echo "Part 2: $answer2\n";
}

main();
