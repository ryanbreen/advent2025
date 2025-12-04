<?php

// Read input file
$input_text = file_get_contents(__DIR__ . '/../input.txt');
$input_text = rtrim($input_text);

// Parse input
$lines = explode("\n", $input_text);

// Directions for 8 neighbors (including diagonals)
const DIRECTIONS = [
    [-1, -1], [-1, 0], [-1, 1],
    [0, -1],           [0, 1],
    [1, -1],  [1, 0],  [1, 1]
];

// ============== PRECOMPUTE ROLL POSITIONS AND NEIGHBORS ==============
$grid = $lines;
$rows = count($grid);
$cols = $rows > 0 ? strlen($grid[0]) : 0;

$roll_positions = [];   // Array of [r, c] for each roll
$pos_to_index = [];     // Array: "r,c" => index in roll_positions
$roll_neighbors = [];   // Array of arrays: neighbors for each roll

for ($r = 0; $r < $rows; $r++) {
    for ($c = 0; $c < $cols; $c++) {
        if ($grid[$r][$c] === '@') {
            $idx = count($roll_positions);
            $roll_positions[] = [$r, $c];
            $pos_to_index["$r,$c"] = $idx;

            // Precompute neighbors for this roll
            $neighbors = [];
            foreach (DIRECTIONS as [$dr, $dc]) {
                $nr = $r + $dr;
                $nc = $c + $dc;
                if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols && $grid[$nr][$nc] === '@') {
                    $neighbors[] = "$nr,$nc";
                }
            }
            $roll_neighbors[] = $neighbors;
        }
    }
}

$num_rolls = count($roll_positions);

function part1($roll_neighbors, $num_rolls) {
    /**
     * Count rolls of paper that can be accessed by a forklift.
     *
     * A roll can be accessed if it has fewer than 4 adjacent rolls
     * in the 8 surrounding positions (including diagonals).
     */
    $accessible_count = 0;

    for ($i = 0; $i < $num_rolls; $i++) {
        $neighbor_count = count($roll_neighbors[$i]);
        if ($neighbor_count < 4) {
            $accessible_count++;
        }
    }

    return $accessible_count;
}

function part2($roll_positions, $roll_neighbors, $pos_to_index, $num_rolls) {
    /**
     * Count total rolls removed by iteratively removing accessible rolls.
     *
     * A roll can be removed if it has fewer than 4 adjacent rolls.
     * After removal, check again for newly accessible rolls.
     * Repeat until no more rolls can be removed.
     */
    // Track which rolls are still active
    $active = [];
    for ($i = 0; $i < $num_rolls; $i++) {
        [$r, $c] = $roll_positions[$i];
        $active["$r,$c"] = true;
    }

    // Compute initial neighbor counts
    $neighbor_count = [];
    for ($i = 0; $i < $num_rolls; $i++) {
        $count = 0;
        foreach ($roll_neighbors[$i] as $neighbor_pos) {
            if (isset($active[$neighbor_pos])) {
                $count++;
            }
        }
        $neighbor_count[$i] = $count;
    }

    // Initialize queue with accessible rolls (neighbor count < 4)
    $queue = [];
    $in_queue = [];
    for ($i = 0; $i < $num_rolls; $i++) {
        if ($neighbor_count[$i] < 4) {
            $queue[] = $i;
            $in_queue[$i] = true;
        }
    }

    // Process queue
    $total_removed = 0;

    while (!empty($queue)) {
        $next_queue = [];

        foreach ($queue as $idx) {
            [$r, $c] = $roll_positions[$idx];
            $pos_key = "$r,$c";

            // Skip if already removed
            if (!isset($active[$pos_key])) {
                continue;
            }

            // Remove this roll
            unset($active[$pos_key]);
            $total_removed++;

            // Update neighbors' counts
            foreach ($roll_neighbors[$idx] as $neighbor_pos) {
                if (isset($active[$neighbor_pos])) {
                    $neighbor_idx = $pos_to_index[$neighbor_pos];
                    $neighbor_count[$neighbor_idx]--;

                    // Add to queue if now accessible and not already queued
                    if ($neighbor_count[$neighbor_idx] < 4 && !isset($in_queue[$neighbor_idx])) {
                        $next_queue[] = $neighbor_idx;
                        $in_queue[$neighbor_idx] = true;
                    }
                }
            }
        }

        $queue = $next_queue;
    }

    return $total_removed;
}

// Run both parts
echo "Part 1: " . part1($roll_neighbors, $num_rolls) . "\n";
echo "Part 2: " . part2($roll_positions, $roll_neighbors, $pos_to_index, $num_rolls) . "\n";
