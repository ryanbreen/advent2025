<?php

$input_text = file_get_contents(__DIR__ . '/../input.txt');
$input_text = trim($input_text);

// Parse input
$grid = explode("\n", $input_text);
$rows = count($grid);
$cols = strlen($grid[0]);

// 8 directions: right, left, down, up, and 4 diagonals
$DIRECTIONS = [
    [0, 1],    // right
    [0, -1],   // left
    [1, 0],    // down
    [-1, 0],   // up
    [1, 1],    // down-right
    [1, -1],   // down-left
    [-1, 1],   // up-right
    [-1, -1],  // up-left
];

function part1() {
    global $grid, $rows, $cols, $DIRECTIONS;

    $target = "XMAS";
    $count = 0;

    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            // Try each direction from this position
            foreach ($DIRECTIONS as [$dr, $dc]) {
                // Check if XMAS fits in this direction
                $found = true;
                for ($i = 0; $i < strlen($target); $i++) {
                    $nr = $r + $dr * $i;
                    $nc = $c + $dc * $i;
                    if ($nr < 0 || $nr >= $rows || $nc < 0 || $nc >= $cols) {
                        $found = false;
                        break;
                    }
                    if ($grid[$nr][$nc] !== $target[$i]) {
                        $found = false;
                        break;
                    }
                }
                if ($found) {
                    $count++;
                }
            }
        }
    }

    return $count;
}

function part2() {
    global $grid, $rows, $cols;

    // Find X-MAS patterns: two MAS strings forming an X with A in the center
    // Each diagonal can be MAS or SAM
    $count = 0;

    // Check each possible center point (A must be in the middle)
    for ($r = 1; $r < $rows - 1; $r++) {
        for ($c = 1; $c < $cols - 1; $c++) {
            if ($grid[$r][$c] !== 'A') {
                continue;
            }

            // Get the four corners
            $top_left = $grid[$r - 1][$c - 1];
            $top_right = $grid[$r - 1][$c + 1];
            $bottom_left = $grid[$r + 1][$c - 1];
            $bottom_right = $grid[$r + 1][$c + 1];

            // Check diagonal 1 (top-left to bottom-right): MAS or SAM
            $diag1_ok = ($top_left === 'M' && $bottom_right === 'S') ||
                        ($top_left === 'S' && $bottom_right === 'M');

            // Check diagonal 2 (top-right to bottom-left): MAS or SAM
            $diag2_ok = ($top_right === 'M' && $bottom_left === 'S') ||
                        ($top_right === 'S' && $bottom_left === 'M');

            if ($diag1_ok && $diag2_ok) {
                $count++;
            }
        }
    }

    return $count;
}

echo "Part 1: " . part1() . "\n";
echo "Part 2: " . part2() . "\n";
