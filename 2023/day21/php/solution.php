<?php
/**
 * Day 21: Step Counter - Garden plot reachability.
 */

/**
 * Parse input and find starting position.
 * @param string $filename Path to input file
 * @return array [grid, start_row, start_col]
 */
function parseInput(string $filename): array {
    $content = trim(file_get_contents($filename));
    $grid = explode("\n", $content);

    $startRow = 0;
    $startCol = 0;

    foreach ($grid as $r => $row) {
        $pos = strpos($row, 'S');
        if ($pos !== false) {
            $startRow = $r;
            $startCol = $pos;
            break;
        }
    }

    return [$grid, $startRow, $startCol];
}

/**
 * Count cells reachable in exactly 'steps' steps (bounded grid).
 * @param array $grid The grid
 * @param int $startRow Starting row
 * @param int $startCol Starting column
 * @param int $steps Number of steps
 * @return int Count of reachable cells
 */
function countReachable(array $grid, int $startRow, int $startCol, int $steps): int {
    $rows = count($grid);
    $cols = strlen($grid[0]);

    // BFS to find minimum steps to each cell
    $visited = [];
    $queue = new SplQueue();
    $queue->enqueue([$startRow, $startCol, 0]);
    $key = "$startRow,$startCol";
    $visited[$key] = 0;

    $directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];

    while (!$queue->isEmpty()) {
        [$r, $c, $dist] = $queue->dequeue();

        if ($dist >= $steps) {
            continue;
        }

        foreach ($directions as [$dr, $dc]) {
            $nr = $r + $dr;
            $nc = $c + $dc;

            if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
                $key = "$nr,$nc";
                if ($grid[$nr][$nc] !== '#' && !isset($visited[$key])) {
                    $visited[$key] = $dist + 1;
                    $queue->enqueue([$nr, $nc, $dist + 1]);
                }
            }
        }
    }

    // Count cells reachable in exactly 'steps' steps
    // A cell reachable in d steps can be reached in d+2, d+4, ... steps
    $targetParity = $steps % 2;
    $count = 0;

    foreach ($visited as $d) {
        if ($d <= $steps && $d % 2 === $targetParity) {
            $count++;
        }
    }

    return $count;
}

/**
 * BFS on infinite tiled grid for small step counts.
 * @param array $grid The grid
 * @param int $startRow Starting row
 * @param int $startCol Starting column
 * @param int $steps Number of steps
 * @return int Count of reachable cells
 */
function countReachableInfiniteBfs(array $grid, int $startRow, int $startCol, int $steps): int {
    $rows = count($grid);
    $cols = strlen($grid[0]);

    $visited = [];
    $queue = new SplQueue();
    $queue->enqueue([$startRow, $startCol, 0]);
    $key = "$startRow,$startCol";
    $visited[$key] = 0;

    $directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];

    while (!$queue->isEmpty()) {
        [$r, $c, $dist] = $queue->dequeue();

        if ($dist >= $steps) {
            continue;
        }

        foreach ($directions as [$dr, $dc]) {
            $nr = $r + $dr;
            $nc = $c + $dc;

            // Map to grid coordinates (infinite tiling) - handle negative modulo
            $gr = (($nr % $rows) + $rows) % $rows;
            $gc = (($nc % $cols) + $cols) % $cols;

            $key = "$nr,$nc";
            if ($grid[$gr][$gc] !== '#' && !isset($visited[$key])) {
                $visited[$key] = $dist + 1;
                $queue->enqueue([$nr, $nc, $dist + 1]);
            }
        }
    }

    $targetParity = $steps % 2;
    $count = 0;

    foreach ($visited as $d) {
        if ($d <= $steps && $d % 2 === $targetParity) {
            $count++;
        }
    }

    return $count;
}

/**
 * Count cells reachable in exactly 'steps' steps on an infinite tiled grid.
 * Uses the quadratic pattern that emerges due to the grid structure.
 * @param array $grid The grid
 * @param int $startRow Starting row
 * @param int $startCol Starting column
 * @param int $steps Number of steps
 * @return string Result (GMP string for large numbers)
 */
function countReachableInfinite(array $grid, int $startRow, int $startCol, int $steps): string {
    $rows = count($grid);
    $size = $rows;
    $half = intdiv($size, 2);

    // For small step counts, use direct BFS
    if ($steps <= $size * 2) {
        return (string) countReachableInfiniteBfs($grid, $startRow, $startCol, $steps);
    }

    // The number of full grid widths we travel
    // n = (steps - half) / size
    $n = intdiv($steps - $half, $size);

    // Calculate reachable counts for n=0, 1, 2
    $y0 = countReachableInfiniteBfs($grid, $startRow, $startCol, $half);
    $y1 = countReachableInfiniteBfs($grid, $startRow, $startCol, $half + $size);
    $y2 = countReachableInfiniteBfs($grid, $startRow, $startCol, $half + 2 * $size);

    // Solve for a, b, c using finite differences
    // f(x) = ax^2 + bx + c
    // a = (y2 - 2*y1 + y0) / 2
    // b = y1 - y0 - a
    // c = y0

    // Use GMP for large integer arithmetic
    $y0_gmp = gmp_init($y0);
    $y1_gmp = gmp_init($y1);
    $y2_gmp = gmp_init($y2);
    $n_gmp = gmp_init($n);

    // a = (y2 - 2*y1 + y0) / 2
    $a = gmp_div(gmp_sub(gmp_sub($y2_gmp, gmp_mul(2, $y1_gmp)), gmp_neg($y0_gmp)), 2);
    // Correct: a = (y2 - 2*y1 + y0) / 2
    $a = gmp_div(gmp_add(gmp_sub($y2_gmp, gmp_mul(2, $y1_gmp)), $y0_gmp), 2);

    // b = y1 - y0 - a
    $b = gmp_sub(gmp_sub($y1_gmp, $y0_gmp), $a);

    // c = y0
    $c = $y0_gmp;

    // result = a*n^2 + b*n + c
    $n_squared = gmp_mul($n_gmp, $n_gmp);
    $result = gmp_add(gmp_add(gmp_mul($a, $n_squared), gmp_mul($b, $n_gmp)), $c);

    return gmp_strval($result);
}

/**
 * Part 1: Count plots reachable in exactly 64 steps.
 */
function part1(array $grid, int $startRow, int $startCol): int {
    return countReachable($grid, $startRow, $startCol, 64);
}

/**
 * Part 2: Count plots reachable in exactly 26501365 steps on infinite grid.
 */
function part2(array $grid, int $startRow, int $startCol): string {
    return countReachableInfinite($grid, $startRow, $startCol, 26501365);
}

// Main execution
$inputFile = __DIR__ . '/../input.txt';
[$grid, $startRow, $startCol] = parseInput($inputFile);

echo "Part 1: " . part1($grid, $startRow, $startCol) . "\n";
echo "Part 2: " . part2($grid, $startRow, $startCol) . "\n";
