<?php

$input = trim(file_get_contents(__DIR__ . '/../input.txt'));
$lines = explode("\n", $input);

// Define pipe connections: each pipe connects to certain directions
// Directions: N=[-1,0], S=[1,0], E=[0,1], W=[0,-1]
$PIPE_CONNECTIONS = [
    '|' => [[-1, 0], [1, 0]],    // N, S
    '-' => [[0, -1], [0, 1]],    // W, E
    'L' => [[-1, 0], [0, 1]],    // N, E
    'J' => [[-1, 0], [0, -1]],   // N, W
    '7' => [[1, 0], [0, -1]],    // S, W
    'F' => [[1, 0], [0, 1]],     // S, E
];

function findStart(array $grid): array {
    foreach ($grid as $r => $row) {
        for ($c = 0; $c < strlen($row); $c++) {
            if ($row[$c] === 'S') {
                return [$r, $c];
            }
        }
    }
    return [-1, -1];
}

function getNeighbors(array $grid, array $pos, array $pipeConnections): array {
    [$r, $c] = $pos;
    $rows = count($grid);
    $cols = strlen($grid[0]);
    $ch = $grid[$r][$c];

    if ($ch === 'S') {
        // S can connect to any adjacent pipe that connects back to it
        $neighbors = [];
        foreach ([[-1, 0], [1, 0], [0, -1], [0, 1]] as [$dr, $dc]) {
            $nr = $r + $dr;
            $nc = $c + $dc;
            if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
                $adjCh = $grid[$nr][$nc];
                if (isset($pipeConnections[$adjCh])) {
                    // Check if adjacent pipe connects back to S
                    foreach ($pipeConnections[$adjCh] as [$adjDr, $adjDc]) {
                        if ($nr + $adjDr === $r && $nc + $adjDc === $c) {
                            $neighbors[] = [$nr, $nc];
                            break;
                        }
                    }
                }
            }
        }
        return $neighbors;
    } elseif (isset($pipeConnections[$ch])) {
        $neighbors = [];
        foreach ($pipeConnections[$ch] as [$dr, $dc]) {
            $nr = $r + $dr;
            $nc = $c + $dc;
            if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
                $neighbors[] = [$nr, $nc];
            }
        }
        return $neighbors;
    }
    return [];
}

function findLoop(array $grid, array $start, array $pipeConnections): array {
    // BFS to find the main loop and distances from start
    $distances = [];
    $distances[$start[0] . ',' . $start[1]] = 0;
    $queue = [$start];
    $head = 0;

    while ($head < count($queue)) {
        $pos = $queue[$head++];
        $currentDist = $distances[$pos[0] . ',' . $pos[1]];

        foreach (getNeighbors($grid, $pos, $pipeConnections) as $neighbor) {
            $key = $neighbor[0] . ',' . $neighbor[1];
            if (!isset($distances[$key])) {
                $distances[$key] = $currentDist + 1;
                $queue[] = $neighbor;
            }
        }
    }

    return $distances;
}

function determineStartPipe(array $grid, array $start, array $loopPositions, array $pipeConnections): string {
    [$r, $c] = $start;
    $rows = count($grid);
    $cols = strlen($grid[0]);

    $connections = [];
    foreach ([[-1, 0], [1, 0], [0, -1], [0, 1]] as [$dr, $dc]) {
        $nr = $r + $dr;
        $nc = $c + $dc;
        $key = $nr . ',' . $nc;

        if (isset($loopPositions[$key])) {
            $adjCh = $grid[$nr][$nc];
            if (isset($pipeConnections[$adjCh])) {
                foreach ($pipeConnections[$adjCh] as [$adjDr, $adjDc]) {
                    if ($nr + $adjDr === $r && $nc + $adjDc === $c) {
                        $connections[] = [$dr, $dc];
                        break;
                    }
                }
            }
        }
    }

    // Convert to a comparable format
    $connSet = [];
    foreach ($connections as $conn) {
        $connSet[$conn[0] . ',' . $conn[1]] = true;
    }

    foreach ($pipeConnections as $pipe => $dirs) {
        $pipeSet = [];
        foreach ($dirs as $dir) {
            $pipeSet[$dir[0] . ',' . $dir[1]] = true;
        }
        if (count($connSet) === count($pipeSet) && count(array_diff_key($connSet, $pipeSet)) === 0) {
            return $pipe;
        }
    }

    return 'S';
}

function part1(array $lines, array $pipeConnections): int {
    $start = findStart($lines);
    $distances = findLoop($lines, $start, $pipeConnections);
    return max($distances);
}

function part2(array $lines, array $pipeConnections): int {
    $start = findStart($lines);
    $distances = findLoop($lines, $start, $pipeConnections);
    $loopPositions = $distances; // The keys are the loop positions

    // Replace S with its actual pipe type
    $startPipe = determineStartPipe($lines, $start, $loopPositions, $pipeConnections);
    $grid = $lines;
    $grid[$start[0]][$start[1]] = $startPipe;

    $rows = count($grid);
    $cols = strlen($grid[0]);
    $enclosed = 0;

    for ($r = 0; $r < $rows; $r++) {
        $inside = false;
        for ($c = 0; $c < $cols; $c++) {
            $key = $r . ',' . $c;
            if (isset($loopPositions[$key])) {
                $ch = $grid[$r][$c];
                // Count vertical crossings (|, L, J go "north")
                // Using "north" rule: count pipes that have a north connection
                if ($ch === '|' || $ch === 'L' || $ch === 'J') {
                    $inside = !$inside;
                }
            } else {
                if ($inside) {
                    $enclosed++;
                }
            }
        }
    }

    return $enclosed;
}

echo "Part 1: " . part1($lines, $PIPE_CONNECTIONS) . "\n";
echo "Part 2: " . part2($lines, $PIPE_CONNECTIONS) . "\n";
