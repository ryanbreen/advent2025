<?php

function parseGrid() {
    $input = file_get_contents(__DIR__ . '/../input.txt');
    $lines = explode("\n", trim($input));

    // Parse the grid
    $grid = [];
    $startRow = 0;
    $startCol = 0;
    $startDir = '';

    foreach ($lines as $i => $line) {
        $grid[] = str_split($line);
        for ($j = 0; $j < strlen($line); $j++) {
            $char = $line[$j];
            if (in_array($char, ['^', 'v', '<', '>'])) {
                $startRow = $i;
                $startCol = $j;
                $startDir = $char;
            }
        }
    }

    return [$grid, $startRow, $startCol, $startDir];
}

function simulateGuard($grid, $startRow, $startCol, $startDir, $detectLoop = false) {
    $height = count($grid);
    $width = count($grid[0]);

    // Direction vectors: up, right, down, left
    $directions = [
        '^' => [-1, 0],
        '>' => [0, 1],
        'v' => [1, 0],
        '<' => [0, -1]
    ];

    // Turn right mapping
    $turnRight = [
        '^' => '>',
        '>' => 'v',
        'v' => '<',
        '<' => '^'
    ];

    // Track visited positions
    $visited = [];
    $visitedKey = $startRow . ',' . $startCol;
    $visited[$visitedKey] = true;

    // Track states (position + direction) for loop detection
    $states = [];
    if ($detectLoop) {
        $stateKey = $startRow . ',' . $startCol . ',' . $startDir;
        $states[$stateKey] = true;
    }

    $row = $startRow;
    $col = $startCol;
    $dir = $startDir;

    // Simulate guard movement
    while (true) {
        $delta = $directions[$dir];
        $nextRow = $row + $delta[0];
        $nextCol = $col + $delta[1];

        // Check if guard leaves the map
        if ($nextRow < 0 || $nextRow >= $height || $nextCol < 0 || $nextCol >= $width) {
            return $detectLoop ? false : $visited;
        }

        // Check if there's an obstacle ahead
        if ($grid[$nextRow][$nextCol] === '#') {
            // Turn right
            $dir = $turnRight[$dir];

            // Check for loop after turning
            if ($detectLoop) {
                $stateKey = $row . ',' . $col . ',' . $dir;
                if (isset($states[$stateKey])) {
                    return true; // Loop detected
                }
                $states[$stateKey] = true;
            }
        } else {
            // Move forward
            $row = $nextRow;
            $col = $nextCol;
            $visitedKey = $row . ',' . $col;
            $visited[$visitedKey] = true;

            // Check for loop (same position and direction)
            if ($detectLoop) {
                $stateKey = $row . ',' . $col . ',' . $dir;
                if (isset($states[$stateKey])) {
                    return true; // Loop detected
                }
                $states[$stateKey] = true;
            }
        }
    }
}

function part1() {
    list($grid, $startRow, $startCol, $startDir) = parseGrid();
    $visited = simulateGuard($grid, $startRow, $startCol, $startDir, false);
    return count($visited);
}

function part2() {
    list($grid, $startRow, $startCol, $startDir) = parseGrid();

    $height = count($grid);
    $width = count($grid[0]);

    // First, get all positions visited in the normal path
    $normalVisited = simulateGuard($grid, $startRow, $startCol, $startDir, false);

    $loopCount = 0;

    // Try placing an obstruction at each position (except the starting position)
    foreach ($normalVisited as $posKey => $value) {
        list($testRow, $testCol) = explode(',', $posKey);
        $testRow = intval($testRow);
        $testCol = intval($testCol);

        // Skip the starting position
        if ($testRow === $startRow && $testCol === $startCol) {
            continue;
        }

        // Skip if already an obstacle
        if ($grid[$testRow][$testCol] === '#') {
            continue;
        }

        // Temporarily place an obstruction
        $grid[$testRow][$testCol] = '#';

        // Simulate and check for loop
        $hasLoop = simulateGuard($grid, $startRow, $startCol, $startDir, true);

        if ($hasLoop) {
            $loopCount++;
        }

        // Remove the obstruction
        $grid[$testRow][$testCol] = '.';
    }

    return $loopCount;
}

echo "Part 1: " . part1() . "\n";
echo "Part 2: " . part2() . "\n";

?>
