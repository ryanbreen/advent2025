<?php

declare(strict_types=1);

/**
 * Advent of Code 2023 - Day 10: Pipe Maze
 *
 * Find the main loop in a pipe maze and count enclosed tiles.
 */

// Character constants
const CHAR_START = 'S';
const CHAR_VERTICAL = '|';
const CHAR_HORIZONTAL = '-';
const CHAR_BEND_NE = 'L';
const CHAR_BEND_NW = 'J';
const CHAR_BEND_SW = '7';
const CHAR_BEND_SE = 'F';

// Direction constants
const DIR_NORTH = [-1, 0];
const DIR_SOUTH = [1, 0];
const DIR_EAST = [0, 1];
const DIR_WEST = [0, -1];

/**
 * Gets pipe connections for a given character.
 *
 * @param string $char The pipe character
 * @return array<int, array{int, int}>|null Connection directions or null if not a pipe
 */
function getPipeConnections(string $char): ?array
{
    return match ($char) {
        CHAR_VERTICAL => [DIR_NORTH, DIR_SOUTH],
        CHAR_HORIZONTAL => [DIR_WEST, DIR_EAST],
        CHAR_BEND_NE => [DIR_NORTH, DIR_EAST],
        CHAR_BEND_NW => [DIR_NORTH, DIR_WEST],
        CHAR_BEND_SW => [DIR_SOUTH, DIR_WEST],
        CHAR_BEND_SE => [DIR_SOUTH, DIR_EAST],
        default => null,
    };
}

// All pipe characters for iteration
const PIPE_CHARS = [
    CHAR_VERTICAL,
    CHAR_HORIZONTAL,
    CHAR_BEND_NE,
    CHAR_BEND_NW,
    CHAR_BEND_SW,
    CHAR_BEND_SE,
];

/**
 * Creates a string key from row and column coordinates.
 *
 * @param int $row Row index
 * @param int $col Column index
 * @return string Position key in "row,col" format
 */
function positionKey(int $row, int $col): string
{
    return $row . ',' . $col;
}

/**
 * Finds the starting position (marked with 'S') in the grid.
 *
 * @param array<int, string> $grid The input grid
 * @return array{int, int} Row and column of start position
 */
function findStart(array $grid): array
{
    foreach ($grid as $row => $line) {
        $col = strpos($line, CHAR_START);
        if ($col !== false) {
            return [$row, $col];
        }
    }
    return [-1, -1];
}

/**
 * Gets valid neighboring positions connected by pipes.
 *
 * @param array<int, string> $grid The input grid
 * @param array{int, int} $pos Current position [row, col]
 * @return array<int, array{int, int}> List of neighbor positions
 */
function getNeighbors(array $grid, array $pos): array
{
    [$row, $col] = $pos;
    $rows = count($grid);
    $cols = strlen($grid[0]);
    $char = $grid[$row][$col];

    if ($char === CHAR_START) {
        return getStartNeighbors($grid, $row, $col, $rows, $cols);
    }

    $connections = getPipeConnections($char);
    if ($connections === null) {
        return [];
    }

    $neighbors = [];
    foreach ($connections as [$dr, $dc]) {
        $newRow = $row + $dr;
        $newCol = $col + $dc;
        if ($newRow >= 0 && $newRow < $rows && $newCol >= 0 && $newCol < $cols) {
            $neighbors[] = [$newRow, $newCol];
        }
    }
    return $neighbors;
}

/**
 * Gets neighbors for the start position by finding pipes that connect back to it.
 *
 * @param array<int, string> $grid The input grid
 * @param int $row Start row
 * @param int $col Start column
 * @param int $rows Total rows
 * @param int $cols Total columns
 * @return array<int, array{int, int}> List of neighbor positions
 */
function getStartNeighbors(array $grid, int $row, int $col, int $rows, int $cols): array
{
    $neighbors = [];
    $directions = [DIR_NORTH, DIR_SOUTH, DIR_WEST, DIR_EAST];

    foreach ($directions as [$dr, $dc]) {
        $newRow = $row + $dr;
        $newCol = $col + $dc;

        if ($newRow < 0 || $newRow >= $rows || $newCol < 0 || $newCol >= $cols) {
            continue;
        }

        $adjChar = $grid[$newRow][$newCol];
        $adjConnections = getPipeConnections($adjChar);
        if ($adjConnections === null) {
            continue;
        }

        // Check if adjacent pipe connects back to start
        foreach ($adjConnections as [$adjDr, $adjDc]) {
            if ($newRow + $adjDr === $row && $newCol + $adjDc === $col) {
                $neighbors[] = [$newRow, $newCol];
                break;
            }
        }
    }
    return $neighbors;
}

/**
 * Finds the main loop using BFS and returns distances from start.
 *
 * @param array<int, string> $grid The input grid
 * @param array{int, int} $start Start position
 * @return array<string, int> Map of position keys to distances
 */
function findLoop(array $grid, array $start): array
{
    $distances = [];
    $distances[positionKey($start[0], $start[1])] = 0;

    $queue = new SplQueue();
    $queue->enqueue($start);

    while (!$queue->isEmpty()) {
        $pos = $queue->dequeue();
        $currentDist = $distances[positionKey($pos[0], $pos[1])];

        foreach (getNeighbors($grid, $pos) as $neighbor) {
            $key = positionKey($neighbor[0], $neighbor[1]);
            if (!isset($distances[$key])) {
                $distances[$key] = $currentDist + 1;
                $queue->enqueue($neighbor);
            }
        }
    }

    return $distances;
}

/**
 * Determines what pipe type the start position should be.
 *
 * @param array<int, string> $grid The input grid
 * @param array{int, int} $start Start position
 * @param array<string, int> $loopPositions Positions that are part of the loop
 * @return string The pipe character that S represents
 */
function determineStartPipe(array $grid, array $start, array $loopPositions): string
{
    [$row, $col] = $start;
    $connections = [];
    $directions = [DIR_NORTH, DIR_SOUTH, DIR_WEST, DIR_EAST];

    foreach ($directions as [$dr, $dc]) {
        $newRow = $row + $dr;
        $newCol = $col + $dc;
        $key = positionKey($newRow, $newCol);

        if (!isset($loopPositions[$key])) {
            continue;
        }

        $adjChar = $grid[$newRow][$newCol];
        $adjConnections = getPipeConnections($adjChar);
        if ($adjConnections === null) {
            continue;
        }

        foreach ($adjConnections as [$adjDr, $adjDc]) {
            if ($newRow + $adjDr === $row && $newCol + $adjDc === $col) {
                $connections[] = [$dr, $dc];
                break;
            }
        }
    }

    // Build connection set for comparison
    $connSet = [];
    foreach ($connections as $conn) {
        $connSet[positionKey($conn[0], $conn[1])] = true;
    }

    // Find matching pipe type
    foreach (PIPE_CHARS as $pipe) {
        $dirs = getPipeConnections($pipe);
        $pipeSet = [];
        foreach ($dirs as $dir) {
            $pipeSet[positionKey($dir[0], $dir[1])] = true;
        }
        if (count($connSet) === count($pipeSet) && count(array_diff_key($connSet, $pipeSet)) === 0) {
            return $pipe;
        }
    }

    return CHAR_START;
}

/**
 * Solves Part 1: Find the farthest point from the start in the main loop.
 *
 * @param array<int, string> $lines The input grid
 * @return int Maximum distance from start
 */
function part1(array $lines): int
{
    $start = findStart($lines);
    $distances = findLoop($lines, $start);
    return max($distances);
}

/**
 * Solves Part 2: Count tiles enclosed by the main loop.
 *
 * Uses ray casting algorithm - a point is inside if it crosses an odd
 * number of loop boundaries when traced horizontally. We count pipes
 * with a north-facing connection (|, L, J) as crossings.
 *
 * @param array<int, string> $lines The input grid
 * @return int Number of enclosed tiles
 */
function part2(array $lines): int
{
    $start = findStart($lines);
    $loopPositions = findLoop($lines, $start);

    // Replace S with its actual pipe type
    $startPipe = determineStartPipe($lines, $start, $loopPositions);
    $grid = $lines;
    $grid[$start[0]][$start[1]] = $startPipe;

    $rows = count($grid);
    $cols = strlen($grid[0]);
    $enclosed = 0;

    for ($row = 0; $row < $rows; $row++) {
        $inside = false;
        for ($col = 0; $col < $cols; $col++) {
            $key = positionKey($row, $col);

            if (isset($loopPositions[$key])) {
                $char = $grid[$row][$col];
                // Count pipes with north-facing connection as crossings
                if ($char === CHAR_VERTICAL || $char === CHAR_BEND_NE || $char === CHAR_BEND_NW) {
                    $inside = !$inside;
                }
            } elseif ($inside) {
                $enclosed++;
            }
        }
    }

    return $enclosed;
}

// Main execution
$input = trim(file_get_contents(__DIR__ . '/../input.txt'));
$lines = explode("\n", $input);

echo "Part 1: " . part1($lines) . "\n";
echo "Part 2: " . part2($lines) . "\n";
