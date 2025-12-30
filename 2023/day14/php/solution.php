<?php
declare(strict_types=1);

/**
 * Parse input into a 2D grid.
 *
 * @param string $text
 * @return array<int, array<int, string>>
 */
function parseInput(string $text): array
{
    $lines = explode("\n", trim($text));
    $grid = [];
    foreach ($lines as $line) {
        $grid[] = str_split($line);
    }
    return $grid;
}

/**
 * Tilt the grid north, moving all round rocks up.
 *
 * @param array<int, array<int, string>> $grid
 * @return void
 */
function tiltNorth(array &$grid): void
{
    $rows = count($grid);
    $cols = count($grid[0]);

    for ($col = 0; $col < $cols; $col++) {
        $writePos = 0;
        for ($row = 0; $row < $rows; $row++) {
            if ($grid[$row][$col] === '#') {
                $writePos = $row + 1;
            } elseif ($grid[$row][$col] === 'O') {
                $grid[$row][$col] = '.';
                $grid[$writePos][$col] = 'O';
                $writePos++;
            }
        }
    }
}

/**
 * Tilt the grid south, moving all round rocks down.
 *
 * @param array<int, array<int, string>> $grid
 * @return void
 */
function tiltSouth(array &$grid): void
{
    $rows = count($grid);
    $cols = count($grid[0]);

    for ($col = 0; $col < $cols; $col++) {
        $writePos = $rows - 1;
        for ($row = $rows - 1; $row >= 0; $row--) {
            if ($grid[$row][$col] === '#') {
                $writePos = $row - 1;
            } elseif ($grid[$row][$col] === 'O') {
                $grid[$row][$col] = '.';
                $grid[$writePos][$col] = 'O';
                $writePos--;
            }
        }
    }
}

/**
 * Tilt the grid west, moving all round rocks left.
 *
 * @param array<int, array<int, string>> $grid
 * @return void
 */
function tiltWest(array &$grid): void
{
    $rows = count($grid);
    $cols = count($grid[0]);

    for ($row = 0; $row < $rows; $row++) {
        $writePos = 0;
        for ($col = 0; $col < $cols; $col++) {
            if ($grid[$row][$col] === '#') {
                $writePos = $col + 1;
            } elseif ($grid[$row][$col] === 'O') {
                $grid[$row][$col] = '.';
                $grid[$row][$writePos] = 'O';
                $writePos++;
            }
        }
    }
}

/**
 * Tilt the grid east, moving all round rocks right.
 *
 * @param array<int, array<int, string>> $grid
 * @return void
 */
function tiltEast(array &$grid): void
{
    $rows = count($grid);
    $cols = count($grid[0]);

    for ($row = 0; $row < $rows; $row++) {
        $writePos = $cols - 1;
        for ($col = $cols - 1; $col >= 0; $col--) {
            if ($grid[$row][$col] === '#') {
                $writePos = $col - 1;
            } elseif ($grid[$row][$col] === 'O') {
                $grid[$row][$col] = '.';
                $grid[$row][$writePos] = 'O';
                $writePos--;
            }
        }
    }
}

/**
 * Perform one spin cycle: N, W, S, E.
 *
 * @param array<int, array<int, string>> $grid
 * @return void
 */
function spinCycle(array &$grid): void
{
    tiltNorth($grid);
    tiltWest($grid);
    tiltSouth($grid);
    tiltEast($grid);
}

/**
 * Convert grid to a string for cycle detection.
 *
 * @param array<int, array<int, string>> $grid
 * @return string
 */
function gridToString(array $grid): string
{
    $result = '';
    foreach ($grid as $row) {
        $result .= implode('', $row) . "\n";
    }
    return $result;
}

/**
 * Calculate total load on north support beams.
 *
 * @param array<int, array<int, string>> $grid
 * @return int
 */
function calculateLoad(array $grid): int
{
    $rows = count($grid);
    $total = 0;

    for ($row = 0; $row < $rows; $row++) {
        foreach ($grid[$row] as $cell) {
            if ($cell === 'O') {
                $total += $rows - $row;
            }
        }
    }

    return $total;
}

/**
 * Part 1: Tilt north and calculate load.
 *
 * @param array<int, array<int, string>> $grid
 * @return int
 */
function part1(array $grid): int
{
    tiltNorth($grid);
    return calculateLoad($grid);
}

/**
 * Part 2: Run 1 billion spin cycles and calculate load.
 *
 * @param array<int, array<int, string>> $grid
 * @return int
 */
function part2(array $grid): int
{
    $target = 1_000_000_000;
    $seen = [];
    $cycleNum = 0;

    while ($cycleNum < $target) {
        $state = gridToString($grid);

        if (isset($seen[$state])) {
            $cycleStart = $seen[$state];
            $cycleLength = $cycleNum - $cycleStart;
            $remaining = ($target - $cycleNum) % $cycleLength;

            for ($i = 0; $i < $remaining; $i++) {
                spinCycle($grid);
            }

            return calculateLoad($grid);
        }

        $seen[$state] = $cycleNum;
        spinCycle($grid);
        $cycleNum++;
    }

    return calculateLoad($grid);
}

/**
 * Main entry point.
 */
function main(): void
{
    $inputFile = __DIR__ . '/../input.txt';
    $text = file_get_contents($inputFile);

    if ($text === false) {
        fwrite(STDERR, "Error: Could not read input file\n");
        exit(1);
    }

    $grid = parseInput($text);

    echo "Part 1: " . part1($grid) . "\n";
    echo "Part 2: " . part2($grid) . "\n";
}

main();
