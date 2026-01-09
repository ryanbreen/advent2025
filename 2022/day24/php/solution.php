#!/usr/bin/env php
<?php

function gcd(int $a, int $b): int {
    while ($b !== 0) {
        $t = $b;
        $b = $a % $b;
        $a = $t;
    }
    return $a;
}

function lcm(int $a, int $b): int {
    return intdiv($a * $b, gcd($a, $b));
}

function parseInput(string $text): array {
    $lines = explode("\n", trim($text));
    $height = count($lines);
    $width = strlen($lines[0]);

    // Inner dimensions (excluding walls)
    $innerH = $height - 2;
    $innerW = $width - 2;

    $blizzards = [];
    foreach ($lines as $r => $line) {
        for ($c = 0; $c < strlen($line); $c++) {
            $ch = $line[$c];
            if (in_array($ch, ['^', 'v', '<', '>'], true)) {
                $blizzards[] = [$r, $c, $ch];
            }
        }
    }

    // Find start and end positions
    $start = [0, strpos($lines[0], '.')];
    $end = [$height - 1, strpos($lines[$height - 1], '.')];

    return [$blizzards, $height, $width, $innerH, $innerW, $start, $end];
}

function getBlizzardPositions(array $blizzards, int $innerH, int $innerW, int $time): array {
    $positions = [];
    foreach ($blizzards as [$r, $c, $direction]) {
        // Adjust to inner coordinates (subtract 1 for wall)
        $ir = $r - 1;
        $ic = $c - 1;

        if ($direction === '^') {
            $nr = (($ir - $time) % $innerH + $innerH) % $innerH;
            $nc = $ic;
        } elseif ($direction === 'v') {
            $nr = (($ir + $time) % $innerH + $innerH) % $innerH;
            $nc = $ic;
        } elseif ($direction === '<') {
            $nc = (($ic - $time) % $innerW + $innerW) % $innerW;
            $nr = $ir;
        } else { // '>'
            $nc = (($ic + $time) % $innerW + $innerW) % $innerW;
            $nr = $ir;
        }

        // Convert back to full coordinates
        $positions[($nr + 1) . ',' . ($nc + 1)] = true;
    }

    return $positions;
}

function bfs(array $blizzards, int $height, int $width, int $innerH, int $innerW, array $start, array $end, int $startTime = 0): int {
    $period = lcm($innerH, $innerW);

    // Precompute blizzard positions for all times in one period
    $blizzardCache = [];
    for ($t = 0; $t < $period; $t++) {
        $blizzardCache[$t] = getBlizzardPositions($blizzards, $innerH, $innerW, $t);
    }

    // BFS: state = [time, row, col]
    $queue = new SplQueue();
    $queue->enqueue([$startTime, $start[0], $start[1]]);
    $visited = [];
    $visited[($startTime % $period) . ',' . $start[0] . ',' . $start[1]] = true;

    // directions: wait, up, down, left, right
    $directions = [[0, 0], [-1, 0], [1, 0], [0, -1], [0, 1]];

    while (!$queue->isEmpty()) {
        [$time, $r, $c] = $queue->dequeue();

        if ($r === $end[0] && $c === $end[1]) {
            return $time;
        }

        $nextTime = $time + 1;
        $nextBlizzards = $blizzardCache[$nextTime % $period];

        foreach ($directions as [$dr, $dc]) {
            $nr = $r + $dr;
            $nc = $c + $dc;

            // Check bounds
            $isStart = ($nr === $start[0] && $nc === $start[1]);
            $isEnd = ($nr === $end[0] && $nc === $end[1]);

            if (!$isStart && !$isEnd) {
                if ($nr <= 0 || $nr >= $height - 1 || $nc <= 0 || $nc >= $width - 1) {
                    continue; // Wall
                }
            }

            // Check blizzards
            $posKey = $nr . ',' . $nc;
            if (isset($nextBlizzards[$posKey])) {
                continue;
            }

            $stateKey = ($nextTime % $period) . ',' . $nr . ',' . $nc;
            if (!isset($visited[$stateKey])) {
                $visited[$stateKey] = true;
                $queue->enqueue([$nextTime, $nr, $nc]);
            }
        }
    }

    return -1; // No path found
}

function part1(string $text): int {
    [$blizzards, $height, $width, $innerH, $innerW, $start, $end] = parseInput($text);
    return bfs($blizzards, $height, $width, $innerH, $innerW, $start, $end, 0);
}

function part2(string $text): int {
    [$blizzards, $height, $width, $innerH, $innerW, $start, $end] = parseInput($text);

    // Trip 1: start to end
    $t1 = bfs($blizzards, $height, $width, $innerH, $innerW, $start, $end, 0);

    // Trip 2: end to start
    $t2 = bfs($blizzards, $height, $width, $innerH, $innerW, $end, $start, $t1);

    // Trip 3: start to end again
    $t3 = bfs($blizzards, $height, $width, $innerH, $innerW, $start, $end, $t2);

    return $t3;
}

function main(): void {
    $scriptDir = dirname(__FILE__);
    $inputFile = $scriptDir . '/../input.txt';

    $text = file_get_contents($inputFile);

    echo 'Part 1: ' . part1($text) . "\n";
    echo 'Part 2: ' . part2($text) . "\n";
}

main();
