#!/usr/bin/env php
<?php

/**
 * Day 15: Beacon Exclusion Zone
 *
 * Parse sensor/beacon positions and calculate coverage using Manhattan distance.
 * Part 1: Count positions in row y=2000000 that cannot contain a beacon
 * Part 2: Find the one uncovered position in 0-4000000 range
 */

function parseSensors(string $text): array {
    $sensors = [];
    $pattern = '/Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/';

    foreach (explode("\n", trim($text)) as $line) {
        if (preg_match($pattern, $line, $matches)) {
            $sx = (int)$matches[1];
            $sy = (int)$matches[2];
            $bx = (int)$matches[3];
            $by = (int)$matches[4];
            $dist = abs($sx - $bx) + abs($sy - $by);
            $sensors[] = [$sx, $sy, $bx, $by, $dist];
        }
    }

    return $sensors;
}

function mergeRanges(array $ranges): array {
    if (empty($ranges)) {
        return [];
    }

    usort($ranges, function($a, $b) {
        return $a[0] <=> $b[0];
    });

    $merged = [$ranges[0]];

    for ($i = 1; $i < count($ranges); $i++) {
        $start = $ranges[$i][0];
        $end = $ranges[$i][1];
        $lastIdx = count($merged) - 1;

        if ($start <= $merged[$lastIdx][1] + 1) {
            $merged[$lastIdx][1] = max($merged[$lastIdx][1], $end);
        } else {
            $merged[] = [$start, $end];
        }
    }

    return $merged;
}

function getCoverageAtRow(array $sensors, int $row): array {
    $ranges = [];

    foreach ($sensors as $sensor) {
        list($sx, $sy, $bx, $by, $dist) = $sensor;

        $rowDist = abs($sy - $row);
        if ($rowDist > $dist) {
            continue;
        }

        $xSpread = $dist - $rowDist;
        $ranges[] = [$sx - $xSpread, $sx + $xSpread];
    }

    return mergeRanges($ranges);
}

function part1(string $text): int {
    $sensors = parseSensors($text);
    $targetRow = 2000000;

    $ranges = getCoverageAtRow($sensors, $targetRow);

    $total = 0;
    foreach ($ranges as $range) {
        $total += $range[1] - $range[0] + 1;
    }

    $beaconsOnRow = [];
    foreach ($sensors as $sensor) {
        list($sx, $sy, $bx, $by, $dist) = $sensor;
        if ($by === $targetRow) {
            $beaconsOnRow[$bx] = true;
        }
    }

    return $total - count($beaconsOnRow);
}

function part2(string $text): int {
    $sensors = parseSensors($text);
    $maxCoord = 4000000;

    for ($row = 0; $row <= $maxCoord; $row++) {
        $ranges = getCoverageAtRow($sensors, $row);

        $clipped = [];
        foreach ($ranges as $range) {
            list($start, $end) = $range;
            if ($end < 0 || $start > $maxCoord) {
                continue;
            }
            $clipped[] = [max(0, $start), min($maxCoord, $end)];
        }

        $clipped = mergeRanges($clipped);

        if (count($clipped) === 1 && $clipped[0][0] === 0 && $clipped[0][1] === $maxCoord) {
            continue;
        }

        if (count($clipped) > 1) {
            $x = $clipped[0][1] + 1;
        } elseif ($clipped[0][0] > 0) {
            $x = 0;
        } else {
            $x = $clipped[0][1] + 1;
        }

        return $x * 4000000 + $row;
    }

    return 0;
}

$scriptDir = dirname(__FILE__);
$inputFile = $scriptDir . '/../input.txt';
$text = file_get_contents($inputFile);

echo 'Part 1: ' . part1($text) . "\n";
echo 'Part 2: ' . part2($text) . "\n";
