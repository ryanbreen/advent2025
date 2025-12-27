<?php
declare(strict_types=1);

$input = trim(file_get_contents(__DIR__ . '/../input.txt'));
$lines = explode("\n", $input);

function parse_races(array $lines): array {
    preg_match_all('/\d+/', $lines[0], $times);
    preg_match_all('/\d+/', $lines[1], $distances);
    $times = array_map('intval', $times[0]);
    $distances = array_map('intval', $distances[0]);
    return array_map(null, $times, $distances);
}

function count_ways_to_win(int $time, int $record): int {
    /**
     * Count the number of ways to beat the record.
     *
     * If we hold the button for t ms, we travel t * (time - t) mm.
     * We need: t * (time - t) > record
     * Solving: -t^2 + time*t - record > 0
     * Roots: t = (time +/- sqrt(time^2 - 4*record)) / 2
     */
    $discriminant = $time * $time - 4 * $record;
    if ($discriminant <= 0) {
        return 0;
    }

    $sqrt_d = sqrt($discriminant);
    $t_low = ($time - $sqrt_d) / 2;
    $t_high = ($time + $sqrt_d) / 2;

    // We need integer values strictly between the roots
    $first = (int)floor($t_low) + 1;
    $last = (int)ceil($t_high) - 1;

    if ($last < $first) {
        return 0;
    }
    return $last - $first + 1;
}

function part1(array $lines): int {
    $races = parse_races($lines);
    return array_reduce($races, fn(int $acc, array $r): int => $acc * count_ways_to_win($r[0], $r[1]), 1);
}

function part2(array $lines): int {
    // Parse by removing all non-digit characters and concatenating
    $time = (int)preg_replace('/\D/', '', $lines[0]);
    $record = (int)preg_replace('/\D/', '', $lines[1]);
    return count_ways_to_win($time, $record);
}

echo "Part 1: " . part1($lines) . "\n";
echo "Part 2: " . part2($lines) . "\n";
