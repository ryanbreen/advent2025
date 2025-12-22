#!/usr/bin/env php
<?php
/**
 * Day 21: Keypad Conundrum - Robot chain control with shortest path optimization
 */

// Keypad layouts - positions as [row, col]
const NUMERIC = [
    '7' => [0, 0], '8' => [0, 1], '9' => [0, 2],
    '4' => [1, 0], '5' => [1, 1], '6' => [1, 2],
    '1' => [2, 0], '2' => [2, 1], '3' => [2, 2],
    '0' => [3, 1], 'A' => [3, 2],
];
const NUMERIC_GAP = [3, 0];

const DIRECTIONAL = [
    '^' => [0, 1], 'A' => [0, 2],
    '<' => [1, 0], 'v' => [1, 1], '>' => [1, 2],
];
const DIRECTIONAL_GAP = [0, 0];

/**
 * Find all shortest paths from start to end, avoiding gap.
 */
function shortest_paths(array $keypad, array $gap, string $start, string $end): array {
    [$sr, $sc] = $keypad[$start];
    [$er, $ec] = $keypad[$end];

    $paths = [];

    $dfs = function($r, $c, $path) use (&$dfs, $gap, $er, $ec, &$paths) {
        if ([$r, $c] === $gap) {
            return;
        }
        if ($r === $er && $c === $ec) {
            $paths[] = $path;
            return;
        }
        // Move vertically toward target
        if ($r < $er) {
            $dfs($r + 1, $c, $path . 'v');
        } elseif ($r > $er) {
            $dfs($r - 1, $c, $path . '^');
        }
        // Move horizontally toward target
        if ($c < $ec) {
            $dfs($r, $c + 1, $path . '>');
        } elseif ($c > $ec) {
            $dfs($r, $c - 1, $path . '<');
        }
    };

    $dfs($sr, $sc, '');
    return $paths ?: [''];  // Empty path if start == end
}

// Memoization cache for min_presses_for_move
$memo = [];

/**
 * Minimum presses needed to move from from_char to to_char and press, at given depth.
 */
function min_presses_for_move(string $from_char, string $to_char, int $depth, bool $is_numeric): int {
    global $memo;

    $cache_key = "{$from_char},{$to_char},{$depth}," . ($is_numeric ? '1' : '0');
    if (isset($memo[$cache_key])) {
        return $memo[$cache_key];
    }

    if ($is_numeric) {
        $keypad = NUMERIC;
        $gap = NUMERIC_GAP;
    } else {
        $keypad = DIRECTIONAL;
        $gap = DIRECTIONAL_GAP;
    }

    $paths = shortest_paths($keypad, $gap, $from_char, $to_char);

    if ($depth === 0) {
        // At human level, just return path length + 1 for 'A' press
        $result = min(array_map('strlen', $paths)) + 1;
        $memo[$cache_key] = $result;
        return $result;
    }

    $best = PHP_INT_MAX;
    foreach ($paths as $path) {
        // Need to type path + 'A' on the directional keypad above
        $sequence = $path . 'A';
        $cost = 0;
        $current = 'A';
        for ($i = 0; $i < strlen($sequence); $i++) {
            $char = $sequence[$i];
            $cost += min_presses_for_move($current, $char, $depth - 1, false);
            $current = $char;
        }
        $best = min($best, $cost);
    }

    $memo[$cache_key] = $best;
    return $best;
}

/**
 * Compute minimum presses to type code on numeric keypad with given robot depth.
 */
function solve_code(string $code, int $depth): int {
    $total = 0;
    $current = 'A';
    for ($i = 0; $i < strlen($code); $i++) {
        $char = $code[$i];
        $total += min_presses_for_move($current, $char, $depth, true);
        $current = $char;
    }
    return $total;
}

/**
 * Compute complexity: length * numeric part of code.
 */
function complexity(string $code, int $length): int {
    $numeric_part = intval(rtrim($code, 'A'));
    return $length * $numeric_part;
}

/**
 * Part 1: 2 intermediate robots (depth = 2).
 */
function part1(array $codes): int {
    $total = 0;
    foreach ($codes as $code) {
        $length = solve_code($code, 2);
        $total += complexity($code, $length);
    }
    return $total;
}

/**
 * Part 2: 25 intermediate robots (depth = 25).
 */
function part2(array $codes): int {
    $total = 0;
    foreach ($codes as $code) {
        $length = solve_code($code, 25);
        $total += complexity($code, $length);
    }
    return $total;
}

function main() {
    $input_text = file_get_contents(__DIR__ . '/../input.txt');
    $lines = array_filter(array_map('trim', explode("\n", trim($input_text))));

    echo 'Part 1: ' . part1($lines) . "\n";
    echo 'Part 2: ' . part2($lines) . "\n";
}

main();
