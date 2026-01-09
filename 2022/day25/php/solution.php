#!/usr/bin/env php
<?php
/**
 * Advent of Code 2022 - Day 25: Full of Hot Air
 * SNAFU number system (balanced base-5) conversion
 */

/**
 * Convert SNAFU number to decimal.
 * SNAFU digits: 2=2, 1=1, 0=0, -=-1, =-2
 */
function snafuToDecimal(string $s): int {
    $digitValues = ['2' => 2, '1' => 1, '0' => 0, '-' => -1, '=' => -2];
    $result = 0;

    for ($i = 0; $i < strlen($s); $i++) {
        $result = $result * 5 + $digitValues[$s[$i]];
    }

    return $result;
}

/**
 * Convert decimal number to SNAFU.
 */
function decimalToSnafu(int $n): string {
    if ($n === 0) {
        return '0';
    }

    $digits = [];

    while ($n !== 0) {
        $remainder = $n % 5;

        if ($remainder <= 2) {
            $digits[] = (string)$remainder;
            $n = intdiv($n, 5);
        } elseif ($remainder === 3) {
            $digits[] = '=';
            $n = intdiv($n, 5) + 1;
        } else { // remainder === 4
            $digits[] = '-';
            $n = intdiv($n, 5) + 1;
        }
    }

    return implode('', array_reverse($digits));
}

/**
 * Part 1: Sum all SNAFU numbers and return result as SNAFU.
 */
function part1(string $text): string {
    $lines = array_filter(explode("\n", trim($text)));
    $total = 0;

    foreach ($lines as $line) {
        $total += snafuToDecimal($line);
    }

    return decimalToSnafu($total);
}

// Main execution
$scriptDir = dirname(__FILE__);
$inputFile = $scriptDir . '/../input.txt';
$text = file_get_contents($inputFile);

echo 'Part 1: ' . part1($text) . "\n";
echo "Part 2: No Part 2 on Day 25!\n";
