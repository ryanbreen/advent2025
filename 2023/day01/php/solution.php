<?php

$input = file_get_contents(__DIR__ . '/../input.txt');
$lines = array_filter(explode("\n", trim($input)));

/**
 * Word-to-digit mapping for Part 2
 */
const WORDS = [
    "one" => "1", "two" => "2", "three" => "3", "four" => "4", "five" => "5",
    "six" => "6", "seven" => "7", "eight" => "8", "nine" => "9"
];

/**
 * Part 1: Extract first and last digit from each line
 *
 * @param array $lines Input lines
 * @return int Sum of calibration values
 */
function part1(array $lines): int {
    return array_reduce($lines, function(int $total, string $line): int {
        preg_match_all('/\d/', $line, $matches);
        $digits = $matches[0];
        if (empty($digits)) {
            return $total;
        }
        return $total + intval($digits[0] . end($digits));
    }, 0);
}

/**
 * Part 2: Extract digits and spelled-out numbers
 *
 * @param array $lines Input lines
 * @return int Sum of calibration values
 */
function part2(array $lines): int {
    return array_reduce($lines, function(int $total, string $line): int {
        $digits = [];
        $len = strlen($line);

        for ($i = 0; $i < $len; $i++) {
            if (ctype_digit($line[$i])) {
                $digits[] = $line[$i];
            } else {
                foreach (WORDS as $word => $digit) {
                    if (str_starts_with(substr($line, $i), $word)) {
                        $digits[] = $digit;
                        break;
                    }
                }
            }
        }

        if (empty($digits)) {
            return $total;
        }
        return $total + intval($digits[0] . end($digits));
    }, 0);
}

echo "Part 1: " . part1($lines) . "\n";
echo "Part 2: " . part2($lines) . "\n";
