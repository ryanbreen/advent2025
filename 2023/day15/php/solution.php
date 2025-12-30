<?php
declare(strict_types=1);

/**
 * Advent of Code 2023 - Day 15: Lens Library
 */

/**
 * Run the HASH algorithm on a string.
 */
function hashAlgorithm(string $s): int
{
    $current = 0;
    for ($i = 0; $i < strlen($s); $i++) {
        $current = (($current + ord($s[$i])) * 17) % 256;
    }
    return $current;
}

/**
 * Part 1: Sum of HASH values for all steps.
 */
function part1(array $steps): int
{
    return array_sum(array_map('hashAlgorithm', $steps));
}

/**
 * Part 2: Run HASHMAP procedure and calculate focusing power.
 */
function part2(array $steps): int
{
    // Initialize 256 boxes as empty arrays
    $boxes = array_fill(0, 256, []);

    foreach ($steps as $step) {
        if (str_contains($step, '=')) {
            [$label, $focal] = explode('=', $step);
            $focal = (int)$focal;
            $boxNum = hashAlgorithm($label);

            // Check if lens already exists in box
            $found = false;
            foreach ($boxes[$boxNum] as $i => $lens) {
                if ($lens[0] === $label) {
                    $boxes[$boxNum][$i] = [$label, $focal];
                    $found = true;
                    break;
                }
            }
            if (!$found) {
                $boxes[$boxNum][] = [$label, $focal];
            }
        } else {
            // '-' operation: remove lens
            $label = substr($step, 0, -1);
            $boxNum = hashAlgorithm($label);
            $boxes[$boxNum] = array_values(array_filter(
                $boxes[$boxNum],
                fn($lens) => $lens[0] !== $label
            ));
        }
    }

    // Calculate focusing power
    $total = 0;
    foreach ($boxes as $boxNum => $box) {
        foreach ($box as $slot => $lens) {
            $total += ($boxNum + 1) * ($slot + 1) * $lens[1];
        }
    }
    return $total;
}

function main(): void
{
    $inputFile = __DIR__ . '/../input.txt';
    $text = trim(str_replace("\n", "", file_get_contents($inputFile)));
    $steps = explode(',', $text);

    echo "Part 1: " . part1($steps) . "\n";
    echo "Part 2: " . part2($steps) . "\n";
}

main();
