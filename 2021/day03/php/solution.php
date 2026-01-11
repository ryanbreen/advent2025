<?php

function parseInput(): array {
    $inputPath = __DIR__ . '/../input.txt';
    $lines = file($inputPath, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    return array_filter($lines, fn($line) => trim($line) !== '');
}

function part1(array $numbers): int {
    $numBits = strlen($numbers[0]);
    $gamma = 0;

    for ($pos = 0; $pos < $numBits; $pos++) {
        $ones = 0;
        foreach ($numbers as $n) {
            if ($n[$pos] === '1') {
                $ones++;
            }
        }
        $zeros = count($numbers) - $ones;

        if ($ones >= $zeros) {
            $gamma |= (1 << ($numBits - 1 - $pos));
        }
    }

    // epsilon is bitwise NOT of gamma (within numBits)
    $epsilon = $gamma ^ ((1 << $numBits) - 1);

    return $gamma * $epsilon;
}

function findRating(array $numbers, bool $useMostCommon): int {
    $numBits = strlen($numbers[0]);
    $candidates = $numbers;

    for ($pos = 0; $pos < $numBits; $pos++) {
        if (count($candidates) === 1) {
            break;
        }

        $ones = 0;
        foreach ($candidates as $n) {
            if ($n[$pos] === '1') {
                $ones++;
            }
        }
        $zeros = count($candidates) - $ones;

        if ($useMostCommon) {
            $target = ($ones >= $zeros) ? '1' : '0';
        } else {
            $target = ($zeros <= $ones) ? '0' : '1';
        }

        $candidates = array_filter($candidates, fn($n) => $n[$pos] === $target);
        $candidates = array_values($candidates); // Re-index array
    }

    return bindec($candidates[0]);
}

function part2(array $numbers): int {
    $oxygen = findRating($numbers, true);
    $co2 = findRating($numbers, false);
    return $oxygen * $co2;
}

$numbers = parseInput();
echo "Part 1: " . part1($numbers) . "\n";
echo "Part 2: " . part2($numbers) . "\n";
