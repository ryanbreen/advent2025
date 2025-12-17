<?php
/**
 * Day 17: Chronospatial Computer - 3-bit VM emulator
 */

/**
 * Parse input to extract registers and program
 */
function parseInput(string $text): array {
    $lines = explode("\n", trim($text));
    preg_match('/Register A: (\d+)/', $lines[0], $matchA);
    preg_match('/Register B: (\d+)/', $lines[1], $matchB);
    preg_match('/Register C: (\d+)/', $lines[2], $matchC);
    preg_match('/Program: ([\d,]+)/', $lines[4], $matchProg);

    $a = (int)$matchA[1];
    $b = (int)$matchB[1];
    $c = (int)$matchC[1];
    $program = array_map('intval', explode(',', $matchProg[1]));

    return [$a, $b, $c, $program];
}

/**
 * Execute the 3-bit computer program and return output
 */
function runProgram(int $a, int $b, int $c, array $program): array {
    $ip = 0;
    $output = [];
    $len = count($program);

    // Combo operand helper
    $combo = function(int $operand) use (&$a, &$b, &$c): int {
        return match($operand) {
            0, 1, 2, 3 => $operand,
            4 => $a,
            5 => $b,
            6 => $c,
            default => throw new InvalidArgumentException("Invalid combo operand: $operand"),
        };
    };

    while ($ip < $len) {
        $opcode = $program[$ip];
        $operand = $program[$ip + 1];

        switch ($opcode) {
            case 0: // adv - A = A >> combo
                $a = $a >> $combo($operand);
                break;
            case 1: // bxl - B = B XOR literal
                $b = $b ^ $operand;
                break;
            case 2: // bst - B = combo % 8
                $b = $combo($operand) & 7;
                break;
            case 3: // jnz - jump if A != 0
                if ($a !== 0) {
                    $ip = $operand;
                    continue 2;
                }
                break;
            case 4: // bxc - B = B XOR C
                $b = $b ^ $c;
                break;
            case 5: // out - output combo % 8
                $output[] = $combo($operand) & 7;
                break;
            case 6: // bdv - B = A >> combo
                $b = $a >> $combo($operand);
                break;
            case 7: // cdv - C = A >> combo
                $c = $a >> $combo($operand);
                break;
        }

        $ip += 2;
    }

    return $output;
}

/**
 * Part 1: Run the program and return comma-separated output
 */
function part1(string $text): string {
    [$a, $b, $c, $program] = parseInput($text);
    $output = runProgram($a, $b, $c, $program);
    return implode(',', $output);
}

/**
 * Part 2: Find initial A value that makes program output itself
 * Work backwards from the last digit, building A 3 bits at a time.
 */
function part2(string $text): int {
    [, $b, $c, $program] = parseInput($text);
    $progLen = count($program);

    // Recursive search for A value
    $search = function(int $targetIdx, int $currentA) use (&$search, $b, $c, $program, $progLen): ?int {
        if ($targetIdx < 0) {
            return $currentA;
        }

        // Try all 8 possible 3-bit values for this position
        for ($bits = 0; $bits < 8; $bits++) {
            $candidateA = ($currentA << 3) | $bits;

            // A can't be 0 at start (would halt immediately without output)
            if ($candidateA === 0 && $targetIdx === $progLen - 1) {
                continue;
            }

            $output = runProgram($candidateA, $b, $c, $program);

            // Check if output matches the suffix of the program
            $expected = array_slice($program, $targetIdx);
            if ($output === $expected) {
                $result = $search($targetIdx - 1, $candidateA);
                if ($result !== null) {
                    return $result;
                }
            }
        }

        return null;
    };

    return $search($progLen - 1, 0);
}

// Main
$inputPath = dirname(__DIR__) . '/input.txt';
$text = file_get_contents($inputPath);

echo "Part 1: " . part1($text) . "\n";
echo "Part 2: " . part2($text) . "\n";
