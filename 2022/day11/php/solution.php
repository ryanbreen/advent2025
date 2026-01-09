<?php
/**
 * Advent of Code 2022 - Day 11: Monkey in the Middle
 */

function parseMonkeys(string $text): array {
    $monkeys = [];
    $blocks = explode("\n\n", trim($text));

    foreach ($blocks as $block) {
        $lines = explode("\n", trim($block));

        // Parse items
        preg_match_all('/\d+/', $lines[1], $matches);
        $items = array_map('intval', $matches[0]);

        // Parse operation
        preg_match('/new = old ([+*]) (\w+)/', $lines[2], $opMatch);
        $operator = $opMatch[1];
        $operand = $opMatch[2];

        // Parse divisor
        preg_match('/\d+/', $lines[3], $divMatch);
        $divisor = (int)$divMatch[0];

        // Parse targets
        preg_match('/\d+/', $lines[4], $trueMatch);
        $ifTrue = (int)$trueMatch[0];

        preg_match('/\d+/', $lines[5], $falseMatch);
        $ifFalse = (int)$falseMatch[0];

        $monkeys[] = [
            'items' => $items,
            'operator' => $operator,
            'operand' => $operand,
            'divisor' => $divisor,
            'if_true' => $ifTrue,
            'if_false' => $ifFalse,
            'inspections' => 0
        ];
    }

    return $monkeys;
}

function applyOperation(int $old, string $operator, string $operand): int {
    $val = ($operand === 'old') ? $old : (int)$operand;
    if ($operator === '+') {
        return $old + $val;
    } else {
        return $old * $val;
    }
}

function applyOperationGmp(\GMP $old, string $operator, string $operand): \GMP {
    $val = ($operand === 'old') ? $old : gmp_init($operand);
    if ($operator === '+') {
        return gmp_add($old, $val);
    } else {
        return gmp_mul($old, $val);
    }
}

function simulate(array &$monkeys, int $rounds, int $reliefDivisor = 3, bool $useModulo = false): void {
    // For part 2, use product of all divisors to keep numbers manageable
    $modValue = null;
    if ($useModulo) {
        $modValue = 1;
        foreach ($monkeys as $monkey) {
            $modValue *= $monkey['divisor'];
        }
    }

    for ($round = 0; $round < $rounds; $round++) {
        for ($i = 0; $i < count($monkeys); $i++) {
            while (!empty($monkeys[$i]['items'])) {
                $item = array_shift($monkeys[$i]['items']);
                $monkeys[$i]['inspections']++;

                // Apply operation
                $newVal = applyOperation($item, $monkeys[$i]['operator'], $monkeys[$i]['operand']);

                // Apply relief
                if ($reliefDivisor > 1) {
                    $newVal = intdiv($newVal, $reliefDivisor);
                }

                // Apply modulo to prevent overflow
                if ($modValue !== null) {
                    $newVal = $newVal % $modValue;
                }

                // Test and throw
                if ($newVal % $monkeys[$i]['divisor'] === 0) {
                    $monkeys[$monkeys[$i]['if_true']]['items'][] = $newVal;
                } else {
                    $monkeys[$monkeys[$i]['if_false']]['items'][] = $newVal;
                }
            }
        }
    }
}

function simulateGmp(array &$monkeys, int $rounds): void {
    // For part 2, use product of all divisors to keep numbers manageable
    $modValue = gmp_init(1);
    foreach ($monkeys as $monkey) {
        $modValue = gmp_mul($modValue, $monkey['divisor']);
    }

    // Convert items to GMP
    for ($i = 0; $i < count($monkeys); $i++) {
        $monkeys[$i]['items'] = array_map(function($item) {
            return gmp_init($item);
        }, $monkeys[$i]['items']);
    }

    for ($round = 0; $round < $rounds; $round++) {
        for ($i = 0; $i < count($monkeys); $i++) {
            while (!empty($monkeys[$i]['items'])) {
                $item = array_shift($monkeys[$i]['items']);
                $monkeys[$i]['inspections']++;

                // Apply operation
                $newVal = applyOperationGmp($item, $monkeys[$i]['operator'], $monkeys[$i]['operand']);

                // Apply modulo to prevent overflow
                $newVal = gmp_mod($newVal, $modValue);

                // Test and throw
                if (gmp_cmp(gmp_mod($newVal, $monkeys[$i]['divisor']), 0) === 0) {
                    $monkeys[$monkeys[$i]['if_true']]['items'][] = $newVal;
                } else {
                    $monkeys[$monkeys[$i]['if_false']]['items'][] = $newVal;
                }
            }
        }
    }
}

function monkeyBusiness(array $monkeys): int {
    $inspections = array_map(function($m) {
        return $m['inspections'];
    }, $monkeys);
    rsort($inspections);
    return $inspections[0] * $inspections[1];
}

function part1(string $text): int {
    $monkeys = parseMonkeys($text);
    simulate($monkeys, 20, 3);
    return monkeyBusiness($monkeys);
}

function part2(string $text): int {
    $monkeys = parseMonkeys($text);
    simulateGmp($monkeys, 10000);
    return monkeyBusiness($monkeys);
}

// Main execution
$scriptDir = dirname(__FILE__);
$inputFile = $scriptDir . '/../input.txt';
$text = file_get_contents($inputFile);

echo 'Part 1: ' . part1($text) . "\n";
echo 'Part 2: ' . part2($text) . "\n";
