#!/usr/bin/env php
<?php

function parseInput($filename) {
    $content = file_get_contents($filename);
    $parts = explode("\n\n", trim($content));

    // Parse initial wire values
    $wires = [];
    $lines = explode("\n", $parts[0]);
    foreach ($lines as $line) {
        list($name, $val) = explode(': ', $line);
        $wires[$name] = (int)$val;
    }

    // Parse gates
    $gates = [];
    $lines = explode("\n", $parts[1]);
    foreach ($lines as $line) {
        // Format: "x00 AND y00 -> z00"
        $parts_line = explode(' ', $line);
        $in1 = $parts_line[0];
        $op = $parts_line[1];
        $in2 = $parts_line[2];
        $out = $parts_line[4];
        $gates[] = [$in1, $op, $in2, $out];
    }

    return [$wires, $gates];
}

function simulate($wires, $gates) {
    // Make a copy
    $wires = $wires;
    $remaining = $gates;

    while (!empty($remaining)) {
        $madeProgress = false;
        $newRemaining = [];

        foreach ($remaining as $gate) {
            list($in1, $op, $in2, $out) = $gate;

            if (isset($wires[$in1]) && isset($wires[$in2])) {
                $v1 = $wires[$in1];
                $v2 = $wires[$in2];

                if ($op === 'AND') {
                    $wires[$out] = $v1 & $v2;
                } elseif ($op === 'OR') {
                    $wires[$out] = $v1 | $v2;
                } elseif ($op === 'XOR') {
                    $wires[$out] = $v1 ^ $v2;
                }

                $madeProgress = true;
            } else {
                $newRemaining[] = $gate;
            }
        }

        $remaining = $newRemaining;

        if (!$madeProgress && !empty($remaining)) {
            throw new Exception("Circuit stuck - missing inputs");
        }
    }

    return $wires;
}

function getZValue($wires) {
    // Extract the number from z wires
    $zWires = [];
    foreach ($wires as $key => $value) {
        if (strpos($key, 'z') === 0) {
            $zWires[] = $key;
        }
    }
    rsort($zWires);

    $result = 0;
    foreach ($zWires as $z) {
        $result = ($result << 1) | $wires[$z];
    }

    return $result;
}

function part1($wires, $gates) {
    $finalWires = simulate($wires, $gates);
    return getZValue($finalWires);
}

function part2($gates) {
    /**
     * Find the 8 swapped wires in the adder circuit.
     *
     * A correct ripple-carry adder has this structure:
     * - Bit 0: z00 = x00 XOR y00, carry0 = x00 AND y00
     * - Bit i: z[i] = (x[i] XOR y[i]) XOR carry[i-1]
     *          carry[i] = (x[i] AND y[i]) OR ((x[i] XOR y[i]) AND carry[i-1])
     *
     * We check structural rules to find violations.
     */
    $swapped = [];

    // Build lookup: output -> [in1, op, in2]
    $gateByOutput = [];
    foreach ($gates as $gate) {
        list($in1, $op, $in2, $out) = $gate;
        $gateByOutput[$out] = [$in1, $op, $in2];
    }

    // Build lookup: (inputs_set, op) -> output
    $gateByInputsOp = [];
    foreach ($gates as $gate) {
        list($in1, $op, $in2, $out) = $gate;
        $inputs = [$in1, $in2];
        sort($inputs);
        $key = implode(',', $inputs) . '|' . $op;
        $gateByInputsOp[$key] = $out;
    }

    // Find the highest bit number
    $maxBit = 0;
    foreach ($gates as $gate) {
        list($in1, $op, $in2, $out) = $gate;
        if (strpos($out, 'z') === 0) {
            $bitNum = (int)substr($out, 1);
            if ($bitNum > $maxBit) {
                $maxBit = $bitNum;
            }
        }
    }

    foreach ($gates as $gate) {
        list($in1, $op, $in2, $out) = $gate;

        // Rule: XOR gates that don't take x,y as input should output to z
        if ($op === 'XOR') {
            $isXyXor = (strpos($in1, 'x') === 0 || strpos($in1, 'y') === 0) &&
                       (strpos($in2, 'x') === 0 || strpos($in2, 'y') === 0);

            if (!$isXyXor) {
                // This is a second-level XOR (sum XOR carry), should output to z
                if (strpos($out, 'z') !== 0) {
                    $swapped[$out] = true;
                }
            }
        }

        // Rule: z outputs (except highest bit) should come from XOR
        if (strpos($out, 'z') === 0 && $out !== sprintf('z%02d', $maxBit)) {
            if ($op !== 'XOR') {
                $swapped[$out] = true;
            }
        }

        // Rule: AND gates (except x00 AND y00) should feed into OR
        if ($op === 'AND') {
            $isFirstBit = (in_array('x00', [$in1, $in2]) && in_array('y00', [$in1, $in2]));

            if (!$isFirstBit) {
                // This AND output should be input to an OR gate
                $usedByOr = false;
                foreach ($gates as $gate2) {
                    list($in1b, $op2, $in2b, $out2) = $gate2;
                    if ($op2 === 'OR' && in_array($out, [$in1b, $in2b])) {
                        $usedByOr = true;
                        break;
                    }
                }
                if (!$usedByOr) {
                    $swapped[$out] = true;
                }
            }
        }

        // Rule: XOR of x,y should feed into another XOR (for z output) or AND (for carry)
        if ($op === 'XOR') {
            $isXyXor = (strpos($in1, 'x') === 0 || strpos($in1, 'y') === 0) &&
                       (strpos($in2, 'x') === 0 || strpos($in2, 'y') === 0);

            // Skip z00 which is x00 XOR y00 directly
            $isZ00 = (in_array('x00', [$in1, $in2]) && in_array('y00', [$in1, $in2]));

            if ($isXyXor && !$isZ00) {
                // Should be used by XOR and AND
                $usedByXor = false;
                $usedByAnd = false;

                foreach ($gates as $gate2) {
                    list($in1b, $op2, $in2b, $out2) = $gate2;
                    if (in_array($out, [$in1b, $in2b])) {
                        if ($op2 === 'XOR') {
                            $usedByXor = true;
                        } elseif ($op2 === 'AND') {
                            $usedByAnd = true;
                        }
                    }
                }

                if (!($usedByXor && $usedByAnd)) {
                    $swapped[$out] = true;
                }
            }
        }
    }

    $result = array_keys($swapped);
    sort($result);
    return implode(',', $result);
}

function main() {
    list($wires, $gates) = parseInput('../input.txt');

    echo 'Part 1: ' . part1($wires, $gates) . "\n";
    echo 'Part 2: ' . part2($gates) . "\n";
}

main();
