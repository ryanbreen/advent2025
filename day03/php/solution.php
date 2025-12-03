<?php

$input_text = trim(file_get_contents(__DIR__ . '/../input.txt'));
$lines = explode("\n", $input_text);

function part1($lines) {
    $total = 0;

    foreach ($lines as $line) {
        $n = strlen($line);

        // Precompute max suffix: max_suffix[i] = max digit from position i to end
        $max_suffix = array_fill(0, $n, 0);
        $max_suffix[$n - 1] = intval($line[$n - 1]);

        for ($i = $n - 2; $i >= 0; $i--) {
            $max_suffix[$i] = max(intval($line[$i]), $max_suffix[$i + 1]);
        }

        $max_joltage = 0;

        // For each possible first battery position
        for ($i = 0; $i < $n - 1; $i++) {
            $first_digit = intval($line[$i]);
            // The maximum second digit is the max from position i+1 onwards
            $max_second = $max_suffix[$i + 1];
            $joltage = $first_digit * 10 + $max_second;
            $max_joltage = max($max_joltage, $joltage);
        }

        $total += $max_joltage;
    }

    return $total;
}

function part2($lines) {
    $total = 0;

    foreach ($lines as $line) {
        $n = strlen($line);
        $k = 12; // Select exactly 12 batteries

        // Greedy algorithm to select k digits that form the maximum number
        $result = [];
        $current_pos = 0;

        for ($i = 0; $i < $k; $i++) {
            // How many digits we still need to select after this one
            $remaining_needed = $k - $i - 1;
            // Latest position we can start searching from
            $search_end = $n - $remaining_needed;

            // Find the maximum digit in the valid range
            $max_digit = -1;
            $max_pos = $current_pos;

            for ($j = $current_pos; $j < $search_end; $j++) {
                $digit = intval($line[$j]);
                if ($digit > $max_digit) {
                    $max_digit = $digit;
                    $max_pos = $j;
                }
            }

            $result[] = strval($max_digit);
            $current_pos = $max_pos + 1;
        }

        $joltage = intval(implode('', $result));
        $total += $joltage;
    }

    return $total;
}

echo "Part 1: " . part1($lines) . "\n";
echo "Part 2: " . part2($lines) . "\n";

?>
