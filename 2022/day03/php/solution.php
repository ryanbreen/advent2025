<?php

function parse_input($filename) {
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    return $lines;
}

function priority($char) {
    if (ctype_lower($char)) {
        return ord($char) - ord('a') + 1;
    } else {
        return ord($char) - ord('A') + 27;
    }
}

function part1($rucksacks) {
    $total = 0;
    foreach ($rucksacks as $rucksack) {
        $mid = strlen($rucksack) / 2;
        $first = str_split(substr($rucksack, 0, $mid));
        $second = str_split(substr($rucksack, $mid));
        $common = array_intersect($first, $second);
        $total += priority(reset($common));
    }
    return $total;
}

function part2($rucksacks) {
    $total = 0;
    for ($i = 0; $i < count($rucksacks); $i += 3) {
        $first = str_split($rucksacks[$i]);
        $second = str_split($rucksacks[$i + 1]);
        $third = str_split($rucksacks[$i + 2]);
        $common = array_intersect($first, $second, $third);
        $total += priority(reset($common));
    }
    return $total;
}

function main() {
    $script_dir = dirname(__FILE__);
    $input_file = $script_dir . '/../input.txt';

    $rucksacks = parse_input($input_file);

    echo 'Part 1: ' . part1($rucksacks) . "\n";
    echo 'Part 2: ' . part2($rucksacks) . "\n";
}

main();
