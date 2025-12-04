#!/usr/bin/env php
<?php

// Read input file
$input_path = dirname(__FILE__) . '/../input.txt';
$input_text = trim(file_get_contents($input_path));

// Parse input into two lists
$left_list = [];
$right_list = [];

foreach (explode("\n", $input_text) as $line) {
    $parts = preg_split('/\s+/', trim($line));
    $left_list[] = (int)$parts[0];
    $right_list[] = (int)$parts[1];
}

function part1($left_list, $right_list) {
    // Sort both lists
    sort($left_list);
    sort($right_list);

    // Calculate total distance
    $total_distance = 0;
    for ($i = 0; $i < count($left_list); $i++) {
        $total_distance += abs($left_list[$i] - $right_list[$i]);
    }

    return $total_distance;
}

function part2($left_list, $right_list) {
    // Count occurrences in right list
    $right_counts = [];
    foreach ($right_list as $num) {
        if (!isset($right_counts[$num])) {
            $right_counts[$num] = 0;
        }
        $right_counts[$num]++;
    }

    // Calculate similarity score
    $similarity_score = 0;
    foreach ($left_list as $num) {
        $count = isset($right_counts[$num]) ? $right_counts[$num] : 0;
        $similarity_score += $num * $count;
    }

    return $similarity_score;
}

echo "Part 1: " . part1($left_list, $right_list) . "\n";
echo "Part 2: " . part2($left_list, $right_list) . "\n";
