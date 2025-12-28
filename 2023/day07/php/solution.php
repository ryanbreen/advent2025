<?php

$input = trim(file_get_contents(__DIR__ . '/../input.txt'));
$lines = explode("\n", $input);

// Card strength order (higher index = stronger)
const CARD_STRENGTH = '23456789TJQKA';
const CARD_STRENGTH_JOKER = 'J23456789TQKA';  // J is weakest in Part 2

/**
 * Return hand type as integer (higher = stronger).
 */
function getHandType(string $hand): int {
    $counts = array_count_values(str_split($hand));
    $values = array_values($counts);
    rsort($values);

    if ($values === [5]) {
        return 6;  // Five of a kind
    } elseif ($values === [4, 1]) {
        return 5;  // Four of a kind
    } elseif ($values === [3, 2]) {
        return 4;  // Full house
    } elseif ($values === [3, 1, 1]) {
        return 3;  // Three of a kind
    } elseif ($values === [2, 2, 1]) {
        return 2;  // Two pair
    } elseif ($values === [2, 1, 1, 1]) {
        return 1;  // One pair
    } else {
        return 0;  // High card
    }
}

/**
 * Return sort key for a hand (type, then card strengths).
 */
function handKey(string $hand): array {
    $handType = getHandType($hand);
    $cardValues = [];
    for ($i = 0; $i < strlen($hand); $i++) {
        $cardValues[] = strpos(CARD_STRENGTH, $hand[$i]);
    }
    return array_merge([$handType], $cardValues);
}

/**
 * Return hand type with J as wildcards (higher = stronger).
 */
function getHandTypeWithJokers(string $hand): int {
    $jokerCount = substr_count($hand, 'J');

    if ($jokerCount === 0) {
        return getHandType($hand);
    }
    if ($jokerCount === 5) {
        return 6;  // Five of a kind
    }

    // Count non-joker cards
    $nonJokers = str_replace('J', '', $hand);
    $counts = array_count_values(str_split($nonJokers));
    $values = array_values($counts);
    rsort($values);

    // Add jokers to the highest count
    $values[0] += $jokerCount;

    if ($values === [5]) {
        return 6;  // Five of a kind
    } elseif ($values === [4, 1]) {
        return 5;  // Four of a kind
    } elseif ($values === [3, 2]) {
        return 4;  // Full house
    } elseif ($values === [3, 1, 1]) {
        return 3;  // Three of a kind
    } elseif ($values === [2, 2, 1]) {
        return 2;  // Two pair
    } elseif ($values === [2, 1, 1, 1]) {
        return 1;  // One pair
    } else {
        return 0;  // High card
    }
}

/**
 * Return sort key for a hand with joker rules.
 */
function handKeyWithJokers(string $hand): array {
    $handType = getHandTypeWithJokers($hand);
    $cardValues = [];
    for ($i = 0; $i < strlen($hand); $i++) {
        $cardValues[] = strpos(CARD_STRENGTH_JOKER, $hand[$i]);
    }
    return array_merge([$handType], $cardValues);
}

/**
 * Compare two arrays element by element.
 */
function compareKeys(array $a, array $b): int {
    for ($i = 0; $i < count($a); $i++) {
        if ($a[$i] !== $b[$i]) {
            return $a[$i] <=> $b[$i];
        }
    }
    return 0;
}

function part1(array $lines): int {
    $hands = [];
    foreach ($lines as $line) {
        $parts = preg_split('/\s+/', $line);
        $hand = $parts[0];
        $bid = (int)$parts[1];
        $hands[] = ['hand' => $hand, 'bid' => $bid, 'key' => handKey($hand)];
    }

    // Sort by hand strength
    usort($hands, function($a, $b) {
        return compareKeys($a['key'], $b['key']);
    });

    // Calculate total winnings
    $total = 0;
    foreach ($hands as $rank => $entry) {
        $total += ($rank + 1) * $entry['bid'];
    }

    return $total;
}

function part2(array $lines): int {
    $hands = [];
    foreach ($lines as $line) {
        $parts = preg_split('/\s+/', $line);
        $hand = $parts[0];
        $bid = (int)$parts[1];
        $hands[] = ['hand' => $hand, 'bid' => $bid, 'key' => handKeyWithJokers($hand)];
    }

    // Sort by hand strength with joker rules
    usort($hands, function($a, $b) {
        return compareKeys($a['key'], $b['key']);
    });

    // Calculate total winnings
    $total = 0;
    foreach ($hands as $rank => $entry) {
        $total += ($rank + 1) * $entry['bid'];
    }

    return $total;
}

echo "Part 1: " . part1($lines) . "\n";
echo "Part 2: " . part2($lines) . "\n";
