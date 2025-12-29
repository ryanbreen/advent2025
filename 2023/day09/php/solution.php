<?php

declare(strict_types=1);

/**
 * Parse input text into array of integer sequences.
 *
 * @param string $text Raw input text
 * @return array<array<int>> Array of integer sequences
 */
function parseInput(string $text): array
{
    $lines = explode("\n", trim($text));
    return array_map(
        fn(string $line): array => array_map('intval', preg_split('/\s+/', trim($line))),
        $lines
    );
}

/**
 * Compute pairwise differences of a sequence.
 *
 * @param array<int> $seq Input sequence
 * @return array<int> Differences between consecutive elements
 */
function getDifferences(array $seq): array
{
    $diffs = [];
    $count = count($seq);
    for ($i = 0; $i < $count - 1; $i++) {
        $diffs[] = $seq[$i + 1] - $seq[$i];
    }
    return $diffs;
}

/**
 * Check if all elements in sequence are zero.
 *
 * @param array<int> $seq Input sequence
 * @return bool True if all elements are zero
 */
function allZeros(array $seq): bool
{
    foreach ($seq as $val) {
        if ($val !== 0) {
            return false;
        }
    }
    return true;
}

/**
 * Extrapolate the next value in a sequence using difference method.
 *
 * Builds a pyramid of differences until reaching all zeros,
 * then works back up adding the last elements.
 *
 * @param array<int> $seq Input sequence
 * @return int The extrapolated next value
 */
function extrapolateNext(array $seq): int
{
    $sequences = [$seq];
    $current = $seq;

    while (!allZeros($current)) {
        $current = getDifferences($current);
        $sequences[] = $current;
    }

    for ($i = count($sequences) - 2; $i >= 0; $i--) {
        $lastCurrent = end($sequences[$i]);
        $lastBelow = end($sequences[$i + 1]);
        $sequences[$i][] = $lastCurrent + $lastBelow;
    }

    return end($sequences[0]);
}

/**
 * Sum results of applying a function to each element.
 *
 * @param callable $fn Function to apply
 * @param array $items Items to process
 * @return int Sum of results
 */
function sumBy(callable $fn, array $items): int
{
    return array_sum(array_map($fn, $items));
}

/**
 * Part 1: Sum of extrapolated next values for all sequences.
 *
 * @param array<array<int>> $histories Input sequences
 * @return int Sum of extrapolated values
 */
function part1(array $histories): int
{
    return sumBy(fn(array $h): int => extrapolateNext($h), $histories);
}

/**
 * Part 2: Sum of extrapolated previous values for all sequences.
 *
 * Key insight: Extrapolating backwards is equivalent to
 * extrapolating forwards on the reversed sequence.
 *
 * @param array<array<int>> $histories Input sequences
 * @return int Sum of extrapolated values
 */
function part2(array $histories): int
{
    return sumBy(
        fn(array $h): int => extrapolateNext(array_reverse($h)),
        $histories
    );
}

// Main execution
$inputText = file_get_contents(__DIR__ . '/../input.txt');
$histories = parseInput($inputText);

echo "Part 1: " . part1($histories) . "\n";
echo "Part 2: " . part2($histories) . "\n";
