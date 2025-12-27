<?php

declare(strict_types=1);

/**
 * Advent of Code 2023 - Day 5: If You Give A Seed A Fertilizer
 *
 * This solution handles seed-to-location mapping through a series of
 * transformation maps. Part 1 processes individual seeds, while Part 2
 * efficiently handles billions of seeds by operating on ranges.
 */

/**
 * Parse the input into seeds array and list of transformation maps.
 *
 * Each map contains ranges with destination start, source start, and length.
 *
 * @param string $text Raw input text
 * @return array{0: int[], 1: array<array<array{dst_start: int, src_start: int, length: int}>>}
 */
function parseInput(string $text): array
{
    $sections = explode("\n\n", trim($text));

    // Parse seeds from first section: "seeds: 79 14 55 13"
    $seedPart = explode(': ', $sections[0])[1];
    $seeds = array_map(intval(...), preg_split('/\s+/', trim($seedPart)));

    // Parse each mapping section (seed-to-soil, soil-to-fertilizer, etc.)
    $maps = array_map(
        function (string $section): array {
            $lines = explode("\n", trim($section));
            // Skip the header line (e.g., "seed-to-soil map:")
            return array_map(
                function (string $line): array {
                    $parts = array_map(intval(...), preg_split('/\s+/', trim($line)));
                    return [
                        'dst_start' => $parts[0],
                        'src_start' => $parts[1],
                        'length' => $parts[2],
                    ];
                },
                array_slice($lines, 1)
            );
        },
        array_slice($sections, 1)
    );

    return [$seeds, $maps];
}

/**
 * Apply a single transformation map to a value.
 *
 * If the value falls within any source range, it gets mapped to the
 * corresponding destination range. Otherwise, it passes through unchanged.
 *
 * @param int $value The value to transform
 * @param array<array{dst_start: int, src_start: int, length: int}> $ranges The mapping ranges
 * @return int The transformed value
 */
function applyMap(int $value, array $ranges): int
{
    foreach ($ranges as $range) {
        $srcStart = $range['src_start'];
        $srcEnd = $srcStart + $range['length'];

        if ($value >= $srcStart && $value < $srcEnd) {
            return $range['dst_start'] + ($value - $srcStart);
        }
    }

    // No matching range found; value maps to itself
    return $value;
}

/**
 * Convert a seed number to a location number through all transformation maps.
 *
 * @param int $seed The initial seed number
 * @param array<array<array{dst_start: int, src_start: int, length: int}>> $maps All transformation maps
 * @return int The final location number
 */
function seedToLocation(int $seed, array $maps): int
{
    return array_reduce(
        $maps,
        fn(int $value, array $mapRanges): int => applyMap($value, $mapRanges),
        $seed
    );
}

/**
 * Part 1: Find the lowest location number for any initial seed.
 *
 * @param int[] $seeds List of seed numbers
 * @param array<array<array{dst_start: int, src_start: int, length: int}>> $maps All transformation maps
 * @return int The minimum location number
 */
function part1(array $seeds, array $maps): int
{
    $locations = array_map(
        fn(int $seed): int => seedToLocation($seed, $maps),
        $seeds
    );

    return min($locations);
}

/**
 * Apply a transformation map to a list of ranges, producing new ranges.
 *
 * Range-Splitting Algorithm:
 * -------------------------
 * For each input range [start, end), we check it against each mapping range.
 * A mapping range transforms values from [src_start, src_end) to [dst_start, dst_end).
 *
 * When an input range overlaps with a mapping range, we split it into up to 3 parts:
 *
 *   Input range:     |-----------|
 *   Mapping range:        |-----|
 *   Result:          |---|xxxxx|--|
 *                     ^    ^     ^
 *                     |    |     |
 *                     |    |     +-- Part after: [max(start, src_end), end) - stays unmapped
 *                     |    +-------- Overlap: [max(start, src_start), min(end, src_end)) - gets transformed
 *                     +------------- Part before: [start, min(end, src_start)) - stays unmapped
 *
 * The "before" and "after" parts continue to be checked against remaining mapping ranges.
 * The overlapping part is transformed and added directly to results.
 * Any parts that don't match any mapping range pass through unchanged (identity mapping).
 *
 * @param array<array{0: int, 1: int}> $inputRanges Ranges as [start, end) pairs
 * @param array<array{dst_start: int, src_start: int, length: int}> $mapRanges The mapping ranges
 * @return array<array{0: int, 1: int}> Transformed ranges
 */
function applyMapToRanges(array $inputRanges, array $mapRanges): array
{
    $result = [];

    foreach ($inputRanges as [$start, $end]) {
        // Track parts of this range that haven't been mapped yet
        $remaining = [[$start, $end]];

        // Try each mapping range against the unmapped parts
        foreach ($mapRanges as $range) {
            $dstStart = $range['dst_start'];
            $srcStart = $range['src_start'];
            $srcEnd = $srcStart + $range['length'];
            $offset = $dstStart - $srcStart;

            $newRemaining = [];

            foreach ($remaining as [$rStart, $rEnd]) {
                // Part before the mapping range (stays unmapped, try other ranges)
                if ($rStart < $srcStart) {
                    $newRemaining[] = [$rStart, min($rEnd, $srcStart)];
                }

                // Overlapping part (gets transformed, goes directly to result)
                $overlapStart = max($rStart, $srcStart);
                $overlapEnd = min($rEnd, $srcEnd);
                if ($overlapStart < $overlapEnd) {
                    $result[] = [$overlapStart + $offset, $overlapEnd + $offset];
                }

                // Part after the mapping range (stays unmapped, try other ranges)
                if ($rEnd > $srcEnd) {
                    $newRemaining[] = [max($rStart, $srcEnd), $rEnd];
                }
            }

            $remaining = $newRemaining;
        }

        // Anything still remaining maps to itself (identity)
        array_push($result, ...$remaining);
    }

    return $result;
}

/**
 * Part 2: Find the lowest location for seed ranges.
 *
 * Instead of individual seeds, the input is interpreted as pairs of
 * (start, length) defining ranges of seeds. This could represent billions
 * of seeds, so we operate on ranges rather than individual values.
 *
 * @param int[] $seeds Seed range specifications as [start1, len1, start2, len2, ...]
 * @param array<array<array{dst_start: int, src_start: int, length: int}>> $maps All transformation maps
 * @return int The minimum location number
 */
function part2(array $seeds, array $maps): int
{
    // Convert seed pairs to ranges: [start, start + length)
    $ranges = [];
    for ($i = 0, $len = count($seeds); $i < $len; $i += 2) {
        $ranges[] = [$seeds[$i], $seeds[$i] + $seeds[$i + 1]];
    }

    // Transform ranges through each map
    $ranges = array_reduce(
        $maps,
        fn(array $ranges, array $mapRanges): array => applyMapToRanges($ranges, $mapRanges),
        $ranges
    );

    // Find minimum start value across all resulting ranges
    return min(array_column($ranges, 0));
}

/**
 * Main entry point.
 */
function main(): void
{
    $inputPath = __DIR__ . '/../input.txt';
    $text = file_get_contents($inputPath);

    [$seeds, $maps] = parseInput($text);

    echo 'Part 1: ' . part1($seeds, $maps) . PHP_EOL;
    echo 'Part 2: ' . part2($seeds, $maps) . PHP_EOL;
}

main();
