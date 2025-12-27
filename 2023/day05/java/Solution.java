import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;

/**
 * Advent of Code 2023 - Day 5: If You Give A Seed A Fertilizer
 *
 * Transforms seed numbers through a series of category mappings to find
 * the minimum location number.
 */
public class Solution {

    /**
     * Represents a mapping range that transforms source values to destination values.
     * Values in [srcStart, srcStart + length) map to [dstStart, dstStart + length).
     *
     * @param dstStart the start of the destination range
     * @param srcStart the start of the source range
     * @param length   the length of both ranges
     */
    record MapRange(long dstStart, long srcStart, long length) {

        /** Returns the exclusive end of the source range. */
        long srcEnd() {
            return srcStart + length;
        }

        /** Returns the offset to add when mapping source to destination. */
        long offset() {
            return dstStart - srcStart;
        }

        /** Checks if the given value falls within this mapping's source range. */
        boolean contains(long value) {
            return value >= srcStart && value < srcEnd();
        }
    }

    /**
     * Represents a half-open range [start, end) of values.
     *
     * @param start inclusive start of the range
     * @param end   exclusive end of the range
     */
    record Range(long start, long end) {

        /** Returns true if this range is non-empty. */
        boolean isValid() {
            return start < end;
        }
    }

    public static void main(String[] args) throws IOException {
        String text = Files.readString(Path.of("../input.txt"));
        String[] sections = text.strip().split("\n\n");

        long[] seeds = parseSeeds(sections[0]);
        List<List<MapRange>> maps = parseMaps(sections);

        System.out.println("Part 1: " + part1(seeds, maps));
        System.out.println("Part 2: " + part2(seeds, maps));
    }

    /**
     * Parses the seed line into an array of seed numbers.
     */
    private static long[] parseSeeds(String seedLine) {
        return Arrays.stream(seedLine.split(": ")[1].split("\\s+"))
                .mapToLong(Long::parseLong)
                .toArray();
    }

    /**
     * Parses all mapping sections into lists of MapRange objects.
     */
    private static List<List<MapRange>> parseMaps(String[] sections) {
        return IntStream.range(1, sections.length)
                .mapToObj(i -> parseMapSection(sections[i]))
                .toList();
    }

    /**
     * Parses a single map section into a list of MapRange objects.
     */
    private static List<MapRange> parseMapSection(String section) {
        return Arrays.stream(section.strip().split("\n"))
                .skip(1) // Skip the header line
                .map(Solution::parseMapRange)
                .toList();
    }

    /**
     * Parses a single line into a MapRange.
     */
    private static MapRange parseMapRange(String line) {
        String[] parts = line.split("\\s+");
        return new MapRange(
                Long.parseLong(parts[0]),
                Long.parseLong(parts[1]),
                Long.parseLong(parts[2])
        );
    }

    /**
     * Applies a single mapping to a value, returning the mapped result.
     * If no mapping applies, returns the original value (identity mapping).
     */
    private static long applyMap(long value, List<MapRange> ranges) {
        return ranges.stream()
                .filter(range -> range.contains(value))
                .findFirst()
                .map(range -> range.dstStart() + (value - range.srcStart()))
                .orElse(value);
    }

    /**
     * Transforms a seed through all category mappings to get its final location.
     */
    private static long seedToLocation(long seed, List<List<MapRange>> maps) {
        long value = seed;
        for (List<MapRange> mapRanges : maps) {
            value = applyMap(value, mapRanges);
        }
        return value;
    }

    /**
     * Part 1: Find the minimum location for individual seed values.
     */
    private static long part1(long[] seeds, List<List<MapRange>> maps) {
        return Arrays.stream(seeds)
                .map(seed -> seedToLocation(seed, maps))
                .min()
                .orElseThrow();
    }

    /**
     * Applies a mapping to a list of ranges, producing transformed ranges.
     * Handles splitting ranges at mapping boundaries and applying offsets.
     */
    private static List<Range> applyMapToRanges(List<Range> inputRanges, List<MapRange> mapRanges) {
        List<Range> result = new ArrayList<>();

        for (Range input : inputRanges) {
            List<Range> remaining = new ArrayList<>(List.of(input));

            for (MapRange mr : mapRanges) {
                List<Range> newRemaining = new ArrayList<>();

                for (Range r : remaining) {
                    // Part before the map range (unmapped, stays for next iteration)
                    Range before = new Range(r.start(), Math.min(r.end(), mr.srcStart()));
                    if (before.isValid()) {
                        newRemaining.add(before);
                    }

                    // Part within the map range (mapped to result)
                    long overlapStart = Math.max(r.start(), mr.srcStart());
                    long overlapEnd = Math.min(r.end(), mr.srcEnd());
                    if (overlapStart < overlapEnd) {
                        result.add(new Range(
                                overlapStart + mr.offset(),
                                overlapEnd + mr.offset()
                        ));
                    }

                    // Part after the map range (unmapped, stays for next iteration)
                    Range after = new Range(Math.max(r.start(), mr.srcEnd()), r.end());
                    if (after.isValid()) {
                        newRemaining.add(after);
                    }
                }

                remaining = newRemaining;
            }

            // Any remaining parts are unmapped (identity mapping)
            result.addAll(remaining);
        }

        return result;
    }

    /**
     * Part 2: Seeds are interpreted as pairs (start, length) defining ranges.
     * Find the minimum location across all seed ranges.
     */
    private static long part2(long[] seeds, List<List<MapRange>> maps) {
        // Convert seed pairs to ranges
        List<Range> ranges = IntStream.iterate(0, i -> i < seeds.length, i -> i + 2)
                .mapToObj(i -> new Range(seeds[i], seeds[i] + seeds[i + 1]))
                .collect(ArrayList::new, ArrayList::add, ArrayList::addAll);

        // Apply each map to the ranges
        for (List<MapRange> mapRanges : maps) {
            ranges = applyMapToRanges(ranges, mapRanges);
        }

        // Find minimum start of any range
        return ranges.stream()
                .mapToLong(Range::start)
                .min()
                .orElseThrow();
    }
}
