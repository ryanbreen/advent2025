import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.stream.*;

/**
 * Day 12: Christmas Tree Farm - Polyomino Packing
 *
 * The solution checks if presents (polyominoes) can fit into rectangular regions.
 * For this problem, the constraint is simply: total cells needed <= available cells.
 *
 * Modern Java implementation using records, Stream API, and var type inference.
 */
public class Solution {

    record ParseResult(Map<Integer, Integer> shapes, List<Region> regions) {}

    record Region(int width, int height, List<Integer> counts) {
        int availableCells() {
            return width * height;
        }

        int totalCellsNeeded(Map<Integer, Integer> shapeSizes) {
            return IntStream.range(0, counts.size())
                .map(i -> counts.get(i) * shapeSizes.get(i))
                .sum();
        }

        boolean canFit(Map<Integer, Integer> shapeSizes) {
            return totalCellsNeeded(shapeSizes) <= availableCells();
        }
    }

    /**
     * Parse input into shapes and regions using modern Java Stream API.
     */
    static ParseResult parseInput(String text) {
        var shapes = new HashMap<Integer, Integer>();
        var regions = new ArrayList<Region>();

        var sections = text.trim().split("\n\n");

        for (var section : sections) {
            var lines = section.trim().split("\n");
            if (lines[0].contains(":") && !lines[0].contains("x")) {
                // Shape definition - count '#' cells
                var idx = Integer.parseInt(lines[0].substring(0, lines[0].indexOf(':')));
                var cellCount = Arrays.stream(lines)
                    .skip(1)
                    .mapToInt(line -> (int) line.chars().filter(c -> c == '#').count())
                    .sum();
                shapes.put(idx, cellCount);
            } else {
                // Region definitions
                Arrays.stream(lines)
                    .filter(line -> line.contains("x"))
                    .forEach(line -> {
                        var parts = line.split(":");
                        var dims = parts[0].trim().split("x");
                        var w = Integer.parseInt(dims[0]);
                        var h = Integer.parseInt(dims[1]);

                        var counts = Arrays.stream(parts[1].trim().split("\\s+"))
                            .map(Integer::parseInt)
                            .toList();

                        regions.add(new Region(w, h, counts));
                    });
            }
        }

        return new ParseResult(shapes, regions);
    }

    /**
     * Count regions that can fit all their presents using Stream API.
     */
    static long part1(Map<Integer, Integer> shapes, List<Region> regions) {
        return regions.stream()
            .filter(region -> region.canFit(shapes))
            .count();
    }

    /**
     * Part 2 is just a button click to finish - no computation needed.
     */
    static int part2() {
        return 0;
    }

    public static void main(String[] args) throws IOException {
        // Modern Java file reading (Java 11+)
        var text = Files.readString(Path.of("../input.txt"));
        var result = parseInput(text);

        System.out.println("Part 1: " + part1(result.shapes(), result.regions()));
        System.out.println("Part 2: " + part2());
    }
}
