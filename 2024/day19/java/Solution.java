import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {
    public static void main(String[] args) throws IOException {
        var content = Files.readString(Path.of("../input.txt")).trim();
        var parts = content.split("\n\n");

        // Parse patterns (comma-separated on first line)
        var patterns = Arrays.stream(parts[0].split(","))
                             .map(String::trim)
                             .toArray(String[]::new);

        // Parse designs (one per line after blank line)
        var designs = parts[1].trim().split("\n");

        // Part 1: Count designs that can be formed
        var part1 = Arrays.stream(designs)
                          .filter(d -> countWays(d, patterns) > 0)
                          .count();
        System.out.println("Part 1: " + part1);

        // Part 2: Sum the number of ways each design can be formed
        var part2 = Arrays.stream(designs)
                          .mapToLong(d -> countWays(d, patterns))
                          .sum();
        System.out.println("Part 2: " + part2);
    }

    /**
     * Count number of ways to form design from patterns.
     * Uses dynamic programming with memoization.
     */
    private static long countWays(String design, String[] patterns) {
        var memo = new HashMap<Integer, Long>();
        return dpCountWays(design, 0, patterns, memo);
    }

    private static long dpCountWays(String design, int pos, String[] patterns, Map<Integer, Long> memo) {
        if (pos == design.length()) {
            return 1;
        }

        var cached = memo.get(pos);
        if (cached != null) {
            return cached;
        }

        var total = 0L;
        for (var pattern : patterns) {
            var plen = pattern.length();
            if (pos + plen <= design.length() &&
                design.regionMatches(pos, pattern, 0, plen)) {
                total += dpCountWays(design, pos + plen, patterns, memo);
            }
        }

        memo.put(pos, total);
        return total;
    }
}
