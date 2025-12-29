import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Solution {

    // Records for type safety
    record State(int pos, int groupIdx, int currentRun) {}
    record SpringRecord(String pattern, int[] groups) {}

    private final Map<State, Long> memo = new HashMap<>();
    private String pattern;
    private int[] groups;

    private long dp(int pos, int groupIdx, int currentRun) {
        var state = new State(pos, groupIdx, currentRun);
        var cached = memo.get(state);
        if (cached != null) {
            return cached;
        }

        // Base case: reached end of pattern
        if (pos == pattern.length()) {
            // Valid if we've matched all groups and no partial run
            if (groupIdx == groups.length && currentRun == 0) {
                return 1;
            }
            // Or if we're on the last group and the run matches
            if (groupIdx == groups.length - 1 && groups[groupIdx] == currentRun) {
                return 1;
            }
            return 0;
        }

        long result = 0;
        var c = pattern.charAt(pos);

        // Option 1: Place operational spring (.)
        if (c == '.' || c == '?') {
            if (currentRun == 0) {
                // No active run, just move forward
                result += dp(pos + 1, groupIdx, 0);
            } else if (groupIdx < groups.length && groups[groupIdx] == currentRun) {
                // End current run if it matches expected group size
                result += dp(pos + 1, groupIdx + 1, 0);
            }
            // Otherwise invalid (run doesn't match group)
        }

        // Option 2: Place damaged spring (#)
        if (c == '#' || c == '?') {
            if (groupIdx < groups.length && currentRun < groups[groupIdx]) {
                // Can extend current run
                result += dp(pos + 1, groupIdx, currentRun + 1);
            }
            // Otherwise invalid (exceeds group size or no more groups)
        }

        memo.put(state, result);
        return result;
    }

    public long countArrangements(String pattern, int[] groups) {
        this.memo.clear();
        this.pattern = pattern;
        this.groups = groups;
        return dp(0, 0, 0);
    }

    private static int[] parseGroups(String groupStr) {
        return Arrays.stream(groupStr.split(","))
                .mapToInt(Integer::parseInt)
                .toArray();
    }

    private static String unfoldPattern(String pattern, int times) {
        return String.join("?", Collections.nCopies(times, pattern));
    }

    private static int[] unfoldGroups(int[] groups, int times) {
        var unfolded = new int[groups.length * times];
        for (var i = 0; i < times; i++) {
            System.arraycopy(groups, 0, unfolded, i * groups.length, groups.length);
        }
        return unfolded;
    }

    private static SpringRecord parseLine(String line) {
        var parts = line.trim().split("\\s+");
        return new SpringRecord(parts[0], parseGroups(parts[1]));
    }

    public static void main(String[] args) throws IOException {
        List<SpringRecord> records = Files.readAllLines(Path.of("../input.txt"))
                .stream()
                .filter(line -> !line.isBlank())
                .map(Solution::parseLine)
                .toList();

        var solver = new Solution();

        // Part 1
        var part1 = records.stream()
                .mapToLong(r -> solver.countArrangements(r.pattern(), r.groups()))
                .sum();
        System.out.println("Part 1: " + part1);

        // Part 2
        var part2 = records.stream()
                .mapToLong(r -> solver.countArrangements(
                        unfoldPattern(r.pattern(), 5),
                        unfoldGroups(r.groups(), 5)))
                .sum();
        System.out.println("Part 2: " + part2);
    }
}
