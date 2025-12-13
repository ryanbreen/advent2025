import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class Solution {
    // Memoization cache: bit-packed key (value << 8) | blinks
    // blinks max is 75, fits in 7 bits
    private static Map<Long, Long> memo = new HashMap<>();

    /**
     * Count how many stones result from a single stone after N blinks.
     * Uses memoization to avoid recomputing the same states.
     */
    private static long countStones(long value, int blinks) {
        if (blinks == 0) {
            return 1;
        }

        // Bit-pack key: (value << 8) | blinks
        long key = (value << 8) | blinks;
        Long cached = memo.get(key);
        if (cached != null) {
            return cached;
        }

        long result;
        // Rule 1: 0 becomes 1
        if (value == 0) {
            result = countStones(1, blinks - 1);
        }
        // Rule 2: Even number of digits -> split
        else {
            String s = String.valueOf(value);
            if (s.length() % 2 == 0) {
                int mid = s.length() / 2;
                long left = Long.parseLong(s.substring(0, mid));
                long right = Long.parseLong(s.substring(mid));
                result = countStones(left, blinks - 1) + countStones(right, blinks - 1);
            }
            // Rule 3: Multiply by 2024
            else {
                result = countStones(value * 2024, blinks - 1);
            }
        }

        memo.put(key, result);
        return result;
    }

    private static long part1(long[] stones) {
        return Arrays.stream(stones)
                .map(stone -> countStones(stone, 25))
                .sum();
    }

    private static long part2(long[] stones) {
        return Arrays.stream(stones)
                .map(stone -> countStones(stone, 75))
                .sum();
    }

    public static void main(String[] args) throws IOException {
        // Read input from ../input.txt
        String input = new String(Files.readAllBytes(Paths.get("../input.txt"))).trim();

        // Parse space-separated numbers
        String[] parts = input.split("\\s+");
        long[] stones = new long[parts.length];
        for (int i = 0; i < parts.length; i++) {
            stones[i] = Long.parseLong(parts[i]);
        }

        System.out.println("Part 1: " + part1(stones));
        System.out.println("Part 2: " + part2(stones));
    }
}
