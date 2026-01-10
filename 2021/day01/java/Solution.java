import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class Solution {
    private static int[] depths;

    public static void main(String[] args) throws IOException {
        // Read and parse input
        Path inputPath = Path.of("../input.txt");
        List<String> lines = Files.readAllLines(inputPath);
        depths = lines.stream()
                .filter(line -> !line.isBlank())
                .mapToInt(Integer::parseInt)
                .toArray();

        System.out.println("Part 1: " + part1());
        System.out.println("Part 2: " + part2());
    }

    /**
     * Count the number of times a depth measurement increases from the previous.
     */
    private static int part1() {
        int count = 0;
        for (int i = 1; i < depths.length; i++) {
            if (depths[i] > depths[i - 1]) {
                count++;
            }
        }
        return count;
    }

    /**
     * Count increases in 3-measurement sliding window sums.
     */
    private static int part2() {
        // Create sliding window sums of 3 consecutive measurements
        int[] windowSums = new int[depths.length - 2];
        for (int i = 0; i < depths.length - 2; i++) {
            windowSums[i] = depths[i] + depths[i + 1] + depths[i + 2];
        }

        // Count how many times the sum increases
        int count = 0;
        for (int i = 1; i < windowSums.length; i++) {
            if (windowSums[i] > windowSums[i - 1]) {
                count++;
            }
        }
        return count;
    }
}
