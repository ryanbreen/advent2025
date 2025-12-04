import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class Solution {

    private static boolean isSafe(List<Integer> levels) {
        if (levels.size() < 2) {
            return true;
        }

        // Calculate differences
        List<Integer> diffs = new ArrayList<>();
        for (int i = 0; i < levels.size() - 1; i++) {
            diffs.add(levels.get(i + 1) - levels.get(i));
        }

        // Check if all increasing (1-3) or all decreasing (-3 to -1)
        boolean allIncreasing = true;
        boolean allDecreasing = true;

        for (int d : diffs) {
            if (d < 1 || d > 3) {
                allIncreasing = false;
            }
            if (d < -3 || d > -1) {
                allDecreasing = false;
            }
        }

        return allIncreasing || allDecreasing;
    }

    private static int part1(List<String> lines) {
        int safeCount = 0;
        for (String line : lines) {
            String[] parts = line.trim().split("\\s+");
            List<Integer> levels = new ArrayList<>();
            for (String part : parts) {
                levels.add(Integer.parseInt(part));
            }
            if (isSafe(levels)) {
                safeCount++;
            }
        }
        return safeCount;
    }

    private static int part2(List<String> lines) {
        int safeCount = 0;
        for (String line : lines) {
            String[] parts = line.trim().split("\\s+");
            List<Integer> levels = new ArrayList<>();
            for (String part : parts) {
                levels.add(Integer.parseInt(part));
            }

            // Check if already safe
            if (isSafe(levels)) {
                safeCount++;
                continue;
            }

            // Try removing each level one at a time
            boolean foundSafe = false;
            for (int i = 0; i < levels.size(); i++) {
                List<Integer> modified = new ArrayList<>();
                for (int j = 0; j < levels.size(); j++) {
                    if (j != i) {
                        modified.add(levels.get(j));
                    }
                }
                if (isSafe(modified)) {
                    safeCount++;
                    foundSafe = true;
                    break;
                }
            }
        }
        return safeCount;
    }

    public static void main(String[] args) {
        try {
            List<String> lines = Files.readAllLines(Paths.get("../input.txt"));
            System.out.println("Part 1: " + part1(lines));
            System.out.println("Part 2: " + part2(lines));
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
            System.exit(1);
        }
    }
}
