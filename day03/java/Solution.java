import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class Solution {

    public static void main(String[] args) throws IOException {
        // Read input file
        String inputPath = "../input.txt";
        List<String> lines = Files.readAllLines(Paths.get(inputPath));

        System.out.println("Part 1: " + part1(lines));
        System.out.println("Part 2: " + part2(lines));
    }

    /**
     * Part 1: Select exactly 2 batteries per bank to maximize the 2-digit joltage.
     * Algorithm: For each position i as the first digit, find the maximum digit
     * from position i+1 onwards as the second digit. Use a precomputed max suffix array.
     */
    public static int part1(List<String> lines) {
        int total = 0;

        for (String line : lines) {
            int n = line.length();

            // Precompute max suffix: max_suffix[i] = max digit from position i to end
            int[] maxSuffix = new int[n];
            maxSuffix[n - 1] = Character.getNumericValue(line.charAt(n - 1));

            for (int i = n - 2; i >= 0; i--) {
                int digit = Character.getNumericValue(line.charAt(i));
                maxSuffix[i] = Math.max(digit, maxSuffix[i + 1]);
            }

            int maxJoltage = 0;

            // For each possible first battery position
            for (int i = 0; i < n - 1; i++) {
                int firstDigit = Character.getNumericValue(line.charAt(i));
                // The maximum second digit is the max from position i+1 onwards
                int maxSecond = maxSuffix[i + 1];
                int joltage = firstDigit * 10 + maxSecond;
                maxJoltage = Math.max(maxJoltage, joltage);
            }

            total += maxJoltage;
        }

        return total;
    }

    /**
     * Part 2: Select exactly 12 batteries per bank to maximize the 12-digit joltage.
     * Algorithm: Greedy approach - at each step, select the maximum digit from
     * the valid range (ensuring enough digits remain for subsequent selections).
     */
    public static long part2(List<String> lines) {
        long total = 0;

        for (String line : lines) {
            int n = line.length();
            int k = 12; // Select exactly 12 batteries

            // Greedy algorithm to select k digits that form the maximum number
            StringBuilder result = new StringBuilder();
            int currentPos = 0;

            for (int i = 0; i < k; i++) {
                // How many digits we still need to select after this one
                int remainingNeeded = k - i - 1;
                // Latest position we can start searching from
                int searchEnd = n - remainingNeeded;

                // Find the maximum digit in the valid range
                int maxDigit = -1;
                int maxPos = currentPos;

                for (int j = currentPos; j < searchEnd; j++) {
                    int digit = Character.getNumericValue(line.charAt(j));
                    if (digit > maxDigit) {
                        maxDigit = digit;
                        maxPos = j;
                    }
                }

                result.append(maxDigit);
                currentPos = maxPos + 1;
            }

            long joltage = Long.parseLong(result.toString());
            total += joltage;
        }

        return total;
    }
}
