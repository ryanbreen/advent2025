import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Solution {
    public static void main(String[] args) throws IOException {
        List<String> numbers = parseInput();
        System.out.println("Part 1: " + part1(numbers));
        System.out.println("Part 2: " + part2(numbers));
    }

    private static List<String> parseInput() throws IOException {
        List<String> numbers = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader("../input.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (!line.isEmpty()) {
                    numbers.add(line);
                }
            }
        }
        return numbers;
    }

    private static int part1(List<String> numbers) {
        int numBits = numbers.get(0).length();
        int gamma = 0;

        for (int pos = 0; pos < numBits; pos++) {
            int ones = 0;
            for (String n : numbers) {
                if (n.charAt(pos) == '1') {
                    ones++;
                }
            }
            int zeros = numbers.size() - ones;

            if (ones >= zeros) {
                gamma |= (1 << (numBits - 1 - pos));
            }
        }

        // epsilon is bitwise NOT of gamma (within numBits)
        int epsilon = gamma ^ ((1 << numBits) - 1);

        return gamma * epsilon;
    }

    private static int findRating(List<String> numbers, boolean useMostCommon) {
        int numBits = numbers.get(0).length();
        List<String> candidates = new ArrayList<>(numbers);

        for (int pos = 0; pos < numBits; pos++) {
            if (candidates.size() == 1) {
                break;
            }

            int ones = 0;
            for (String n : candidates) {
                if (n.charAt(pos) == '1') {
                    ones++;
                }
            }
            int zeros = candidates.size() - ones;

            char target;
            if (useMostCommon) {
                target = (ones >= zeros) ? '1' : '0';
            } else {
                target = (zeros <= ones) ? '0' : '1';
            }

            final int currentPos = pos;
            final char targetBit = target;
            List<String> filtered = new ArrayList<>();
            for (String n : candidates) {
                if (n.charAt(currentPos) == targetBit) {
                    filtered.add(n);
                }
            }
            candidates = filtered;
        }

        return Integer.parseInt(candidates.get(0), 2);
    }

    private static int part2(List<String> numbers) {
        int oxygen = findRating(numbers, true);
        int co2 = findRating(numbers, false);
        return oxygen * co2;
    }
}
