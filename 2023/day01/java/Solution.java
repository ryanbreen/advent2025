import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;

public class Solution {
    // Immutable map using Map.of() for Java 9+
    private static final Map<String, Integer> WORD_DIGITS = Map.of(
        "one", 1, "two", 2, "three", 3, "four", 4, "five", 5,
        "six", 6, "seven", 7, "eight", 8, "nine", 9
    );

    // Ordered list of word-digit pairs for deterministic iteration
    private static final List<Map.Entry<String, Integer>> WORD_DIGIT_ENTRIES =
        List.of(
            Map.entry("one", 1), Map.entry("two", 2), Map.entry("three", 3),
            Map.entry("four", 4), Map.entry("five", 5), Map.entry("six", 6),
            Map.entry("seven", 7), Map.entry("eight", 8), Map.entry("nine", 9)
        );

    public static void main(String[] args) throws IOException {
        var lines = Files.readString(Path.of("../input.txt")).strip().split("\n");

        System.out.println("Part 1: " + part1(lines));
        System.out.println("Part 2: " + part2(lines));
    }

    private static int part1(String[] lines) {
        return Arrays.stream(lines)
            .mapToInt(Solution::extractCalibrationValue)
            .sum();
    }

    private static int part2(String[] lines) {
        return Arrays.stream(lines)
            .mapToInt(Solution::extractCalibrationValueWithWords)
            .sum();
    }

    private static int extractCalibrationValue(String line) {
        var firstDigit = line.chars()
            .filter(Character::isDigit)
            .findFirst();

        var lastDigit = IntStream.iterate(line.length() - 1, i -> i >= 0, i -> i - 1)
            .map(line::charAt)
            .filter(Character::isDigit)
            .findFirst();

        if (firstDigit.isPresent() && lastDigit.isPresent()) {
            return (firstDigit.getAsInt() - '0') * 10 + (lastDigit.getAsInt() - '0');
        }
        return 0;
    }

    private static int extractCalibrationValueWithWords(String line) {
        Integer first = null;
        Integer last = null;

        // Use IntStream.range for index-based iteration
        for (int i : IntStream.range(0, line.length()).toArray()) {
            var digit = findDigitAt(line, i);
            if (digit != null) {
                if (first == null) {
                    first = digit;
                }
                last = digit;
            }
        }

        return (first != null) ? first * 10 + last : 0;
    }

    private static Integer findDigitAt(String line, int pos) {
        var c = line.charAt(pos);
        if (Character.isDigit(c)) {
            return c - '0';
        }

        // Use ordered list for deterministic iteration with startsWith(prefix, offset)
        return WORD_DIGIT_ENTRIES.stream()
            .filter(entry -> line.startsWith(entry.getKey(), pos))
            .map(Map.Entry::getValue)
            .findFirst()
            .orElse(null);
    }
}
