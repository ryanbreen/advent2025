import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class Solution {

    /**
     * Convert a SNAFU digit to its decimal value.
     */
    private static int digitValue(char c) {
        return switch (c) {
            case '2' -> 2;
            case '1' -> 1;
            case '0' -> 0;
            case '-' -> -1;
            case '=' -> -2;
            default -> throw new IllegalArgumentException("Invalid SNAFU digit: " + c);
        };
    }

    /**
     * Convert a SNAFU number string to decimal.
     */
    private static long snafuToDecimal(String snafu) {
        long result = 0;
        for (int i = 0; i < snafu.length(); i++) {
            result = result * 5 + digitValue(snafu.charAt(i));
        }
        return result;
    }

    /**
     * Convert a decimal number to SNAFU string.
     */
    private static String decimalToSnafu(long n) {
        if (n == 0) {
            return "0";
        }

        StringBuilder digits = new StringBuilder();
        while (n != 0) {
            int remainder = (int) (n % 5);
            if (remainder <= 2) {
                digits.append(remainder);
                n /= 5;
            } else if (remainder == 3) {
                digits.append('=');
                n = n / 5 + 1;
            } else { // remainder == 4
                digits.append('-');
                n = n / 5 + 1;
            }
        }

        return digits.reverse().toString();
    }

    /**
     * Part 1: Sum all SNAFU numbers and return result as SNAFU.
     */
    private static String part1(List<String> lines) {
        long total = lines.stream()
                .filter(line -> !line.isBlank())
                .mapToLong(Solution::snafuToDecimal)
                .sum();
        return decimalToSnafu(total);
    }

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of(args.length > 0 ? args[0] : "../input.txt");
        List<String> lines = Files.readAllLines(inputPath);

        System.out.println("Part 1: " + part1(lines));
        System.out.println("Part 2: No Part 2 on Day 25!");
    }
}
