import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Solution {

    // Pre-compile patterns as static constants to avoid recompilation
    private static final Pattern MUL_PATTERN = Pattern.compile("mul\\((\\d{1,3}),(\\d{1,3})\\)");
    private static final Pattern COMBINED_PATTERN = Pattern.compile("mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don't\\(\\)");

    static int part1(String data) {
        // Find all valid mul(X,Y) instructions and sum their products
        Matcher matcher = MUL_PATTERN.matcher(data);

        int total = 0;
        while (matcher.find()) {
            int x = Integer.parseInt(matcher.group(1));
            int y = Integer.parseInt(matcher.group(2));
            total += x * y;
        }

        return total;
    }

    static int part2(String data) {
        // Like part1, but do() enables and don't() disables mul instructions
        // Use a single combined pattern to process everything in order
        Matcher matcher = COMBINED_PATTERN.matcher(data);

        int total = 0;
        boolean enabled = true;

        while (matcher.find()) {
            String match = matcher.group();
            if (match.startsWith("mul(")) {
                if (enabled) {
                    int x = Integer.parseInt(matcher.group(1));
                    int y = Integer.parseInt(matcher.group(2));
                    total += x * y;
                }
            } else if (match.equals("do()")) {
                enabled = true;
            } else { // don't()
                enabled = false;
            }
        }

        return total;
    }

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of("../input.txt");
        String data = Files.readString(inputPath);

        System.out.println("Part 1: " + part1(data));
        System.out.println("Part 2: " + part2(data));
    }
}
