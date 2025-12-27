import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Solution {

    private static List<long[]> parseRaces(String[] lines) {
        List<Long> times = parseNumbers(lines[0]);
        List<Long> distances = parseNumbers(lines[1]);

        List<long[]> races = new ArrayList<>();
        for (int i = 0; i < times.size(); i++) {
            races.add(new long[]{times.get(i), distances.get(i)});
        }
        return races;
    }

    private static List<Long> parseNumbers(String line) {
        List<Long> numbers = new ArrayList<>();
        Pattern pattern = Pattern.compile("\\d+");
        Matcher matcher = pattern.matcher(line);
        while (matcher.find()) {
            numbers.add(Long.parseLong(matcher.group()));
        }
        return numbers;
    }

    private static long countWaysToWin(long time, long record) {
        /*
         * Count the number of ways to beat the record.
         *
         * If we hold the button for t ms, we travel t * (time - t) mm.
         * We need: t * (time - t) > record
         * Solving: -t^2 + time*t - record > 0
         * Roots: t = (time +/- sqrt(time^2 - 4*record)) / 2
         */
        double discriminant = (double) time * time - 4.0 * record;
        if (discriminant <= 0) {
            return 0;
        }

        double sqrtD = Math.sqrt(discriminant);
        double tLow = (time - sqrtD) / 2.0;
        double tHigh = (time + sqrtD) / 2.0;

        // We need integer values strictly between the roots
        long first = (long) Math.floor(tLow) + 1;
        long last = (long) Math.ceil(tHigh) - 1;

        if (last < first) {
            return 0;
        }
        return last - first + 1;
    }

    private static long part1(List<long[]> races) {
        long result = 1;
        for (long[] race : races) {
            long ways = countWaysToWin(race[0], race[1]);
            result *= ways;
        }
        return result;
    }

    private static long part2(List<long[]> races) {
        StringBuilder timeStr = new StringBuilder();
        StringBuilder recordStr = new StringBuilder();

        for (long[] race : races) {
            timeStr.append(race[0]);
            recordStr.append(race[1]);
        }

        long time = Long.parseLong(timeStr.toString());
        long record = Long.parseLong(recordStr.toString());

        return countWaysToWin(time, record);
    }

    public static void main(String[] args) throws IOException {
        String content = Files.readString(Path.of("../input.txt")).trim();
        String[] lines = content.split("\n");

        List<long[]> races = parseRaces(lines);

        System.out.println("Part 1: " + part1(races));
        System.out.println("Part 2: " + part2(races));
    }
}
