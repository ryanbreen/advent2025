import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class Solution {

    private record Race(long time, long record) {}

    private static final Pattern NUMBER_PATTERN = Pattern.compile("\\d+");

    private static List<Race> parseRaces(List<String> lines) {
        var times = parseNumbers(lines.get(0));
        var distances = parseNumbers(lines.get(1));

        var races = new ArrayList<Race>();
        for (int i = 0; i < times.size(); i++) {
            races.add(new Race(times.get(i), distances.get(i)));
        }
        return races;
    }

    private static List<Long> parseNumbers(String line) {
        return NUMBER_PATTERN.matcher(line).results()
            .map(m -> Long.parseLong(m.group()))
            .toList();
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

    private static long part1(List<Race> races) {
        return races.stream()
            .mapToLong(r -> countWaysToWin(r.time(), r.record()))
            .reduce(1, (a, b) -> a * b);
    }

    private static long part2(List<Race> races) {
        var timeStr = new StringBuilder();
        var recordStr = new StringBuilder();

        for (Race race : races) {
            timeStr.append(race.time());
            recordStr.append(race.record());
        }

        long time = Long.parseLong(timeStr.toString());
        long record = Long.parseLong(recordStr.toString());

        return countWaysToWin(time, record);
    }

    public static void main(String[] args) throws IOException {
        List<String> lines = Files.readAllLines(Path.of("../input.txt"));

        List<Race> races = parseRaces(lines);

        System.out.println("Part 1: " + part1(races));
        System.out.println("Part 2: " + part2(races));
    }
}
