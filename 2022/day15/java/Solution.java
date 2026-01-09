import java.io.*;
import java.util.*;
import java.util.regex.*;

public class Solution {

    static class Sensor {
        int sx, sy, bx, by, dist;

        Sensor(int sx, int sy, int bx, int by) {
            this.sx = sx;
            this.sy = sy;
            this.bx = bx;
            this.by = by;
            this.dist = Math.abs(sx - bx) + Math.abs(sy - by);
        }
    }

    static List<Sensor> parseSensors(String text) {
        List<Sensor> sensors = new ArrayList<>();
        Pattern pattern = Pattern.compile(
            "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)"
        );

        for (String line : text.trim().split("\n")) {
            Matcher matcher = pattern.matcher(line);
            if (matcher.find()) {
                int sx = Integer.parseInt(matcher.group(1));
                int sy = Integer.parseInt(matcher.group(2));
                int bx = Integer.parseInt(matcher.group(3));
                int by = Integer.parseInt(matcher.group(4));
                sensors.add(new Sensor(sx, sy, bx, by));
            }
        }

        return sensors;
    }

    static List<int[]> getCoverageAtRow(List<Sensor> sensors, int row) {
        List<int[]> ranges = new ArrayList<>();

        for (Sensor s : sensors) {
            int rowDist = Math.abs(s.sy - row);
            if (rowDist > s.dist) {
                continue;
            }

            int xSpread = s.dist - rowDist;
            ranges.add(new int[]{s.sx - xSpread, s.sx + xSpread});
        }

        return mergeRanges(ranges);
    }

    static List<int[]> mergeRanges(List<int[]> ranges) {
        if (ranges.isEmpty()) {
            return new ArrayList<>();
        }

        ranges.sort((a, b) -> a[0] != b[0] ? a[0] - b[0] : a[1] - b[1]);

        List<int[]> merged = new ArrayList<>();
        merged.add(ranges.get(0).clone());

        for (int i = 1; i < ranges.size(); i++) {
            int[] current = ranges.get(i);
            int[] last = merged.get(merged.size() - 1);

            if (current[0] <= last[1] + 1) {
                last[1] = Math.max(last[1], current[1]);
            } else {
                merged.add(current.clone());
            }
        }

        return merged;
    }

    static long part1(String text) {
        List<Sensor> sensors = parseSensors(text);
        int targetRow = 2000000;

        List<int[]> ranges = getCoverageAtRow(sensors, targetRow);

        long total = 0;
        for (int[] range : ranges) {
            total += range[1] - range[0] + 1;
        }

        Set<Integer> beaconsOnRow = new HashSet<>();
        for (Sensor s : sensors) {
            if (s.by == targetRow) {
                beaconsOnRow.add(s.bx);
            }
        }

        return total - beaconsOnRow.size();
    }

    static long part2(String text) {
        List<Sensor> sensors = parseSensors(text);
        int maxCoord = 4000000;

        for (int row = 0; row <= maxCoord; row++) {
            List<int[]> ranges = getCoverageAtRow(sensors, row);

            List<int[]> clipped = new ArrayList<>();
            for (int[] range : ranges) {
                if (range[1] < 0 || range[0] > maxCoord) {
                    continue;
                }
                clipped.add(new int[]{Math.max(0, range[0]), Math.min(maxCoord, range[1])});
            }

            clipped = mergeRanges(clipped);

            if (clipped.size() == 1 && clipped.get(0)[0] == 0 && clipped.get(0)[1] == maxCoord) {
                continue;
            }

            long x;
            if (clipped.size() > 1) {
                x = clipped.get(0)[1] + 1;
            } else if (clipped.get(0)[0] > 0) {
                x = 0;
            } else {
                x = clipped.get(0)[1] + 1;
            }

            return x * 4000000L + row;
        }

        return -1;
    }

    public static void main(String[] args) throws IOException {
        String inputFile = "../input.txt";
        if (args.length > 0) {
            inputFile = args[0];
        }

        String text = new String(java.nio.file.Files.readAllBytes(
            java.nio.file.Paths.get(inputFile)));

        System.out.println("Part 1: " + part1(text));
        System.out.println("Part 2: " + part2(text));
    }
}
