import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {

    private static Set<Long> parsePaths(String text) {
        Set<Long> rocks = new HashSet<>();

        for (String line : text.trim().split("\n")) {
            String[] points = line.split(" -> ");

            for (int i = 0; i < points.length - 1; i++) {
                String[] p1 = points[i].split(",");
                String[] p2 = points[i + 1].split(",");

                int x1 = Integer.parseInt(p1[0]);
                int y1 = Integer.parseInt(p1[1]);
                int x2 = Integer.parseInt(p2[0]);
                int y2 = Integer.parseInt(p2[1]);

                if (x1 == x2) {
                    // Vertical line
                    for (int y = Math.min(y1, y2); y <= Math.max(y1, y2); y++) {
                        rocks.add(encode(x1, y));
                    }
                } else {
                    // Horizontal line
                    for (int x = Math.min(x1, x2); x <= Math.max(x1, x2); x++) {
                        rocks.add(encode(x, y1));
                    }
                }
            }
        }

        return rocks;
    }

    private static long encode(int x, int y) {
        return ((long) x << 16) | (y & 0xFFFF);
    }

    private static int[] simulateSand(Set<Long> blocked, int maxY, boolean floor) {
        int x = 500;
        int y = 0;

        while (true) {
            // Check if sand has fallen below all rocks (into abyss)
            if (!floor && y > maxY) {
                return null;
            }

            // Try to move down
            if (floor && y + 1 == maxY + 2) {
                // Hit the floor
                return new int[]{x, y};
            } else if (!blocked.contains(encode(x, y + 1))) {
                y++;
            } else if (!blocked.contains(encode(x - 1, y + 1))) {
                // Try to move down-left
                x--;
                y++;
            } else if (!blocked.contains(encode(x + 1, y + 1))) {
                // Try to move down-right
                x++;
                y++;
            } else {
                // Sand comes to rest
                return new int[]{x, y};
            }
        }
    }

    private static int part1(String text) {
        Set<Long> rocks = parsePaths(text);
        int maxY = 0;
        for (long pos : rocks) {
            int y = (int) (pos & 0xFFFF);
            maxY = Math.max(maxY, y);
        }

        Set<Long> blocked = new HashSet<>(rocks);
        int count = 0;

        while (true) {
            int[] pos = simulateSand(blocked, maxY, false);
            if (pos == null) {
                break;
            }
            blocked.add(encode(pos[0], pos[1]));
            count++;
        }

        return count;
    }

    private static int part2(String text) {
        Set<Long> rocks = parsePaths(text);
        int maxY = 0;
        for (long pos : rocks) {
            int y = (int) (pos & 0xFFFF);
            maxY = Math.max(maxY, y);
        }

        Set<Long> blocked = new HashSet<>(rocks);
        int count = 0;

        while (true) {
            int[] pos = simulateSand(blocked, maxY, true);
            blocked.add(encode(pos[0], pos[1]));
            count++;
            if (pos[0] == 500 && pos[1] == 0) {
                break;
            }
        }

        return count;
    }

    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get(System.getProperty("user.dir")).resolve("../input.txt");
        String text = Files.readString(inputPath);

        System.out.println("Part 1: " + part1(text));
        System.out.println("Part 2: " + part2(text));
    }
}
