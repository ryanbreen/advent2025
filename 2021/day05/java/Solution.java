import java.io.*;
import java.util.*;

public class Solution {

    public static void main(String[] args) throws IOException {
        List<int[]> lines = parseInput();
        System.out.println("Part 1: " + part1(lines));
        System.out.println("Part 2: " + part2(lines));
    }

    private static List<int[]> parseInput() throws IOException {
        String classPath = Solution.class.getProtectionDomain()
            .getCodeSource().getLocation().getPath();
        File classDir = new File(classPath).isDirectory()
            ? new File(classPath)
            : new File(classPath).getParentFile();
        String inputPath = new File(classDir, "../input.txt").getCanonicalPath();

        List<int[]> lines = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(inputPath))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;

                String[] parts = line.split(" -> ");
                String[] start = parts[0].split(",");
                String[] end = parts[1].split(",");

                int x1 = Integer.parseInt(start[0]);
                int y1 = Integer.parseInt(start[1]);
                int x2 = Integer.parseInt(end[0]);
                int y2 = Integer.parseInt(end[1]);

                lines.add(new int[]{x1, y1, x2, y2});
            }
        }
        return lines;
    }

    private static int sign(int x) {
        if (x > 0) return 1;
        if (x < 0) return -1;
        return 0;
    }

    private static int countOverlaps(List<int[]> lines, boolean includeDiagonals) {
        Map<Long, Integer> grid = new HashMap<>();

        for (int[] coords : lines) {
            int x1 = coords[0], y1 = coords[1], x2 = coords[2], y2 = coords[3];
            int dx = sign(x2 - x1);
            int dy = sign(y2 - y1);

            // Skip diagonals in part 1
            if (!includeDiagonals && dx != 0 && dy != 0) {
                continue;
            }

            int x = x1, y = y1;
            while (true) {
                long key = ((long) x << 16) | (y & 0xFFFFL);
                grid.merge(key, 1, Integer::sum);

                if (x == x2 && y == y2) break;
                x += dx;
                y += dy;
            }
        }

        int count = 0;
        for (int v : grid.values()) {
            if (v >= 2) count++;
        }
        return count;
    }

    private static int part1(List<int[]> lines) {
        return countOverlaps(lines, false);
    }

    private static int part2(List<int[]> lines) {
        return countOverlaps(lines, true);
    }
}
