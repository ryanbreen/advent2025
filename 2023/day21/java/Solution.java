import java.io.*;
import java.util.*;

public class Solution {
    private static final int[][] DIRS = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

    public static void main(String[] args) throws IOException {
        String inputPath = "../input.txt";
        List<String> grid = new ArrayList<>();

        try (BufferedReader reader = new BufferedReader(new FileReader(inputPath))) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (!line.isEmpty()) {
                    grid.add(line);
                }
            }
        }

        int[] start = findStart(grid);

        System.out.println("Part 1: " + part1(grid, start));
        System.out.println("Part 2: " + part2(grid, start));
    }

    private static int[] findStart(List<String> grid) {
        for (int r = 0; r < grid.size(); r++) {
            String row = grid.get(r);
            for (int c = 0; c < row.length(); c++) {
                if (row.charAt(c) == 'S') {
                    return new int[]{r, c};
                }
            }
        }
        throw new RuntimeException("Start position not found");
    }

    private static long part1(List<String> grid, int[] start) {
        return countReachable(grid, start, 64);
    }

    private static long part2(List<String> grid, int[] start) {
        return countReachableInfinite(grid, start, 26501365);
    }

    private static long countReachable(List<String> grid, int[] start, int steps) {
        int rows = grid.size();
        int cols = grid.get(0).length();

        // BFS to find minimum steps to each cell
        Map<Long, Integer> visited = new HashMap<>();
        Deque<int[]> queue = new ArrayDeque<>();

        queue.add(new int[]{start[0], start[1], 0});
        visited.put(encodePos(start[0], start[1]), 0);

        while (!queue.isEmpty()) {
            int[] curr = queue.poll();
            int r = curr[0], c = curr[1], dist = curr[2];

            if (dist >= steps) {
                continue;
            }

            for (int[] dir : DIRS) {
                int nr = r + dir[0];
                int nc = c + dir[1];

                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                    if (grid.get(nr).charAt(nc) != '#') {
                        long key = encodePos(nr, nc);
                        if (!visited.containsKey(key)) {
                            visited.put(key, dist + 1);
                            queue.add(new int[]{nr, nc, dist + 1});
                        }
                    }
                }
            }
        }

        // Count cells reachable in exactly 'steps' steps
        int targetParity = steps % 2;
        long count = 0;
        for (int d : visited.values()) {
            if (d <= steps && d % 2 == targetParity) {
                count++;
            }
        }

        return count;
    }

    private static long countReachableInfinite(List<String> grid, int[] start, int steps) {
        int size = grid.size();
        int half = size / 2;

        // For small step counts, use direct BFS
        if (steps <= size * 2) {
            return countReachableInfiniteBFS(grid, start, steps);
        }

        // The number of full grid widths we travel
        long n = (steps - half) / size;

        // Calculate reachable counts for n=0, 1, 2
        long y0 = countReachableInfiniteBFS(grid, start, half);
        long y1 = countReachableInfiniteBFS(grid, start, half + size);
        long y2 = countReachableInfiniteBFS(grid, start, half + 2 * size);

        // Solve for a, b, c using finite differences
        // f(x) = ax^2 + bx + c
        long a = (y2 - 2 * y1 + y0) / 2;
        long b = y1 - y0 - a;
        long c = y0;

        return a * n * n + b * n + c;
    }

    private static long countReachableInfiniteBFS(List<String> grid, int[] start, int steps) {
        int rows = grid.size();
        int cols = grid.get(0).length();

        Map<Long, Integer> visited = new HashMap<>();
        Deque<int[]> queue = new ArrayDeque<>();

        queue.add(new int[]{start[0], start[1], 0});
        visited.put(encodePos(start[0], start[1]), 0);

        while (!queue.isEmpty()) {
            int[] curr = queue.poll();
            int r = curr[0], c = curr[1], dist = curr[2];

            if (dist >= steps) {
                continue;
            }

            for (int[] dir : DIRS) {
                int nr = r + dir[0];
                int nc = c + dir[1];

                // Map to grid coordinates (infinite tiling)
                int gr = ((nr % rows) + rows) % rows;
                int gc = ((nc % cols) + cols) % cols;

                if (grid.get(gr).charAt(gc) != '#') {
                    long key = encodePos(nr, nc);
                    if (!visited.containsKey(key)) {
                        visited.put(key, dist + 1);
                        queue.add(new int[]{nr, nc, dist + 1});
                    }
                }
            }
        }

        int targetParity = steps % 2;
        long count = 0;
        for (int d : visited.values()) {
            if (d <= steps && d % 2 == targetParity) {
                count++;
            }
        }

        return count;
    }

    private static long encodePos(int r, int c) {
        // Encode position as a single long for use as a hash key
        // Need to handle negative values for infinite grid
        return ((long)(r + 500000) << 20) | ((long)(c + 500000) & 0xFFFFF);
    }
}
