import java.io.*;
import java.nio.file.*;
import java.util.*;

/**
 * Day 17: Clumsy Crucible - Dijkstra's shortest path with movement constraints.
 */
public class Solution {

    private static final int[] DR = {0, 1, 0, -1};  // right, down, left, up
    private static final int[] DC = {1, 0, -1, 0};

    public static void main(String[] args) throws IOException {
        int[][] grid = parseInput(Paths.get("..", "input.txt"));

        System.out.println("Part 1: " + dijkstra(grid, 0, 3));
        System.out.println("Part 2: " + dijkstra(grid, 4, 10));
    }

    private static int[][] parseInput(Path path) throws IOException {
        List<String> lines = Files.readAllLines(path);
        int[][] grid = new int[lines.size()][];

        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            grid[i] = new int[line.length()];
            for (int j = 0; j < line.length(); j++) {
                grid[i][j] = line.charAt(j) - '0';
            }
        }
        return grid;
    }

    /**
     * Find minimum heat loss path using Dijkstra's algorithm.
     *
     * State: (row, col, direction, consecutive_steps)
     * Directions: 0=right, 1=down, 2=left, 3=up
     */
    private static int dijkstra(int[][] grid, int minStraight, int maxStraight) {
        int rows = grid.length;
        int cols = grid[0].length;

        // Priority queue: (heat_loss, row, col, direction, consecutive)
        // Using int array: [heat, row, col, direction, consecutive]
        PriorityQueue<int[]> pq = new PriorityQueue<>(Comparator.comparingInt(a -> a[0]));

        // Start with direction -1 (no direction yet)
        pq.offer(new int[]{0, 0, 0, -1, 0});

        // Visited set: encode state as a single long
        Set<Long> visited = new HashSet<>();

        while (!pq.isEmpty()) {
            int[] current = pq.poll();
            int heat = current[0];
            int r = current[1];
            int c = current[2];
            int d = current[3];
            int consec = current[4];

            // Check if we reached the goal
            if (r == rows - 1 && c == cols - 1) {
                if (minStraight == 0 || consec >= minStraight) {
                    return heat;
                }
            }

            // Encode state for visited check
            // d ranges from -1 to 3, consec from 0 to 10
            // Shift d to positive range (0-4) for encoding
            long state = encodeState(r, c, d, consec);
            if (visited.contains(state)) {
                continue;
            }
            visited.add(state);

            // Try all four directions
            for (int nd = 0; nd < 4; nd++) {
                // Can't reverse direction
                if (d != -1 && nd == (d + 2) % 4) {
                    continue;
                }

                int nr = r + DR[nd];
                int nc = c + DC[nd];

                // Bounds check
                if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
                    continue;
                }

                int newConsec;
                if (nd == d) {
                    // Continuing in same direction
                    newConsec = consec + 1;
                    if (newConsec > maxStraight) {
                        continue;
                    }
                } else {
                    // Turning - must have gone minStraight in previous direction first
                    if (d != -1 && consec < minStraight) {
                        continue;
                    }
                    newConsec = 1;
                }

                int newHeat = heat + grid[nr][nc];
                long newState = encodeState(nr, nc, nd, newConsec);

                if (!visited.contains(newState)) {
                    pq.offer(new int[]{newHeat, nr, nc, nd, newConsec});
                }
            }
        }

        return -1;  // No path found
    }

    private static long encodeState(int r, int c, int d, int consec) {
        // d ranges from -1 to 3, shift to 0-4
        // consec ranges from 0 to 10
        // Using 16 bits each for r, c; 4 bits for d+1; 4 bits for consec
        return ((long)r << 24) | ((long)c << 8) | ((long)(d + 1) << 4) | consec;
    }
}
