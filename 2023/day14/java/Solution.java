import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Solution {

    public static void main(String[] args) throws IOException {
        List<String> lines = Files.readAllLines(Path.of("../input.txt"));
        char[][] grid = parseGrid(lines);

        System.out.println("Part 1: " + part1(copyGrid(grid)));
        System.out.println("Part 2: " + part2(copyGrid(grid)));
    }

    static char[][] parseGrid(List<String> lines) {
        int rows = lines.size();
        int cols = lines.get(0).length();
        char[][] grid = new char[rows][cols];
        for (int r = 0; r < rows; r++) {
            grid[r] = lines.get(r).toCharArray();
        }
        return grid;
    }

    static char[][] copyGrid(char[][] grid) {
        int rows = grid.length;
        char[][] copy = new char[rows][];
        for (int r = 0; r < rows; r++) {
            copy[r] = grid[r].clone();
        }
        return copy;
    }

    static long part1(char[][] grid) {
        tiltNorth(grid);
        return calculateLoad(grid);
    }

    static long part2(char[][] grid) {
        final long targetCycles = 1_000_000_000L;
        Map<String, Long> seen = new HashMap<>();

        long cycle = 0;
        while (cycle < targetCycles) {
            String state = gridToString(grid);
            if (seen.containsKey(state)) {
                long cycleStart = seen.get(state);
                long cycleLength = cycle - cycleStart;
                long remaining = (targetCycles - cycle) % cycleLength;

                for (long i = 0; i < remaining; i++) {
                    spinCycle(grid);
                }
                return calculateLoad(grid);
            }
            seen.put(state, cycle);
            spinCycle(grid);
            cycle++;
        }

        return calculateLoad(grid);
    }

    static void spinCycle(char[][] grid) {
        tiltNorth(grid);
        tiltWest(grid);
        tiltSouth(grid);
        tiltEast(grid);
    }

    static void tiltNorth(char[][] grid) {
        int rows = grid.length;
        int cols = grid[0].length;

        for (int col = 0; col < cols; col++) {
            int nextFree = 0;
            for (int row = 0; row < rows; row++) {
                if (grid[row][col] == '#') {
                    nextFree = row + 1;
                } else if (grid[row][col] == 'O') {
                    if (nextFree < row) {
                        grid[nextFree][col] = 'O';
                        grid[row][col] = '.';
                    }
                    nextFree++;
                }
            }
        }
    }

    static void tiltSouth(char[][] grid) {
        int rows = grid.length;
        int cols = grid[0].length;

        for (int col = 0; col < cols; col++) {
            int nextFree = rows - 1;
            for (int row = rows - 1; row >= 0; row--) {
                if (grid[row][col] == '#') {
                    nextFree = row - 1;
                } else if (grid[row][col] == 'O') {
                    if (nextFree > row) {
                        grid[nextFree][col] = 'O';
                        grid[row][col] = '.';
                    }
                    nextFree--;
                }
            }
        }
    }

    static void tiltWest(char[][] grid) {
        int rows = grid.length;
        int cols = grid[0].length;

        for (int row = 0; row < rows; row++) {
            int nextFree = 0;
            for (int col = 0; col < cols; col++) {
                if (grid[row][col] == '#') {
                    nextFree = col + 1;
                } else if (grid[row][col] == 'O') {
                    if (nextFree < col) {
                        grid[row][nextFree] = 'O';
                        grid[row][col] = '.';
                    }
                    nextFree++;
                }
            }
        }
    }

    static void tiltEast(char[][] grid) {
        int rows = grid.length;
        int cols = grid[0].length;

        for (int row = 0; row < rows; row++) {
            int nextFree = cols - 1;
            for (int col = cols - 1; col >= 0; col--) {
                if (grid[row][col] == '#') {
                    nextFree = col - 1;
                } else if (grid[row][col] == 'O') {
                    if (nextFree > col) {
                        grid[row][nextFree] = 'O';
                        grid[row][col] = '.';
                    }
                    nextFree--;
                }
            }
        }
    }

    static long calculateLoad(char[][] grid) {
        int rows = grid.length;
        int cols = grid[0].length;
        long load = 0;

        for (int row = 0; row < rows; row++) {
            for (int col = 0; col < cols; col++) {
                if (grid[row][col] == 'O') {
                    load += rows - row;
                }
            }
        }
        return load;
    }

    static String gridToString(char[][] grid) {
        StringBuilder sb = new StringBuilder();
        for (char[] row : grid) {
            sb.append(row);
        }
        return sb.toString();
    }
}
