import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class Solution {
    // Direction indices: 0=Up, 1=Right, 2=Down, 3=Left
    private static final int[] DR = {-1, 0, 1, 0};
    private static final int[] DC = {0, 1, 0, -1};

    private static int rows, cols;
    private static char[][] grid;
    private static int startRow, startCol, startDir;

    // Flat array for state tracking - better cache performance
    // Index = (row * cols + col) * 4 + dir
    private static int[] stateVersion;
    private static int currentVersion = 0;

    // Separate array for tracking original visited positions
    private static boolean[][] originalPath;
    private static int part1Answer = -1;

    private static void parseGrid(List<String> lines) {
        rows = lines.size();
        cols = lines.get(0).length();
        grid = new char[rows][cols];
        stateVersion = new int[rows * cols * 4];
        originalPath = new boolean[rows][cols];
        currentVersion = 0;
        part1Answer = -1;

        for (int r = 0; r < rows; r++) {
            String line = lines.get(r);
            for (int c = 0; c < line.length(); c++) {
                char ch = line.charAt(c);
                grid[r][c] = ch;

                if (ch == '^') {
                    startRow = r; startCol = c; startDir = 0;
                    grid[r][c] = '.';
                } else if (ch == '>') {
                    startRow = r; startCol = c; startDir = 1;
                    grid[r][c] = '.';
                } else if (ch == 'v') {
                    startRow = r; startCol = c; startDir = 2;
                    grid[r][c] = '.';
                } else if (ch == '<') {
                    startRow = r; startCol = c; startDir = 3;
                    grid[r][c] = '.';
                }
            }
        }
    }

    // Get original visited path (for part1 and part2 optimization)
    private static int getOriginalPath() {
        if (part1Answer >= 0) return part1Answer;

        int row = startRow, col = startCol, dir = startDir;
        int count = 0;

        while (true) {
            if (!originalPath[row][col]) {
                originalPath[row][col] = true;
                count++;
            }

            int nextRow = row + DR[dir];
            int nextCol = col + DC[dir];

            if (nextRow < 0 || nextRow >= rows || nextCol < 0 || nextCol >= cols) {
                part1Answer = count;
                return count;
            }

            if (grid[nextRow][nextCol] == '#') {
                dir = (dir + 1) & 3;
            } else {
                row = nextRow;
                col = nextCol;
            }
        }
    }

    // Simulate with obstruction, returns true if loop detected
    private static boolean detectsLoop(int obsRow, int obsCol) {
        // Increment version to "clear" stateVersion array in O(1)
        currentVersion++;

        int row = startRow, col = startCol, dir = startDir;

        while (true) {
            // Flat index calculation
            int stateIdx = (row * cols + col) * 4 + dir;

            // Loop detection check using version comparison
            if (stateVersion[stateIdx] == currentVersion) {
                return true;  // Loop detected
            }
            stateVersion[stateIdx] = currentVersion;

            int nextRow = row + DR[dir];
            int nextCol = col + DC[dir];

            // Check bounds
            if (nextRow < 0 || nextRow >= rows || nextCol < 0 || nextCol >= cols) {
                return false;  // Guard exits map
            }

            // Check obstacle (including temporary obstruction)
            if (grid[nextRow][nextCol] == '#' || (nextRow == obsRow && nextCol == obsCol)) {
                dir = (dir + 1) & 3;  // Turn right
            } else {
                row = nextRow;
                col = nextCol;
            }
        }
    }

    public static int part1(List<String> lines) {
        parseGrid(lines);
        return getOriginalPath();
    }

    public static int part2(List<String> lines) {
        // Only reparse if needed (allows sharing with part1)
        if (grid == null || grid.length != lines.size()) {
            parseGrid(lines);
        }

        // First get visited positions from unobstructed path
        getOriginalPath();

        int loopCount = 0;

        // Try placing obstruction at each visited position (except start)
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                // Check if position was visited in original path
                if (!originalPath[r][c]) continue;
                if (r == startRow && c == startCol) continue;

                if (detectsLoop(r, c)) {
                    loopCount++;
                }
            }
        }

        return loopCount;
    }

    public static void main(String[] args) throws IOException {
        List<String> lines = Files.readAllLines(Paths.get("../input.txt"));

        // Parse once, use for both parts
        parseGrid(lines);

        int result1 = getOriginalPath();
        System.out.println("Part 1: " + result1);

        int result2 = part2(lines);
        System.out.println("Part 2: " + result2);
    }
}
