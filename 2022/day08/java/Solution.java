import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class Solution {

    private static int[][] parseGrid(List<String> lines) {
        int rows = lines.size();
        int cols = lines.get(0).length();
        int[][] grid = new int[rows][cols];

        for (int r = 0; r < rows; r++) {
            String line = lines.get(r);
            for (int c = 0; c < cols; c++) {
                grid[r][c] = line.charAt(c) - '0';
            }
        }
        return grid;
    }

    private static boolean isVisible(int[][] grid, int row, int col) {
        int rows = grid.length;
        int cols = grid[0].length;
        int height = grid[row][col];

        // Check from left
        boolean visibleLeft = true;
        for (int c = 0; c < col; c++) {
            if (grid[row][c] >= height) {
                visibleLeft = false;
                break;
            }
        }
        if (visibleLeft) return true;

        // Check from right
        boolean visibleRight = true;
        for (int c = col + 1; c < cols; c++) {
            if (grid[row][c] >= height) {
                visibleRight = false;
                break;
            }
        }
        if (visibleRight) return true;

        // Check from top
        boolean visibleTop = true;
        for (int r = 0; r < row; r++) {
            if (grid[r][col] >= height) {
                visibleTop = false;
                break;
            }
        }
        if (visibleTop) return true;

        // Check from bottom
        boolean visibleBottom = true;
        for (int r = row + 1; r < rows; r++) {
            if (grid[r][col] >= height) {
                visibleBottom = false;
                break;
            }
        }
        return visibleBottom;
    }

    private static int scenicScore(int[][] grid, int row, int col) {
        int rows = grid.length;
        int cols = grid[0].length;
        int height = grid[row][col];

        // Count trees visible in each direction
        // Left
        int left = 0;
        for (int c = col - 1; c >= 0; c--) {
            left++;
            if (grid[row][c] >= height) break;
        }

        // Right
        int right = 0;
        for (int c = col + 1; c < cols; c++) {
            right++;
            if (grid[row][c] >= height) break;
        }

        // Up
        int up = 0;
        for (int r = row - 1; r >= 0; r--) {
            up++;
            if (grid[r][col] >= height) break;
        }

        // Down
        int down = 0;
        for (int r = row + 1; r < rows; r++) {
            down++;
            if (grid[r][col] >= height) break;
        }

        return left * right * up * down;
    }

    private static int part1(int[][] grid) {
        int rows = grid.length;
        int cols = grid[0].length;
        int count = 0;

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (isVisible(grid, r, c)) {
                    count++;
                }
            }
        }
        return count;
    }

    private static int part2(int[][] grid) {
        int rows = grid.length;
        int cols = grid[0].length;
        int maxScore = 0;

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                int score = scenicScore(grid, r, c);
                maxScore = Math.max(maxScore, score);
            }
        }
        return maxScore;
    }

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of(System.getProperty("user.dir")).resolve("../input.txt");
        List<String> lines = Files.readAllLines(inputPath);

        // Remove empty lines if any
        lines = lines.stream().filter(line -> !line.isEmpty()).toList();

        int[][] grid = parseGrid(lines);

        System.out.println("Part 1: " + part1(grid));
        System.out.println("Part 2: " + part2(grid));
    }
}
