import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {
    private static char[][] grid;
    private static int rows, cols;
    private static int[] start = new int[2];
    private static int[] end = new int[2];

    public static void main(String[] args) throws IOException {
        String inputPath = Paths.get(System.getProperty("user.dir"), "..", "input.txt")
                                .normalize().toString();
        String text = Files.readString(Path.of(inputPath));
        parseGrid(text);

        System.out.println("Part 1: " + part1());
        System.out.println("Part 2: " + part2());
    }

    private static void parseGrid(String text) {
        String[] lines = text.trim().split("\n");
        rows = lines.length;
        cols = lines[0].length();
        grid = new char[rows][cols];

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                char ch = lines[r].charAt(c);
                if (ch == 'S') {
                    start[0] = r;
                    start[1] = c;
                    grid[r][c] = 'a';
                } else if (ch == 'E') {
                    end[0] = r;
                    end[1] = c;
                    grid[r][c] = 'z';
                } else {
                    grid[r][c] = ch;
                }
            }
        }
    }

    private static int bfs(List<int[]> starts, int[] target) {
        boolean[][] visited = new boolean[rows][cols];
        LinkedList<int[]> queue = new LinkedList<>();

        for (int[] s : starts) {
            queue.add(new int[]{s[0], s[1], 0});
            visited[s[0]][s[1]] = true;
        }

        int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

        while (!queue.isEmpty()) {
            int[] current = queue.removeFirst();
            int r = current[0];
            int c = current[1];
            int dist = current[2];

            if (r == target[0] && c == target[1]) {
                return dist;
            }

            int currentHeight = grid[r][c];

            for (int[] dir : directions) {
                int nr = r + dir[0];
                int nc = c + dir[1];

                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && !visited[nr][nc]) {
                    int nextHeight = grid[nr][nc];
                    // Can move if destination is at most 1 higher
                    if (nextHeight <= currentHeight + 1) {
                        visited[nr][nc] = true;
                        queue.add(new int[]{nr, nc, dist + 1});
                    }
                }
            }
        }

        return -1; // No path found
    }

    private static int part1() {
        List<int[]> starts = new ArrayList<>();
        starts.add(start);
        return bfs(starts, end);
    }

    private static int part2() {
        // Find all cells with elevation 'a'
        List<int[]> starts = new ArrayList<>();
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (grid[r][c] == 'a') {
                    starts.add(new int[]{r, c});
                }
            }
        }
        return bfs(starts, end);
    }
}
