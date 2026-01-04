import java.io.*;
import java.nio.file.*;
import java.util.*;

/**
 * Day 23: A Long Walk - Longest path through hiking trails.
 *
 * Optimized version using:
 * - Integer indices for junctions instead of encoded longs
 * - Bitmask for visited tracking (O(1) operations, no allocation)
 * - Array-based adjacency list instead of nested HashMaps
 */
public class Solution {

    private static final int MAX_JUNCTIONS = 64;
    private static final int MAX_EDGES = 10;

    private static final int[] DR = {-1, 1, 0, 0};
    private static final int[] DC = {0, 0, -1, 1};

    // Grid data
    private static char[][] grid;
    private static int rows, cols;

    // Junction data
    private static int[][] junctions = new int[MAX_JUNCTIONS][2];  // [row, col]
    private static int numJunctions;
    private static int[][] junctionIndex;  // junctionIndex[r][c] = index or -1

    // Adjacency list: adjTo[node][i] = neighbor index, adjWeight[node][i] = edge weight
    private static int[][] adjTo = new int[MAX_JUNCTIONS][MAX_EDGES];
    private static int[][] adjWeight = new int[MAX_JUNCTIONS][MAX_EDGES];
    private static int[] adjCount = new int[MAX_JUNCTIONS];

    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get(args.length > 0 ? args[0] : "../input.txt");
        List<String> lines = Files.readAllLines(inputPath);

        rows = lines.size();
        cols = lines.get(0).length();
        grid = new char[rows][cols];
        junctionIndex = new int[rows][cols];

        for (int r = 0; r < rows; r++) {
            grid[r] = lines.get(r).toCharArray();
            Arrays.fill(junctionIndex[r], -1);
        }

        System.out.println("Part 1: " + solve(true));
        System.out.println("Part 2: " + solve(false));
    }

    private static int solve(boolean respectSlopes) {
        findJunctions();
        buildGraph(respectSlopes);
        return longestPath();
    }

    private static int getSlopeDir(char c) {
        switch (c) {
            case '^': return 0;
            case 'v': return 1;
            case '<': return 2;
            case '>': return 3;
            default: return -1;
        }
    }

    private static void findJunctions() {
        numJunctions = 0;
        for (int r = 0; r < rows; r++) {
            Arrays.fill(junctionIndex[r], -1);
        }

        // Find start (first . in row 0)
        for (int c = 0; c < cols; c++) {
            if (grid[0][c] == '.') {
                junctions[numJunctions][0] = 0;
                junctions[numJunctions][1] = c;
                junctionIndex[0][c] = numJunctions++;
                break;
            }
        }

        // Find end (first . in last row)
        for (int c = 0; c < cols; c++) {
            if (grid[rows - 1][c] == '.') {
                junctions[numJunctions][0] = rows - 1;
                junctions[numJunctions][1] = c;
                junctionIndex[rows - 1][c] = numJunctions++;
                break;
            }
        }

        // Find intersections (cells with 3+ walkable neighbors)
        for (int r = 1; r < rows - 1; r++) {
            for (int c = 0; c < cols; c++) {
                if (grid[r][c] == '#') continue;

                int neighbors = 0;
                for (int d = 0; d < 4; d++) {
                    int nr = r + DR[d];
                    int nc = c + DC[d];
                    if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#') {
                        neighbors++;
                    }
                }

                if (neighbors >= 3) {
                    junctions[numJunctions][0] = r;
                    junctions[numJunctions][1] = c;
                    junctionIndex[r][c] = numJunctions++;
                }
            }
        }
    }

    private static void buildGraph(boolean respectSlopes) {
        Arrays.fill(adjCount, 0);

        // Stack for DFS: [r, c, dist]
        int[] stackR = new int[rows * cols];
        int[] stackC = new int[rows * cols];
        int[] stackDist = new int[rows * cols];
        boolean[][] visited = new boolean[rows][cols];

        for (int ji = 0; ji < numJunctions; ji++) {
            int startR = junctions[ji][0];
            int startC = junctions[ji][1];

            // Reset visited
            for (int r = 0; r < rows; r++) {
                Arrays.fill(visited[r], false);
            }

            int sp = 0;
            stackR[sp] = startR;
            stackC[sp] = startC;
            stackDist[sp] = 0;
            sp++;
            visited[startR][startC] = true;

            while (sp > 0) {
                sp--;
                int r = stackR[sp];
                int c = stackC[sp];
                int dist = stackDist[sp];

                // If we've reached another junction (not the start), record edge
                if (dist > 0 && junctionIndex[r][c] >= 0) {
                    int targetJi = junctionIndex[r][c];
                    int idx = adjCount[ji];
                    adjTo[ji][idx] = targetJi;
                    adjWeight[ji][idx] = dist;
                    adjCount[ji]++;
                    continue;
                }

                // Explore neighbors
                for (int d = 0; d < 4; d++) {
                    int nr = r + DR[d];
                    int nc = c + DC[d];

                    if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) continue;
                    if (grid[nr][nc] == '#') continue;
                    if (visited[nr][nc]) continue;

                    // Check slope constraints for Part 1
                    if (respectSlopes) {
                        int slopeDir = getSlopeDir(grid[r][c]);
                        if (slopeDir >= 0 && slopeDir != d) {
                            continue;
                        }
                    }

                    visited[nr][nc] = true;
                    stackR[sp] = nr;
                    stackC[sp] = nc;
                    stackDist[sp] = dist + 1;
                    sp++;
                }
            }
        }
    }

    private static int longestPath() {
        // Start is junction 0, end is junction 1
        return dfs(0, 1, 0L);
    }

    private static int dfs(int node, int end, long visitedMask) {
        if (node == end) {
            return 0;
        }

        visitedMask |= (1L << node);
        int maxDist = -1;

        int count = adjCount[node];
        for (int i = 0; i < count; i++) {
            int neighbor = adjTo[node][i];
            int weight = adjWeight[node][i];

            if ((visitedMask & (1L << neighbor)) != 0) continue;

            int result = dfs(neighbor, end, visitedMask);
            if (result >= 0) {
                int total = weight + result;
                if (total > maxDist) {
                    maxDist = total;
                }
            }
        }

        return maxDist;
    }
}
