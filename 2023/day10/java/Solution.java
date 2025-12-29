import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {

    // Pipe connections: each pipe connects to certain directions
    // Directions: N=(-1,0), S=(1,0), E=(0,1), W=(0,-1)
    private static final Map<Character, int[][]> PIPE_CONNECTIONS = new HashMap<>();

    static {
        PIPE_CONNECTIONS.put('|', new int[][]{{-1, 0}, {1, 0}});   // N, S
        PIPE_CONNECTIONS.put('-', new int[][]{{0, -1}, {0, 1}});   // W, E
        PIPE_CONNECTIONS.put('L', new int[][]{{-1, 0}, {0, 1}});   // N, E
        PIPE_CONNECTIONS.put('J', new int[][]{{-1, 0}, {0, -1}});  // N, W
        PIPE_CONNECTIONS.put('7', new int[][]{{1, 0}, {0, -1}});   // S, W
        PIPE_CONNECTIONS.put('F', new int[][]{{1, 0}, {0, 1}});    // S, E
    }

    private final List<String> lines;
    private char[][] grid;

    public Solution(List<String> lines) {
        this.lines = lines;
        this.grid = new char[lines.size()][];
        for (int i = 0; i < lines.size(); i++) {
            this.grid[i] = lines.get(i).toCharArray();
        }
    }

    private int[] findStart() {
        for (int r = 0; r < grid.length; r++) {
            for (int c = 0; c < grid[r].length; c++) {
                if (grid[r][c] == 'S') {
                    return new int[]{r, c};
                }
            }
        }
        return null;
    }

    private List<int[]> getNeighbors(int r, int c) {
        int rows = grid.length;
        int cols = grid[0].length;
        char ch = grid[r][c];
        List<int[]> neighbors = new ArrayList<>();

        if (ch == 'S') {
            // S can connect to any adjacent pipe that connects back to it
            int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
            for (int[] dir : directions) {
                int nr = r + dir[0];
                int nc = c + dir[1];
                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                    char adjCh = grid[nr][nc];
                    if (PIPE_CONNECTIONS.containsKey(adjCh)) {
                        // Check if adjacent pipe connects back to S
                        for (int[] adjDir : PIPE_CONNECTIONS.get(adjCh)) {
                            if (nr + adjDir[0] == r && nc + adjDir[1] == c) {
                                neighbors.add(new int[]{nr, nc});
                                break;
                            }
                        }
                    }
                }
            }
        } else if (PIPE_CONNECTIONS.containsKey(ch)) {
            for (int[] dir : PIPE_CONNECTIONS.get(ch)) {
                int nr = r + dir[0];
                int nc = c + dir[1];
                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                    neighbors.add(new int[]{nr, nc});
                }
            }
        }

        return neighbors;
    }

    private Map<Long, Integer> findLoop(int[] start) {
        // BFS to find the main loop and distances from start
        Map<Long, Integer> distances = new HashMap<>();
        long startKey = encodePosition(start[0], start[1]);
        distances.put(startKey, 0);

        Queue<int[]> queue = new LinkedList<>();
        queue.add(start);

        while (!queue.isEmpty()) {
            int[] pos = queue.poll();
            long posKey = encodePosition(pos[0], pos[1]);
            int dist = distances.get(posKey);

            for (int[] neighbor : getNeighbors(pos[0], pos[1])) {
                long neighborKey = encodePosition(neighbor[0], neighbor[1]);
                if (!distances.containsKey(neighborKey)) {
                    distances.put(neighborKey, dist + 1);
                    queue.add(neighbor);
                }
            }
        }

        return distances;
    }

    private long encodePosition(int r, int c) {
        return ((long) r << 32) | (c & 0xFFFFFFFFL);
    }

    private char determineStartPipe(int[] start, Set<Long> loopPositions) {
        int r = start[0];
        int c = start[1];
        int rows = grid.length;
        int cols = grid[0].length;

        Set<String> connections = new HashSet<>();
        int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

        for (int[] dir : directions) {
            int nr = r + dir[0];
            int nc = c + dir[1];
            long nKey = encodePosition(nr, nc);

            if (loopPositions.contains(nKey)) {
                char adjCh = grid[nr][nc];
                if (PIPE_CONNECTIONS.containsKey(adjCh)) {
                    // Check if this pipe connects back to S
                    for (int[] adjDir : PIPE_CONNECTIONS.get(adjCh)) {
                        if (nr + adjDir[0] == r && nc + adjDir[1] == c) {
                            connections.add(dir[0] + "," + dir[1]);
                            break;
                        }
                    }
                }
            }
        }

        // Match connections to pipe type
        for (Map.Entry<Character, int[][]> entry : PIPE_CONNECTIONS.entrySet()) {
            Set<String> pipeConns = new HashSet<>();
            for (int[] dir : entry.getValue()) {
                pipeConns.add(dir[0] + "," + dir[1]);
            }
            if (pipeConns.equals(connections)) {
                return entry.getKey();
            }
        }

        return 'S';
    }

    public int part1() {
        int[] start = findStart();
        Map<Long, Integer> distances = findLoop(start);
        return Collections.max(distances.values());
    }

    public int part2() {
        int[] start = findStart();
        Map<Long, Integer> distances = findLoop(start);
        Set<Long> loopPositions = distances.keySet();

        // Replace S with its actual pipe type
        char startPipe = determineStartPipe(start, loopPositions);
        grid[start[0]][start[1]] = startPipe;

        int rows = grid.length;
        int cols = grid[0].length;
        int enclosed = 0;

        for (int r = 0; r < rows; r++) {
            boolean inside = false;
            for (int c = 0; c < cols; c++) {
                long key = encodePosition(r, c);
                if (loopPositions.contains(key)) {
                    char ch = grid[r][c];
                    // Count vertical crossings (|, L, J go "north")
                    // Using "north" rule: count pipes that have a north connection
                    if (ch == '|' || ch == 'L' || ch == 'J') {
                        inside = !inside;
                    }
                } else {
                    if (inside) {
                        enclosed++;
                    }
                }
            }
        }

        return enclosed;
    }

    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get(args.length > 0 ? args[0] : "../input.txt");
        List<String> lines = Files.readAllLines(inputPath);

        Solution solution = new Solution(lines);
        System.out.println("Part 1: " + solution.part1());
        System.out.println("Part 2: " + solution.part2());
    }
}
