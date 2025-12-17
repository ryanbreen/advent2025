import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/**
 * Day 16: Reindeer Maze - Weighted shortest path with turn costs
 *
 * Part 1: Find shortest path where moving costs 1, turning costs 1000
 * Part 2: Count tiles on any optimal path using bidirectional Dijkstra
 */
public class Solution {

    // Directions: 0=East, 1=South, 2=West, 3=North
    private static final int[] DX = {1, 0, -1, 0};
    private static final int[] DY = {0, 1, 0, -1};

    private static class State {
        int cost;
        int x;
        int y;
        int dir;

        State(int cost, int x, int y, int dir) {
            this.cost = cost;
            this.x = x;
            this.y = y;
            this.dir = dir;
        }
    }

    private static class StateKey {
        int x;
        int y;
        int dir;

        StateKey(int x, int y, int dir) {
            this.x = x;
            this.y = y;
            this.dir = dir;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            StateKey stateKey = (StateKey) o;
            return x == stateKey.x && y == stateKey.y && dir == stateKey.dir;
        }

        @Override
        public int hashCode() {
            return Objects.hash(x, y, dir);
        }
    }

    private static class Position {
        int x;
        int y;

        Position(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Position position = (Position) o;
            return x == position.x && y == position.y;
        }

        @Override
        public int hashCode() {
            return Objects.hash(x, y);
        }
    }

    private static class MazeData {
        char[][] grid;
        Position start;
        Position end;

        MazeData(char[][] grid, Position start, Position end) {
            this.grid = grid;
            this.start = start;
            this.end = end;
        }
    }

    private static MazeData parseInput(String text) {
        String[] lines = text.trim().split("\n");
        int height = lines.length;
        int width = lines[0].length();
        char[][] grid = new char[height][width];
        Position start = null;
        Position end = null;

        for (int y = 0; y < height; y++) {
            grid[y] = lines[y].toCharArray();
            for (int x = 0; x < width; x++) {
                if (grid[y][x] == 'S') {
                    start = new Position(x, y);
                } else if (grid[y][x] == 'E') {
                    end = new Position(x, y);
                }
            }
        }

        return new MazeData(grid, start, end);
    }

    /**
     * Run Dijkstra from start facing East.
     * Returns map of (x, y, dir) -> minimum cost to reach that state.
     */
    private static Map<StateKey, Integer> dijkstraForward(char[][] grid, Position start) {
        PriorityQueue<State> pq = new PriorityQueue<>(Comparator.comparingInt(s -> s.cost));
        pq.offer(new State(0, start.x, start.y, 0)); // Start facing East
        Map<StateKey, Integer> dist = new HashMap<>();

        int height = grid.length;
        int width = grid[0].length;

        while (!pq.isEmpty()) {
            State current = pq.poll();

            StateKey key = new StateKey(current.x, current.y, current.dir);
            if (dist.containsKey(key)) {
                continue;
            }
            dist.put(key, current.cost);

            // Move forward
            int nx = current.x + DX[current.dir];
            int ny = current.y + DY[current.dir];
            if (ny >= 0 && ny < height && nx >= 0 && nx < width && grid[ny][nx] != '#') {
                pq.offer(new State(current.cost + 1, nx, ny, current.dir));
            }

            // Turn left
            int leftDir = (current.dir - 1 + 4) % 4;
            pq.offer(new State(current.cost + 1000, current.x, current.y, leftDir));

            // Turn right
            int rightDir = (current.dir + 1) % 4;
            pq.offer(new State(current.cost + 1000, current.x, current.y, rightDir));
        }

        return dist;
    }

    /**
     * Run Dijkstra backward from end (all directions at end have cost 0).
     * Returns map of (x, y, dir) -> minimum cost from that state to reach end.
     */
    private static Map<StateKey, Integer> dijkstraBackward(char[][] grid, Position end) {
        PriorityQueue<State> pq = new PriorityQueue<>(Comparator.comparingInt(s -> s.cost));

        // At end, we can arrive facing any direction
        for (int d = 0; d < 4; d++) {
            pq.offer(new State(0, end.x, end.y, d));
        }

        Map<StateKey, Integer> dist = new HashMap<>();
        int height = grid.length;
        int width = grid[0].length;

        while (!pq.isEmpty()) {
            State current = pq.poll();

            StateKey key = new StateKey(current.x, current.y, current.dir);
            if (dist.containsKey(key)) {
                continue;
            }
            dist.put(key, current.cost);

            // Reverse of "move forward": come from behind
            int px = current.x - DX[current.dir];
            int py = current.y - DY[current.dir];
            if (py >= 0 && py < height && px >= 0 && px < width && grid[py][px] != '#') {
                pq.offer(new State(current.cost + 1, px, py, current.dir));
            }

            // Reverse of turn: came from same position with different direction
            int leftDir = (current.dir - 1 + 4) % 4;
            pq.offer(new State(current.cost + 1000, current.x, current.y, leftDir));

            int rightDir = (current.dir + 1) % 4;
            pq.offer(new State(current.cost + 1000, current.x, current.y, rightDir));
        }

        return dist;
    }

    private static int part1(char[][] grid, Position start, Position end,
                            Map<StateKey, Integer> distFromStart) {
        int minCost = Integer.MAX_VALUE;
        for (int d = 0; d < 4; d++) {
            StateKey endState = new StateKey(end.x, end.y, d);
            int cost = distFromStart.getOrDefault(endState, Integer.MAX_VALUE);
            minCost = Math.min(minCost, cost);
        }
        return minCost;
    }

    private static int part2(char[][] grid, Position start, Position end, int bestScore) {
        Map<StateKey, Integer> distFromStart = dijkstraForward(grid, start);
        Map<StateKey, Integer> distToEnd = dijkstraBackward(grid, end);

        Set<Position> tilesOnBestPath = new HashSet<>();
        int height = grid.length;
        int width = grid[0].length;

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                if (grid[y][x] == '#') {
                    continue;
                }

                // Check if this tile is on any optimal path
                for (int d = 0; d < 4; d++) {
                    StateKey state = new StateKey(x, y, d);
                    int fromStart = distFromStart.getOrDefault(state, Integer.MAX_VALUE);
                    int toEnd = distToEnd.getOrDefault(state, Integer.MAX_VALUE);

                    if (fromStart != Integer.MAX_VALUE && toEnd != Integer.MAX_VALUE
                        && fromStart + toEnd == bestScore) {
                        tilesOnBestPath.add(new Position(x, y));
                        break;
                    }
                }
            }
        }

        return tilesOnBestPath.size();
    }

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of("../input.txt");
        String text = Files.readString(inputPath);
        MazeData maze = parseInput(text);

        Map<StateKey, Integer> distFromStart = dijkstraForward(maze.grid, maze.start);
        int answer1 = part1(maze.grid, maze.start, maze.end, distFromStart);
        System.out.println("Part 1: " + answer1);

        int answer2 = part2(maze.grid, maze.start, maze.end, answer1);
        System.out.println("Part 2: " + answer2);
    }
}
