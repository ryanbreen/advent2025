import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {

    record Point(int row, int col) {}

    enum Direction {
        NORTH(-1, 0),
        SOUTH(1, 0),
        EAST(0, 1),
        WEST(0, -1);

        final int dr, dc;

        Direction(int dr, int dc) {
            this.dr = dr;
            this.dc = dc;
        }

        Direction opposite() {
            return switch (this) {
                case NORTH -> SOUTH;
                case SOUTH -> NORTH;
                case EAST -> WEST;
                case WEST -> EAST;
            };
        }
    }

    private static final Map<Character, Set<Direction>> PIPE_CONNECTIONS = Map.of(
        '|', EnumSet.of(Direction.NORTH, Direction.SOUTH),
        '-', EnumSet.of(Direction.WEST, Direction.EAST),
        'L', EnumSet.of(Direction.NORTH, Direction.EAST),
        'J', EnumSet.of(Direction.NORTH, Direction.WEST),
        '7', EnumSet.of(Direction.SOUTH, Direction.WEST),
        'F', EnumSet.of(Direction.SOUTH, Direction.EAST)
    );

    private final char[][] grid;
    private Map<Point, Integer> cachedLoop;
    private Point cachedStart;

    public Solution(List<String> lines) {
        this.grid = new char[lines.size()][];
        for (var i = 0; i < lines.size(); i++) {
            this.grid[i] = lines.get(i).toCharArray();
        }
    }

    private Point findStart() {
        for (var r = 0; r < grid.length; r++) {
            for (var c = 0; c < grid[r].length; c++) {
                if (grid[r][c] == 'S') {
                    return new Point(r, c);
                }
            }
        }
        throw new IllegalStateException("No start position 'S' found in grid");
    }

    private List<Point> getNeighbors(Point pos) {
        var rows = grid.length;
        var cols = grid[0].length;
        var ch = grid[pos.row()][pos.col()];
        var neighbors = new ArrayList<Point>();

        if (ch == 'S') {
            for (var dir : Direction.values()) {
                var nr = pos.row() + dir.dr;
                var nc = pos.col() + dir.dc;
                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                    var adjCh = grid[nr][nc];
                    var connections = PIPE_CONNECTIONS.get(adjCh);
                    if (connections != null && connections.contains(dir.opposite())) {
                        neighbors.add(new Point(nr, nc));
                    }
                }
            }
        } else {
            var connections = PIPE_CONNECTIONS.get(ch);
            if (connections != null) {
                for (var dir : connections) {
                    var nr = pos.row() + dir.dr;
                    var nc = pos.col() + dir.dc;
                    if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                        neighbors.add(new Point(nr, nc));
                    }
                }
            }
        }

        return neighbors;
    }

    private Map<Point, Integer> findLoop(Point start) {
        var distances = new HashMap<Point, Integer>();
        distances.put(start, 0);

        var queue = new ArrayDeque<Point>();
        queue.add(start);

        while (!queue.isEmpty()) {
            var pos = queue.poll();
            var dist = distances.get(pos);

            for (var neighbor : getNeighbors(pos)) {
                if (!distances.containsKey(neighbor)) {
                    distances.put(neighbor, dist + 1);
                    queue.add(neighbor);
                }
            }
        }

        return distances;
    }

    private void ensureLoopCached() {
        if (cachedLoop == null) {
            cachedStart = findStart();
            cachedLoop = findLoop(cachedStart);
        }
    }

    private char determineStartPipe(Point start, Set<Point> loopPositions) {
        var rows = grid.length;
        var cols = grid[0].length;
        var connections = EnumSet.noneOf(Direction.class);

        for (var dir : Direction.values()) {
            var nr = start.row() + dir.dr;
            var nc = start.col() + dir.dc;
            var neighbor = new Point(nr, nc);

            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && loopPositions.contains(neighbor)) {
                var adjCh = grid[nr][nc];
                var adjConnections = PIPE_CONNECTIONS.get(adjCh);
                if (adjConnections != null && adjConnections.contains(dir.opposite())) {
                    connections.add(dir);
                }
            }
        }

        for (var entry : PIPE_CONNECTIONS.entrySet()) {
            if (entry.getValue().equals(connections)) {
                return entry.getKey();
            }
        }

        throw new IllegalStateException("Could not determine start pipe type");
    }

    public int part1() {
        ensureLoopCached();
        return Collections.max(cachedLoop.values());
    }

    public int part2() {
        ensureLoopCached();
        var loopPositions = cachedLoop.keySet();

        var startPipe = determineStartPipe(cachedStart, loopPositions);
        grid[cachedStart.row()][cachedStart.col()] = startPipe;

        var rows = grid.length;
        var cols = grid[0].length;
        var enclosed = 0;

        for (var r = 0; r < rows; r++) {
            var inside = false;
            for (var c = 0; c < cols; c++) {
                var pos = new Point(r, c);
                if (loopPositions.contains(pos)) {
                    var ch = grid[r][c];
                    // Count vertical crossings using "north" rule
                    var connections = PIPE_CONNECTIONS.get(ch);
                    if (connections != null && connections.contains(Direction.NORTH)) {
                        inside = !inside;
                    }
                } else if (inside) {
                    enclosed++;
                }
            }
        }

        return enclosed;
    }

    public static void main(String[] args) throws IOException {
        var inputPath = Paths.get(args.length > 0 ? args[0] : "../input.txt");
        var lines = Files.readAllLines(inputPath);

        var solution = new Solution(lines);
        System.out.println("Part 1: " + solution.part1());
        System.out.println("Part 2: " + solution.part2());
    }
}
