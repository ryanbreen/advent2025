import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Solution {
    private static class Position {
        int row, col;

        Position(int row, int col) {
            this.row = row;
            this.col = col;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Position position = (Position) o;
            return row == position.row && col == position.col;
        }

        @Override
        public int hashCode() {
            return 31 * row + col;
        }
    }

    private static class Direction {
        int dRow, dCol;

        Direction(int dRow, int dCol) {
            this.dRow = dRow;
            this.dCol = dCol;
        }

        // Turn right 90 degrees
        Direction turnRight() {
            // Up (−1,0) → Right (0,1)
            // Right (0,1) → Down (1,0)
            // Down (1,0) → Left (0,−1)
            // Left (0,−1) → Up (−1,0)
            return new Direction(dCol, -dRow);
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Direction direction = (Direction) o;
            return dRow == direction.dRow && dCol == direction.dCol;
        }

        @Override
        public int hashCode() {
            return 31 * dRow + dCol;
        }
    }

    private static class State {
        Position pos;
        Direction dir;

        State(Position pos, Direction dir) {
            this.pos = pos;
            this.dir = dir;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            State state = (State) o;
            return pos.equals(state.pos) && dir.equals(state.dir);
        }

        @Override
        public int hashCode() {
            return 31 * pos.hashCode() + dir.hashCode();
        }
    }

    public static int part1(List<String> lines) {
        int rows = lines.size();
        int cols = lines.get(0).length();
        char[][] grid = new char[rows][cols];

        // Parse grid and find starting position
        Position start = null;
        Direction startDir = null;

        for (int r = 0; r < rows; r++) {
            String line = lines.get(r);
            for (int c = 0; c < line.length(); c++) {
                char ch = line.charAt(c);
                grid[r][c] = ch;

                if (ch == '^') {
                    start = new Position(r, c);
                    startDir = new Direction(-1, 0);  // Up
                    grid[r][c] = '.';  // Clear starting position
                } else if (ch == 'v') {
                    start = new Position(r, c);
                    startDir = new Direction(1, 0);   // Down
                    grid[r][c] = '.';
                } else if (ch == '<') {
                    start = new Position(r, c);
                    startDir = new Direction(0, -1);  // Left
                    grid[r][c] = '.';
                } else if (ch == '>') {
                    start = new Position(r, c);
                    startDir = new Direction(0, 1);   // Right
                    grid[r][c] = '.';
                }
            }
        }

        // Simulate guard movement
        Set<Position> visited = new HashSet<>();
        Position current = start;
        Direction dir = startDir;

        while (true) {
            // Mark current position as visited
            visited.add(new Position(current.row, current.col));

            // Calculate next position
            int nextRow = current.row + dir.dRow;
            int nextCol = current.col + dir.dCol;

            // Check if guard leaves the map
            if (nextRow < 0 || nextRow >= rows || nextCol < 0 || nextCol >= cols) {
                break;
            }

            // Check if there's an obstacle ahead
            if (grid[nextRow][nextCol] == '#') {
                // Turn right
                dir = dir.turnRight();
            } else {
                // Move forward
                current = new Position(nextRow, nextCol);
            }
        }

        return visited.size();
    }

    private static Set<Position> getVisitedPositions(List<String> lines) {
        int rows = lines.size();
        int cols = lines.get(0).length();
        char[][] grid = new char[rows][cols];

        // Parse grid and find starting position
        Position start = null;
        Direction startDir = null;

        for (int r = 0; r < rows; r++) {
            String line = lines.get(r);
            for (int c = 0; c < line.length(); c++) {
                char ch = line.charAt(c);
                grid[r][c] = ch;

                if (ch == '^') {
                    start = new Position(r, c);
                    startDir = new Direction(-1, 0);
                } else if (ch == 'v') {
                    start = new Position(r, c);
                    startDir = new Direction(1, 0);
                } else if (ch == '<') {
                    start = new Position(r, c);
                    startDir = new Direction(0, -1);
                } else if (ch == '>') {
                    start = new Position(r, c);
                    startDir = new Direction(0, 1);
                }
            }
        }

        // Simulate guard movement to get all visited positions
        Set<Position> visited = new HashSet<>();
        Position current = start;
        Direction dir = startDir;

        while (true) {
            visited.add(new Position(current.row, current.col));

            int nextRow = current.row + dir.dRow;
            int nextCol = current.col + dir.dCol;

            if (nextRow < 0 || nextRow >= rows || nextCol < 0 || nextCol >= cols) {
                break;
            }

            if (grid[nextRow][nextCol] == '#') {
                dir = dir.turnRight();
            } else {
                current = new Position(nextRow, nextCol);
            }
        }

        return visited;
    }

    private static boolean createsLoop(List<String> lines, Position obstruction) {
        int rows = lines.size();
        int cols = lines.get(0).length();
        char[][] grid = new char[rows][cols];

        // Parse grid and find starting position
        Position start = null;
        Direction startDir = null;

        for (int r = 0; r < rows; r++) {
            String line = lines.get(r);
            for (int c = 0; c < line.length(); c++) {
                char ch = line.charAt(c);
                grid[r][c] = ch;

                if (ch == '^') {
                    start = new Position(r, c);
                    startDir = new Direction(-1, 0);
                } else if (ch == 'v') {
                    start = new Position(r, c);
                    startDir = new Direction(1, 0);
                } else if (ch == '<') {
                    start = new Position(r, c);
                    startDir = new Direction(0, -1);
                } else if (ch == '>') {
                    start = new Position(r, c);
                    startDir = new Direction(0, 1);
                }
            }
        }

        // Place the new obstruction
        grid[obstruction.row][obstruction.col] = '#';

        // Simulate guard movement with new obstruction
        Set<State> states = new HashSet<>();
        Position current = start;
        Direction dir = startDir;

        while (true) {
            State state = new State(new Position(current.row, current.col), new Direction(dir.dRow, dir.dCol));

            // If we've seen this exact state before, we're in a loop
            if (states.contains(state)) {
                return true;
            }

            states.add(state);

            int nextRow = current.row + dir.dRow;
            int nextCol = current.col + dir.dCol;

            // Guard leaves the map
            if (nextRow < 0 || nextRow >= rows || nextCol < 0 || nextCol >= cols) {
                return false;
            }

            if (grid[nextRow][nextCol] == '#') {
                dir = dir.turnRight();
            } else {
                current = new Position(nextRow, nextCol);
            }
        }
    }

    public static int part2(List<String> lines) {
        // Get starting position
        Position start = null;
        for (int r = 0; r < lines.size(); r++) {
            String line = lines.get(r);
            for (int c = 0; c < line.length(); c++) {
                char ch = line.charAt(c);
                if (ch == '^' || ch == 'v' || ch == '<' || ch == '>') {
                    start = new Position(r, c);
                    break;
                }
            }
            if (start != null) break;
        }

        // Get all positions the guard visits in the original path
        Set<Position> visitedPositions = getVisitedPositions(lines);

        int loopCount = 0;

        // Try placing an obstruction at each visited position (except start)
        for (Position pos : visitedPositions) {
            // Can't place obstruction at starting position
            if (pos.equals(start)) {
                continue;
            }

            // Check if placing obstruction here creates a loop
            if (createsLoop(lines, pos)) {
                loopCount++;
            }
        }

        return loopCount;
    }

    public static void main(String[] args) throws IOException {
        List<String> lines = Files.readAllLines(Paths.get("../input.txt"));

        int result1 = part1(lines);
        System.out.println("Part 1: " + result1);

        int result2 = part2(lines);
        System.out.println("Part 2: " + result2);
    }
}
