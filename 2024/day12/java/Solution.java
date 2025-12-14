import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Solution {
    private static char[][] grid;
    private static int rows;
    private static int cols;

    public static void main(String[] args) throws IOException {
        // Read input file
        String input = Files.readString(Paths.get("../input.txt")).strip();
        String[] lines = input.split("\n");

        rows = lines.length;
        cols = lines[0].length();
        grid = new char[rows][cols];

        for (int i = 0; i < rows; i++) {
            grid[i] = lines[i].toCharArray();
        }

        System.out.println("Part 1: " + part1());
        System.out.println("Part 2: " + part2());
    }

    private static List<Set<Coord>> findRegions() {
        Set<Coord> visited = new HashSet<>();
        List<Set<Coord>> regions = new ArrayList<>();

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                Coord start = new Coord(r, c);
                if (visited.contains(start)) {
                    continue;
                }

                // BFS to find all cells in this region
                char plant = grid[r][c];
                Set<Coord> region = new HashSet<>();
                Deque<Coord> queue = new ArrayDeque<>();
                queue.add(start);

                while (!queue.isEmpty()) {
                    Coord current = queue.poll();

                    if (visited.contains(current)) {
                        continue;
                    }
                    if (current.r < 0 || current.r >= rows || current.c < 0 || current.c >= cols) {
                        continue;
                    }
                    if (grid[current.r][current.c] != plant) {
                        continue;
                    }

                    visited.add(current);
                    region.add(current);

                    // Add neighbors
                    queue.add(new Coord(current.r, current.c + 1));
                    queue.add(new Coord(current.r, current.c - 1));
                    queue.add(new Coord(current.r + 1, current.c));
                    queue.add(new Coord(current.r - 1, current.c));
                }

                regions.add(region);
            }
        }

        return regions;
    }

    private static int calculatePerimeter(Set<Coord> region) {
        int perimeter = 0;
        for (Coord coord : region) {
            // Check all 4 neighbors
            int[][] directions = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};
            for (int[] dir : directions) {
                Coord neighbor = new Coord(coord.r + dir[0], coord.c + dir[1]);
                if (!region.contains(neighbor)) {
                    perimeter++;
                }
            }
        }
        return perimeter;
    }

    private static int part1() {
        List<Set<Coord>> regions = findRegions();
        int total = 0;
        for (Set<Coord> region : regions) {
            int area = region.size();
            int perimeter = calculatePerimeter(region);
            total += area * perimeter;
        }
        return total;
    }

    private static int countSides(Set<Coord> region) {
        int corners = 0;

        for (Coord coord : region) {
            int r = coord.r;
            int c = coord.c;

            // Check all 4 corners of this cell
            boolean up = region.contains(new Coord(r - 1, c));
            boolean down = region.contains(new Coord(r + 1, c));
            boolean left = region.contains(new Coord(r, c - 1));
            boolean right = region.contains(new Coord(r, c + 1));
            boolean upLeft = region.contains(new Coord(r - 1, c - 1));
            boolean upRight = region.contains(new Coord(r - 1, c + 1));
            boolean downLeft = region.contains(new Coord(r + 1, c - 1));
            boolean downRight = region.contains(new Coord(r + 1, c + 1));

            // Top-left corner
            if (!up && !left) {  // convex
                corners++;
            } else if (up && left && !upLeft) {  // concave
                corners++;
            }

            // Top-right corner
            if (!up && !right) {  // convex
                corners++;
            } else if (up && right && !upRight) {  // concave
                corners++;
            }

            // Bottom-left corner
            if (!down && !left) {  // convex
                corners++;
            } else if (down && left && !downLeft) {  // concave
                corners++;
            }

            // Bottom-right corner
            if (!down && !right) {  // convex
                corners++;
            } else if (down && right && !downRight) {  // concave
                corners++;
            }
        }

        return corners;
    }

    private static int part2() {
        List<Set<Coord>> regions = findRegions();
        int total = 0;
        for (Set<Coord> region : regions) {
            int area = region.size();
            int sides = countSides(region);
            total += area * sides;
        }
        return total;
    }

    // Helper class for coordinates with proper equals/hashCode
    private static class Coord {
        final int r;
        final int c;

        Coord(int r, int c) {
            this.r = r;
            this.c = c;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Coord coord = (Coord) o;
            return r == coord.r && c == coord.c;
        }

        @Override
        public int hashCode() {
            return Objects.hash(r, c);
        }
    }
}
