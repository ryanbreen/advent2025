import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {

    // 6 directions: +x, -x, +y, -y, +z, -z
    private static final int[][] DIRECTIONS = {
        {1, 0, 0}, {-1, 0, 0},
        {0, 1, 0}, {0, -1, 0},
        {0, 0, 1}, {0, 0, -1}
    };

    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get(System.getProperty("user.dir"), "..", "input.txt");
        String text = Files.readString(inputPath);

        System.out.println("Part 1: " + part1(text));
        System.out.println("Part 2: " + part2(text));
    }

    private static Set<String> parseInput(String text) {
        Set<String> cubes = new HashSet<>();
        for (String line : text.strip().split("\n")) {
            cubes.add(line.strip());
        }
        return cubes;
    }

    private static int[] parseCube(String cube) {
        String[] parts = cube.split(",");
        return new int[] {
            Integer.parseInt(parts[0]),
            Integer.parseInt(parts[1]),
            Integer.parseInt(parts[2])
        };
    }

    private static String toKey(int x, int y, int z) {
        return x + "," + y + "," + z;
    }

    private static int part1(String text) {
        Set<String> cubes = parseInput(text);
        int surfaceArea = 0;

        for (String cube : cubes) {
            int[] coords = parseCube(cube);
            int x = coords[0], y = coords[1], z = coords[2];

            for (int[] dir : DIRECTIONS) {
                String neighbor = toKey(x + dir[0], y + dir[1], z + dir[2]);
                if (!cubes.contains(neighbor)) {
                    surfaceArea++;
                }
            }
        }

        return surfaceArea;
    }

    private static int part2(String text) {
        Set<String> cubes = parseInput(text);

        // Find bounding box with 1 unit padding
        int minX = Integer.MAX_VALUE, maxX = Integer.MIN_VALUE;
        int minY = Integer.MAX_VALUE, maxY = Integer.MIN_VALUE;
        int minZ = Integer.MAX_VALUE, maxZ = Integer.MIN_VALUE;

        for (String cube : cubes) {
            int[] coords = parseCube(cube);
            minX = Math.min(minX, coords[0]);
            maxX = Math.max(maxX, coords[0]);
            minY = Math.min(minY, coords[1]);
            maxY = Math.max(maxY, coords[1]);
            minZ = Math.min(minZ, coords[2]);
            maxZ = Math.max(maxZ, coords[2]);
        }

        // Add 1 unit padding
        minX--; maxX++;
        minY--; maxY++;
        minZ--; maxZ++;

        // BFS to find all exterior air cells
        Set<String> exterior = new HashSet<>();
        Queue<int[]> queue = new LinkedList<>();

        int[] start = {minX, minY, minZ};
        queue.add(start);
        exterior.add(toKey(minX, minY, minZ));

        while (!queue.isEmpty()) {
            int[] current = queue.poll();
            int x = current[0], y = current[1], z = current[2];

            for (int[] dir : DIRECTIONS) {
                int nx = x + dir[0];
                int ny = y + dir[1];
                int nz = z + dir[2];

                // Stay within bounds
                if (nx < minX || nx > maxX || ny < minY || ny > maxY || nz < minZ || nz > maxZ) {
                    continue;
                }

                String key = toKey(nx, ny, nz);

                // Skip cubes and already visited
                if (cubes.contains(key) || exterior.contains(key)) {
                    continue;
                }

                exterior.add(key);
                queue.add(new int[]{nx, ny, nz});
            }
        }

        // Count faces touching exterior air
        int surfaceArea = 0;
        for (String cube : cubes) {
            int[] coords = parseCube(cube);
            int x = coords[0], y = coords[1], z = coords[2];

            for (int[] dir : DIRECTIONS) {
                String neighbor = toKey(x + dir[0], y + dir[1], z + dir[2]);
                if (exterior.contains(neighbor)) {
                    surfaceArea++;
                }
            }
        }

        return surfaceArea;
    }
}
