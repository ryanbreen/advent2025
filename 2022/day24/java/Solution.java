import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {

    private static int height, width, innerH, innerW;
    private static int startRow, startCol, endRow, endCol;
    private static List<int[]> blizzards = new ArrayList<>();
    private static int period;
    private static Set<Long>[] blizzardCache;

    public static void main(String[] args) throws Exception {
        Path classPath = Paths.get(Solution.class.getProtectionDomain().getCodeSource().getLocation().toURI());
        Path scriptDir = classPath.getParent();
        if (Files.isRegularFile(classPath)) {
            // classPath is the .class file itself when run directly
        } else {
            // classPath is the directory containing the .class file
            scriptDir = classPath;
        }
        String inputPath = scriptDir.resolve("../input.txt").normalize().toString();

        List<String> lines = Files.readAllLines(Paths.get(inputPath));
        parseInput(lines);

        System.out.println("Part 1: " + part1());
        System.out.println("Part 2: " + part2());
    }

    private static void parseInput(List<String> lines) {
        height = lines.size();
        width = lines.get(0).length();
        innerH = height - 2;
        innerW = width - 2;

        // Find start and end positions
        startRow = 0;
        startCol = lines.get(0).indexOf('.');
        endRow = height - 1;
        endCol = lines.get(height - 1).indexOf('.');

        // Parse blizzards
        for (int r = 0; r < height; r++) {
            String line = lines.get(r);
            for (int c = 0; c < width; c++) {
                char ch = line.charAt(c);
                if (ch == '^' || ch == 'v' || ch == '<' || ch == '>') {
                    blizzards.add(new int[]{r, c, ch});
                }
            }
        }

        // Compute period (LCM of inner dimensions)
        period = lcm(innerH, innerW);

        // Precompute blizzard positions for all times in one period
        blizzardCache = new HashSet[period];
        for (int t = 0; t < period; t++) {
            blizzardCache[t] = getBlizzardPositions(t);
        }
    }

    private static int gcd(int a, int b) {
        while (b != 0) {
            int temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }

    private static int lcm(int a, int b) {
        return a * b / gcd(a, b);
    }

    private static long encode(int r, int c) {
        return ((long) r << 16) | c;
    }

    private static Set<Long> getBlizzardPositions(int time) {
        Set<Long> positions = new HashSet<>();

        for (int[] bliz : blizzards) {
            int r = bliz[0];
            int c = bliz[1];
            char dir = (char) bliz[2];

            // Adjust to inner coordinates (subtract 1 for wall)
            int ir = r - 1;
            int ic = c - 1;

            int nr, nc;

            switch (dir) {
                case '^':
                    nr = ((ir - time) % innerH + innerH) % innerH;
                    nc = ic;
                    break;
                case 'v':
                    nr = (ir + time) % innerH;
                    nc = ic;
                    break;
                case '<':
                    nr = ir;
                    nc = ((ic - time) % innerW + innerW) % innerW;
                    break;
                case '>':
                    nr = ir;
                    nc = (ic + time) % innerW;
                    break;
                default:
                    throw new IllegalStateException("Unknown direction: " + dir);
            }

            // Convert back to full coordinates
            positions.add(encode(nr + 1, nc + 1));
        }

        return positions;
    }

    private static int bfs(int sr, int sc, int er, int ec, int startTime) {
        // BFS: state = (time % period, row, col)
        // Use a queue with (time, row, col)
        Deque<int[]> queue = new ArrayDeque<>();
        queue.add(new int[]{startTime, sr, sc});

        Set<Long> visited = new HashSet<>();
        visited.add(encodeState(startTime % period, sr, sc));

        // Directions: wait, up, down, left, right
        int[][] directions = {{0, 0}, {-1, 0}, {1, 0}, {0, -1}, {0, 1}};

        while (!queue.isEmpty()) {
            int[] current = queue.poll();
            int time = current[0];
            int r = current[1];
            int c = current[2];

            if (r == er && c == ec) {
                return time;
            }

            int nextTime = time + 1;
            Set<Long> nextBlizzards = blizzardCache[nextTime % period];

            for (int[] dir : directions) {
                int nr = r + dir[0];
                int nc = c + dir[1];

                // Check if valid position
                boolean valid = false;
                if ((nr == startRow && nc == startCol) || (nr == endRow && nc == endCol)) {
                    valid = true;
                } else if (nr > 0 && nr < height - 1 && nc > 0 && nc < width - 1) {
                    valid = true;
                }

                if (!valid) continue;

                // Check blizzards
                if (nextBlizzards.contains(encode(nr, nc))) {
                    continue;
                }

                long state = encodeState(nextTime % period, nr, nc);
                if (!visited.contains(state)) {
                    visited.add(state);
                    queue.add(new int[]{nextTime, nr, nc});
                }
            }
        }

        return -1; // No path found
    }

    private static long encodeState(int time, int r, int c) {
        return ((long) time << 32) | ((long) r << 16) | c;
    }

    private static int part1() {
        return bfs(startRow, startCol, endRow, endCol, 0);
    }

    private static int part2() {
        // Trip 1: start to end
        int t1 = bfs(startRow, startCol, endRow, endCol, 0);

        // Trip 2: end to start
        int t2 = bfs(endRow, endCol, startRow, startCol, t1);

        // Trip 3: start to end again
        int t3 = bfs(startRow, startCol, endRow, endCol, t2);

        return t3;
    }
}
