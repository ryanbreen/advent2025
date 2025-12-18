import java.io.*;
import java.util.*;

public class Solution {
    private static final int SIZE = 71;
    private static final int[] DX = {0, 0, 1, -1};
    private static final int[] DY = {1, -1, 0, 0};

    public static void main(String[] args) throws IOException {
        List<int[]> positions = parseInput("../input.txt");

        System.out.println("Part 1: " + part1(positions));
        System.out.println("Part 2: " + part2(positions));
    }

    private static List<int[]> parseInput(String filename) throws IOException {
        List<int[]> positions = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (!line.isEmpty()) {
                    String[] parts = line.split(",");
                    int x = Integer.parseInt(parts[0]);
                    int y = Integer.parseInt(parts[1]);
                    positions.add(new int[]{x, y});
                }
            }
        }
        return positions;
    }

    private static int bfs(Set<Long> corrupted) {
        long start = encode(0, 0);
        long goal = encode(SIZE - 1, SIZE - 1);

        if (corrupted.contains(start) || corrupted.contains(goal)) {
            return -1;
        }

        Queue<long[]> queue = new LinkedList<>();
        queue.offer(new long[]{start, 0});
        Set<Long> visited = new HashSet<>();
        visited.add(start);

        while (!queue.isEmpty()) {
            long[] current = queue.poll();
            long pos = current[0];
            int steps = (int) current[1];

            if (pos == goal) {
                return steps;
            }

            int x = (int) (pos >> 32);
            int y = (int) (pos & 0xFFFFFFFFL);

            for (int i = 0; i < 4; i++) {
                int nx = x + DX[i];
                int ny = y + DY[i];

                if (nx >= 0 && nx < SIZE && ny >= 0 && ny < SIZE) {
                    long newPos = encode(nx, ny);
                    if (!visited.contains(newPos) && !corrupted.contains(newPos)) {
                        visited.add(newPos);
                        queue.offer(new long[]{newPos, steps + 1});
                    }
                }
            }
        }

        return -1;
    }

    private static long encode(int x, int y) {
        return ((long) x << 32) | (y & 0xFFFFFFFFL);
    }

    private static Set<Long> buildCorruptedSet(List<int[]> positions, int count) {
        Set<Long> corrupted = new HashSet<>();
        for (int i = 0; i < count && i < positions.size(); i++) {
            int[] pos = positions.get(i);
            corrupted.add(encode(pos[0], pos[1]));
        }
        return corrupted;
    }

    private static int part1(List<int[]> positions) {
        Set<Long> corrupted = buildCorruptedSet(positions, 1024);
        return bfs(corrupted);
    }

    private static String part2(List<int[]> positions) {
        int left = 0;
        int right = positions.size();

        while (left < right) {
            int mid = (left + right) / 2;
            Set<Long> corrupted = buildCorruptedSet(positions, mid + 1);
            if (bfs(corrupted) == -1) {
                right = mid;
            } else {
                left = mid + 1;
            }
        }

        int[] blockingPos = positions.get(left);
        return blockingPos[0] + "," + blockingPos[1];
    }
}
