import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Solution {
    private static final int WIDTH = 101;
    private static final int HEIGHT = 103;

    static class Robot {
        int px, py, vx, vy;

        Robot(int px, int py, int vx, int vy) {
            this.px = px;
            this.py = py;
            this.vx = vx;
            this.vy = vy;
        }
    }

    static class Position {
        int x, y;

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
            return 31 * x + y;
        }
    }

    private static List<Robot> parseRobots(String text) {
        List<Robot> robots = new ArrayList<>();
        Pattern pattern = Pattern.compile("p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)");

        for (String line : text.split("\n")) {
            Matcher matcher = pattern.matcher(line);
            if (matcher.find()) {
                int px = Integer.parseInt(matcher.group(1));
                int py = Integer.parseInt(matcher.group(2));
                int vx = Integer.parseInt(matcher.group(3));
                int vy = Integer.parseInt(matcher.group(4));
                robots.add(new Robot(px, py, vx, vy));
            }
        }

        return robots;
    }

    private static List<Position> simulate(List<Robot> robots, int seconds) {
        List<Position> positions = new ArrayList<>();

        for (Robot robot : robots) {
            // Position after 'seconds' time, with wrapping
            // Use Math.floorMod for proper modulo with negative numbers
            int newX = Math.floorMod(robot.px + robot.vx * seconds, WIDTH);
            int newY = Math.floorMod(robot.py + robot.vy * seconds, HEIGHT);
            positions.add(new Position(newX, newY));
        }

        return positions;
    }

    private static int[] countQuadrants(List<Position> positions) {
        int midX = WIDTH / 2;   // 50
        int midY = HEIGHT / 2;  // 51

        int q1 = 0, q2 = 0, q3 = 0, q4 = 0;

        for (Position pos : positions) {
            if (pos.x == midX || pos.y == midY) {
                continue;  // Skip robots on middle lines
            }

            if (pos.x < midX && pos.y < midY) {
                q1++;  // Top-left
            } else if (pos.x > midX && pos.y < midY) {
                q2++;  // Top-right
            } else if (pos.x < midX && pos.y > midY) {
                q3++;  // Bottom-left
            } else {
                q4++;  // Bottom-right
            }
        }

        return new int[]{q1, q2, q3, q4};
    }

    private static long part1(List<Robot> robots) {
        List<Position> positions = simulate(robots, 100);
        int[] quadrants = countQuadrants(positions);
        return (long) quadrants[0] * quadrants[1] * quadrants[2] * quadrants[3];
    }

    private static int part2(List<Robot> robots) {
        // The Christmas tree appears when robots cluster together
        // Look for a frame with a long horizontal line of robots (tree base/border)
        for (int seconds = 1; seconds <= WIDTH * HEIGHT; seconds++) {
            List<Position> positions = simulate(robots, seconds);
            Set<Position> posSet = new HashSet<>(positions);

            // Look for a horizontal line of at least 20 consecutive robots
            for (int y = 0; y < HEIGHT; y++) {
                int maxConsecutive = 0;
                int consecutive = 0;

                for (int x = 0; x < WIDTH; x++) {
                    if (posSet.contains(new Position(x, y))) {
                        consecutive++;
                        maxConsecutive = Math.max(maxConsecutive, consecutive);
                    } else {
                        consecutive = 0;
                    }
                }

                if (maxConsecutive >= 20) {
                    return seconds;
                }
            }
        }

        return -1;
    }

    public static void main(String[] args) throws IOException {
        String inputText = Files.readString(Path.of("../input.txt")).strip();
        List<Robot> robots = parseRobots(inputText);

        System.out.println("Part 1: " + part1(robots));
        System.out.println("Part 2: " + part2(robots));
    }
}
