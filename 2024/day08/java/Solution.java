import java.io.*;
import java.util.*;

public class Solution {
    record Point(int r, int c) {}

    record Input(int rows, int cols, Map<Character, List<Point>> antennas) {}

    private static boolean inBounds(int r, int c, int rows, int cols) {
        return r >= 0 && r < rows && c >= 0 && c < cols;
    }

    /**
     * Parses the input file and extracts grid dimensions and antenna positions.
     *
     * @param filename the path to the input file
     * @return an Input record containing grid dimensions and antenna positions grouped by frequency
     * @throws IOException if the file cannot be read
     */
    static Input parseInput(String filename) throws IOException {
        var grid = new ArrayList<String>();

        try (var br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                grid.add(line);
            }
        }

        int rows = grid.size();
        int cols = rows > 0 ? grid.get(0).length() : 0;

        // Group antenna positions by frequency
        var antennas = new HashMap<Character, List<Point>>();
        for (int r = 0; r < rows; r++) {
            var row = grid.get(r);
            for (int c = 0; c < row.length(); c++) {
                char ch = row.charAt(c);
                if (ch != '.') {
                    antennas.computeIfAbsent(ch, k -> new ArrayList<>()).add(new Point(r, c));
                }
            }
        }

        return new Input(rows, cols, antennas);
    }

    /**
     * Solves Part 1: counts unique antinode positions created by pairs of antennas.
     * An antinode occurs at any point that is twice as far from one antenna as the other.
     *
     * @param input the parsed input containing grid dimensions and antenna positions
     * @return the count of unique antinode positions within the grid bounds
     */
    static int part1(Input input) {
        var antinodes = new HashSet<Point>();

        for (var entry : input.antennas.entrySet()) {
            var positions = entry.getValue();

            // For each pair of antennas with same frequency
            for (int i = 0; i < positions.size(); i++) {
                for (int j = i + 1; j < positions.size(); j++) {
                    var p1 = positions.get(i);
                    var p2 = positions.get(j);

                    // Calculate the two antinodes
                    // Antinode beyond antenna 1 (away from antenna 2)
                    int ar1 = 2 * p1.r - p2.r;
                    int ac1 = 2 * p1.c - p2.c;
                    // Antinode beyond antenna 2 (away from antenna 1)
                    int ar2 = 2 * p2.r - p1.r;
                    int ac2 = 2 * p2.c - p1.c;

                    // Add if within bounds
                    if (inBounds(ar1, ac1, input.rows, input.cols)) {
                        antinodes.add(new Point(ar1, ac1));
                    }
                    if (inBounds(ar2, ac2, input.rows, input.cols)) {
                        antinodes.add(new Point(ar2, ac2));
                    }
                }
            }
        }

        return antinodes.size();
    }

    /**
     * Solves Part 2: counts unique antinode positions with resonant harmonics.
     * Antinodes occur at any grid position exactly in line with at least two antennas
     * of the same frequency, regardless of distance.
     *
     * @param input the parsed input containing grid dimensions and antenna positions
     * @return the count of unique antinode positions within the grid bounds
     */
    static int part2(Input input) {
        var antinodes = new HashSet<Point>();

        for (var entry : input.antennas.entrySet()) {
            var positions = entry.getValue();

            // For each pair of antennas with same frequency
            for (int i = 0; i < positions.size(); i++) {
                for (int j = i + 1; j < positions.size(); j++) {
                    var p1 = positions.get(i);
                    var p2 = positions.get(j);

                    int dr = p2.r - p1.r;
                    int dc = p2.c - p1.c;

                    // Extend in both directions along the line
                    // Direction 1: from antenna 1 towards and beyond antenna 2
                    int r = p1.r;
                    int c = p1.c;
                    while (inBounds(r, c, input.rows, input.cols)) {
                        antinodes.add(new Point(r, c));
                        r += dr;
                        c += dc;
                    }

                    // Direction 2: from antenna 1 away from antenna 2
                    r = p1.r - dr;
                    c = p1.c - dc;
                    while (inBounds(r, c, input.rows, input.cols)) {
                        antinodes.add(new Point(r, c));
                        r -= dr;
                        c -= dc;
                    }
                }
            }
        }

        return antinodes.size();
    }

    /**
     * Main entry point. Parses input once and runs both parts.
     *
     * @param args command-line arguments (unused)
     */
    public static void main(String[] args) {
        try {
            var input = parseInput("../input.txt");
            System.out.println("Part 1: " + part1(input));
            System.out.println("Part 2: " + part2(input));
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
