import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class solution {
    private static List<String> grid;
    private static int rows;
    private static int cols;

    // 8 directions: right, left, down, up, and 4 diagonals
    private static final int[][] DIRECTIONS = {
        {0, 1},   // right
        {0, -1},  // left
        {1, 0},   // down
        {-1, 0},  // up
        {1, 1},   // down-right
        {1, -1},  // down-left
        {-1, 1},  // up-right
        {-1, -1}  // up-left
    };

    public static void main(String[] args) throws IOException {
        // Read input from ../input.txt
        String inputText = Files.readString(Paths.get("../input.txt")).strip();
        grid = List.of(inputText.split("\n"));
        rows = grid.size();
        cols = grid.get(0).length();

        System.out.println("Part 1: " + part1());
        System.out.println("Part 2: " + part2());
    }

    private static int part1() {
        String target = "XMAS";
        int count = 0;

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                // Try each direction from this position
                for (int[] dir : DIRECTIONS) {
                    int dr = dir[0];
                    int dc = dir[1];

                    // Check if XMAS fits in this direction
                    boolean found = true;
                    for (int i = 0; i < target.length(); i++) {
                        int nr = r + dr * i;
                        int nc = c + dc * i;

                        if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
                            found = false;
                            break;
                        }
                        if (grid.get(nr).charAt(nc) != target.charAt(i)) {
                            found = false;
                            break;
                        }
                    }
                    if (found) {
                        count++;
                    }
                }
            }
        }

        return count;
    }

    private static int part2() {
        // Find X-MAS patterns: two MAS strings forming an X with A in the center
        // Each diagonal can be MAS or SAM
        int count = 0;

        // Check each possible center point (A must be in the middle)
        for (int r = 1; r < rows - 1; r++) {
            for (int c = 1; c < cols - 1; c++) {
                if (grid.get(r).charAt(c) != 'A') {
                    continue;
                }

                // Get the four corners
                char topLeft = grid.get(r - 1).charAt(c - 1);
                char topRight = grid.get(r - 1).charAt(c + 1);
                char bottomLeft = grid.get(r + 1).charAt(c - 1);
                char bottomRight = grid.get(r + 1).charAt(c + 1);

                // Check diagonal 1 (top-left to bottom-right): MAS or SAM
                boolean diag1Ok = (topLeft == 'M' && bottomRight == 'S') ||
                                  (topLeft == 'S' && bottomRight == 'M');

                // Check diagonal 2 (top-right to bottom-left): MAS or SAM
                boolean diag2Ok = (topRight == 'M' && bottomLeft == 'S') ||
                                  (topRight == 'S' && bottomLeft == 'M');

                if (diag1Ok && diag2Ok) {
                    count++;
                }
            }
        }

        return count;
    }
}
