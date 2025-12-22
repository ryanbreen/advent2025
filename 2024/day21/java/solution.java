import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/**
 * Day 21: Keypad Conundrum - Robot chain control with shortest path optimization
 */
public class solution {

    static class Position {
        final int row;
        final int col;

        Position(int row, int col) {
            this.row = row;
            this.col = col;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof Position)) return false;
            Position p = (Position) o;
            return row == p.row && col == p.col;
        }

        @Override
        public int hashCode() {
            return Objects.hash(row, col);
        }
    }

    // Keypad layouts
    static final Map<Character, Position> NUMERIC = new HashMap<>();
    static final Position NUMERIC_GAP = new Position(3, 0);

    static final Map<Character, Position> DIRECTIONAL = new HashMap<>();
    static final Position DIRECTIONAL_GAP = new Position(0, 0);

    static {
        // Numeric keypad
        NUMERIC.put('7', new Position(0, 0));
        NUMERIC.put('8', new Position(0, 1));
        NUMERIC.put('9', new Position(0, 2));
        NUMERIC.put('4', new Position(1, 0));
        NUMERIC.put('5', new Position(1, 1));
        NUMERIC.put('6', new Position(1, 2));
        NUMERIC.put('1', new Position(2, 0));
        NUMERIC.put('2', new Position(2, 1));
        NUMERIC.put('3', new Position(2, 2));
        NUMERIC.put('0', new Position(3, 1));
        NUMERIC.put('A', new Position(3, 2));

        // Directional keypad
        DIRECTIONAL.put('^', new Position(0, 1));
        DIRECTIONAL.put('A', new Position(0, 2));
        DIRECTIONAL.put('<', new Position(1, 0));
        DIRECTIONAL.put('v', new Position(1, 1));
        DIRECTIONAL.put('>', new Position(1, 2));
    }

    /**
     * Find all shortest paths from start to end, avoiding gap.
     */
    static List<String> shortestPaths(Map<Character, Position> keypad, Position gap,
                                      char start, char end) {
        Position startPos = keypad.get(start);
        Position endPos = keypad.get(end);

        List<String> paths = new ArrayList<>();
        dfs(startPos.row, startPos.col, endPos.row, endPos.col, gap, "", paths);

        if (paths.isEmpty()) {
            paths.add("");  // Empty path if start == end
        }

        return paths;
    }

    static void dfs(int r, int c, int er, int ec, Position gap, String path, List<String> paths) {
        if (r == gap.row && c == gap.col) {
            return;  // Hit the gap, invalid path
        }
        if (r == er && c == ec) {
            paths.add(path);
            return;
        }

        // Move vertically toward target
        if (r < er) {
            dfs(r + 1, c, er, ec, gap, path + 'v', paths);
        } else if (r > er) {
            dfs(r - 1, c, er, ec, gap, path + '^', paths);
        }

        // Move horizontally toward target
        if (c < ec) {
            dfs(r, c + 1, er, ec, gap, path + '>', paths);
        } else if (c > ec) {
            dfs(r, c - 1, er, ec, gap, path + '<', paths);
        }
    }

    static class MemoKey {
        final char from;
        final char to;
        final int depth;
        final boolean isNumeric;

        MemoKey(char from, char to, int depth, boolean isNumeric) {
            this.from = from;
            this.to = to;
            this.depth = depth;
            this.isNumeric = isNumeric;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof MemoKey)) return false;
            MemoKey k = (MemoKey) o;
            return from == k.from && to == k.to && depth == k.depth && isNumeric == k.isNumeric;
        }

        @Override
        public int hashCode() {
            return Objects.hash(from, to, depth, isNumeric);
        }
    }

    static final Map<MemoKey, Long> memo = new HashMap<>();

    /**
     * Minimum presses needed to move from fromChar to toChar and press, at given depth.
     */
    static long minPressesForMove(char fromChar, char toChar, int depth, boolean isNumeric) {
        MemoKey key = new MemoKey(fromChar, toChar, depth, isNumeric);
        if (memo.containsKey(key)) {
            return memo.get(key);
        }

        Map<Character, Position> keypad = isNumeric ? NUMERIC : DIRECTIONAL;
        Position gap = isNumeric ? NUMERIC_GAP : DIRECTIONAL_GAP;

        List<String> paths = shortestPaths(keypad, gap, fromChar, toChar);

        long result;
        if (depth == 0) {
            // At human level, just return path length + 1 for 'A' press
            result = paths.stream().mapToInt(String::length).min().orElse(0) + 1;
        } else {
            long best = Long.MAX_VALUE;
            for (String path : paths) {
                // Need to type path + 'A' on the directional keypad above
                String sequence = path + 'A';
                long cost = 0;
                char current = 'A';
                for (char ch : sequence.toCharArray()) {
                    cost += minPressesForMove(current, ch, depth - 1, false);
                    current = ch;
                }
                best = Math.min(best, cost);
            }
            result = best;
        }

        memo.put(key, result);
        return result;
    }

    /**
     * Compute minimum presses to type code on numeric keypad with given robot depth.
     */
    static long solveCode(String code, int depth) {
        long total = 0;
        char current = 'A';
        for (char ch : code.toCharArray()) {
            total += minPressesForMove(current, ch, depth, true);
            current = ch;
        }
        return total;
    }

    /**
     * Compute complexity: length * numeric part of code.
     */
    static long complexity(String code, long length) {
        // Remove trailing 'A' and parse as integer
        String numericPart = code.replaceAll("A$", "");
        int number = Integer.parseInt(numericPart);
        return length * number;
    }

    static long part1(List<String> codes) {
        long total = 0;
        for (String code : codes) {
            long length = solveCode(code, 2);
            total += complexity(code, length);
        }
        return total;
    }

    static long part2(List<String> codes) {
        long total = 0;
        for (String code : codes) {
            long length = solveCode(code, 25);
            total += complexity(code, length);
        }
        return total;
    }

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Path.of("../input.txt")).strip();
        List<String> codes = Arrays.stream(input.split("\n"))
                                   .map(String::strip)
                                   .filter(s -> !s.isEmpty())
                                   .toList();

        System.out.println("Part 1: " + part1(codes));
        System.out.println("Part 2: " + part2(codes));
    }
}
