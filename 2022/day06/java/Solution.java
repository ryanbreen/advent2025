import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

public class Solution {

    public static int findMarker(String data, int windowSize) {
        for (int i = windowSize; i <= data.length(); i++) {
            String window = data.substring(i - windowSize, i);
            Set<Character> chars = new HashSet<>();
            for (char c : window.toCharArray()) {
                chars.add(c);
            }
            if (chars.size() == windowSize) {
                return i;
            }
        }
        return -1;
    }

    public static int part1(String data) {
        return findMarker(data, 4);
    }

    public static int part2(String data) {
        return findMarker(data, 14);
    }

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of(System.getProperty("user.dir"), "..", "input.txt");
        String data = Files.readString(inputPath).trim();

        System.out.println("Part 1: " + part1(data));
        System.out.println("Part 2: " + part2(data));
    }
}
