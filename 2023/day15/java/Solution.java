import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Solution {

    record Lens(String label, int focalLength) {}

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Path.of("../input.txt"))
                .strip()
                .replace("\n", "");
        String[] steps = input.split(",");

        System.out.println("Part 1: " + part1(steps));
        System.out.println("Part 2: " + part2(steps));
    }

    static int hash(String s) {
        int current = 0;
        for (char c : s.toCharArray()) {
            current = ((current + c) * 17) % 256;
        }
        return current;
    }

    static int part1(String[] steps) {
        return Arrays.stream(steps)
                .mapToInt(Solution::hash)
                .sum();
    }

    static int part2(String[] steps) {
        @SuppressWarnings("unchecked")
        List<Lens>[] boxes = new ArrayList[256];
        for (int i = 0; i < 256; i++) {
            boxes[i] = new ArrayList<>();
        }

        for (String step : steps) {
            if (step.contains("=")) {
                String[] parts = step.split("=");
                String label = parts[0];
                int focalLength = Integer.parseInt(parts[1]);
                int boxNum = hash(label);

                // Find and replace, or add
                boolean found = false;
                for (int i = 0; i < boxes[boxNum].size(); i++) {
                    if (boxes[boxNum].get(i).label().equals(label)) {
                        boxes[boxNum].set(i, new Lens(label, focalLength));
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    boxes[boxNum].add(new Lens(label, focalLength));
                }
            } else {
                // Remove operation (ends with '-')
                String label = step.substring(0, step.length() - 1);
                int boxNum = hash(label);
                boxes[boxNum].removeIf(lens -> lens.label().equals(label));
            }
        }

        // Calculate focusing power
        int total = 0;
        for (int boxNum = 0; boxNum < 256; boxNum++) {
            for (int slot = 0; slot < boxes[boxNum].size(); slot++) {
                Lens lens = boxes[boxNum].get(slot);
                total += (boxNum + 1) * (slot + 1) * lens.focalLength();
            }
        }
        return total;
    }
}
