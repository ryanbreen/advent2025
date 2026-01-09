import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Solution {

    public static void main(String[] args) throws IOException {
        String inputFile = "../input.txt";

        int part1 = 0;
        int part2 = 0;

        try (BufferedReader reader = new BufferedReader(new FileReader(inputFile))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) {
                    continue;
                }

                String[] parts = line.split(",");
                String[] left = parts[0].split("-");
                String[] right = parts[1].split("-");

                int a1 = Integer.parseInt(left[0]);
                int b1 = Integer.parseInt(left[1]);
                int a2 = Integer.parseInt(right[0]);
                int b2 = Integer.parseInt(right[1]);

                // Part 1: Check if one range fully contains the other
                if ((a1 <= a2 && b1 >= b2) || (a2 <= a1 && b2 >= b1)) {
                    part1++;
                }

                // Part 2: Check if ranges overlap at all
                if (a1 <= b2 && a2 <= b1) {
                    part2++;
                }
            }
        }

        System.out.println("Part 1: " + part1);
        System.out.println("Part 2: " + part2);
    }
}
