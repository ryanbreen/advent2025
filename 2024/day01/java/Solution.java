import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Solution {
    private static class InputLists {
        List<Integer> left;
        List<Integer> right;

        InputLists(List<Integer> left, List<Integer> right) {
            this.left = left;
            this.right = right;
        }
    }

    private static InputLists readInput() throws IOException {
        List<String> lines = Files.readAllLines(Paths.get("../input.txt"));
        List<Integer> leftList = new ArrayList<>();
        List<Integer> rightList = new ArrayList<>();

        for (String line : lines) {
            String[] parts = line.trim().split("\\s+");
            if (parts.length == 2) {
                leftList.add(Integer.parseInt(parts[0]));
                rightList.add(Integer.parseInt(parts[1]));
            }
        }

        return new InputLists(leftList, rightList);
    }

    private static int part1(List<Integer> leftList, List<Integer> rightList) {
        // Create copies to avoid modifying originals
        List<Integer> left = new ArrayList<>(leftList);
        List<Integer> right = new ArrayList<>(rightList);

        // Sort both lists
        Collections.sort(left);
        Collections.sort(right);

        // Calculate total distance
        int totalDistance = 0;
        for (int i = 0; i < left.size(); i++) {
            totalDistance += Math.abs(left.get(i) - right.get(i));
        }

        return totalDistance;
    }

    private static int part2(List<Integer> leftList, List<Integer> rightList) {
        // Count occurrences in right list
        Map<Integer, Integer> rightCounts = new HashMap<>();
        for (int num : rightList) {
            rightCounts.put(num, rightCounts.getOrDefault(num, 0) + 1);
        }

        // Calculate similarity score
        int similarityScore = 0;
        for (int num : leftList) {
            similarityScore += num * rightCounts.getOrDefault(num, 0);
        }

        return similarityScore;
    }

    public static void main(String[] args) {
        try {
            InputLists input = readInput();
            System.out.println("Part 1: " + part1(input.left, input.right));
            System.out.println("Part 2: " + part2(input.left, input.right));
        } catch (IOException e) {
            System.err.println("Error reading input: " + e.getMessage());
            System.exit(1);
        }
    }
}
