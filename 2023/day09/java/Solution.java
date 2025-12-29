import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Solution {

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Path.of("../input.txt")).trim();
        List<long[]> histories = parseInput(input);

        System.out.println("Part 1: " + part1(histories));
        System.out.println("Part 2: " + part2(histories));
    }

    private static List<long[]> parseInput(String text) {
        return Arrays.stream(text.split("\n"))
                .map(line -> Arrays.stream(line.split("\\s+"))
                        .mapToLong(Long::parseLong)
                        .toArray())
                .toList();
    }

    private static long[] getDifferences(long[] seq) {
        long[] diffs = new long[seq.length - 1];
        for (int i = 0; i < seq.length - 1; i++) {
            diffs[i] = seq[i + 1] - seq[i];
        }
        return diffs;
    }

    private static boolean allZeros(long[] seq) {
        return Arrays.stream(seq).allMatch(v -> v == 0);
    }

    private static List<long[]> buildDifferenceSequences(long[] seq) {
        List<long[]> sequences = new ArrayList<>();
        sequences.add(seq);

        long[] current = seq;
        while (!allZeros(current)) {
            current = getDifferences(current);
            sequences.add(current);
        }

        return sequences;
    }

    private static long extrapolateNext(long[] seq) {
        List<long[]> sequences = buildDifferenceSequences(seq);

        // Work backwards from bottom, adding last values
        long extrapolated = 0;
        for (int i = sequences.size() - 2; i >= 0; i--) {
            extrapolated += sequences.get(i)[sequences.get(i).length - 1];
        }

        return extrapolated;
    }

    private static long[] reverse(long[] arr) {
        long[] reversed = new long[arr.length];
        for (int i = 0; i < arr.length; i++) {
            reversed[i] = arr[arr.length - 1 - i];
        }
        return reversed;
    }

    private static long extrapolatePrev(long[] seq) {
        // Extrapolating backwards is equivalent to reversing and extrapolating forward
        return extrapolateNext(reverse(seq));
    }

    private static long part1(List<long[]> histories) {
        return histories.stream()
                .mapToLong(Solution::extrapolateNext)
                .sum();
    }

    private static long part2(List<long[]> histories) {
        return histories.stream()
                .mapToLong(Solution::extrapolatePrev)
                .sum();
    }
}
