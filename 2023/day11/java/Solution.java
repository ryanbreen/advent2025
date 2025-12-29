import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.stream.*;

public class Solution {

    record Galaxy(int row, int col) {}

    public static void main(String[] args) throws IOException {
        String inputFile = args.length > 0 ? args[0] : "../input.txt";
        List<String> lines = Files.readAllLines(Paths.get(inputFile)).stream()
                .filter(line -> !line.isEmpty())
                .collect(Collectors.toCollection(ArrayList::new));

        System.out.println("Part 1: " + solve(lines, 2));
        System.out.println("Part 2: " + solve(lines, 1_000_000));
    }

    private static long solve(List<String> lines, int expansionFactor) {
        List<Galaxy> galaxies = parseGalaxies(lines);
        Set<Integer> emptyRows = findEmptyRows(lines);
        Set<Integer> emptyCols = findEmptyCols(lines);

        return calculateDistances(galaxies, emptyRows, emptyCols, expansionFactor);
    }

    private static List<Galaxy> parseGalaxies(List<String> lines) {
        return IntStream.range(0, lines.size())
                .boxed()
                .flatMap(row -> {
                    String line = lines.get(row);
                    return IntStream.range(0, line.length())
                            .filter(col -> line.charAt(col) == '#')
                            .mapToObj(col -> new Galaxy(row, col));
                })
                .collect(Collectors.toList());
    }

    private static Set<Integer> findEmptyRows(List<String> lines) {
        return IntStream.range(0, lines.size())
                .filter(row -> !lines.get(row).contains("#"))
                .boxed()
                .collect(Collectors.toSet());
    }

    private static Set<Integer> findEmptyCols(List<String> lines) {
        if (lines.isEmpty()) return Collections.emptySet();

        int numCols = lines.get(0).length();
        return IntStream.range(0, numCols)
                .filter(col -> lines.stream().noneMatch(line -> col < line.length() && line.charAt(col) == '#'))
                .boxed()
                .collect(Collectors.toSet());
    }

    private static long calculateDistances(List<Galaxy> galaxies, Set<Integer> emptyRows,
                                           Set<Integer> emptyCols, int expansionFactor) {
        long total = 0;

        for (int i = 0; i < galaxies.size(); i++) {
            for (int j = i + 1; j < galaxies.size(); j++) {
                Galaxy g1 = galaxies.get(i);
                Galaxy g2 = galaxies.get(j);

                int minRow = Math.min(g1.row(), g2.row());
                int maxRow = Math.max(g1.row(), g2.row());
                long rowDist = maxRow - minRow;
                for (int row = minRow; row < maxRow; row++) {
                    if (emptyRows.contains(row)) {
                        rowDist += expansionFactor - 1;
                    }
                }

                int minCol = Math.min(g1.col(), g2.col());
                int maxCol = Math.max(g1.col(), g2.col());
                long colDist = maxCol - minCol;
                for (int col = minCol; col < maxCol; col++) {
                    if (emptyCols.contains(col)) {
                        colDist += expansionFactor - 1;
                    }
                }

                total += rowDist + colDist;
            }
        }

        return total;
    }
}
