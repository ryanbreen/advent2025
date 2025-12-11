import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.stream.*;

public class Solution {
    private static Map<String, List<String>> graph;

    public static void main(String[] args) throws IOException {
        String inputFile = "../input.txt";
        if (args.length > 0) {
            inputFile = args[0];
        }

        graph = parseInput(inputFile);

        System.out.println("Part 1: " + part1());
        System.out.println("Part 2: " + part2());
    }

    private static Map<String, List<String>> parseInput(String filename) throws IOException {
        try (Stream<String> lines = Files.lines(Paths.get(filename))) {
            return lines
                .map(String::trim)
                .filter(line -> !line.isEmpty())
                .collect(Collectors.toMap(
                    line -> line.split(": ")[0],
                    line -> {
                        String[] parts = line.split(": ");
                        if (parts.length > 1) {
                            return Arrays.asList(parts[1].split(" "));
                        }
                        return new ArrayList<String>();
                    }
                ));
        }
    }

    private static long part1() {
        Map<String, Long> memo = new HashMap<>();
        return countPaths("you", "out", memo);
    }

    private static long countPaths(String node, String target, Map<String, Long> memo) {
        // Use Objects.hash for efficient memoization key
        String key = node + ":" + target;

        Long cached = memo.get(key);
        if (cached != null) {
            return cached;
        }

        if (node.equals(target)) {
            return 1;
        }

        List<String> neighbors = graph.get(node);
        if (neighbors == null) {
            return 0;
        }

        long total = 0;
        for (String neighbor : neighbors) {
            total += countPaths(neighbor, target, memo);
        }

        memo.put(key, total);
        return total;
    }

    private static long part2() {
        // Create three separate memoization caches for counting paths to different targets
        Map<String, Long> pathsToOut = new HashMap<>();
        Map<String, Long> pathsToDac = new HashMap<>();
        Map<String, Long> pathsToFft = new HashMap<>();

        // Count paths from svr to each intermediate node
        long svrToDac = countPaths("svr", "dac", pathsToDac);
        long svrToFft = countPaths("svr", "fft", pathsToFft);

        // Count paths between intermediate nodes
        long dacToFft = countPaths("dac", "fft", pathsToFft);
        long fftToDac = countPaths("fft", "dac", pathsToDac);

        // Count paths from intermediate nodes to out
        long dacToOut = countPaths("dac", "out", pathsToOut);
        long fftToOut = countPaths("fft", "out", pathsToOut);

        // Paths that visit dac before fft: svr -> dac -> fft -> out
        long dacBeforeFft = svrToDac * dacToFft * fftToOut;

        // Paths that visit fft before dac: svr -> fft -> dac -> out
        long fftBeforeDac = svrToFft * fftToDac * dacToOut;

        return dacBeforeFft + fftBeforeDac;
    }
}
