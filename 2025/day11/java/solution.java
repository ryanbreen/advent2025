import java.io.*;
import java.util.*;

public class solution {
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
        Map<String, List<String>> g = new HashMap<>();
        BufferedReader reader = new BufferedReader(new FileReader(filename));
        String line;

        while ((line = reader.readLine()) != null) {
            line = line.trim();
            if (line.isEmpty()) continue;

            String[] parts = line.split(": ");
            String node = parts[0];
            List<String> neighbors = new ArrayList<>();

            if (parts.length > 1) {
                String[] neighborArray = parts[1].split(" ");
                for (String neighbor : neighborArray) {
                    neighbors.add(neighbor);
                }
            }

            g.put(node, neighbors);
        }

        reader.close();
        return g;
    }

    private static long part1() {
        Map<String, Long> memo = new HashMap<>();
        return countPaths("you", "out", memo);
    }

    private static long countPaths(String node, String target, Map<String, Long> memo) {
        String key = node + "->" + target;
        if (memo.containsKey(key)) {
            return memo.get(key);
        }

        if (node.equals(target)) {
            return 1;
        }

        if (!graph.containsKey(node)) {
            return 0;
        }

        long total = 0;
        for (String neighbor : graph.get(node)) {
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
