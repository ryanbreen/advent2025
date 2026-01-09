import java.io.*;
import java.util.*;
import java.util.regex.*;

public class Solution {
    private Map<String, Integer> flowRates = new HashMap<>();
    private Map<String, List<String>> tunnels = new HashMap<>();
    private Map<String, Map<String, Integer>> distances = new HashMap<>();
    private List<String> valuableValves = new ArrayList<>();
    private Map<String, Integer> valveIndex = new HashMap<>();

    public static void main(String[] args) throws IOException {
        Solution sol = new Solution();
        sol.run();
    }

    private void run() throws IOException {
        // Get the directory where the .class file resides
        String classPath = Solution.class.getProtectionDomain()
                .getCodeSource().getLocation().getPath();
        File classDir = new File(classPath);
        if (classDir.isFile()) {
            classDir = classDir.getParentFile();
        }
        String inputPath = new File(classDir, "../input.txt").getCanonicalPath();

        String text = new String(java.nio.file.Files.readAllBytes(
                java.nio.file.Paths.get(inputPath)));

        parseInput(text);
        computeDistances();

        System.out.println("Part 1: " + part1());
        System.out.println("Part 2: " + part2());
    }

    private void parseInput(String text) {
        Pattern pattern = Pattern.compile(
                "Valve (\\w+) has flow rate=(\\d+); tunnels? leads? to valves? (.+)");

        for (String line : text.strip().split("\n")) {
            Matcher m = pattern.matcher(line);
            if (m.matches()) {
                String name = m.group(1);
                int rate = Integer.parseInt(m.group(2));
                String[] neighbors = m.group(3).split(", ");

                flowRates.put(name, rate);
                tunnels.put(name, Arrays.asList(neighbors));
            }
        }
    }

    private void computeDistances() {
        // Find all relevant valves (flow > 0) plus AA
        List<String> relevant = new ArrayList<>();
        relevant.add("AA");
        for (String v : flowRates.keySet()) {
            if (flowRates.get(v) > 0) {
                relevant.add(v);
                valuableValves.add(v);
            }
        }

        // Assign indices to valuable valves for bitmask operations
        for (int i = 0; i < valuableValves.size(); i++) {
            valveIndex.put(valuableValves.get(i), i);
        }

        // BFS from each relevant valve
        for (String start : relevant) {
            distances.put(start, new HashMap<>());
            Queue<String> queue = new LinkedList<>();
            Map<String, Integer> dist = new HashMap<>();
            queue.add(start);
            dist.put(start, 0);

            while (!queue.isEmpty()) {
                String curr = queue.poll();
                int d = dist.get(curr);

                if (relevant.contains(curr) && !curr.equals(start)) {
                    distances.get(start).put(curr, d);
                }

                for (String neighbor : tunnels.get(curr)) {
                    if (!dist.containsKey(neighbor)) {
                        dist.put(neighbor, d + 1);
                        queue.add(neighbor);
                    }
                }
            }
        }
    }

    private int part1() {
        int n = valuableValves.size();
        int fullMask = (1 << n) - 1;
        Map<Long, Integer> memo = new HashMap<>();
        return dfs("AA", 30, 0, fullMask, memo);
    }

    private int dfs(String pos, int timeLeft, int opened, int allowed,
                    Map<Long, Integer> memo) {
        if (timeLeft <= 0) return 0;

        // Create unique key for memoization
        int posIdx = pos.equals("AA") ? valuableValves.size() : valveIndex.get(pos);
        long key = ((long) posIdx << 40) | ((long) timeLeft << 32) |
                   ((long) opened << 16) | allowed;

        if (memo.containsKey(key)) {
            return memo.get(key);
        }

        int best = 0;
        for (int i = 0; i < valuableValves.size(); i++) {
            // Check if valve is allowed and not yet opened
            if ((allowed & (1 << i)) != 0 && (opened & (1 << i)) == 0) {
                String nextValve = valuableValves.get(i);
                int timeCost = distances.get(pos).get(nextValve) + 1;

                if (timeCost < timeLeft) {
                    int newTime = timeLeft - timeCost;
                    int pressure = flowRates.get(nextValve) * newTime;
                    int newOpened = opened | (1 << i);
                    best = Math.max(best, pressure + dfs(nextValve, newTime,
                            newOpened, allowed, memo));
                }
            }
        }

        memo.put(key, best);
        return best;
    }

    private int part2() {
        int n = valuableValves.size();
        int fullMask = (1 << n) - 1;

        // Compute max pressure for each subset
        int[] maxScores = new int[1 << n];

        for (int mask = 0; mask < (1 << n); mask++) {
            Map<Long, Integer> memo = new HashMap<>();
            maxScores[mask] = dfsSubset("AA", 26, 0, mask, memo);
        }

        // Find best partition
        int best = 0;
        for (int mask = 0; mask < (1 << n); mask++) {
            int complement = fullMask ^ mask;
            if (mask <= complement) {
                best = Math.max(best, maxScores[mask] + maxScores[complement]);
            }
        }

        return best;
    }

    private int dfsSubset(String pos, int timeLeft, int opened, int allowed,
                          Map<Long, Integer> memo) {
        if (timeLeft <= 0) return 0;

        int posIdx = pos.equals("AA") ? valuableValves.size() : valveIndex.get(pos);
        long key = ((long) posIdx << 40) | ((long) timeLeft << 32) |
                   ((long) opened << 16) | allowed;

        if (memo.containsKey(key)) {
            return memo.get(key);
        }

        int best = 0;
        for (int i = 0; i < valuableValves.size(); i++) {
            if ((allowed & (1 << i)) != 0 && (opened & (1 << i)) == 0) {
                String nextValve = valuableValves.get(i);
                int timeCost = distances.get(pos).get(nextValve) + 1;

                if (timeCost < timeLeft) {
                    int newTime = timeLeft - timeCost;
                    int pressure = flowRates.get(nextValve) * newTime;
                    int newOpened = opened | (1 << i);
                    best = Math.max(best, pressure + dfsSubset(nextValve, newTime,
                            newOpened, allowed, memo));
                }
            }
        }

        memo.put(key, best);
        return best;
    }
}
