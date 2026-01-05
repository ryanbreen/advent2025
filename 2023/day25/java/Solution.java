import java.io.*;
import java.util.*;

/**
 * Advent of Code 2023 - Day 25: Snowverload
 * Find the minimum cut of 3 edges that divides the graph into two components.
 *
 * Uses edge betweenness centrality: edges that form the cut between two large
 * components will have high betweenness (many shortest paths pass through them).
 */
public class Solution {

    static class Edge {
        String a, b;

        Edge(String node1, String node2) {
            // Keep edges sorted for consistent comparison
            if (node1.compareTo(node2) < 0) {
                a = node1;
                b = node2;
            } else {
                a = node1;
                b = node2;
            }
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof Edge)) return false;
            Edge edge = (Edge) o;
            return a.equals(edge.a) && b.equals(edge.b);
        }

        @Override
        public int hashCode() {
            return Objects.hash(a, b);
        }

        @Override
        public String toString() {
            return a + "-" + b;
        }
    }

    /**
     * Parse the input file into an adjacency list representation.
     */
    static Map<String, Set<String>> parseInput(String filename) throws IOException {
        Map<String, Set<String>> graph = new HashMap<>();

        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;

                String[] parts = line.split(": ");
                String left = parts[0];
                String[] neighbors = parts[1].split(" ");

                graph.putIfAbsent(left, new HashSet<>());
                for (String neighbor : neighbors) {
                    graph.get(left).add(neighbor);
                    graph.putIfAbsent(neighbor, new HashSet<>());
                    graph.get(neighbor).add(left);
                }
            }
        }

        return graph;
    }

    /**
     * BFS to find component size, ignoring excluded edges.
     */
    static int bfsComponentSize(Map<String, Set<String>> graph, String start, Set<Edge> excludedEdges) {
        Set<String> visited = new HashSet<>();
        Queue<String> queue = new LinkedList<>();

        visited.add(start);
        queue.offer(start);

        while (!queue.isEmpty()) {
            String node = queue.poll();
            Set<String> neighbors = graph.get(node);
            if (neighbors == null) continue;

            for (String neighbor : neighbors) {
                Edge edge = new Edge(node, neighbor);
                if (!visited.contains(neighbor) && !excludedEdges.contains(edge)) {
                    visited.add(neighbor);
                    queue.offer(neighbor);
                }
            }
        }

        return visited.size();
    }

    /**
     * Compute approximate edge betweenness centrality.
     * Higher values indicate edges that many shortest paths pass through.
     */
    static Map<Edge, Double> computeEdgeBetweenness(Map<String, Set<String>> graph, int sampleNodes) {
        Map<Edge, Double> edgeCount = new HashMap<>();
        List<String> nodes = new ArrayList<>(graph.keySet());

        // Sample nodes for efficiency
        if (sampleNodes > 0 && nodes.size() > sampleNodes) {
            Random rand = new Random(42);
            Collections.shuffle(nodes, rand);
            nodes = nodes.subList(0, sampleNodes);
        }

        for (String source : nodes) {
            // BFS to find shortest paths
            Map<String, Integer> dist = new HashMap<>();
            Map<String, List<String>> pred = new HashMap<>();
            Queue<String> queue = new LinkedList<>();

            dist.put(source, 0);
            queue.offer(source);

            while (!queue.isEmpty()) {
                String node = queue.poll();
                int nodeDist = dist.get(node);

                Set<String> neighbors = graph.get(node);
                if (neighbors == null) continue;

                for (String neighbor : neighbors) {
                    if (!dist.containsKey(neighbor)) {
                        dist.put(neighbor, nodeDist + 1);
                        pred.putIfAbsent(neighbor, new ArrayList<>());
                        pred.get(neighbor).add(node);
                        queue.offer(neighbor);
                    } else if (dist.get(neighbor) == nodeDist + 1) {
                        pred.putIfAbsent(neighbor, new ArrayList<>());
                        pred.get(neighbor).add(node);
                    }
                }
            }

            // Backtrack to count edge usage
            // Number of shortest paths to each node
            Map<String, Double> numPaths = new HashMap<>();
            numPaths.put(source, 1.0);

            List<String> sortedNodes = new ArrayList<>(dist.keySet());
            sortedNodes.sort(Comparator.comparingInt(dist::get));

            for (String node : sortedNodes) {
                List<String> predecessors = pred.get(node);
                if (predecessors != null) {
                    for (String p : predecessors) {
                        double prevPaths = numPaths.getOrDefault(p, 0.0);
                        numPaths.put(node, numPaths.getOrDefault(node, 0.0) + prevPaths);
                    }
                }
            }

            // Accumulate edge betweenness (reverse BFS order)
            Map<String, Double> dependency = new HashMap<>();
            sortedNodes.sort((a, b) -> Integer.compare(dist.get(b), dist.get(a)));

            for (String node : sortedNodes) {
                List<String> predecessors = pred.get(node);
                if (predecessors != null) {
                    for (String p : predecessors) {
                        Edge edge = new Edge(p, node);
                        double frac = numPaths.get(p) / numPaths.get(node);
                        double contrib = frac * (1.0 + dependency.getOrDefault(node, 0.0));
                        edgeCount.put(edge, edgeCount.getOrDefault(edge, 0.0) + contrib);
                        dependency.put(p, dependency.getOrDefault(p, 0.0) + contrib);
                    }
                }
            }
        }

        return edgeCount;
    }

    /**
     * Find the 3 edges to cut using edge betweenness.
     */
    static Integer findCutEdges(Map<String, Set<String>> graph) {
        // Compute edge betweenness with sampling for speed
        Map<Edge, Double> edgeBetweenness = computeEdgeBetweenness(graph, 100);

        // Sort edges by betweenness (highest first)
        List<Edge> sortedEdges = new ArrayList<>(edgeBetweenness.keySet());
        sortedEdges.sort((e1, e2) -> Double.compare(edgeBetweenness.get(e2), edgeBetweenness.get(e1)));

        int totalNodes = graph.size();

        // Try removing top candidate edges
        // We need to find 3 edges that disconnect the graph
        List<Edge> topEdges = sortedEdges.subList(0, Math.min(20, sortedEdges.size()));

        for (int i = 0; i < topEdges.size(); i++) {
            for (int j = i + 1; j < topEdges.size(); j++) {
                for (int k = j + 1; k < topEdges.size(); k++) {
                    Set<Edge> excluded = new HashSet<>();
                    excluded.add(topEdges.get(i));
                    excluded.add(topEdges.get(j));
                    excluded.add(topEdges.get(k));

                    String start = graph.keySet().iterator().next();
                    int size1 = bfsComponentSize(graph, start, excluded);

                    if (size1 < totalNodes) {
                        // Graph is disconnected!
                        int size2 = totalNodes - size1;
                        return size1 * size2;
                    }
                }
            }
        }

        return null;
    }

    /**
     * Solve Part 1: Find the 3-edge cut and return product of component sizes.
     */
    static int part1(String filename) throws IOException {
        Map<String, Set<String>> graph = parseInput(filename);
        Integer result = findCutEdges(graph);
        return result != null ? result : 0;
    }

    /**
     * Part 2: Day 25 Part 2 is traditionally unlocked by having 49 stars.
     * There's no computation needed - just push the button!
     */
    static String part2(String filename) {
        return "Push the big red button!";
    }

    public static void main(String[] args) throws IOException {
        String inputFile = "../input.txt";

        System.out.println("Part 1: " + part1(inputFile));
        System.out.println("Part 2: " + part2(inputFile));
    }
}
