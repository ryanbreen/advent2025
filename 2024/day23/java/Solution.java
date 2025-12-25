import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.stream.*;

public class Solution {

    /**
     * Parse the network connections into an adjacency map.
     */
    static Map<String, Set<String>> parseInput(String filename) throws IOException {
        Map<String, Set<String>> graph = new HashMap<>();
        List<String> lines = Files.readAllLines(Paths.get(filename));

        for (String line : lines) {
            String[] parts = line.trim().split("-");
            String a = parts[0];
            String b = parts[1];

            graph.computeIfAbsent(a, k -> new HashSet<>()).add(b);
            graph.computeIfAbsent(b, k -> new HashSet<>()).add(a);
        }

        return graph;
    }

    /**
     * Find all triangles (sets of 3 interconnected nodes).
     */
    static Set<List<String>> findTriangles(Map<String, Set<String>> graph) {
        Set<List<String>> triangles = new HashSet<>();

        for (String a : graph.keySet()) {
            for (String b : graph.get(a)) {
                if (a.compareTo(b) < 0) {  // Only process each edge once
                    // Find common neighbors
                    Set<String> commonNeighbors = new HashSet<>(graph.get(a));
                    commonNeighbors.retainAll(graph.get(b));

                    for (String c : commonNeighbors) {
                        // Create sorted triangle to avoid duplicates
                        List<String> tri = Arrays.asList(a, b, c);
                        Collections.sort(tri);
                        triangles.add(tri);
                    }
                }
            }
        }

        return triangles;
    }

    /**
     * Part 1: Count triangles containing at least one node starting with 't'.
     */
    static int part1(Map<String, Set<String>> graph) {
        Set<List<String>> triangles = findTriangles(graph);
        int count = 0;

        for (List<String> tri : triangles) {
            if (tri.stream().anyMatch(node -> node.startsWith("t"))) {
                count++;
            }
        }

        return count;
    }

    /**
     * Bron-Kerbosch algorithm to find all maximal cliques.
     */
    static void bronKerbosch(
            Map<String, Set<String>> graph,
            Set<String> r,
            Set<String> p,
            Set<String> x,
            List<Set<String>> cliques) {

        if (p.isEmpty() && x.isEmpty()) {
            cliques.add(new HashSet<>(r));
            return;
        }

        // Find pivot - node in P ∪ X with most neighbors in P
        Set<String> unionPX = new HashSet<>(p);
        unionPX.addAll(x);

        String pivot = null;
        int maxNeighbors = -1;
        for (String v : unionPX) {
            Set<String> neighbors = new HashSet<>(graph.get(v));
            neighbors.retainAll(p);
            int count = neighbors.size();
            if (count > maxNeighbors) {
                maxNeighbors = count;
                pivot = v;
            }
        }

        // P \ N(pivot)
        Set<String> candidates = new HashSet<>(p);
        if (pivot != null) {
            candidates.removeAll(graph.get(pivot));
        }

        for (String v : candidates) {
            Set<String> newR = new HashSet<>(r);
            newR.add(v);

            Set<String> newP = new HashSet<>(p);
            newP.retainAll(graph.get(v));

            Set<String> newX = new HashSet<>(x);
            newX.retainAll(graph.get(v));

            bronKerbosch(graph, newR, newP, newX, cliques);

            p.remove(v);
            x.add(v);
        }
    }

    /**
     * Part 2: Find the largest clique (fully connected subgraph).
     */
    static String part2(Map<String, Set<String>> graph) {
        List<Set<String>> cliques = new ArrayList<>();
        Set<String> allNodes = new HashSet<>(graph.keySet());

        bronKerbosch(graph, new HashSet<>(), allNodes, new HashSet<>(), cliques);

        // Find the largest clique
        Set<String> largest = cliques.stream()
            .max(Comparator.comparingInt(Set::size))
            .orElse(new HashSet<>());

        // Return sorted, comma-joined password
        return largest.stream()
            .sorted()
            .collect(Collectors.joining(","));
    }

    public static void main(String[] args) throws IOException {
        Map<String, Set<String>> graph = parseInput("../input.txt");

        System.out.println("Part 1: " + part1(graph));
        System.out.println("Part 2: " + part2(graph));
    }
}
