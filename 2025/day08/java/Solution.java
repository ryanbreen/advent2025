import java.io.*;
import java.util.*;

public class Solution {

    static class Point {
        int x, y, z;

        Point(int x, int y, int z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }
    }

    static class UnionFind {
        int[] parent;
        int[] rank;
        int[] size;

        UnionFind(int n) {
            parent = new int[n];
            rank = new int[n];
            size = new int[n];

            for (int i = 0; i < n; i++) {
                parent[i] = i;
                rank[i] = 0;
                size[i] = 1;
            }
        }

        int find(int x) {
            if (parent[x] != x) {
                parent[x] = find(parent[x]); // Path compression
            }
            return parent[x];
        }

        boolean union(int x, int y) {
            int px = find(x);
            int py = find(y);

            if (px == py) {
                return false; // Already in same set
            }

            // Union by rank
            if (rank[px] < rank[py]) {
                int temp = px;
                px = py;
                py = temp;
            }

            parent[py] = px;
            size[px] += size[py];

            if (rank[px] == rank[py]) {
                rank[px]++;
            }

            return true;
        }

        List<Integer> getComponentSizes() {
            var sizes = new ArrayList<Integer>();
            for (int i = 0; i < parent.length; i++) {
                if (parent[i] == i) { // Root of a component
                    sizes.add(size[i]);
                }
            }
            return sizes;
        }
    }

    static class Edge implements Comparable<Edge> {
        long distSq;
        int i, j;

        Edge(long distSq, int i, int j) {
            this.distSq = distSq;
            this.i = i;
            this.j = j;
        }

        @Override
        public int compareTo(Edge other) {
            return Long.compare(this.distSq, other.distSq);
        }
    }

    static List<Point> parseInput(String filename) throws IOException {
        var points = new ArrayList<Point>();

        try (var br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (!line.isEmpty()) {
                    var parts = line.split(",");
                    int x = Integer.parseInt(parts[0]);
                    int y = Integer.parseInt(parts[1]);
                    int z = Integer.parseInt(parts[2]);
                    points.add(new Point(x, y, z));
                }
            }
        }

        return points;
    }

    static long euclideanDistanceSquared(Point p1, Point p2) {
        long dx = (long)p1.x - p2.x;
        long dy = (long)p1.y - p2.y;
        long dz = (long)p1.z - p2.z;
        return dx * dx + dy * dy + dz * dz;
    }

    static long part1(List<Point> points, int numConnections) {
        int n = points.size();

        // Generate all pairs with distances
        var edges = new ArrayList<Edge>();
        for (int i = 0; i < n; i++) {
            for (int j = i + 1; j < n; j++) {
                long distSq = euclideanDistanceSquared(points.get(i), points.get(j));
                edges.add(new Edge(distSq, i, j));
            }
        }

        // Sort by distance
        Collections.sort(edges);

        // Union-Find to connect closest pairs
        var uf = new UnionFind(n);
        int connections = 0;

        for (var edge : edges) {
            uf.union(edge.i, edge.j); // Always attempt union
            connections++;
            if (connections == numConnections) {
                break;
            }
        }

        // Get component sizes and find the 3 largest
        var sizes = uf.getComponentSizes();
        Collections.sort(sizes, Collections.reverseOrder());

        // Multiply the 3 largest
        return (long)sizes.get(0) * sizes.get(1) * sizes.get(2);
    }

    static long part2(List<Point> points) {
        int n = points.size();

        // Generate all pairs with distances
        var edges = new ArrayList<Edge>();
        for (int i = 0; i < n; i++) {
            for (int j = i + 1; j < n; j++) {
                long distSq = euclideanDistanceSquared(points.get(i), points.get(j));
                edges.add(new Edge(distSq, i, j));
            }
        }

        // Sort by distance
        Collections.sort(edges);

        // Union-Find to connect until all in one circuit
        var uf = new UnionFind(n);
        int numComponents = n;

        for (var edge : edges) {
            if (uf.union(edge.i, edge.j)) { // Actually merged two components
                numComponents--;
                if (numComponents == 1) {
                    // This was the last connection - all in one circuit now
                    var p1 = points.get(edge.i);
                    var p2 = points.get(edge.j);
                    return (long)p1.x * p2.x; // Product of X coordinates
                }
            }
        }

        return 0;
    }

    public static void main(String[] args) throws IOException {
        var inputFile = args.length > 0 ? args[0] : "../input.txt";
        var points = parseInput(inputFile);

        System.out.println("Part 1: " + part1(points, 1000));
        System.out.println("Part 2: " + part2(points));
    }
}
