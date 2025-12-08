#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <string>
#include <sstream>
#include <cmath>

struct Point {
    int x, y, z;
};

struct Pair {
    long long distSq;
    int i, j;

    bool operator<(const Pair& other) const {
        return distSq < other.distSq;
    }
};

class UnionFind {
private:
    std::vector<int> parent;
    std::vector<int> rank;
    std::vector<int> size;

public:
    UnionFind(int n) : parent(n), rank(n, 0), size(n, 1) {
        for (int i = 0; i < n; i++) {
            parent[i] = i;
        }
    }

    int find(int x) {
        if (parent[x] != x) {
            parent[x] = find(parent[x]);  // Path compression
        }
        return parent[x];
    }

    bool unite(int x, int y) {
        int px = find(x);
        int py = find(y);

        if (px == py) {
            return false;  // Already in same set
        }

        // Union by rank
        if (rank[px] < rank[py]) {
            std::swap(px, py);
        }
        parent[py] = px;
        size[px] += size[py];
        if (rank[px] == rank[py]) {
            rank[px]++;
        }
        return true;
    }

    std::vector<int> getComponentSizes() {
        std::vector<int> sizes;
        for (size_t i = 0; i < parent.size(); i++) {
            if (parent[i] == static_cast<int>(i)) {  // Root of a component
                sizes.push_back(size[i]);
            }
        }
        return sizes;
    }
};

long long euclideanDistanceSquared(const Point& p1, const Point& p2) {
    long long dx = p1.x - p2.x;
    long long dy = p1.y - p2.y;
    long long dz = p1.z - p2.z;
    return dx * dx + dy * dy + dz * dz;
}

std::vector<Point> parseInput(const std::string& filename) {
    std::vector<Point> points;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) continue;

        std::stringstream ss(line);
        Point p;
        char comma;
        ss >> p.x >> comma >> p.y >> comma >> p.z;
        points.push_back(p);
    }

    return points;
}

long long part1(const std::vector<Point>& points, int numConnections = 1000) {
    int n = points.size();

    // Generate all pairs with distances
    std::vector<Pair> pairs;
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            long long distSq = euclideanDistanceSquared(points[i], points[j]);
            pairs.push_back({distSq, i, j});
        }
    }

    // Sort by distance
    std::sort(pairs.begin(), pairs.end());

    // Union-Find to connect closest pairs
    UnionFind uf(n);
    int connections = 0;
    for (const auto& pair : pairs) {
        uf.unite(pair.i, pair.j);
        connections++;
        if (connections == numConnections) {
            break;
        }
    }

    // Get component sizes and find the 3 largest
    std::vector<int> sizes = uf.getComponentSizes();
    std::sort(sizes.begin(), sizes.end(), std::greater<int>());

    // Multiply the 3 largest
    return static_cast<long long>(sizes[0]) * sizes[1] * sizes[2];
}

long long part2(const std::vector<Point>& points) {
    int n = points.size();

    // Generate all pairs with distances
    std::vector<Pair> pairs;
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            long long distSq = euclideanDistanceSquared(points[i], points[j]);
            pairs.push_back({distSq, i, j});
        }
    }

    // Sort by distance
    std::sort(pairs.begin(), pairs.end());

    // Union-Find to connect until all in one circuit
    UnionFind uf(n);
    int numComponents = n;

    for (const auto& pair : pairs) {
        if (uf.unite(pair.i, pair.j)) {  // Actually merged two components
            numComponents--;
            if (numComponents == 1) {
                // This was the last connection - all in one circuit now
                return static_cast<long long>(points[pair.i].x) * points[pair.j].x;
            }
        }
    }

    return 0;
}

int main(int argc, char* argv[]) {
    std::string inputFile = (argc > 1) ? argv[1] : "../input.txt";
    std::vector<Point> points = parseInput(inputFile);

    std::cout << "Part 1: " << part1(points) << std::endl;
    std::cout << "Part 2: " << part2(points) << std::endl;

    return 0;
}
