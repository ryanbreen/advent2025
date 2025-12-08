#!/usr/bin/env python3
import sys

def parse_input(filename):
    """Parse the input file and return list of (x, y, z) tuples."""
    points = []
    with open(filename) as f:
        for line in f:
            line = line.strip()
            if line:
                x, y, z = map(int, line.split(','))
                points.append((x, y, z))
    return points

class UnionFind:
    def __init__(self, n):
        self.parent = list(range(n))
        self.rank = [0] * n
        self.size = [1] * n

    def find(self, x):
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])  # Path compression
        return self.parent[x]

    def union(self, x, y):
        """Union two elements. Returns True if they were in different sets."""
        px, py = self.find(x), self.find(y)
        if px == py:
            return False  # Already in same set

        # Union by rank
        if self.rank[px] < self.rank[py]:
            px, py = py, px
        self.parent[py] = px
        self.size[px] += self.size[py]
        if self.rank[px] == self.rank[py]:
            self.rank[px] += 1
        return True

    def get_component_sizes(self):
        """Get sizes of all connected components."""
        sizes = []
        for i in range(len(self.parent)):
            if self.parent[i] == i:  # Root of a component
                sizes.append(self.size[i])
        return sizes

def euclidean_distance_sq(p1, p2):
    """Squared Euclidean distance (avoid sqrt for comparison)."""
    return (p1[0] - p2[0])**2 + (p1[1] - p2[1])**2 + (p1[2] - p2[2])**2

def part1(points, num_connections=1000):
    """Connect the num_connections closest pairs and return product of 3 largest component sizes."""
    n = len(points)

    # Generate all pairs with distances
    pairs = []
    for i in range(n):
        for j in range(i + 1, n):
            dist_sq = euclidean_distance_sq(points[i], points[j])
            pairs.append((dist_sq, i, j))

    # Sort by distance
    pairs.sort()

    # Union-Find to connect closest pairs
    uf = UnionFind(n)
    connections = 0
    for dist_sq, i, j in pairs:
        uf.union(i, j)  # Always attempt union (even if already connected)
        connections += 1
        if connections == num_connections:
            break

    # Get component sizes and find the 3 largest
    sizes = sorted(uf.get_component_sizes(), reverse=True)

    # Multiply the 3 largest
    return sizes[0] * sizes[1] * sizes[2]

def part2(points):
    """Connect all junction boxes into one circuit. Return product of X coordinates of last connection."""
    n = len(points)

    # Generate all pairs with distances
    pairs = []
    for i in range(n):
        for j in range(i + 1, n):
            dist_sq = euclidean_distance_sq(points[i], points[j])
            pairs.append((dist_sq, i, j))

    # Sort by distance
    pairs.sort()

    # Union-Find to connect until all in one circuit
    uf = UnionFind(n)
    num_components = n

    for dist_sq, i, j in pairs:
        if uf.union(i, j):  # Actually merged two components
            num_components -= 1
            if num_components == 1:
                # This was the last connection - all in one circuit now
                return points[i][0] * points[j][0]  # Product of X coordinates

    return 0

def main():
    input_file = sys.argv[1] if len(sys.argv) > 1 else '../input.txt'
    points = parse_input(input_file)

    print(f"Part 1: {part1(points)}")
    print(f"Part 2: {part2(points)}")

if __name__ == '__main__':
    main()
