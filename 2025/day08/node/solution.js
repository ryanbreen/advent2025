import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

function parseInput(filename) {
    const content = readFileSync(filename, 'utf-8');
    const points = [];
    for (const line of content.trim().split('\n')) {
        if (line.trim()) {
            const [x, y, z] = line.split(',').map(Number);
            points.push([x, y, z]);
        }
    }
    return points;
}

class UnionFind {
    constructor(n) {
        this.parent = Array.from({ length: n }, (_, i) => i);
        this.rank = new Array(n).fill(0);
        this.size = new Array(n).fill(1);
    }

    find(x) {
        if (this.parent[x] !== x) {
            this.parent[x] = this.find(this.parent[x]); // Path compression
        }
        return this.parent[x];
    }

    union(x, y) {
        let px = this.find(x);
        let py = this.find(y);
        if (px === py) return false; // Already in same set

        // Union by rank
        if (this.rank[px] < this.rank[py]) {
            [px, py] = [py, px];
        }
        this.parent[py] = px;
        this.size[px] += this.size[py];
        if (this.rank[px] === this.rank[py]) {
            this.rank[px]++;
        }
        return true;
    }

    getComponentSizes() {
        const sizes = [];
        for (let i = 0; i < this.parent.length; i++) {
            if (this.parent[i] === i) {
                sizes.push(this.size[i]);
            }
        }
        return sizes;
    }
}

function distanceSq(p1, p2) {
    return (p1[0] - p2[0]) ** 2 + (p1[1] - p2[1]) ** 2 + (p1[2] - p2[2]) ** 2;
}

function part1(points, numConnections = 1000) {
    const n = points.length;

    // Generate all pairs with distances
    const pairs = [];
    for (let i = 0; i < n; i++) {
        for (let j = i + 1; j < n; j++) {
            pairs.push([distanceSq(points[i], points[j]), i, j]);
        }
    }

    // Sort by distance
    pairs.sort((a, b) => a[0] - b[0]);

    // Union-Find to connect closest pairs
    const uf = new UnionFind(n);
    let connections = 0;
    for (const [distSq, i, j] of pairs) {
        uf.union(i, j); // Always attempt union
        connections++;
        if (connections === numConnections) break;
    }

    // Get component sizes and find 3 largest
    const sizes = uf.getComponentSizes().sort((a, b) => b - a);

    // Multiply the 3 largest
    return sizes[0] * sizes[1] * sizes[2];
}

function part2(points) {
    const n = points.length;

    // Generate all pairs with distances
    const pairs = [];
    for (let i = 0; i < n; i++) {
        for (let j = i + 1; j < n; j++) {
            pairs.push([distanceSq(points[i], points[j]), i, j]);
        }
    }

    // Sort by distance
    pairs.sort((a, b) => a[0] - b[0]);

    // Union-Find to connect until all in one circuit
    const uf = new UnionFind(n);
    let numComponents = n;

    for (const [distSq, i, j] of pairs) {
        if (uf.union(i, j)) {  // Actually merged two components
            numComponents--;
            if (numComponents === 1) {
                // This was the last connection - all in one circuit now
                return points[i][0] * points[j][0];  // Product of X coordinates
            }
        }
    }

    return 0;
}

function main() {
    const inputFile = process.argv[2] || join(__dirname, '../input.txt');
    const points = parseInput(inputFile);

    console.log(`Part 1: ${part1(points)}`);
    console.log(`Part 2: ${part2(points)}`);
}

main();
