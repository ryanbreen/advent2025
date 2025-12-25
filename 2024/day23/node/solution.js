import { readFileSync } from 'fs';

function parseInput(filename) {
    const graph = new Map();
    const lines = readFileSync(filename, 'utf8').trim().split('\n');

    for (const line of lines) {
        const [a, b] = line.split('-');
        if (!graph.has(a)) graph.set(a, new Set());
        if (!graph.has(b)) graph.set(b, new Set());
        graph.get(a).add(b);
        graph.get(b).add(a);
    }
    return graph;
}

function findTriangles(graph) {
    const triangles = new Set();

    for (const [a, neighborsA] of graph) {
        for (const b of neighborsA) {
            if (a < b) {
                const neighborsB = graph.get(b);
                // Find common neighbors
                for (const c of neighborsA) {
                    if (neighborsB.has(c)) {
                        // Create sorted tuple key
                        const tri = [a, b, c].sort().join(',');
                        triangles.add(tri);
                    }
                }
            }
        }
    }
    return triangles;
}

function part1(graph) {
    const triangles = findTriangles(graph);
    let count = 0;
    for (const tri of triangles) {
        const nodes = tri.split(',');
        if (nodes.some(n => n.startsWith('t'))) {
            count++;
        }
    }
    return count;
}

function bronKerbosch(graph, r, p, x, cliques) {
    if (p.size === 0 && x.size === 0) {
        cliques.push(new Set(r));
        return;
    }

    // Use pivot to reduce branching
    const px = new Set([...p, ...x]);
    let pivot = null;
    let maxNeighbors = -1;
    for (const v of px) {
        const neighbors = graph.get(v);
        const count = [...p].filter(n => neighbors.has(n)).length;
        if (count > maxNeighbors) {
            maxNeighbors = count;
            pivot = v;
        }
    }

    const pivotNeighbors = graph.get(pivot);
    const candidates = [...p].filter(v => !pivotNeighbors.has(v));

    for (const v of candidates) {
        const neighbors = graph.get(v);
        const newR = new Set(r);
        newR.add(v);
        const newP = new Set([...p].filter(n => neighbors.has(n)));
        const newX = new Set([...x].filter(n => neighbors.has(n)));

        bronKerbosch(graph, newR, newP, newX, cliques);

        p.delete(v);
        x.add(v);
    }
}

function part2(graph) {
    const cliques = [];
    const allNodes = new Set(graph.keys());
    bronKerbosch(graph, new Set(), allNodes, new Set(), cliques);

    // Find largest clique
    let largest = cliques[0];
    for (const clique of cliques) {
        if (clique.size > largest.size) {
            largest = clique;
        }
    }

    return [...largest].sort().join(',');
}

const graph = parseInput('../input.txt');
console.log('Part 1:', part1(graph));
console.log('Part 2:', part2(graph));
