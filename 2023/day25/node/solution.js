#!/usr/bin/env node
/**
 * Advent of Code 2023 - Day 25: Snowverload
 * Find the minimum cut of 3 edges that divides the graph into two components.
 *
 * Uses edge betweenness centrality: edges that form the cut between two large
 * components will have high betweenness (many shortest paths pass through them).
 */

import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(filename) {
  const content = readFileSync(filename, 'utf-8').trim();
  const graph = new Map();

  for (const line of content.split('\n')) {
    const [left, right] = line.split(': ');
    const neighbors = right.split(' ');

    if (!graph.has(left)) graph.set(left, new Set());
    for (const neighbor of neighbors) {
      if (!graph.has(neighbor)) graph.set(neighbor, new Set());
      graph.get(left).add(neighbor);
      graph.get(neighbor).add(left);
    }
  }

  return graph;
}

function makeEdgeKey(a, b) {
  return a < b ? `${a}-${b}` : `${b}-${a}`;
}

function bfsComponentSize(graph, start, excludedEdges) {
  const visited = new Set([start]);
  const queue = [start];

  while (queue.length > 0) {
    const node = queue.shift();
    for (const neighbor of graph.get(node)) {
      const edgeKey = makeEdgeKey(node, neighbor);
      if (!visited.has(neighbor) && !excludedEdges.has(edgeKey)) {
        visited.add(neighbor);
        queue.push(neighbor);
      }
    }
  }

  return visited.size;
}

function computeEdgeBetweenness(graph, sampleSize = null) {
  const edgeCount = new Map();
  let nodes = Array.from(graph.keys());

  // Sample nodes for efficiency
  if (sampleSize && nodes.length > sampleSize) {
    // Simple deterministic sampling
    const step = Math.floor(nodes.length / sampleSize);
    nodes = nodes.filter((_, i) => i % step === 0).slice(0, sampleSize);
  }

  for (const source of nodes) {
    // BFS to find shortest paths
    const dist = new Map([[source, 0]]);
    const pred = new Map(); // Predecessors on shortest paths
    const queue = [source];

    while (queue.length > 0) {
      const node = queue.shift();
      for (const neighbor of graph.get(node)) {
        if (!dist.has(neighbor)) {
          dist.set(neighbor, dist.get(node) + 1);
          pred.set(neighbor, [node]);
          queue.push(neighbor);
        } else if (dist.get(neighbor) === dist.get(node) + 1) {
          pred.get(neighbor).push(node);
        }
      }
    }

    // Compute number of shortest paths to each node
    const numPaths = new Map([[source, 1]]);
    const sortedByDist = Array.from(dist.keys()).sort(
      (a, b) => dist.get(a) - dist.get(b)
    );
    for (const node of sortedByDist) {
      if (!numPaths.has(node)) numPaths.set(node, 0);
      const preds = pred.get(node) || [];
      for (const p of preds) {
        numPaths.set(node, numPaths.get(node) + numPaths.get(p));
      }
    }

    // Accumulate edge betweenness (reverse BFS order)
    const dependency = new Map();
    for (const node of sortedByDist.reverse()) {
      const preds = pred.get(node) || [];
      const nodeDep = dependency.get(node) || 0;
      for (const p of preds) {
        const edgeKey = makeEdgeKey(p, node);
        const frac = numPaths.get(p) / numPaths.get(node);
        const contrib = frac * (1 + nodeDep);
        edgeCount.set(edgeKey, (edgeCount.get(edgeKey) || 0) + contrib);
        dependency.set(p, (dependency.get(p) || 0) + contrib);
      }
    }
  }

  return edgeCount;
}

function findCutEdges(graph) {
  // Compute edge betweenness with sampling for speed
  const edgeBetweenness = computeEdgeBetweenness(graph, 100);

  // Sort edges by betweenness (highest first)
  const sortedEdges = Array.from(edgeBetweenness.entries())
    .sort((a, b) => b[1] - a[1])
    .map(([edge]) => edge);

  const totalNodes = graph.size;

  // Try removing top candidate edges
  const topEdges = sortedEdges.slice(0, 20);

  for (let i = 0; i < topEdges.length; i++) {
    for (let j = i + 1; j < topEdges.length; j++) {
      for (let k = j + 1; k < topEdges.length; k++) {
        const excluded = new Set([topEdges[i], topEdges[j], topEdges[k]]);
        const start = graph.keys().next().value;
        const size1 = bfsComponentSize(graph, start, excluded);

        if (size1 < totalNodes) {
          // Graph is disconnected!
          const size2 = totalNodes - size1;
          return size1 * size2;
        }
      }
    }
  }

  return null;
}

function part1(filename) {
  const graph = parseInput(filename);
  return findCutEdges(graph);
}

function part2() {
  // Day 25 Part 2 is traditionally unlocked by having 49 stars.
  // There's no computation needed - just push the button!
  return 'Push the big red button!';
}

const inputFile = join(__dirname, '..', 'input.txt');
console.log('Part 1:', part1(inputFile));
console.log('Part 2:', part2());
