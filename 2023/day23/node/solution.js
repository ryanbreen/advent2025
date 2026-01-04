#!/usr/bin/env node
/**
 * Day 23: A Long Walk - Longest path through hiking trails.
 * Optimized with typed arrays for visited set and flattened adjacency list.
 */

import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(filename) {
  return readFileSync(filename, 'utf-8').trim().split('\n');
}

function findJunctions(grid) {
  const rows = grid.length;
  const cols = grid[0].length;
  const junctions = [];

  // Start and end points
  const startCol = grid[0].indexOf('.');
  const endCol = grid[rows - 1].indexOf('.');

  // Use encoded position: r * cols + c
  const junctionSet = new Set();
  junctionSet.add(0 * cols + startCol);
  junctionSet.add((rows - 1) * cols + endCol);

  // Find intersections (cells with 3+ walkable neighbors)
  for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
      if (grid[r][c] === '#') continue;
      let neighbors = 0;
      if (r > 0 && grid[r - 1][c] !== '#') neighbors++;
      if (r < rows - 1 && grid[r + 1][c] !== '#') neighbors++;
      if (c > 0 && grid[r][c - 1] !== '#') neighbors++;
      if (c < cols - 1 && grid[r][c + 1] !== '#') neighbors++;
      if (neighbors >= 3) {
        junctionSet.add(r * cols + c);
      }
    }
  }

  // Convert to array and create position-to-index map
  for (const pos of junctionSet) {
    junctions.push(pos);
  }

  const posToIndex = new Map();
  for (let i = 0; i < junctions.length; i++) {
    posToIndex.set(junctions[i], i);
  }

  return { junctions, junctionSet, posToIndex, cols };
}

function buildGraph(grid, junctionData, respectSlopes) {
  const { junctions, junctionSet, posToIndex, cols } = junctionData;
  const rows = grid.length;
  const numJunctions = junctions.length;

  // Collect edges first
  const tempAdj = new Array(numJunctions);
  for (let i = 0; i < numJunctions; i++) {
    tempAdj[i] = [];
  }

  const slopeDirs = { '^': -cols, 'v': cols, '<': -1, '>': 1 };

  for (let startIdx = 0; startIdx < numJunctions; startIdx++) {
    const startPos = junctions[startIdx];
    const startR = Math.floor(startPos / cols);
    const startC = startPos % cols;

    // DFS from junction to find reachable junctions
    const stack = [[startR, startC, 0]];
    const visited = new Set([startPos]);

    while (stack.length > 0) {
      const [r, c, dist] = stack.pop();
      const pos = r * cols + c;

      if (dist > 0 && junctionSet.has(pos)) {
        const neighborIdx = posToIndex.get(pos);
        tempAdj[startIdx].push(neighborIdx, dist);
        continue;
      }

      const cell = grid[r][c];

      // Check all 4 directions - unrolled for speed
      const nr1 = r - 1;
      if (nr1 >= 0 && grid[nr1][c] !== '#') {
        const npos = pos - cols;
        if (!visited.has(npos)) {
          if (!respectSlopes || !(cell in slopeDirs) || slopeDirs[cell] === -cols) {
            visited.add(npos);
            stack.push([nr1, c, dist + 1]);
          }
        }
      }

      const nr2 = r + 1;
      if (nr2 < rows && grid[nr2][c] !== '#') {
        const npos = pos + cols;
        if (!visited.has(npos)) {
          if (!respectSlopes || !(cell in slopeDirs) || slopeDirs[cell] === cols) {
            visited.add(npos);
            stack.push([nr2, c, dist + 1]);
          }
        }
      }

      const nc3 = c - 1;
      if (nc3 >= 0 && grid[r][nc3] !== '#') {
        const npos = pos - 1;
        if (!visited.has(npos)) {
          if (!respectSlopes || !(cell in slopeDirs) || slopeDirs[cell] === -1) {
            visited.add(npos);
            stack.push([r, nc3, dist + 1]);
          }
        }
      }

      const nc4 = c + 1;
      if (nc4 < cols && grid[r][nc4] !== '#') {
        const npos = pos + 1;
        if (!visited.has(npos)) {
          if (!respectSlopes || !(cell in slopeDirs) || slopeDirs[cell] === 1) {
            visited.add(npos);
            stack.push([r, nc4, dist + 1]);
          }
        }
      }
    }
  }

  // Flatten to typed arrays with offsets
  const offsets = new Uint32Array(numJunctions + 1);
  let totalEdges = 0;
  for (let i = 0; i < numJunctions; i++) {
    offsets[i] = totalEdges;
    totalEdges += tempAdj[i].length / 2;
  }
  offsets[numJunctions] = totalEdges;

  const neighbors = new Uint8Array(totalEdges);
  const dists = new Uint16Array(totalEdges);

  for (let i = 0; i < numJunctions; i++) {
    const edges = tempAdj[i];
    let idx = offsets[i];
    for (let j = 0; j < edges.length; j += 2) {
      neighbors[idx] = edges[j];
      dists[idx] = edges[j + 1];
      idx++;
    }
  }

  return { offsets, neighbors, dists };
}

function longestPath(graph, startIdx, endIdx, numNodes) {
  const { offsets, neighbors, dists } = graph;
  const visited = new Uint8Array(numNodes);
  let maxDist = -1;

  function dfs(node, dist) {
    if (node === endIdx) {
      if (dist > maxDist) maxDist = dist;
      return;
    }

    visited[node] = 1;

    const start = offsets[node];
    const end = offsets[node + 1];
    for (let i = start; i < end; i++) {
      const neighbor = neighbors[i];
      if (visited[neighbor] === 0) {
        dfs(neighbor, dist + dists[i]);
      }
    }

    visited[node] = 0;
  }

  dfs(startIdx, 0);
  return maxDist;
}

function solve(grid, respectSlopes) {
  const rows = grid.length;
  const cols = grid[0].length;
  const startCol = grid[0].indexOf('.');
  const endCol = grid[rows - 1].indexOf('.');

  const startPos = 0 * cols + startCol;
  const endPos = (rows - 1) * cols + endCol;

  const junctionData = findJunctions(grid);
  const startIdx = junctionData.posToIndex.get(startPos);
  const endIdx = junctionData.posToIndex.get(endPos);

  const graph = buildGraph(grid, junctionData, respectSlopes);

  return longestPath(graph, startIdx, endIdx, junctionData.junctions.length);
}

function part1(grid) {
  return solve(grid, true);
}

function part2(grid) {
  return solve(grid, false);
}

const inputPath = join(__dirname, '..', 'input.txt');
const grid = parseInput(inputPath);
console.log(`Part 1: ${part1(grid)}`);
console.log(`Part 2: ${part2(grid)}`);
