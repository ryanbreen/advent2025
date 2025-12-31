#!/usr/bin/env node
/**
 * Day 17: Clumsy Crucible - Dijkstra's shortest path with movement constraints.
 */

import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(filename) {
  const text = readFileSync(filename, 'utf-8').trim();
  return text.split('\n').map(line => line.split('').map(Number));
}

// Min-heap implementation
class MinHeap {
  constructor() {
    this.heap = [];
  }

  push(item) {
    this.heap.push(item);
    this.bubbleUp(this.heap.length - 1);
  }

  pop() {
    if (this.heap.length === 0) return null;
    const min = this.heap[0];
    const last = this.heap.pop();
    if (this.heap.length > 0) {
      this.heap[0] = last;
      this.bubbleDown(0);
    }
    return min;
  }

  bubbleUp(idx) {
    while (idx > 0) {
      const parent = Math.floor((idx - 1) / 2);
      if (this.heap[parent][0] <= this.heap[idx][0]) break;
      [this.heap[parent], this.heap[idx]] = [this.heap[idx], this.heap[parent]];
      idx = parent;
    }
  }

  bubbleDown(idx) {
    const length = this.heap.length;
    while (true) {
      const left = 2 * idx + 1;
      const right = 2 * idx + 2;
      let smallest = idx;
      if (left < length && this.heap[left][0] < this.heap[smallest][0]) {
        smallest = left;
      }
      if (right < length && this.heap[right][0] < this.heap[smallest][0]) {
        smallest = right;
      }
      if (smallest === idx) break;
      [this.heap[smallest], this.heap[idx]] = [this.heap[idx], this.heap[smallest]];
      idx = smallest;
    }
  }

  get length() {
    return this.heap.length;
  }
}

function dijkstra(grid, minStraight, maxStraight) {
  const rows = grid.length;
  const cols = grid[0].length;
  const dr = [0, 1, 0, -1];
  const dc = [1, 0, -1, 0];

  // Priority queue: [heat_loss, row, col, direction, consecutive]
  const pq = new MinHeap();
  pq.push([0, 0, 0, -1, 0]); // -1 direction means no direction yet

  const visited = new Set();

  while (pq.length > 0) {
    const [heat, r, c, d, consec] = pq.pop();

    // Check if we reached the goal
    if (r === rows - 1 && c === cols - 1) {
      if (minStraight === 0 || consec >= minStraight) {
        return heat;
      }
    }

    const stateKey = `${r},${c},${d},${consec}`;
    if (visited.has(stateKey)) continue;
    visited.add(stateKey);

    // Try all four directions
    for (let nd = 0; nd < 4; nd++) {
      // Can't reverse direction
      if (d !== -1 && nd === (d + 2) % 4) continue;

      const nr = r + dr[nd];
      const nc = c + dc[nd];

      // Bounds check
      if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) continue;

      let newConsec;
      if (nd === d) {
        // Continuing in same direction
        newConsec = consec + 1;
        if (newConsec > maxStraight) continue;
      } else {
        // Turning - must have gone minStraight in previous direction first
        if (d !== -1 && consec < minStraight) continue;
        newConsec = 1;
      }

      const newHeat = heat + grid[nr][nc];
      const newStateKey = `${nr},${nc},${nd},${newConsec}`;

      if (!visited.has(newStateKey)) {
        pq.push([newHeat, nr, nc, nd, newConsec]);
      }
    }
  }

  return -1; // No path found
}

function part1(grid) {
  return dijkstra(grid, 0, 3);
}

function part2(grid) {
  return dijkstra(grid, 4, 10);
}

const grid = parseInput(join(__dirname, '..', 'input.txt'));
console.log(`Part 1: ${part1(grid)}`);
console.log(`Part 2: ${part2(grid)}`);
