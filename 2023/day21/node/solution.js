#!/usr/bin/env node
/**
 * Day 21: Step Counter - Garden plot reachability.
 */

import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(filename) {
  const grid = readFileSync(filename, 'utf-8').trim().split('\n');
  let start = null;

  for (let r = 0; r < grid.length; r++) {
    const c = grid[r].indexOf('S');
    if (c !== -1) {
      start = [r, c];
      break;
    }
  }

  return { grid, start };
}

function countReachable(grid, start, steps) {
  const rows = grid.length;
  const cols = grid[0].length;

  // BFS to find minimum steps to each cell
  const visited = new Map();
  const queue = [[start[0], start[1], 0]];
  visited.set(`${start[0]},${start[1]}`, 0);

  let head = 0;
  while (head < queue.length) {
    const [r, c, dist] = queue[head++];

    if (dist >= steps) continue;

    for (const [dr, dc] of [[-1, 0], [1, 0], [0, -1], [0, 1]]) {
      const nr = r + dr;
      const nc = c + dc;

      if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
        const key = `${nr},${nc}`;
        if (grid[nr][nc] !== '#' && !visited.has(key)) {
          visited.set(key, dist + 1);
          queue.push([nr, nc, dist + 1]);
        }
      }
    }
  }

  // Count cells with same parity as steps
  const targetParity = steps % 2;
  let count = 0;
  for (const d of visited.values()) {
    if (d <= steps && d % 2 === targetParity) {
      count++;
    }
  }

  return count;
}

function countReachableInfiniteBfs(grid, start, steps) {
  const rows = grid.length;
  const cols = grid[0].length;

  const visited = new Map();
  const queue = [[start[0], start[1], 0]];
  visited.set(`${start[0]},${start[1]}`, 0);

  let head = 0;
  while (head < queue.length) {
    const [r, c, dist] = queue[head++];

    if (dist >= steps) continue;

    for (const [dr, dc] of [[-1, 0], [1, 0], [0, -1], [0, 1]]) {
      const nr = r + dr;
      const nc = c + dc;

      // Map to grid coordinates (infinite tiling)
      const gr = ((nr % rows) + rows) % rows;
      const gc = ((nc % cols) + cols) % cols;

      const key = `${nr},${nc}`;
      if (grid[gr][gc] !== '#' && !visited.has(key)) {
        visited.set(key, dist + 1);
        queue.push([nr, nc, dist + 1]);
      }
    }
  }

  const targetParity = steps % 2;
  let count = 0;
  for (const d of visited.values()) {
    if (d <= steps && d % 2 === targetParity) {
      count++;
    }
  }

  return count;
}

function countReachableInfinite(grid, start, steps) {
  const size = grid.length;
  const half = Math.floor(size / 2);

  if (steps <= size * 2) {
    return countReachableInfiniteBfs(grid, start, steps);
  }

  // The number of full grid widths we travel
  const n = BigInt(Math.floor((steps - half) / size));

  // Calculate reachable counts for n=0, 1, 2 to determine quadratic
  const y0 = BigInt(countReachableInfiniteBfs(grid, start, half));
  const y1 = BigInt(countReachableInfiniteBfs(grid, start, half + size));
  const y2 = BigInt(countReachableInfiniteBfs(grid, start, half + 2 * size));

  // Solve for a, b, c using finite differences
  const a = (y2 - 2n * y1 + y0) / 2n;
  const b = y1 - y0 - a;
  const c = y0;

  return a * n * n + b * n + c;
}

function part1(grid, start) {
  return countReachable(grid, start, 64);
}

function part2(grid, start) {
  return countReachableInfinite(grid, start, 26501365);
}

const inputPath = join(__dirname, '..', 'input.txt');
const { grid, start } = parseInput(inputPath);
console.log(`Part 1: ${part1(grid, start)}`);
console.log(`Part 2: ${part2(grid, start)}`);
