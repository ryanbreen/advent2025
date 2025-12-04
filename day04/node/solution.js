import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\n');

// Direction offsets for 8 neighbors (including diagonals)
const DIRECTIONS = [
  [-1, -1], [-1, 0], [-1, 1],  // top-left, top, top-right
  [0, -1],           [0, 1],   // left, right
  [1, -1],  [1, 0],  [1, 1]    // bottom-left, bottom, bottom-right
];

// ============== PRECOMPUTE ROLL POSITIONS AND NEIGHBORS ==============
const grid = lines;
const rows = grid.length;
const cols = grid[0].length;

const rollPositions = [];   // Array of [r, c] for each roll
const posToIndex = new Map();  // Map: "r,c" => index in rollPositions
const rollNeighbors = [];   // Array of arrays: neighbors for each roll

for (let r = 0; r < rows; r++) {
  for (let c = 0; c < cols; c++) {
    if (grid[r][c] === '@') {
      const idx = rollPositions.length;
      rollPositions.push([r, c]);
      posToIndex.set(`${r},${c}`, idx);

      // Precompute neighbors for this roll
      const neighbors = [];
      for (const [dr, dc] of DIRECTIONS) {
        const nr = r + dr;
        const nc = c + dc;
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] === '@') {
          neighbors.push(`${nr},${nc}`);
        }
      }
      rollNeighbors.push(neighbors);
    }
  }
}

const numRolls = rollPositions.length;

// Part 1
function part1() {
  let accessibleCount = 0;

  for (let i = 0; i < numRolls; i++) {
    const neighborCount = rollNeighbors[i].length;
    if (neighborCount < 4) {
      accessibleCount++;
    }
  }

  return accessibleCount;
}

// Part 2
function part2() {
  // Track which rolls are still active
  const active = new Set();
  for (let i = 0; i < numRolls; i++) {
    const [r, c] = rollPositions[i];
    active.add(`${r},${c}`);
  }

  // Compute initial neighbor counts
  const neighborCount = new Array(numRolls);
  for (let i = 0; i < numRolls; i++) {
    let count = 0;
    for (const neighborPos of rollNeighbors[i]) {
      if (active.has(neighborPos)) {
        count++;
      }
    }
    neighborCount[i] = count;
  }

  // Initialize queue with accessible rolls (neighbor count < 4)
  let queue = [];
  const inQueue = new Set();
  for (let i = 0; i < numRolls; i++) {
    if (neighborCount[i] < 4) {
      queue.push(i);
      inQueue.add(i);
    }
  }

  // Process queue
  let totalRemoved = 0;

  while (queue.length > 0) {
    const nextQueue = [];

    for (const idx of queue) {
      const [r, c] = rollPositions[idx];
      const posKey = `${r},${c}`;

      // Skip if already removed
      if (!active.has(posKey)) {
        continue;
      }

      // Remove this roll
      active.delete(posKey);
      totalRemoved++;

      // Update neighbors' counts
      for (const neighborPos of rollNeighbors[idx]) {
        if (active.has(neighborPos)) {
          const neighborIdx = posToIndex.get(neighborPos);
          neighborCount[neighborIdx]--;

          // Add to queue if now accessible and not already queued
          if (neighborCount[neighborIdx] < 4 && !inQueue.has(neighborIdx)) {
            nextQueue.push(neighborIdx);
            inQueue.add(neighborIdx);
          }
        }
      }
    }

    queue = nextQueue;
  }

  return totalRemoved;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
