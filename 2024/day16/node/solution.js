import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf8').trim();

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
    if (this.heap.length === 1) return this.heap.pop();
    const result = this.heap[0];
    this.heap[0] = this.heap.pop();
    this.bubbleDown(0);
    return result;
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
      [this.heap[idx], this.heap[smallest]] = [this.heap[smallest], this.heap[idx]];
      idx = smallest;
    }
  }

  get length() {
    return this.heap.length;
  }
}

// Directions: 0=East, 1=South, 2=West, 3=North
const DX = [1, 0, -1, 0];
const DY = [0, 1, 0, -1];

function parseInput(text) {
  const grid = text.split('\n').map(line => line.split(''));
  let start = null, end = null;

  for (let y = 0; y < grid.length; y++) {
    for (let x = 0; x < grid[y].length; x++) {
      if (grid[y][x] === 'S') start = [x, y];
      if (grid[y][x] === 'E') end = [x, y];
    }
  }

  return { grid, start, end };
}

function dijkstraForward(grid, start) {
  const pq = new MinHeap();
  pq.push([0, start[0], start[1], 0]); // Start facing East
  const dist = new Map();

  while (pq.length > 0) {
    const [cost, x, y, d] = pq.pop();
    const state = `${x},${y},${d}`;

    if (dist.has(state)) continue;
    dist.set(state, cost);

    // Move forward
    const nx = x + DX[d];
    const ny = y + DY[d];
    if (ny >= 0 && ny < grid.length && nx >= 0 && nx < grid[0].length && grid[ny][nx] !== '#') {
      pq.push([cost + 1, nx, ny, d]);
    }

    // Turn left/right
    pq.push([cost + 1000, x, y, (d + 3) % 4]);
    pq.push([cost + 1000, x, y, (d + 1) % 4]);
  }

  return dist;
}

function dijkstraBackward(grid, end) {
  const pq = new MinHeap();
  // At end, can arrive facing any direction
  for (let d = 0; d < 4; d++) {
    pq.push([0, end[0], end[1], d]);
  }
  const dist = new Map();

  while (pq.length > 0) {
    const [cost, x, y, d] = pq.pop();
    const state = `${x},${y},${d}`;

    if (dist.has(state)) continue;
    dist.set(state, cost);

    // Reverse of move forward: come from behind
    const px = x - DX[d];
    const py = y - DY[d];
    if (py >= 0 && py < grid.length && px >= 0 && px < grid[0].length && grid[py][px] !== '#') {
      pq.push([cost + 1, px, py, d]);
    }

    // Reverse of turn
    pq.push([cost + 1000, x, y, (d + 3) % 4]);
    pq.push([cost + 1000, x, y, (d + 1) % 4]);
  }

  return dist;
}

function part1(grid, start, end) {
  const dist = dijkstraForward(grid, start);
  let best = Infinity;
  for (let d = 0; d < 4; d++) {
    const state = `${end[0]},${end[1]},${d}`;
    if (dist.has(state)) {
      best = Math.min(best, dist.get(state));
    }
  }
  return best;
}

function part2(grid, start, end, bestScore) {
  const distFromStart = dijkstraForward(grid, start);
  const distToEnd = dijkstraBackward(grid, end);

  const tilesOnBestPath = new Set();

  for (let y = 0; y < grid.length; y++) {
    for (let x = 0; x < grid[0].length; x++) {
      if (grid[y][x] === '#') continue;

      for (let d = 0; d < 4; d++) {
        const state = `${x},${y},${d}`;
        const fromStart = distFromStart.get(state) ?? Infinity;
        const toEnd = distToEnd.get(state) ?? Infinity;
        if (fromStart + toEnd === bestScore) {
          tilesOnBestPath.add(`${x},${y}`);
          break;
        }
      }
    }
  }

  return tilesOnBestPath.size;
}

const { grid, start, end } = parseInput(input);

const answer1 = part1(grid, start, end);
console.log('Part 1:', answer1);

const answer2 = part2(grid, start, end, answer1);
console.log('Part 2:', answer2);
