import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();
const grid = input.split('\n');
const rows = grid.length;
const cols = grid[0].length;

// Helper for position keys
const posKey = (r, c) => `${r},${c}`;

// Define pipe connections: each pipe connects to certain directions
// Directions: N=[-1,0], S=[1,0], E=[0,1], W=[0,-1]
const PIPE_CONNECTIONS = {
  '|': [[-1, 0], [1, 0]],   // N, S
  '-': [[0, -1], [0, 1]],   // W, E
  'L': [[-1, 0], [0, 1]],   // N, E
  'J': [[-1, 0], [0, -1]],  // N, W
  '7': [[1, 0], [0, -1]],   // S, W
  'F': [[1, 0], [0, 1]],    // S, E
};

const DIRECTIONS = [[-1, 0], [1, 0], [0, -1], [0, 1]];

function findStart() {
  for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
      if (grid[r][c] === 'S') {
        return [r, c];
      }
    }
  }
  return null;
}

function getNeighbors(pos) {
  const [r, c] = pos;
  const ch = grid[r][c];

  if (ch === 'S') {
    // S can connect to any adjacent pipe that connects back to it
    const neighbors = [];
    for (const [dr, dc] of DIRECTIONS) {
      const nr = r + dr;
      const nc = c + dc;
      if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
        const adjCh = grid[nr][nc];
        if (adjCh in PIPE_CONNECTIONS) {
          // Check if adjacent pipe connects back to S
          for (const [adjDr, adjDc] of PIPE_CONNECTIONS[adjCh]) {
            if (nr + adjDr === r && nc + adjDc === c) {
              neighbors.push([nr, nc]);
              break;
            }
          }
        }
      }
    }
    return neighbors;
  }

  if (ch in PIPE_CONNECTIONS) {
    const neighbors = [];
    for (const [dr, dc] of PIPE_CONNECTIONS[ch]) {
      const nr = r + dr;
      const nc = c + dc;
      if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
        neighbors.push([nr, nc]);
      }
    }
    return neighbors;
  }

  return [];
}

function findLoop(start) {
  const distances = new Map();
  distances.set(posKey(start[0], start[1]), 0);
  const queue = [start];
  let idx = 0;

  while (idx < queue.length) {
    const pos = queue[idx++];
    const posDist = distances.get(posKey(pos[0], pos[1]));

    for (const neighbor of getNeighbors(pos)) {
      const key = posKey(neighbor[0], neighbor[1]);
      if (!distances.has(key)) {
        distances.set(key, posDist + 1);
        queue.push(neighbor);
      }
    }
  }

  return distances;
}

function determineStartPipe(start, loopPositions) {
  const [r, c] = start;

  const connections = new Set();
  for (const [dr, dc] of DIRECTIONS) {
    const nr = r + dr;
    const nc = c + dc;
    const key = posKey(nr, nc);
    if (loopPositions.has(key)) {
      const adjCh = grid[nr][nc];
      if (adjCh in PIPE_CONNECTIONS) {
        // Check if this pipe connects back to S
        for (const [adjDr, adjDc] of PIPE_CONNECTIONS[adjCh]) {
          if (nr + adjDr === r && nc + adjDc === c) {
            connections.add(posKey(dr, dc));
            break;
          }
        }
      }
    }
  }

  // Find matching pipe type
  for (const [pipe, dirs] of Object.entries(PIPE_CONNECTIONS)) {
    const dirSet = new Set(dirs.map(([dr, dc]) => posKey(dr, dc)));
    if (connections.size === dirSet.size &&
        [...connections].every(d => dirSet.has(d))) {
      return pipe;
    }
  }

  return 'S';
}

// Part 1
function part1() {
  const start = findStart();
  const distances = findLoop(start);
  return Math.max(...distances.values());
}

// Part 2
function part2() {
  const start = findStart();
  const distances = findLoop(start);
  const loopPositions = new Set(distances.keys());

  // Replace S with its actual pipe type
  const startPipe = determineStartPipe(start, loopPositions);
  const modifiedGrid = grid.map(row => row.split(''));
  modifiedGrid[start[0]][start[1]] = startPipe;

  let enclosed = 0;

  for (let r = 0; r < rows; r++) {
    let inside = false;
    for (let c = 0; c < cols; c++) {
      const key = posKey(r, c);
      if (loopPositions.has(key)) {
        const ch = modifiedGrid[r][c];
        // Count vertical crossings (|, L, J go "north")
        if ('|LJ'.includes(ch)) {
          inside = !inside;
        }
      } else if (inside) {
        enclosed++;
      }
    }
  }

  return enclosed;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
