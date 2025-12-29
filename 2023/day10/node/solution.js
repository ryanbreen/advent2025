import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();
const lines = input.split('\n');

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

function findStart(grid) {
  for (let r = 0; r < grid.length; r++) {
    for (let c = 0; c < grid[r].length; c++) {
      if (grid[r][c] === 'S') {
        return [r, c];
      }
    }
  }
  return null;
}

function getNeighbors(grid, pos) {
  const [r, c] = pos;
  const rows = grid.length;
  const cols = grid[0].length;
  const ch = grid[r][c];

  if (ch === 'S') {
    // S can connect to any adjacent pipe that connects back to it
    const neighbors = [];
    for (const [dr, dc] of [[-1, 0], [1, 0], [0, -1], [0, 1]]) {
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
  } else if (ch in PIPE_CONNECTIONS) {
    const neighbors = [];
    for (const [dr, dc] of PIPE_CONNECTIONS[ch]) {
      const nr = r + dr;
      const nc = c + dc;
      if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
        neighbors.push([nr, nc]);
      }
    }
    return neighbors;
  } else {
    return [];
  }
}

function findLoop(grid, start) {
  const distances = new Map();
  distances.set(`${start[0]},${start[1]}`, 0);
  const queue = [start];

  while (queue.length > 0) {
    const pos = queue.shift();
    const posDist = distances.get(`${pos[0]},${pos[1]}`);

    for (const neighbor of getNeighbors(grid, pos)) {
      const key = `${neighbor[0]},${neighbor[1]}`;
      if (!distances.has(key)) {
        distances.set(key, posDist + 1);
        queue.push(neighbor);
      }
    }
  }

  return distances;
}

function determineStartPipe(grid, start, loopPositions) {
  const [r, c] = start;
  const rows = grid.length;
  const cols = grid[0].length;

  const connections = [];
  for (const [dr, dc] of [[-1, 0], [1, 0], [0, -1], [0, 1]]) {
    const nr = r + dr;
    const nc = c + dc;
    const key = `${nr},${nc}`;
    if (loopPositions.has(key)) {
      const adjCh = grid[nr][nc];
      if (adjCh in PIPE_CONNECTIONS) {
        // Check if this pipe connects back to S
        for (const [adjDr, adjDc] of PIPE_CONNECTIONS[adjCh]) {
          if (nr + adjDr === r && nc + adjDc === c) {
            connections.push([dr, dc]);
            break;
          }
        }
      }
    }
  }

  // Find matching pipe type
  for (const [pipe, dirs] of Object.entries(PIPE_CONNECTIONS)) {
    if (dirs.length === connections.length) {
      const connSet = new Set(connections.map(([dr, dc]) => `${dr},${dc}`));
      const dirSet = new Set(dirs.map(([dr, dc]) => `${dr},${dc}`));
      if ([...connSet].every(d => dirSet.has(d)) && [...dirSet].every(d => connSet.has(d))) {
        return pipe;
      }
    }
  }

  return 'S';
}

// Part 1
function part1() {
  const start = findStart(lines);
  const distances = findLoop(lines, start);
  return Math.max(...distances.values());
}

// Part 2
function part2() {
  const start = findStart(lines);
  const distances = findLoop(lines, start);
  const loopPositions = new Set(distances.keys());

  // Replace S with its actual pipe type
  const startPipe = determineStartPipe(lines, start, loopPositions);
  const grid = lines.map(row => row.split(''));
  grid[start[0]][start[1]] = startPipe;

  const rows = grid.length;
  const cols = grid[0].length;
  let enclosed = 0;

  for (let r = 0; r < rows; r++) {
    let inside = false;
    for (let c = 0; c < cols; c++) {
      const key = `${r},${c}`;
      if (loopPositions.has(key)) {
        const ch = grid[r][c];
        // Count vertical crossings (|, L, J go "north")
        if ('|LJ'.includes(ch)) {
          inside = !inside;
        }
      } else {
        if (inside) {
          enclosed++;
        }
      }
    }
  }

  return enclosed;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
