import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseGrid(text) {
  const grid = text.trim().split('\n').map(line => line.split(''));
  let start = null;
  let end = null;

  for (let r = 0; r < grid.length; r++) {
    for (let c = 0; c < grid[r].length; c++) {
      if (grid[r][c] === 'S') {
        start = [r, c];
        grid[r][c] = 'a';
      } else if (grid[r][c] === 'E') {
        end = [r, c];
        grid[r][c] = 'z';
      }
    }
  }

  return { grid, start, end };
}

function bfs(grid, starts, end) {
  const rows = grid.length;
  const cols = grid[0].length;
  const visited = new Set();
  const queue = [];

  for (const start of starts) {
    queue.push([start[0], start[1], 0]);
    visited.add(`${start[0]},${start[1]}`);
  }

  const directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];

  while (queue.length > 0) {
    const [r, c, dist] = queue.shift();

    if (r === end[0] && c === end[1]) {
      return dist;
    }

    const currentHeight = grid[r][c].charCodeAt(0);

    for (const [dr, dc] of directions) {
      const nr = r + dr;
      const nc = c + dc;
      const key = `${nr},${nc}`;

      if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && !visited.has(key)) {
        const nextHeight = grid[nr][nc].charCodeAt(0);
        // Can move if destination is at most 1 higher
        if (nextHeight <= currentHeight + 1) {
          visited.add(key);
          queue.push([nr, nc, dist + 1]);
        }
      }
    }
  }

  return -1; // No path found
}

function part1(text) {
  const { grid, start, end } = parseGrid(text);
  return bfs(grid, [start], end);
}

function part2(text) {
  const { grid, end } = parseGrid(text);

  // Find all cells with elevation 'a'
  const starts = [];
  for (let r = 0; r < grid.length; r++) {
    for (let c = 0; c < grid[r].length; c++) {
      if (grid[r][c] === 'a') {
        starts.push([r, c]);
      }
    }
  }

  return bfs(grid, starts, end);
}

const inputFile = join(__dirname, '..', 'input.txt');
const text = readFileSync(inputFile, 'utf-8');

console.log('Part 1:', part1(text));
console.log('Part 2:', part2(text));
