import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const grid = input.split('\n');
const rows = grid.length;
const cols = grid[0].length;

// 8 directions: right, left, down, up, and 4 diagonals
const DIRECTIONS = [
  [0, 1],   // right
  [0, -1],  // left
  [1, 0],   // down
  [-1, 0],  // up
  [1, 1],   // down-right
  [1, -1],  // down-left
  [-1, 1],  // up-right
  [-1, -1], // up-left
];

// Part 1
function part1() {
  const target = "XMAS";
  let count = 0;

  for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
      for (const [dr, dc] of DIRECTIONS) {
        let found = true;
        for (let i = 0; i < target.length; i++) {
          const nr = r + dr * i;
          const nc = c + dc * i;
          if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
            found = false;
            break;
          }
          if (grid[nr][nc] !== target[i]) {
            found = false;
            break;
          }
        }
        if (found) count++;
      }
    }
  }

  return count;
}

// Part 2
function part2() {
  // Find X-MAS patterns: two MAS strings forming an X with A in the center
  // Each diagonal can be MAS or SAM
  let count = 0;

  // Check each possible center point (A must be in the middle)
  for (let r = 1; r < rows - 1; r++) {
    for (let c = 1; c < cols - 1; c++) {
      if (grid[r][c] !== 'A') continue;

      // Get the four corners
      const topLeft = grid[r - 1][c - 1];
      const topRight = grid[r - 1][c + 1];
      const bottomLeft = grid[r + 1][c - 1];
      const bottomRight = grid[r + 1][c + 1];

      // Check diagonal 1 (top-left to bottom-right): MAS or SAM
      const diag1Ok = (topLeft === 'M' && bottomRight === 'S') || (topLeft === 'S' && bottomRight === 'M');

      // Check diagonal 2 (top-right to bottom-left): MAS or SAM
      const diag2Ok = (topRight === 'M' && bottomLeft === 'S') || (topRight === 'S' && bottomLeft === 'M');

      if (diag1Ok && diag2Ok) count++;
    }
  }

  return count;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
