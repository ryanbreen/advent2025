import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseGrid(lines) {
  return lines.map(line => line.split('').map(Number));
}

function isVisible(grid, row, col) {
  const rows = grid.length;
  const cols = grid[0].length;
  const height = grid[row][col];

  // Check from left
  let visibleLeft = true;
  for (let c = 0; c < col; c++) {
    if (grid[row][c] >= height) {
      visibleLeft = false;
      break;
    }
  }

  // Check from right
  let visibleRight = true;
  for (let c = col + 1; c < cols; c++) {
    if (grid[row][c] >= height) {
      visibleRight = false;
      break;
    }
  }

  // Check from top
  let visibleTop = true;
  for (let r = 0; r < row; r++) {
    if (grid[r][col] >= height) {
      visibleTop = false;
      break;
    }
  }

  // Check from bottom
  let visibleBottom = true;
  for (let r = row + 1; r < rows; r++) {
    if (grid[r][col] >= height) {
      visibleBottom = false;
      break;
    }
  }

  return visibleLeft || visibleRight || visibleTop || visibleBottom;
}

function scenicScore(grid, row, col) {
  const rows = grid.length;
  const cols = grid[0].length;
  const height = grid[row][col];

  // Left
  let left = 0;
  for (let c = col - 1; c >= 0; c--) {
    left++;
    if (grid[row][c] >= height) break;
  }

  // Right
  let right = 0;
  for (let c = col + 1; c < cols; c++) {
    right++;
    if (grid[row][c] >= height) break;
  }

  // Up
  let up = 0;
  for (let r = row - 1; r >= 0; r--) {
    up++;
    if (grid[r][col] >= height) break;
  }

  // Down
  let down = 0;
  for (let r = row + 1; r < rows; r++) {
    down++;
    if (grid[r][col] >= height) break;
  }

  return left * right * up * down;
}

function part1(grid) {
  const rows = grid.length;
  const cols = grid[0].length;
  let count = 0;
  for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
      if (isVisible(grid, r, c)) {
        count++;
      }
    }
  }
  return count;
}

function part2(grid) {
  const rows = grid.length;
  const cols = grid[0].length;
  let maxScore = 0;
  for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
      const score = scenicScore(grid, r, c);
      if (score > maxScore) {
        maxScore = score;
      }
    }
  }
  return maxScore;
}

const inputFile = join(__dirname, '..', 'input.txt');
const lines = readFileSync(inputFile, 'utf-8').trim().split('\n');
const grid = parseGrid(lines);

console.log('Part 1:', part1(grid));
console.log('Part 2:', part2(grid));
