import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

function parseInput() {
  return input.split('\n').map(line => {
    const [start, end] = line.split(' -> ');
    const [x1, y1] = start.split(',').map(Number);
    const [x2, y2] = end.split(',').map(Number);
    return { x1, y1, x2, y2 };
  });
}

function sign(x) {
  if (x > 0) return 1;
  if (x < 0) return -1;
  return 0;
}

function countOverlaps(lines, includeDiagonals = false) {
  const grid = new Map();

  for (const { x1, y1, x2, y2 } of lines) {
    const dx = sign(x2 - x1);
    const dy = sign(y2 - y1);

    // Skip diagonals in part 1
    if (!includeDiagonals && dx !== 0 && dy !== 0) {
      continue;
    }

    let x = x1, y = y1;
    while (true) {
      const key = `${x},${y}`;
      grid.set(key, (grid.get(key) || 0) + 1);
      if (x === x2 && y === y2) break;
      x += dx;
      y += dy;
    }
  }

  let count = 0;
  for (const v of grid.values()) {
    if (v >= 2) count++;
  }
  return count;
}

function part1() {
  const lines = parseInput();
  return countOverlaps(lines, false);
}

function part2() {
  const lines = parseInput();
  return countOverlaps(lines, true);
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
