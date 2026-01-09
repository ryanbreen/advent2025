import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

function parsePaths(text) {
  const rocks = new Set();

  for (const line of text.split('\n')) {
    const points = line.split(' -> ').map(p => {
      const [x, y] = p.split(',').map(Number);
      return { x, y };
    });

    for (let i = 0; i < points.length - 1; i++) {
      const { x: x1, y: y1 } = points[i];
      const { x: x2, y: y2 } = points[i + 1];

      if (x1 === x2) {
        // Vertical line
        for (let y = Math.min(y1, y2); y <= Math.max(y1, y2); y++) {
          rocks.add(`${x1},${y}`);
        }
      } else {
        // Horizontal line
        for (let x = Math.min(x1, x2); x <= Math.max(x1, x2); x++) {
          rocks.add(`${x},${y1}`);
        }
      }
    }
  }

  return rocks;
}

function simulateSand(blocked, maxY, floor = false) {
  let x = 500, y = 0;

  while (true) {
    // Check if sand has fallen below all rocks (into abyss)
    if (!floor && y > maxY) {
      return null;
    }

    // Check floor for Part 2
    if (floor && y + 1 === maxY + 2) {
      return `${x},${y}`;
    }

    // Try to move down
    if (!blocked.has(`${x},${y + 1}`)) {
      y++;
    }
    // Try to move down-left
    else if (!blocked.has(`${x - 1},${y + 1}`)) {
      x--;
      y++;
    }
    // Try to move down-right
    else if (!blocked.has(`${x + 1},${y + 1}`)) {
      x++;
      y++;
    }
    // Sand comes to rest
    else {
      return `${x},${y}`;
    }
  }
}

function part1() {
  const rocks = parsePaths(input);
  let maxY = 0;
  for (const pos of rocks) {
    const y = parseInt(pos.split(',')[1]);
    if (y > maxY) maxY = y;
  }

  const blocked = new Set(rocks);
  let count = 0;

  while (true) {
    const pos = simulateSand(blocked, maxY);
    if (pos === null) break;
    blocked.add(pos);
    count++;
  }

  return count;
}

function part2() {
  const rocks = parsePaths(input);
  let maxY = 0;
  for (const pos of rocks) {
    const y = parseInt(pos.split(',')[1]);
    if (y > maxY) maxY = y;
  }

  const blocked = new Set(rocks);
  let count = 0;

  while (true) {
    const pos = simulateSand(blocked, maxY, true);
    blocked.add(pos);
    count++;
    if (pos === '500,0') break;
  }

  return count;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
