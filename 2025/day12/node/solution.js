import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

/**
 * Day 12: Christmas Tree Farm - Polyomino Packing
 *
 * The solution checks if presents (polyominoes) can fit into rectangular regions.
 * For this problem, the constraint is simply: total cells needed <= available cells.
 */

const parseInput = (text) => {
  const sections = text.split('\n\n');
  const shapes = new Map();
  const regions = [];

  for (const section of sections) {
    const lines = section.trim().split('\n');
    const [firstLine] = lines;

    if (firstLine.includes(':') && !firstLine.includes('x')) {
      // Shape definition
      const idx = parseInt(firstLine.replace(':', ''), 10);
      const shapeLines = lines.slice(1);
      const cellCount = shapeLines.reduce((sum, line) =>
        sum + (line.match(/#/g)?.length ?? 0), 0);
      shapes.set(idx, cellCount);
    } else {
      // Region definitions
      lines
        .filter(line => line.includes('x'))
        .forEach(line => {
          const [dims, countsStr] = line.split(':');
          const [w, h] = dims.trim().split('x').map(Number);
          const counts = countsStr.trim().split(/\s+/).map(Number);
          regions.push({ w, h, counts });
        });
    }
  }

  return { shapes, regions };
};

const canFitRegion = (width, height, counts, shapeSizes) => {
  const totalCellsNeeded = counts.reduce((sum, count, idx) =>
    sum + count * shapeSizes.get(idx), 0);
  const available = width * height;
  return totalCellsNeeded <= available;
};

// Part 1
const part1 = () => {
  const { shapes, regions } = parseInput(input);
  return regions.filter(({ w, h, counts }) =>
    canFitRegion(w, h, counts, shapes)
  ).length;
};

// Part 2 - just a button click, no computation
const part2 = () => 0;

console.log('Part 1:', part1());
console.log('Part 2:', part2());
