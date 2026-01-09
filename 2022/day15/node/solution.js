import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

function parseSensors(text) {
  const sensors = [];
  const pattern = /Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/;

  for (const line of text.split('\n')) {
    const match = line.match(pattern);
    const sx = parseInt(match[1]);
    const sy = parseInt(match[2]);
    const bx = parseInt(match[3]);
    const by = parseInt(match[4]);
    const dist = Math.abs(sx - bx) + Math.abs(sy - by);
    sensors.push({ sx, sy, bx, by, dist });
  }

  return sensors;
}

function mergeRanges(ranges) {
  if (ranges.length === 0) return [];

  ranges.sort((a, b) => a[0] - b[0]);
  const merged = [ranges[0]];

  for (let i = 1; i < ranges.length; i++) {
    const [start, end] = ranges[i];
    const last = merged[merged.length - 1];
    if (start <= last[1] + 1) {
      last[1] = Math.max(last[1], end);
    } else {
      merged.push([start, end]);
    }
  }

  return merged;
}

function getCoverageAtRow(sensors, row) {
  const ranges = [];

  for (const { sx, sy, dist } of sensors) {
    const rowDist = Math.abs(sy - row);
    if (rowDist > dist) continue;

    const xSpread = dist - rowDist;
    ranges.push([sx - xSpread, sx + xSpread]);
  }

  return mergeRanges(ranges);
}

function part1() {
  const sensors = parseSensors(input);
  const targetRow = 2000000;

  const ranges = getCoverageAtRow(sensors, targetRow);

  // Count total coverage
  let total = 0;
  for (const [start, end] of ranges) {
    total += end - start + 1;
  }

  // Subtract beacons on this row
  const beaconsOnRow = new Set();
  for (const { bx, by } of sensors) {
    if (by === targetRow) {
      beaconsOnRow.add(bx);
    }
  }

  return total - beaconsOnRow.size;
}

function part2() {
  const sensors = parseSensors(input);
  const maxCoord = 4000000;

  for (let row = 0; row <= maxCoord; row++) {
    const ranges = getCoverageAtRow(sensors, row);

    // Clip ranges to search area
    const clipped = [];
    for (const [start, end] of ranges) {
      if (end < 0 || start > maxCoord) continue;
      clipped.push([Math.max(0, start), Math.min(maxCoord, end)]);
    }

    const merged = mergeRanges(clipped);

    // Check if full row is covered
    if (merged.length === 1 && merged[0][0] === 0 && merged[0][1] === maxCoord) {
      continue;
    }

    // Found a gap
    let x;
    if (merged.length > 1) {
      x = merged[0][1] + 1;
    } else if (merged[0][0] > 0) {
      x = 0;
    } else {
      x = merged[0][1] + 1;
    }

    return BigInt(x) * 4000000n + BigInt(row);
  }

  return null;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2().toString());
