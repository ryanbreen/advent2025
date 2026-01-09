import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// 6 directions: +x, -x, +y, -y, +z, -z
const DIRECTIONS = [
  [1, 0, 0], [-1, 0, 0],
  [0, 1, 0], [0, -1, 0],
  [0, 0, 1], [0, 0, -1]
];

function parseInput(text) {
  const cubes = new Set();
  for (const line of text.split('\n')) {
    const [x, y, z] = line.split(',').map(Number);
    cubes.add(`${x},${y},${z}`);
  }
  return cubes;
}

function getCubeCoords(text) {
  const coords = [];
  for (const line of text.split('\n')) {
    const [x, y, z] = line.split(',').map(Number);
    coords.push([x, y, z]);
  }
  return coords;
}

function part1() {
  const cubes = parseInput(input);
  const coords = getCubeCoords(input);
  let surfaceArea = 0;

  for (const [x, y, z] of coords) {
    for (const [dx, dy, dz] of DIRECTIONS) {
      if (!cubes.has(`${x + dx},${y + dy},${z + dz}`)) {
        surfaceArea++;
      }
    }
  }

  return surfaceArea;
}

function part2() {
  const cubes = parseInput(input);
  const coords = getCubeCoords(input);

  // Find bounding box with 1 unit padding
  let minX = Infinity, maxX = -Infinity;
  let minY = Infinity, maxY = -Infinity;
  let minZ = Infinity, maxZ = -Infinity;

  for (const [x, y, z] of coords) {
    minX = Math.min(minX, x);
    maxX = Math.max(maxX, x);
    minY = Math.min(minY, y);
    maxY = Math.max(maxY, y);
    minZ = Math.min(minZ, z);
    maxZ = Math.max(maxZ, z);
  }

  minX--; maxX++;
  minY--; maxY++;
  minZ--; maxZ++;

  // BFS to find all exterior air cells
  const exterior = new Set();
  const queue = [[minX, minY, minZ]];
  exterior.add(`${minX},${minY},${minZ}`);

  while (queue.length > 0) {
    const [x, y, z] = queue.shift();

    for (const [dx, dy, dz] of DIRECTIONS) {
      const nx = x + dx;
      const ny = y + dy;
      const nz = z + dz;

      // Stay within bounds
      if (nx < minX || nx > maxX || ny < minY || ny > maxY || nz < minZ || nz > maxZ) {
        continue;
      }

      const key = `${nx},${ny},${nz}`;

      // Skip cubes and already visited
      if (cubes.has(key) || exterior.has(key)) {
        continue;
      }

      exterior.add(key);
      queue.push([nx, ny, nz]);
    }
  }

  // Count faces touching exterior air
  let surfaceArea = 0;
  for (const [x, y, z] of coords) {
    for (const [dx, dy, dz] of DIRECTIONS) {
      if (exterior.has(`${x + dx},${y + dy},${z + dz}`)) {
        surfaceArea++;
      }
    }
  }

  return surfaceArea;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
