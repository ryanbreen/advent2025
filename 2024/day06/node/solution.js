import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\n');
const grid = lines.map(line => line.split(''));

// Find starting position and direction
function findStart(grid) {
  const directions = {
    '^': [0, -1],
    'v': [0, 1],
    '<': [-1, 0],
    '>': [1, 0]
  };

  for (let y = 0; y < grid.length; y++) {
    for (let x = 0; x < grid[y].length; x++) {
      if (directions[grid[y][x]]) {
        return { x, y, dir: directions[grid[y][x]], symbol: grid[y][x] };
      }
    }
  }
  return null;
}

// Turn right 90 degrees
function turnRight(dir) {
  const [dx, dy] = dir;
  // Up (0,-1) -> Right (1,0)
  // Right (1,0) -> Down (0,1)
  // Down (0,1) -> Left (-1,0)
  // Left (-1,0) -> Up (0,-1)
  return [-dy, dx];
}

// Simulate guard patrol
function simulatePatrol(grid, detectLoop = false) {
  const start = findStart(grid);
  if (!start) return { count: 0, visited: new Set(), isLoop: false };

  let { x, y, dir } = start;
  const visited = new Set();
  visited.add(`${x},${y}`);

  const states = new Set(); // Track (x, y, dir) to detect loops
  if (detectLoop) {
    states.add(`${x},${y},${dir[0]},${dir[1]}`);
  }

  const height = grid.length;
  const width = grid[0].length;

  while (true) {
    // Calculate next position
    const [dx, dy] = dir;
    const nextX = x + dx;
    const nextY = y + dy;

    // Check if guard leaves the map
    if (nextX < 0 || nextX >= width || nextY < 0 || nextY >= height) {
      return { count: visited.size, visited, isLoop: false };
    }

    // Check if there's an obstacle ahead
    if (grid[nextY][nextX] === '#') {
      // Turn right
      dir = turnRight(dir);
    } else {
      // Move forward
      x = nextX;
      y = nextY;
      visited.add(`${x},${y}`);

      // Check for loop if requested
      if (detectLoop) {
        const state = `${x},${y},${dir[0]},${dir[1]}`;
        if (states.has(state)) {
          return { count: visited.size, visited, isLoop: true };
        }
        states.add(state);
      }
    }
  }
}

// Part 1
function part1() {
  return simulatePatrol(grid).count;
}

// Part 2
function part2() {
  const start = findStart(grid);
  if (!start) return 0;

  // First, get all positions visited in normal patrol
  const { visited } = simulatePatrol(grid);

  let loopCount = 0;

  // Try placing an obstruction at each visited position (except starting position)
  for (const posStr of visited) {
    const [px, py] = posStr.split(',').map(Number);

    // Skip the guard's starting position
    if (px === start.x && py === start.y) {
      continue;
    }

    // Skip if already an obstacle
    if (grid[py][px] === '#') {
      continue;
    }

    // Try placing an obstruction here
    grid[py][px] = '#';

    // Simulate and check if it creates a loop
    const result = simulatePatrol(grid, true);
    if (result.isLoop) {
      loopCount++;
    }

    // Remove the obstruction
    grid[py][px] = '.';
  }

  return loopCount;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
