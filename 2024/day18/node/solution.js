import { readFileSync } from 'fs';

function parseInput(filename) {
  const content = readFileSync(filename, 'utf-8');
  return content.trim().split('\n').map(line => {
    const [x, y] = line.split(',').map(Number);
    return [x, y];
  });
}

function bfs(corrupted, size = 71) {
  const start = [0, 0];
  const goal = [size - 1, size - 1];

  const key = (x, y) => `${x},${y}`;

  if (corrupted.has(key(...start)) || corrupted.has(key(...goal))) {
    return -1;
  }

  const queue = [[...start, 0]];
  const visited = new Set([key(...start)]);
  const directions = [[0, 1], [0, -1], [1, 0], [-1, 0]];

  let head = 0;
  while (head < queue.length) {
    const [x, y, steps] = queue[head++];

    if (x === goal[0] && y === goal[1]) {
      return steps;
    }

    for (const [dx, dy] of directions) {
      const nx = x + dx;
      const ny = y + dy;
      const nkey = key(nx, ny);

      if (nx >= 0 && nx < size && ny >= 0 && ny < size &&
          !visited.has(nkey) && !corrupted.has(nkey)) {
        visited.add(nkey);
        queue.push([nx, ny, steps + 1]);
      }
    }
  }

  return -1;
}

function part1(positions, numBytes = 1024, size = 71) {
  const corrupted = new Set();
  for (let i = 0; i < numBytes && i < positions.length; i++) {
    corrupted.add(`${positions[i][0]},${positions[i][1]}`);
  }
  return bfs(corrupted, size);
}

function part2(positions, size = 71) {
  // Binary search to find the first byte that blocks the path
  let left = 0;
  let right = positions.length;

  while (left < right) {
    const mid = Math.floor((left + right) / 2);
    const corrupted = new Set();
    for (let i = 0; i <= mid; i++) {
      corrupted.add(`${positions[i][0]},${positions[i][1]}`);
    }

    if (bfs(corrupted, size) === -1) {
      right = mid;
    } else {
      left = mid + 1;
    }
  }

  const [bx, by] = positions[left];
  return `${bx},${by}`;
}

const positions = parseInput('../input.txt');

console.log('Part 1:', part1(positions));
console.log('Part 2:', part2(positions));
