import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

const WIDTH = 101;
const HEIGHT = 103;

function parseRobots(text) {
  const robots = [];
  const regex = /p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/;

  for (const line of text.split('\n')) {
    const match = line.match(regex);
    if (match) {
      robots.push({
        px: parseInt(match[1]),
        py: parseInt(match[2]),
        vx: parseInt(match[3]),
        vy: parseInt(match[4])
      });
    }
  }
  return robots;
}

function simulate(robots, seconds) {
  return robots.map(({ px, py, vx, vy }) => ({
    x: ((px + vx * seconds) % WIDTH + WIDTH) % WIDTH,
    y: ((py + vy * seconds) % HEIGHT + HEIGHT) % HEIGHT
  }));
}

function countQuadrants(positions) {
  const midX = Math.floor(WIDTH / 2);  // 50
  const midY = Math.floor(HEIGHT / 2); // 51

  let q1 = 0, q2 = 0, q3 = 0, q4 = 0;

  for (const { x, y } of positions) {
    if (x === midX || y === midY) continue;

    if (x < midX && y < midY) q1++;
    else if (x > midX && y < midY) q2++;
    else if (x < midX && y > midY) q3++;
    else q4++;
  }

  return [q1, q2, q3, q4];
}

function part1() {
  const robots = parseRobots(input);
  const positions = simulate(robots, 100);
  const [q1, q2, q3, q4] = countQuadrants(positions);
  return q1 * q2 * q3 * q4;
}

function part2() {
  const robots = parseRobots(input);

  // Look for a frame with a long horizontal line (Christmas tree pattern)
  for (let seconds = 1; seconds <= WIDTH * HEIGHT; seconds++) {
    const positions = simulate(robots, seconds);
    const posSet = new Set(positions.map(p => `${p.x},${p.y}`));

    // Look for a horizontal line of at least 20 consecutive robots
    for (let y = 0; y < HEIGHT; y++) {
      let maxConsecutive = 0;
      let consecutive = 0;

      for (let x = 0; x < WIDTH; x++) {
        if (posSet.has(`${x},${y}`)) {
          consecutive++;
          maxConsecutive = Math.max(maxConsecutive, consecutive);
        } else {
          consecutive = 0;
        }
      }

      if (maxConsecutive >= 20) {
        return seconds;
      }
    }
  }

  return -1;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
