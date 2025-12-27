import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

const lines = input.split('\n');

function parseRaces() {
  const times = lines[0].split(':')[1].trim().split(/\s+/).map(Number);
  const distances = lines[1].split(':')[1].trim().split(/\s+/).map(Number);
  return times.map((t, i) => [t, distances[i]]);
}

function countWaysToWin(time, record) {
  // If we hold button for t ms, we travel t * (time - t) mm.
  // We need: t * (time - t) > record
  // Solving: -t^2 + time*t - record > 0
  // Roots: t = (time ± sqrt(time^2 - 4*record)) / 2

  const discriminant = time * time - 4 * record;
  if (discriminant <= 0) return 0;

  const sqrtD = Math.sqrt(discriminant);
  const tLow = (time - sqrtD) / 2;
  const tHigh = (time + sqrtD) / 2;

  // We need integer values strictly between the roots
  const first = Math.floor(tLow) + 1;
  const last = Math.ceil(tHigh) - 1;

  if (last < first) return 0;
  return last - first + 1;
}

function part1() {
  const races = parseRaces();
  let result = 1;
  for (const [time, record] of races) {
    result *= countWaysToWin(time, record);
  }
  return result;
}

function part2() {
  const races = parseRaces();
  const time = parseInt(races.map(([t]) => t).join(''), 10);
  const record = parseInt(races.map(([, d]) => d).join(''), 10);
  return countWaysToWin(time, record);
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
