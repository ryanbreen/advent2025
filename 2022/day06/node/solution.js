import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function findMarker(data, windowSize) {
  for (let i = windowSize; i <= data.length; i++) {
    const window = data.slice(i - windowSize, i);
    if (new Set(window).size === windowSize) {
      return i;
    }
  }
  return -1;
}

function part1(data) {
  return findMarker(data, 4);
}

function part2(data) {
  return findMarker(data, 14);
}

const inputFile = join(__dirname, '..', 'input.txt');
const data = readFileSync(inputFile, 'utf-8').trim();

console.log('Part 1:', part1(data));
console.log('Part 2:', part2(data));
