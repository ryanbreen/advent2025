import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(filename) {
  return readFileSync(filename, 'utf-8')
    .trim()
    .split('\n')
    .map(line => {
      const [left, right] = line.split(',');
      const [a1, b1] = left.split('-').map(Number);
      const [a2, b2] = right.split('-').map(Number);
      return [a1, b1, a2, b2];
    });
}

function fullyContains(a1, b1, a2, b2) {
  return (a1 <= a2 && b1 >= b2) || (a2 <= a1 && b2 >= b1);
}

function overlaps(a1, b1, a2, b2) {
  return a1 <= b2 && a2 <= b1;
}

function part1(pairs) {
  return pairs.filter(([a1, b1, a2, b2]) => fullyContains(a1, b1, a2, b2)).length;
}

function part2(pairs) {
  return pairs.filter(([a1, b1, a2, b2]) => overlaps(a1, b1, a2, b2)).length;
}

const inputFile = join(__dirname, '..', 'input.txt');
const pairs = parseInput(inputFile);

console.log('Part 1:', part1(pairs));
console.log('Part 2:', part2(pairs));
