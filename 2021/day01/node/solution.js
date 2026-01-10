import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input - convert to integers
const depths = input.split('\n').map(Number);

// Part 1: Count the number of times a depth measurement increases from the previous
function part1() {
  let count = 0;
  for (let i = 1; i < depths.length; i++) {
    if (depths[i] > depths[i - 1]) {
      count++;
    }
  }
  return count;
}

// Part 2: Count increases in 3-measurement sliding window sums
function part2() {
  // Create sliding window sums of 3 consecutive measurements
  const windowSums = [];
  for (let i = 0; i < depths.length - 2; i++) {
    windowSums.push(depths[i] + depths[i + 1] + depths[i + 2]);
  }

  // Count how many times the sum increases
  let count = 0;
  for (let i = 1; i < windowSums.length; i++) {
    if (windowSums[i] > windowSums[i - 1]) {
      count++;
    }
  }
  return count;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
