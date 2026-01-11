import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

const numbers = input.split('\n');

function part1() {
  const numBits = numbers[0].length;
  let gamma = 0;

  for (let pos = 0; pos < numBits; pos++) {
    let ones = 0;
    for (const n of numbers) {
      if (n[pos] === '1') ones++;
    }
    const zeros = numbers.length - ones;

    if (ones >= zeros) {
      gamma |= (1 << (numBits - 1 - pos));
    }
  }

  // epsilon is bitwise NOT of gamma (within numBits)
  const epsilon = gamma ^ ((1 << numBits) - 1);

  return gamma * epsilon;
}

function findRating(nums, useMostCommon) {
  const numBits = nums[0].length;
  let candidates = [...nums];

  for (let pos = 0; pos < numBits && candidates.length > 1; pos++) {
    let ones = 0;
    for (const n of candidates) {
      if (n[pos] === '1') ones++;
    }
    const zeros = candidates.length - ones;

    let target;
    if (useMostCommon) {
      target = ones >= zeros ? '1' : '0';
    } else {
      target = zeros <= ones ? '0' : '1';
    }

    candidates = candidates.filter(n => n[pos] === target);
  }

  return parseInt(candidates[0], 2);
}

function part2() {
  const oxygen = findRating(numbers, true);
  const co2 = findRating(numbers, false);
  return oxygen * co2;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
