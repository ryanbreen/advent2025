import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input - space-separated numbers as BigInt
const stones = input.split(' ').map(BigInt);

// Memoization cache: "value,blinks" -> count
const cache = new Map();

function countStones(value, blinks) {
  if (blinks === 0) return 1n;

  const key = `${value},${blinks}`;
  if (cache.has(key)) return cache.get(key);

  let result;

  // Rule 1: 0 becomes 1
  if (value === 0n) {
    result = countStones(1n, blinks - 1);
  }
  // Rule 2: Even number of digits -> split
  else {
    const s = value.toString();
    if (s.length % 2 === 0) {
      const mid = s.length / 2;
      const left = BigInt(s.slice(0, mid));
      const right = BigInt(s.slice(mid));
      result = countStones(left, blinks - 1) + countStones(right, blinks - 1);
    }
    // Rule 3: Multiply by 2024
    else {
      result = countStones(value * 2024n, blinks - 1);
    }
  }

  cache.set(key, result);
  return result;
}

// Part 1
function part1() {
  let total = 0n;
  for (const stone of stones) {
    total += countStones(stone, 25);
  }
  return total;
}

// Part 2
function part2() {
  let total = 0n;
  for (const stone of stones) {
    total += countStones(stone, 75);
  }
  return total;
}

console.log('Part 1:', part1().toString());
console.log('Part 2:', part2().toString());
