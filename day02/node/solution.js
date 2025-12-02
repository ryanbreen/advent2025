import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\n');

// Check if a number is invalid (pattern repeated twice)
function isInvalidID(num) {
  const str = num.toString();
  const len = str.length;

  // Must be even length to be repeated twice
  if (len % 2 !== 0) return false;

  const halfLen = len / 2;
  const firstHalf = str.substring(0, halfLen);
  const secondHalf = str.substring(halfLen);

  // Check if both halves are identical
  return firstHalf === secondHalf;
}

// Part 1
function part1() {
  // Parse the comma-separated ranges
  const ranges = input.split(',').map(range => {
    const [start, end] = range.split('-').map(Number);
    return { start, end };
  });

  let sum = 0;

  // Check each range for invalid IDs
  for (const { start, end } of ranges) {
    for (let id = start; id <= end; id++) {
      if (isInvalidID(id)) {
        sum += id;
      }
    }
  }

  return sum;
}

// Check if a number is invalid (pattern repeated at least twice)
function isInvalidIDPart2(num) {
  const str = num.toString();
  const len = str.length;

  // Try all possible pattern lengths from 1 to len/2
  for (let patternLen = 1; patternLen <= len / 2; patternLen++) {
    // Check if the string can be divided evenly by this pattern length
    if (len % patternLen === 0) {
      const pattern = str.substring(0, patternLen);
      const repetitions = len / patternLen;

      // Check if repeating the pattern creates the full string
      if (repetitions >= 2 && pattern.repeat(repetitions) === str) {
        return true;
      }
    }
  }

  return false;
}

// Part 2
function part2() {
  // Parse the comma-separated ranges
  const ranges = input.split(',').map(range => {
    const [start, end] = range.split('-').map(Number);
    return { start, end };
  });

  let sum = 0;

  // Check each range for invalid IDs
  for (const { start, end } of ranges) {
    for (let id = start; id <= end; id++) {
      if (isInvalidIDPart2(id)) {
        sum += id;
      }
    }
  }

  return sum;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
