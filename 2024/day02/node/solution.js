import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\n');

function isSafe(levels) {
  if (levels.length < 2) return true;

  // Calculate all differences between adjacent levels
  const diffs = [];
  for (let i = 0; i < levels.length - 1; i++) {
    diffs.push(levels[i + 1] - levels[i]);
  }

  // Check if all increasing (1-3) or all decreasing (-3 to -1)
  const allIncreasing = diffs.every(d => d >= 1 && d <= 3);
  const allDecreasing = diffs.every(d => d >= -3 && d <= -1);

  return allIncreasing || allDecreasing;
}

// Part 1
function part1() {
  let safeCount = 0;
  for (const line of lines) {
    const levels = line.split(' ').map(Number);
    if (isSafe(levels)) {
      safeCount++;
    }
  }
  return safeCount;
}

// Part 2
function part2() {
  let safeCount = 0;
  for (const line of lines) {
    const levels = line.split(' ').map(Number);

    // Check if already safe
    if (isSafe(levels)) {
      safeCount++;
      continue;
    }

    // Try removing each level one at a time
    let foundSafe = false;
    for (let i = 0; i < levels.length; i++) {
      const modified = [...levels.slice(0, i), ...levels.slice(i + 1)];
      if (isSafe(modified)) {
        safeCount++;
        foundSafe = true;
        break;
      }
    }
  }
  return safeCount;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
