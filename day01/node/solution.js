import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\n');

// Part 1
function part1() {
  let position = 50; // Starting position
  let zerosCount = 0;

  for (const line of lines) {
    const direction = line[0]; // 'L' or 'R'
    const distance = parseInt(line.slice(1));

    if (direction === 'L') {
      // Moving left (toward lower numbers)
      position = (position - distance) % 100;
      if (position < 0) position += 100;
    } else {
      // Moving right (toward higher numbers)
      position = (position + distance) % 100;
    }

    // Check if we landed on 0
    if (position === 0) {
      zerosCount++;
    }
  }

  return zerosCount;
}

// Part 2
function part2() {
  let position = 50; // Starting position
  let zerosCount = 0;

  for (const line of lines) {
    const direction = line[0]; // 'L' or 'R'
    const distance = parseInt(line.slice(1));

    if (direction === 'L') {
      // Moving left (toward lower numbers)
      // Going left from position P by distance D
      // We hit 0 when we've moved P clicks (since we go P, P-1, ..., 1, 0)
      // Then every 100 clicks after that, we hit 0 again

      if (distance >= position && position > 0) {
        // We will pass through 0 at least once
        // First time: after 'position' clicks, we're at 0
        zerosCount++; // Count the first crossing at position 0

        // After the first crossing, how many more times?
        const remainingAfterFirstZero = distance - position;
        zerosCount += Math.floor(remainingAfterFirstZero / 100);
      } else if (position === 0 && distance > 0) {
        // Starting at 0, going left means we immediately leave 0
        // We come back to 0 every 100 clicks
        zerosCount += Math.floor(distance / 100);
      }

      // Calculate final position
      position = (position - distance) % 100;
      if (position < 0) position += 100;
    } else {
      // Moving right (toward higher numbers)
      // Going right from position P by distance D
      // We hit 0 when we've moved (100 - P) clicks
      // Then every 100 clicks after that, we hit 0 again

      if (distance >= 100 - position && position > 0) {
        // We will pass through 0 at least once
        // First time: after (100 - position) clicks, we're at 0
        zerosCount++; // Count the first crossing at position 0

        // After the first crossing, how many more times?
        const remainingAfterFirstZero = distance - (100 - position);
        zerosCount += Math.floor(remainingAfterFirstZero / 100);
      } else if (position === 0 && distance > 0) {
        // Starting at 0, going right means we immediately leave 0
        // We come back to 0 every 100 clicks
        zerosCount += Math.floor(distance / 100);
      }

      // Calculate final position
      position = (position + distance) % 100;
    }
  }

  return zerosCount;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
