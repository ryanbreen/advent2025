import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\n');

// Part 1
function part1() {
  const leftList = [];
  const rightList = [];

  for (const line of lines) {
    const [left, right] = line.split(/\s+/).map(Number);
    leftList.push(left);
    rightList.push(right);
  }

  // Sort both lists
  leftList.sort((a, b) => a - b);
  rightList.sort((a, b) => a - b);

  // Calculate total distance
  let totalDistance = 0;
  for (let i = 0; i < leftList.length; i++) {
    totalDistance += Math.abs(leftList[i] - rightList[i]);
  }

  return totalDistance;
}

// Part 2
function part2() {
  const leftList = [];
  const rightList = [];

  for (const line of lines) {
    const [left, right] = line.split(/\s+/).map(Number);
    leftList.push(left);
    rightList.push(right);
  }

  // Count occurrences in right list
  const rightCounts = new Map();
  for (const num of rightList) {
    rightCounts.set(num, (rightCounts.get(num) || 0) + 1);
  }

  // Calculate similarity score
  let similarityScore = 0;
  for (const num of leftList) {
    similarityScore += num * (rightCounts.get(num) || 0);
  }

  return similarityScore;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
