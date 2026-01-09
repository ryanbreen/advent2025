import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(filename) {
  return readFileSync(filename, 'utf-8')
    .trim()
    .split('\n');
}

function priority(char) {
  const code = char.charCodeAt(0);
  if (code >= 97) { // lowercase a-z
    return code - 96;
  } else { // uppercase A-Z
    return code - 64 + 26;
  }
}

function part1(rucksacks) {
  let total = 0;
  for (const rucksack of rucksacks) {
    const mid = rucksack.length / 2;
    const first = new Set(rucksack.slice(0, mid));
    const second = new Set(rucksack.slice(mid));

    for (const char of first) {
      if (second.has(char)) {
        total += priority(char);
        break;
      }
    }
  }
  return total;
}

function part2(rucksacks) {
  let total = 0;
  for (let i = 0; i < rucksacks.length; i += 3) {
    const set1 = new Set(rucksacks[i]);
    const set2 = new Set(rucksacks[i + 1]);
    const set3 = new Set(rucksacks[i + 2]);

    for (const char of set1) {
      if (set2.has(char) && set3.has(char)) {
        total += priority(char);
        break;
      }
    }
  }
  return total;
}

const inputFile = join(__dirname, '..', 'input.txt');
const rucksacks = parseInput(inputFile);

console.log('Part 1:', part1(rucksacks));
console.log('Part 2:', part2(rucksacks));
