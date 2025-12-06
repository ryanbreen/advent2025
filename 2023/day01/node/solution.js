import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();
const lines = input.split('\n');

const WORDS = {
  one: '1', two: '2', three: '3', four: '4', five: '5',
  six: '6', seven: '7', eight: '8', nine: '9'
};

const DIGIT_RE = /\d/;

function part1() {
  return lines.reduce((total, line) => {
    const digits = line.match(/\d/g);
    return total + (digits ? parseInt(digits[0] + digits.at(-1)) : 0);
  }, 0);
}

function part2() {
  return lines.reduce((total, line) => {
    const digits = [];

    for (let i = 0; i < line.length; i++) {
      if (DIGIT_RE.test(line[i])) {
        digits.push(line[i]);
      } else {
        for (const [word, digit] of Object.entries(WORDS)) {
          if (line.slice(i).startsWith(word)) {
            digits.push(digit);
            break;
          }
        }
      }
    }

    return total + (digits.length > 0 ? parseInt(digits[0] + digits.at(-1)) : 0);
  }, 0);
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
