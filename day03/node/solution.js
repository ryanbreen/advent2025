import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\n');

// Part 1
function part1() {
  let totalJoltage = 0;

  for (const line of lines) {
    // For each bank, find the maximum two-digit number
    // We pick two positions i < j and form digit[i]*10 + digit[j]

    // Efficient approach: For each position i, we want to know the maximum digit
    // that comes after it (at position j > i)
    const digits = line.split('').map(Number);
    const n = digits.length;

    // Precompute max suffix: maxSuffix[i] = max(digits[i], digits[i+1], ..., digits[n-1])
    const maxSuffix = new Array(n);
    maxSuffix[n - 1] = digits[n - 1];
    for (let i = n - 2; i >= 0; i--) {
      maxSuffix[i] = Math.max(digits[i], maxSuffix[i + 1]);
    }

    // Find maximum joltage for this bank
    let maxJoltage = 0;
    for (let i = 0; i < n - 1; i++) {
      // Pick digit at position i as tens digit
      // Pick the maximum digit after position i as ones digit
      const joltage = digits[i] * 10 + maxSuffix[i + 1];
      maxJoltage = Math.max(maxJoltage, joltage);
    }

    totalJoltage += maxJoltage;
  }

  return totalJoltage;
}

// Part 2
function part2() {
  let totalJoltage = 0n; // Use BigInt for large numbers

  for (const line of lines) {
    const digits = line.split('').map(Number);
    const n = digits.length;
    const k = 12; // Select exactly 12 batteries

    // Greedy algorithm to select k digits to form maximum number
    // For each position i in result (0 to k-1):
    //   Search from current_pos to (n - (k - i)) for the maximum digit
    //   This ensures we leave enough digits for the remaining positions

    let result = '';
    let currentPos = 0;

    for (let i = 0; i < k; i++) {
      // How many more digits do we need after this one?
      const remaining = k - i - 1;
      // Latest position we can search to while still having enough digits left
      const searchEnd = n - remaining;

      // Find the maximum digit in the range [currentPos, searchEnd)
      let maxDigit = -1;
      let maxPos = currentPos;

      for (let j = currentPos; j < searchEnd; j++) {
        if (digits[j] > maxDigit) {
          maxDigit = digits[j];
          maxPos = j;
        }
      }

      // Add the maximum digit to our result
      result += maxDigit;
      // Move current position past the selected digit
      currentPos = maxPos + 1;
    }

    // Convert to BigInt and add to total
    totalJoltage += BigInt(result);
  }

  return totalJoltage;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2().toString());
