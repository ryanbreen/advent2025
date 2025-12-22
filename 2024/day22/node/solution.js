import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const initialSecrets = input.split('\n').map(line => parseInt(line.trim()));

/**
 * Generate the next secret number using mix and prune operations.
 * Mix: XOR the value into the secret
 * Prune: Modulo 16777216 (which is 2^24, so we can use bitwise AND)
 */
function nextSecret(secret) {
  // Step 1: multiply by 64, mix, prune
  secret ^= (secret << 6);  // * 64 = << 6
  secret &= 0xFFFFFF;       // % 16777216 = & (2^24 - 1)

  // Step 2: divide by 32, mix, prune
  secret ^= (secret >> 5);  // // 32 = >> 5
  secret &= 0xFFFFFF;

  // Step 3: multiply by 2048, mix, prune
  secret ^= (secret << 11); // * 2048 = << 11
  secret &= 0xFFFFFF;

  return secret;
}

/**
 * Generate a sequence of secret numbers.
 */
function generateSecrets(initial, count) {
  const secrets = [initial];
  let secret = initial;
  for (let i = 0; i < count; i++) {
    secret = nextSecret(secret);
    secrets.push(secret);
  }
  return secrets;
}

// Part 1: Sum of the 2000th secret number for each buyer
function part1() {
  let total = 0;
  for (const initial of initialSecrets) {
    let secret = initial;
    for (let i = 0; i < 2000; i++) {
      secret = nextSecret(secret);
    }
    total += secret;
  }
  return total;
}

// Part 2: Find the best sequence of 4 price changes to maximize bananas
function part2() {
  // Map from sequence (as string) -> total bananas
  const sequenceTotals = new Map();

  for (const initial of initialSecrets) {
    // Generate 2001 secrets (initial + 2000 new)
    const secrets = generateSecrets(initial, 2000);
    const prices = secrets.map(s => s % 10);

    // Calculate changes
    const changes = [];
    for (let i = 0; i < prices.length - 1; i++) {
      changes.push(prices[i + 1] - prices[i]);
    }

    // Track first occurrence of each 4-change sequence for this buyer
    const seen = new Set();
    for (let i = 0; i <= changes.length - 4; i++) {
      const seq = `${changes[i]},${changes[i + 1]},${changes[i + 2]},${changes[i + 3]}`;
      if (!seen.has(seq)) {
        seen.add(seq);
        // Price we get is after these 4 changes
        const currentTotal = sequenceTotals.get(seq) || 0;
        sequenceTotals.set(seq, currentTotal + prices[i + 4]);
      }
    }
  }

  return Math.max(...sequenceTotals.values());
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
