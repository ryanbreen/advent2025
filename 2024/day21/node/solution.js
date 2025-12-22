import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Keypad layouts - positions as [row, col]
const NUMERIC = {
  '7': [0, 0], '8': [0, 1], '9': [0, 2],
  '4': [1, 0], '5': [1, 1], '6': [1, 2],
  '1': [2, 0], '2': [2, 1], '3': [2, 2],
  '0': [3, 1], 'A': [3, 2],
};
const NUMERIC_GAP = [3, 0];

const DIRECTIONAL = {
  '^': [0, 1], 'A': [0, 2],
  '<': [1, 0], 'v': [1, 1], '>': [1, 2],
};
const DIRECTIONAL_GAP = [0, 0];

// Find all shortest paths from start to end, avoiding gap
function shortestPaths(keypad, gap, start, end) {
  const [sr, sc] = keypad[start];
  const [er, ec] = keypad[end];
  const paths = [];

  function dfs(r, c, path) {
    if (r === gap[0] && c === gap[1]) return;
    if (r === er && c === ec) {
      paths.push(path);
      return;
    }
    // Move vertically toward target
    if (r < er) dfs(r + 1, c, path + 'v');
    else if (r > er) dfs(r - 1, c, path + '^');
    // Move horizontally toward target
    if (c < ec) dfs(r, c + 1, path + '>');
    else if (c > ec) dfs(r, c - 1, path + '<');
  }

  dfs(sr, sc, '');
  return paths.length > 0 ? paths : [''];
}

// Memoization cache
const cache = new Map();

// Minimum presses needed to move from fromChar to toChar and press, at given depth
function minPressesForMove(fromChar, toChar, depth, isNumeric) {
  const cacheKey = `${fromChar}|${toChar}|${depth}|${isNumeric}`;
  if (cache.has(cacheKey)) return cache.get(cacheKey);

  const keypad = isNumeric ? NUMERIC : DIRECTIONAL;
  const gap = isNumeric ? NUMERIC_GAP : DIRECTIONAL_GAP;
  const paths = shortestPaths(keypad, gap, fromChar, toChar);

  let result;
  if (depth === 0) {
    // At human level, just return path length + 1 for 'A' press
    result = Math.min(...paths.map(p => p.length)) + 1;
  } else {
    let best = Infinity;
    for (const path of paths) {
      // Need to type path + 'A' on the directional keypad above
      const sequence = path + 'A';
      let cost = 0;
      let current = 'A';
      for (const char of sequence) {
        cost += minPressesForMove(current, char, depth - 1, false);
        current = char;
      }
      best = Math.min(best, cost);
    }
    result = best;
  }

  cache.set(cacheKey, result);
  return result;
}

// Compute minimum presses to type code on numeric keypad with given robot depth
function solveCode(code, depth) {
  let total = 0;
  let current = 'A';
  for (const char of code) {
    total += minPressesForMove(current, char, depth, true);
    current = char;
  }
  return total;
}

// Compute complexity: length * numeric part of code
function complexity(code, length) {
  const numericPart = parseInt(code.replace(/A/g, ''), 10);
  return length * numericPart;
}

// Part 1: 2 intermediate robots
function part1(codes) {
  let total = 0;
  for (const code of codes) {
    const length = solveCode(code, 2);
    total += complexity(code, length);
  }
  return total;
}

// Part 2: 25 intermediate robots
function part2(codes) {
  cache.clear(); // Clear cache for fresh calculation
  let total = 0n;
  for (const code of codes) {
    const length = BigInt(solveCode(code, 25));
    const numericPart = BigInt(parseInt(code.replace(/A/g, ''), 10));
    total += length * numericPart;
  }
  return total;
}

const codes = input.split('\n').filter(line => line.trim());
console.log('Part 1:', part1(codes));
console.log('Part 2:', part2(codes).toString());
