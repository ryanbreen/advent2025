import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

function parseInput(text) {
  const valves = {};
  const tunnels = {};
  const pattern = /Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)/;

  for (const line of text.split('\n')) {
    const match = line.match(pattern);
    const [, name, rate, neighbors] = match;
    valves[name] = parseInt(rate);
    tunnels[name] = neighbors.split(', ');
  }

  return { valves, tunnels };
}

function computeDistances(valves, tunnels) {
  const relevant = ['AA', ...Object.keys(valves).filter(v => valves[v] > 0)];
  const distances = {};

  for (const start of relevant) {
    distances[start] = {};
    const queue = [[start, 0]];
    const visited = new Set([start]);

    while (queue.length > 0) {
      const [curr, dist] = queue.shift();
      if (relevant.includes(curr) && curr !== start) {
        distances[start][curr] = dist;
      }

      for (const neighbor of tunnels[curr]) {
        if (!visited.has(neighbor)) {
          visited.add(neighbor);
          queue.push([neighbor, dist + 1]);
        }
      }
    }
  }

  return distances;
}

function part1() {
  const { valves, tunnels } = parseInput(input);
  const distances = computeDistances(valves, tunnels);
  const valuable = Object.keys(valves).filter(v => valves[v] > 0);

  const memo = new Map();

  function dfs(pos, timeLeft, openedMask) {
    if (timeLeft <= 0) return 0;

    const key = `${pos},${timeLeft},${openedMask}`;
    if (memo.has(key)) return memo.get(key);

    let best = 0;
    for (let i = 0; i < valuable.length; i++) {
      if (openedMask & (1 << i)) continue;

      const nextValve = valuable[i];
      const timeCost = distances[pos][nextValve] + 1;
      if (timeCost < timeLeft) {
        const newTime = timeLeft - timeCost;
        const pressure = valves[nextValve] * newTime;
        best = Math.max(best, pressure + dfs(nextValve, newTime, openedMask | (1 << i)));
      }
    }

    memo.set(key, best);
    return best;
  }

  return dfs('AA', 30, 0);
}

function part2() {
  const { valves, tunnels } = parseInput(input);
  const distances = computeDistances(valves, tunnels);
  const valuable = Object.keys(valves).filter(v => valves[v] > 0);
  const n = valuable.length;

  // Compute max pressure for each subset
  function maxPressureForSubset(subsetMask) {
    const memo = new Map();

    function dfs(pos, timeLeft, openedMask) {
      if (timeLeft <= 0) return 0;

      const key = `${pos},${timeLeft},${openedMask}`;
      if (memo.has(key)) return memo.get(key);

      let best = 0;
      for (let i = 0; i < n; i++) {
        if (!(subsetMask & (1 << i))) continue; // Not in our subset
        if (openedMask & (1 << i)) continue; // Already opened

        const nextValve = valuable[i];
        const timeCost = distances[pos][nextValve] + 1;
        if (timeCost < timeLeft) {
          const newTime = timeLeft - timeCost;
          const pressure = valves[nextValve] * newTime;
          best = Math.max(best, pressure + dfs(nextValve, newTime, openedMask | (1 << i)));
        }
      }

      memo.set(key, best);
      return best;
    }

    return dfs('AA', 26, 0);
  }

  // Compute max pressure for all subsets
  const maxScores = new Array(1 << n);
  for (let mask = 0; mask < (1 << n); mask++) {
    maxScores[mask] = maxPressureForSubset(mask);
  }

  // Find best partition
  let best = 0;
  const fullMask = (1 << n) - 1;
  for (let mask = 0; mask < (1 << n); mask++) {
    const complement = fullMask ^ mask;
    if (mask <= complement) {
      best = Math.max(best, maxScores[mask] + maxScores[complement]);
    }
  }

  return best;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
