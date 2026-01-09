import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseFilesystem(lines) {
  const path = [];
  const dirSizes = new Map();

  for (const line of lines) {
    if (line.startsWith('$ cd')) {
      const target = line.slice(5);
      if (target === '/') {
        path.length = 0;
        path.push('/');
      } else if (target === '..') {
        path.pop();
      } else {
        path.push(target);
      }
    } else if (line.startsWith('$ ls') || line.startsWith('dir ')) {
      continue;
    } else {
      // It's a file with size
      const size = parseInt(line.split(' ')[0], 10);
      // Add size to current directory and all parent directories
      for (let i = 0; i < path.length; i++) {
        const dirPath = path.slice(0, i + 1).join('/') || '/';
        dirSizes.set(dirPath, (dirSizes.get(dirPath) || 0) + size);
      }
    }
  }

  return dirSizes;
}

function part1(dirSizes) {
  let sum = 0;
  for (const size of dirSizes.values()) {
    if (size <= 100000) {
      sum += size;
    }
  }
  return sum;
}

function part2(dirSizes) {
  const totalSpace = 70000000;
  const neededSpace = 30000000;
  const usedSpace = dirSizes.get('/');
  const freeSpace = totalSpace - usedSpace;
  const needToFree = neededSpace - freeSpace;

  let smallest = Infinity;
  for (const size of dirSizes.values()) {
    if (size >= needToFree && size < smallest) {
      smallest = size;
    }
  }
  return smallest;
}

const inputFile = join(__dirname, '..', 'input.txt');
const lines = readFileSync(inputFile, 'utf-8').trim().split('\n');
const dirSizes = parseFilesystem(lines);

console.log('Part 1:', part1(dirSizes));
console.log('Part 2:', part2(dirSizes));
