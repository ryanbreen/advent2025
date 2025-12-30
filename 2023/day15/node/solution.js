import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

const hashAlgorithm = (s) => {
    let current = 0;
    for (const c of s) {
        current = ((current + c.charCodeAt(0)) * 17) % 256;
    }
    return current;
};

const part1 = (steps) => steps.reduce((sum, step) => sum + hashAlgorithm(step), 0);

const part2 = (steps) => {
    const boxes = Array.from({ length: 256 }, () => []);

    for (const step of steps) {
        if (step.includes('=')) {
            const [label, focalStr] = step.split('=');
            const focal = parseInt(focalStr);
            const boxNum = hashAlgorithm(label);
            const idx = boxes[boxNum].findIndex(([l]) => l === label);
            if (idx >= 0) {
                boxes[boxNum][idx] = [label, focal];
            } else {
                boxes[boxNum].push([label, focal]);
            }
        } else {
            const label = step.slice(0, -1);
            const boxNum = hashAlgorithm(label);
            boxes[boxNum] = boxes[boxNum].filter(([l]) => l !== label);
        }
    }

    let total = 0;
    for (let boxNum = 0; boxNum < 256; boxNum++) {
        for (let slot = 0; slot < boxes[boxNum].length; slot++) {
            const [, focal] = boxes[boxNum][slot];
            total += (boxNum + 1) * (slot + 1) * focal;
        }
    }
    return total;
};

const inputFile = join(__dirname, '../input.txt');
const text = readFileSync(inputFile, 'utf-8').trim().replace(/\n/g, '');
const steps = text.split(',');

console.log(`Part 1: ${part1(steps)}`);
console.log(`Part 2: ${part2(steps)}`);
