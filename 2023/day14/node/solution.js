import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

const parseInput = (text) => text.trim().split('\n').map(line => [...line]);

const tiltNorth = (grid) => {
    const rows = grid.length;
    const cols = grid[0].length;
    for (let col = 0; col < cols; col++) {
        let writePos = 0;
        for (let row = 0; row < rows; row++) {
            if (grid[row][col] === '#') {
                writePos = row + 1;
            } else if (grid[row][col] === 'O') {
                grid[row][col] = '.';
                grid[writePos][col] = 'O';
                writePos++;
            }
        }
    }
};

const tiltSouth = (grid) => {
    const rows = grid.length;
    const cols = grid[0].length;
    for (let col = 0; col < cols; col++) {
        let writePos = rows - 1;
        for (let row = rows - 1; row >= 0; row--) {
            if (grid[row][col] === '#') {
                writePos = row - 1;
            } else if (grid[row][col] === 'O') {
                grid[row][col] = '.';
                grid[writePos][col] = 'O';
                writePos--;
            }
        }
    }
};

const tiltWest = (grid) => {
    const rows = grid.length;
    const cols = grid[0].length;
    for (let row = 0; row < rows; row++) {
        let writePos = 0;
        for (let col = 0; col < cols; col++) {
            if (grid[row][col] === '#') {
                writePos = col + 1;
            } else if (grid[row][col] === 'O') {
                grid[row][col] = '.';
                grid[row][writePos] = 'O';
                writePos++;
            }
        }
    }
};

const tiltEast = (grid) => {
    const rows = grid.length;
    const cols = grid[0].length;
    for (let row = 0; row < rows; row++) {
        let writePos = cols - 1;
        for (let col = cols - 1; col >= 0; col--) {
            if (grid[row][col] === '#') {
                writePos = col - 1;
            } else if (grid[row][col] === 'O') {
                grid[row][col] = '.';
                grid[row][writePos] = 'O';
                writePos--;
            }
        }
    }
};

const spinCycle = (grid) => {
    tiltNorth(grid);
    tiltWest(grid);
    tiltSouth(grid);
    tiltEast(grid);
};

const gridToString = (grid) => grid.map(row => row.join('')).join('\n');

const calculateLoad = (grid) => {
    const rows = grid.length;
    let total = 0;
    for (let row = 0; row < rows; row++) {
        for (const cell of grid[row]) {
            if (cell === 'O') {
                total += rows - row;
            }
        }
    }
    return total;
};

const part1 = (grid) => {
    const copy = grid.map(row => [...row]);
    tiltNorth(copy);
    return calculateLoad(copy);
};

const part2 = (grid) => {
    const copy = grid.map(row => [...row]);
    const target = 1_000_000_000;
    const seen = new Map();
    let cycleNum = 0;

    while (cycleNum < target) {
        const state = gridToString(copy);
        if (seen.has(state)) {
            const cycleStart = seen.get(state);
            const cycleLength = cycleNum - cycleStart;
            const remaining = (target - cycleNum) % cycleLength;
            for (let i = 0; i < remaining; i++) {
                spinCycle(copy);
            }
            return calculateLoad(copy);
        }
        seen.set(state, cycleNum);
        spinCycle(copy);
        cycleNum++;
    }

    return calculateLoad(copy);
};

const inputFile = join(__dirname, '../input.txt');
const text = readFileSync(inputFile, 'utf-8');
const grid = parseInput(text);

console.log(`Part 1: ${part1(grid)}`);
console.log(`Part 2: ${part2(grid)}`);
