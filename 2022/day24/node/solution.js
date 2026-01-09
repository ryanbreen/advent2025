#!/usr/bin/env node
import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function gcd(a, b) {
    while (b) [a, b] = [b, a % b];
    return a;
}

function lcm(a, b) {
    return (a * b) / gcd(a, b);
}

function parseInput(text) {
    const lines = text.trim().split('\n');
    const height = lines.length;
    const width = lines[0].length;
    const innerH = height - 2;
    const innerW = width - 2;
    
    const blizzards = [];
    for (let r = 0; r < height; r++) {
        for (let c = 0; c < width; c++) {
            const ch = lines[r][c];
            if ('^v<>'.includes(ch)) {
                blizzards.push([r, c, ch]);
            }
        }
    }
    
    const start = [0, lines[0].indexOf('.')];
    const end = [height - 1, lines[height - 1].indexOf('.')];
    
    return { blizzards, height, width, innerH, innerW, start, end };
}

function getBlizzardPositions(blizzards, innerH, innerW, time) {
    const positions = new Set();
    for (const [r, c, direction] of blizzards) {
        const ir = r - 1;
        const ic = c - 1;
        let nr, nc;
        
        if (direction === '^') {
            nr = ((ir - time) % innerH + innerH) % innerH;
            nc = ic;
        } else if (direction === 'v') {
            nr = ((ir + time) % innerH + innerH) % innerH;
            nc = ic;
        } else if (direction === '<') {
            nr = ir;
            nc = ((ic - time) % innerW + innerW) % innerW;
        } else if (direction === '>') {
            nr = ir;
            nc = ((ic + time) % innerW + innerW) % innerW;
        }
        
        positions.add(`${nr + 1},${nc + 1}`);
    }
    return positions;
}

function bfs(blizzards, height, width, innerH, innerW, start, end, startTime = 0) {
    const period = lcm(innerH, innerW);
    
    // Precompute blizzard positions
    const blizzardCache = new Map();
    for (let t = 0; t < period; t++) {
        blizzardCache.set(t, getBlizzardPositions(blizzards, innerH, innerW, t));
    }
    
    const queue = [[startTime, start[0], start[1]]];
    const visited = new Set();
    visited.add(`${startTime % period},${start[0]},${start[1]}`);
    
    const directions = [[0, 0], [-1, 0], [1, 0], [0, -1], [0, 1]];
    
    while (queue.length > 0) {
        const [time, r, c] = queue.shift();
        
        if (r === end[0] && c === end[1]) {
            return time;
        }
        
        const nextTime = time + 1;
        const nextBlizzards = blizzardCache.get(nextTime % period);
        
        for (const [dr, dc] of directions) {
            const nr = r + dr;
            const nc = c + dc;
            
            // Check bounds
            const isStart = nr === start[0] && nc === start[1];
            const isEnd = nr === end[0] && nc === end[1];
            
            if (!isStart && !isEnd) {
                if (nr <= 0 || nr >= height - 1 || nc <= 0 || nc >= width - 1) {
                    continue;
                }
            }
            
            // Check blizzards
            if (nextBlizzards.has(`${nr},${nc}`)) {
                continue;
            }
            
            const state = `${nextTime % period},${nr},${nc}`;
            if (!visited.has(state)) {
                visited.add(state);
                queue.push([nextTime, nr, nc]);
            }
        }
    }
    
    return -1;
}

function part1(text) {
    const { blizzards, height, width, innerH, innerW, start, end } = parseInput(text);
    return bfs(blizzards, height, width, innerH, innerW, start, end, 0);
}

function part2(text) {
    const { blizzards, height, width, innerH, innerW, start, end } = parseInput(text);
    
    const t1 = bfs(blizzards, height, width, innerH, innerW, start, end, 0);
    const t2 = bfs(blizzards, height, width, innerH, innerW, end, start, t1);
    const t3 = bfs(blizzards, height, width, innerH, innerW, start, end, t2);
    
    return t3;
}

const inputFile = join(__dirname, '..', 'input.txt');
const text = readFileSync(inputFile, 'utf-8');

console.log('Part 1:', part1(text));
console.log('Part 2:', part2(text));
