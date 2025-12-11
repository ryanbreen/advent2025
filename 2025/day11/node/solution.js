import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(filename) {
    const graph = new Map();
    const content = readFileSync(filename, 'utf-8');

    for (const line of content.trim().split('\n')) {
        if (!line.trim()) continue;
        const [node, rest] = line.split(': ');
        const neighbors = rest ? rest.split(' ') : [];
        graph.set(node, neighbors);
    }

    return graph;
}

function part1(graph) {
    // Count all paths from 'you' to 'out' using memoization
    const memo = new Map();

    function countPaths(node) {
        if (node === 'out') return 1n;
        if (memo.has(node)) return memo.get(node);
        if (!graph.has(node)) return 0n;

        let total = 0n;
        for (const neighbor of graph.get(node)) {
            total += countPaths(neighbor);
        }

        memo.set(node, total);
        return total;
    }

    return countPaths('you');
}

function part2(graph) {
    // Count paths from 'svr' to 'out' that visit both 'dac' and 'fft'
    // Since it's a DAG, paths either visit dac before fft, or fft before dac

    function countPathsToTarget(target) {
        const memo = new Map();

        function count(node) {
            if (node === target) return 1n;
            if (memo.has(node)) return memo.get(node);
            if (!graph.has(node)) return 0n;

            let total = 0n;
            for (const neighbor of graph.get(node)) {
                total += count(neighbor);
            }
            memo.set(node, total);
            return total;
        }
        return count;
    }

    const pathsToOut = countPathsToTarget('out');
    const pathsToDac = countPathsToTarget('dac');
    const pathsToFft = countPathsToTarget('fft');

    // Paths that visit dac before fft: svr -> dac -> fft -> out
    const dacBeforeFft = pathsToDac('svr') * pathsToFft('dac') * pathsToOut('fft');

    // Paths that visit fft before dac: svr -> fft -> dac -> out
    const fftBeforeDac = pathsToFft('svr') * pathsToDac('fft') * pathsToOut('dac');

    return dacBeforeFft + fftBeforeDac;
}

function main() {
    const inputFile = process.argv[2] || join(__dirname, '../input.txt');
    const graph = parseInput(inputFile);

    console.log(`Part 1: ${part1(graph)}`);
    console.log(`Part 2: ${part2(graph)}`);
}

main();
