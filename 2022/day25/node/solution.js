#!/usr/bin/env node
import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function snafuToDecimal(s) {
    const digitValues = { '2': 2, '1': 1, '0': 0, '-': -1, '=': -2 };
    let result = 0n;
    for (const char of s) {
        result = result * 5n + BigInt(digitValues[char]);
    }
    return result;
}

function decimalToSnafu(n) {
    if (n === 0n) return '0';

    const digits = [];
    while (n > 0n) {
        const remainder = Number(n % 5n);
        if (remainder <= 2) {
            digits.push(String(remainder));
            n = n / 5n;
        } else if (remainder === 3) {
            digits.push('=');
            n = n / 5n + 1n;
        } else { // remainder === 4
            digits.push('-');
            n = n / 5n + 1n;
        }
    }

    return digits.reverse().join('');
}

function part1(text) {
    const lines = text.trim().split('\n');
    let total = 0n;
    for (const line of lines) {
        total += snafuToDecimal(line);
    }
    return decimalToSnafu(total);
}

const inputFile = join(__dirname, '..', 'input.txt');
const text = readFileSync(inputFile, 'utf-8');

console.log('Part 1:', part1(text));
console.log('Part 2: No Part 2 on Day 25!');
