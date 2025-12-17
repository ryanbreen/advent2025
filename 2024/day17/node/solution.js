#!/usr/bin/env node
/**
 * Day 17: Chronospatial Computer - 3-bit VM emulator
 */

import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(text) {
  const lines = text.trim().split('\n');
  const a = BigInt(lines[0].match(/Register A: (\d+)/)[1]);
  const b = BigInt(lines[1].match(/Register B: (\d+)/)[1]);
  const c = BigInt(lines[2].match(/Register C: (\d+)/)[1]);
  const program = lines[4].match(/Program: ([\d,]+)/)[1].split(',').map(Number);
  return { a, b, c, program };
}

function runProgram(initialA, initialB, initialC, program) {
  let a = initialA;
  let b = initialB;
  let c = initialC;
  let ip = 0;
  const output = [];

  const combo = (operand) => {
    if (operand <= 3) return BigInt(operand);
    if (operand === 4) return a;
    if (operand === 5) return b;
    if (operand === 6) return c;
    throw new Error(`Invalid combo operand: ${operand}`);
  };

  while (ip < program.length) {
    const opcode = program[ip];
    const operand = program[ip + 1];

    switch (opcode) {
      case 0: // adv - A = A >> combo
        a = a >> combo(operand);
        break;
      case 1: // bxl - B = B XOR literal
        b = b ^ BigInt(operand);
        break;
      case 2: // bst - B = combo % 8
        b = combo(operand) & 7n;
        break;
      case 3: // jnz - jump if A != 0
        if (a !== 0n) {
          ip = operand;
          continue;
        }
        break;
      case 4: // bxc - B = B XOR C
        b = b ^ c;
        break;
      case 5: // out - output combo % 8
        output.push(Number(combo(operand) & 7n));
        break;
      case 6: // bdv - B = A >> combo
        b = a >> combo(operand);
        break;
      case 7: // cdv - C = A >> combo
        c = a >> combo(operand);
        break;
    }

    ip += 2;
  }

  return output;
}

function part1(text) {
  const { a, b, c, program } = parseInput(text);
  const output = runProgram(a, b, c, program);
  return output.join(',');
}

function part2(text) {
  const { b, c, program } = parseInput(text);

  // Work backwards - build A 3 bits at a time
  function search(targetIdx, currentA) {
    if (targetIdx < 0) return currentA;

    for (let bits = 0n; bits < 8n; bits++) {
      const candidateA = (currentA << 3n) | bits;
      if (candidateA === 0n && targetIdx === program.length - 1) continue;

      const output = runProgram(candidateA, b, c, program);
      const expected = program.slice(targetIdx);

      if (output.length === expected.length && output.every((v, i) => v === expected[i])) {
        const result = search(targetIdx - 1, candidateA);
        if (result !== null) return result;
      }
    }

    return null;
  }

  return search(program.length - 1, 0n);
}

const inputPath = join(__dirname, '..', 'input.txt');
const text = readFileSync(inputPath, 'utf-8');

console.log('Part 1:', part1(text));
console.log('Part 2:', String(part2(text)));
