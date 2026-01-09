import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseMonkeys(text) {
  return text.trim().split('\n\n').map(block => {
    const lines = block.trim().split('\n');
    const items = lines[1].match(/\d+/g).map(Number);

    const opMatch = lines[2].match(/new = old ([+*]) (\w+)/);
    const operator = opMatch[1];
    const operand = opMatch[2];

    const divisor = parseInt(lines[3].match(/\d+/)[0], 10);
    const ifTrue = parseInt(lines[4].match(/\d+/)[0], 10);
    const ifFalse = parseInt(lines[5].match(/\d+/)[0], 10);

    return {
      items,
      operator,
      operand,
      divisor,
      ifTrue,
      ifFalse,
      inspections: 0
    };
  });
}

function applyOperation(old, operator, operand) {
  const val = operand === 'old' ? old : BigInt(operand);
  if (operator === '+') {
    return old + val;
  } else {
    return old * val;
  }
}

function simulate(monkeys, rounds, reliefDivisor = 3n, useModulo = false) {
  // For part 2, use product of all divisors to keep numbers manageable
  const modValue = useModulo
    ? monkeys.reduce((acc, m) => acc * BigInt(m.divisor), 1n)
    : null;

  for (let round = 0; round < rounds; round++) {
    for (const monkey of monkeys) {
      while (monkey.items.length > 0) {
        let item = monkey.items.shift();
        monkey.inspections++;

        // Apply operation
        let newVal = applyOperation(item, monkey.operator, monkey.operand);

        // Apply relief
        if (reliefDivisor > 1n) {
          newVal = newVal / reliefDivisor;
        }

        // Apply modulo to prevent overflow
        if (modValue) {
          newVal = newVal % modValue;
        }

        // Test and throw
        if (newVal % BigInt(monkey.divisor) === 0n) {
          monkeys[monkey.ifTrue].items.push(newVal);
        } else {
          monkeys[monkey.ifFalse].items.push(newVal);
        }
      }
    }
  }

  return monkeys;
}

function monkeyBusiness(monkeys) {
  const inspections = monkeys.map(m => m.inspections).sort((a, b) => b - a);
  return inspections[0] * inspections[1];
}

function part1(text) {
  const monkeys = parseMonkeys(text);
  // Convert items to BigInt
  for (const m of monkeys) {
    m.items = m.items.map(BigInt);
  }
  simulate(monkeys, 20, 3n);
  return monkeyBusiness(monkeys);
}

function part2(text) {
  const monkeys = parseMonkeys(text);
  // Convert items to BigInt
  for (const m of monkeys) {
    m.items = m.items.map(BigInt);
  }
  simulate(monkeys, 10000, 1n, true);
  return monkeyBusiness(monkeys);
}

const inputFile = join(__dirname, '..', 'input.txt');
const text = readFileSync(inputFile, 'utf-8');

console.log('Part 1:', part1(text));
console.log('Part 2:', part2(text));
