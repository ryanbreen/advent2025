#!/usr/bin/env python3
import os
import re
from functools import reduce
from operator import mul

def parse_monkeys(text):
    """Parse input into list of monkey dictionaries."""
    monkeys = []
    for block in text.strip().split('\n\n'):
        lines = block.strip().split('\n')
        items = list(map(int, re.findall(r'\d+', lines[1])))

        # Parse operation
        op_match = re.search(r'new = old ([+*]) (\w+)', lines[2])
        operator = op_match.group(1)
        operand = op_match.group(2)

        divisor = int(re.search(r'\d+', lines[3]).group())
        if_true = int(re.search(r'\d+', lines[4]).group())
        if_false = int(re.search(r'\d+', lines[5]).group())

        monkeys.append({
            'items': items,
            'operator': operator,
            'operand': operand,
            'divisor': divisor,
            'if_true': if_true,
            'if_false': if_false,
            'inspections': 0
        })
    return monkeys

def apply_operation(old, operator, operand):
    """Apply the monkey's operation to the worry level."""
    val = old if operand == 'old' else int(operand)
    if operator == '+':
        return old + val
    else:
        return old * val

def simulate(monkeys, rounds, relief_divisor=3, use_modulo=False):
    """Simulate monkey business for given number of rounds."""
    # For part 2, use product of all divisors to keep numbers manageable
    mod_value = reduce(mul, (m['divisor'] for m in monkeys)) if use_modulo else None

    for _ in range(rounds):
        for monkey in monkeys:
            while monkey['items']:
                item = monkey['items'].pop(0)
                monkey['inspections'] += 1

                # Apply operation
                new_val = apply_operation(item, monkey['operator'], monkey['operand'])

                # Apply relief
                if relief_divisor > 1:
                    new_val //= relief_divisor

                # Apply modulo to prevent overflow
                if mod_value:
                    new_val %= mod_value

                # Test and throw
                if new_val % monkey['divisor'] == 0:
                    monkeys[monkey['if_true']]['items'].append(new_val)
                else:
                    monkeys[monkey['if_false']]['items'].append(new_val)

    return monkeys

def monkey_business(monkeys):
    """Calculate monkey business: product of top 2 inspection counts."""
    inspections = sorted([m['inspections'] for m in monkeys], reverse=True)
    return inspections[0] * inspections[1]

def part1(text):
    """Run 20 rounds with relief (divide by 3)."""
    monkeys = parse_monkeys(text)
    simulate(monkeys, 20, relief_divisor=3)
    return monkey_business(monkeys)

def part2(text):
    """Run 10000 rounds without relief."""
    monkeys = parse_monkeys(text)
    simulate(monkeys, 10000, relief_divisor=1, use_modulo=True)
    return monkey_business(monkeys)

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        text = f.read()

    print('Part 1:', part1(text))
    print('Part 2:', part2(text))

if __name__ == '__main__':
    main()
