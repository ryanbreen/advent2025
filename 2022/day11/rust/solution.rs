use std::fs;

#[derive(Clone)]
struct Monkey {
    items: Vec<u64>,
    operator: char,
    operand: Option<u64>, // None means "old"
    divisor: u64,
    if_true: usize,
    if_false: usize,
    inspections: u64,
}

fn parse_monkeys(text: &str) -> Vec<Monkey> {
    let mut monkeys = Vec::new();

    for block in text.trim().split("\n\n") {
        let lines: Vec<&str> = block.lines().collect();

        // Parse items
        let items: Vec<u64> = lines[1]
            .split_once(": ")
            .unwrap()
            .1
            .split(", ")
            .map(|s| s.parse().unwrap())
            .collect();

        // Parse operation (e.g., "new = old * 19" or "new = old + old")
        let op_part = lines[2].split("new = old ").nth(1).unwrap();
        let mut parts = op_part.split_whitespace();
        let operator = parts.next().unwrap().chars().next().unwrap();
        let operand_str = parts.next().unwrap();
        let operand = if operand_str == "old" {
            None
        } else {
            Some(operand_str.parse().unwrap())
        };

        // Parse divisor
        let divisor: u64 = lines[3]
            .split_whitespace()
            .last()
            .unwrap()
            .parse()
            .unwrap();

        // Parse targets
        let if_true: usize = lines[4]
            .split_whitespace()
            .last()
            .unwrap()
            .parse()
            .unwrap();
        let if_false: usize = lines[5]
            .split_whitespace()
            .last()
            .unwrap()
            .parse()
            .unwrap();

        monkeys.push(Monkey {
            items,
            operator,
            operand,
            divisor,
            if_true,
            if_false,
            inspections: 0,
        });
    }

    monkeys
}

fn apply_operation(old: u64, operator: char, operand: Option<u64>) -> u64 {
    let val = operand.unwrap_or(old);
    match operator {
        '+' => old + val,
        '*' => old * val,
        _ => panic!("Unknown operator"),
    }
}

fn simulate(monkeys: &mut Vec<Monkey>, rounds: u32, relief_divisor: u64, use_modulo: bool) {
    // For part 2, use product of all divisors to keep numbers manageable
    let mod_value: u64 = if use_modulo {
        monkeys.iter().map(|m| m.divisor).product()
    } else {
        0
    };

    for _ in 0..rounds {
        for i in 0..monkeys.len() {
            while !monkeys[i].items.is_empty() {
                let item = monkeys[i].items.remove(0);
                monkeys[i].inspections += 1;

                // Apply operation
                let mut new_val = apply_operation(item, monkeys[i].operator, monkeys[i].operand);

                // Apply relief
                if relief_divisor > 1 {
                    new_val /= relief_divisor;
                }

                // Apply modulo to prevent overflow
                if use_modulo {
                    new_val %= mod_value;
                }

                // Test and throw
                let target = if new_val % monkeys[i].divisor == 0 {
                    monkeys[i].if_true
                } else {
                    monkeys[i].if_false
                };
                monkeys[target].items.push(new_val);
            }
        }
    }
}

fn monkey_business(monkeys: &[Monkey]) -> u64 {
    let mut inspections: Vec<u64> = monkeys.iter().map(|m| m.inspections).collect();
    inspections.sort_by(|a, b| b.cmp(a));
    inspections[0] * inspections[1]
}

fn part1(text: &str) -> u64 {
    let mut monkeys = parse_monkeys(text);
    simulate(&mut monkeys, 20, 3, false);
    monkey_business(&monkeys)
}

fn part2(text: &str) -> u64 {
    let mut monkeys = parse_monkeys(text);
    simulate(&mut monkeys, 10000, 1, true);
    monkey_business(&monkeys)
}

fn main() {
    let text = fs::read_to_string("../input.txt").expect("Could not read input file");

    println!("Part 1: {}", part1(&text));
    println!("Part 2: {}", part2(&text));
}
