use std::collections::{HashMap, HashSet};
use std::fs;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Op {
    AND,
    OR,
    XOR,
}

impl Op {
    fn from_str(s: &str) -> Self {
        match s {
            "AND" => Op::AND,
            "OR" => Op::OR,
            "XOR" => Op::XOR,
            _ => panic!("Unknown operation: {}", s),
        }
    }
}

#[derive(Debug, Clone)]
struct Gate {
    in1: String,
    op: Op,
    in2: String,
    out: String,
}

fn parse_input(filename: &str) -> (HashMap<String, u8>, Vec<Gate>) {
    let content = fs::read_to_string(filename).expect("Failed to read input file");
    let parts: Vec<&str> = content.trim().split("\n\n").collect();

    // Parse initial wire values
    let mut wires = HashMap::new();
    for line in parts[0].lines() {
        let parts: Vec<&str> = line.split(": ").collect();
        let name = parts[0].to_string();
        let value = parts[1].parse::<u8>().unwrap();
        wires.insert(name, value);
    }

    // Parse gates
    let mut gates = Vec::new();
    for line in parts[1].lines() {
        let tokens: Vec<&str> = line.split_whitespace().collect();
        let in1 = tokens[0].to_string();
        let op = Op::from_str(tokens[1]);
        let in2 = tokens[2].to_string();
        let out = tokens[4].to_string();
        gates.push(Gate { in1, op, in2, out });
    }

    (wires, gates)
}

fn simulate(initial_wires: &HashMap<String, u8>, gates: &[Gate]) -> HashMap<String, u8> {
    let mut wires = initial_wires.clone();
    let mut remaining: Vec<Gate> = gates.to_vec();

    while !remaining.is_empty() {
        let mut made_progress = false;
        let mut new_remaining = Vec::new();

        for gate in remaining {
            if wires.contains_key(&gate.in1) && wires.contains_key(&gate.in2) {
                let v1 = wires[&gate.in1];
                let v2 = wires[&gate.in2];
                let result = match gate.op {
                    Op::AND => v1 & v2,
                    Op::OR => v1 | v2,
                    Op::XOR => v1 ^ v2,
                };
                wires.insert(gate.out.clone(), result);
                made_progress = true;
            } else {
                new_remaining.push(gate);
            }
        }

        remaining = new_remaining;
        if !made_progress && !remaining.is_empty() {
            panic!("Circuit stuck - missing inputs");
        }
    }

    wires
}

fn get_z_value(wires: &HashMap<String, u8>) -> u64 {
    let mut z_wires: Vec<_> = wires
        .keys()
        .filter(|k| k.starts_with('z'))
        .collect();
    z_wires.sort_by(|a, b| b.cmp(a)); // Reverse sort

    let mut result = 0u64;
    for z in z_wires {
        result = (result << 1) | (wires[z] as u64);
    }
    result
}

fn part1(wires: &HashMap<String, u8>, gates: &[Gate]) -> u64 {
    let final_wires = simulate(wires, gates);
    get_z_value(&final_wires)
}

fn part2(gates: &[Gate]) -> String {
    let mut swapped = HashSet::new();

    // Find the highest bit number
    let max_bit = gates
        .iter()
        .filter(|g| g.out.starts_with('z'))
        .map(|g| g.out[1..].parse::<usize>().unwrap())
        .max()
        .unwrap();

    for gate in gates {
        // Rule: XOR gates that don't take x,y as input should output to z
        if gate.op == Op::XOR {
            let is_xy_xor = (gate.in1.starts_with('x') || gate.in1.starts_with('y'))
                && (gate.in2.starts_with('x') || gate.in2.starts_with('y'));

            if !is_xy_xor {
                // This is a second-level XOR (sum XOR carry), should output to z
                if !gate.out.starts_with('z') {
                    swapped.insert(gate.out.clone());
                }
            }
        }

        // Rule: z outputs (except the highest bit) should come from XOR
        if gate.out.starts_with('z') && gate.out != format!("z{:02}", max_bit) {
            if gate.op != Op::XOR {
                swapped.insert(gate.out.clone());
            }
        }

        // Rule: AND gates (except x00 AND y00) should feed into OR
        if gate.op == Op::AND {
            let is_first_bit =
                (gate.in1 == "x00" && gate.in2 == "y00") || (gate.in1 == "y00" && gate.in2 == "x00");

            if !is_first_bit {
                // This AND output should be input to an OR gate
                let mut used_by_or = false;
                for other in gates {
                    if other.op == Op::OR
                        && (other.in1 == gate.out || other.in2 == gate.out)
                    {
                        used_by_or = true;
                        break;
                    }
                }
                if !used_by_or {
                    swapped.insert(gate.out.clone());
                }
            }
        }

        // Rule: XOR of x,y should feed into another XOR (for z output) or AND (for carry)
        if gate.op == Op::XOR {
            let is_xy_xor = (gate.in1.starts_with('x') || gate.in1.starts_with('y'))
                && (gate.in2.starts_with('x') || gate.in2.starts_with('y'));

            // Skip z00 which is x00 XOR y00 directly
            let is_z00 =
                (gate.in1 == "x00" && gate.in2 == "y00") || (gate.in1 == "y00" && gate.in2 == "x00");

            if is_xy_xor && !is_z00 {
                // Should be used by XOR and AND
                let mut used_by_xor = false;
                let mut used_by_and = false;
                for other in gates {
                    if other.in1 == gate.out || other.in2 == gate.out {
                        if other.op == Op::XOR {
                            used_by_xor = true;
                        } else if other.op == Op::AND {
                            used_by_and = true;
                        }
                    }
                }
                if !(used_by_xor && used_by_and) {
                    swapped.insert(gate.out.clone());
                }
            }
        }
    }

    let mut result: Vec<_> = swapped.into_iter().collect();
    result.sort();
    result.join(",")
}

fn main() {
    let (wires, gates) = parse_input("../input.txt");

    println!("Part 1: {}", part1(&wires, &gates));
    println!("Part 2: {}", part2(&gates));
}
