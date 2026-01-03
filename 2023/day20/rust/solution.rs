// Day 20: Pulse Propagation - Module communication simulation

use std::collections::VecDeque;
use std::fs;

const MAX_MODULES: usize = 64;

#[derive(Clone, Copy, PartialEq)]
enum ModuleType {
    Broadcaster,
    FlipFlop,
    Conjunction,
    Output,
}

struct Module {
    module_type: ModuleType,
    destinations: Vec<usize>,
    state: bool,                     // For flip-flops
    input_mask: u64,                 // Bitmask of which modules feed into this conjunction
    memory: u64,                     // Bitmask of current high inputs for conjunctions
}

impl Module {
    fn new(module_type: ModuleType) -> Self {
        Module {
            module_type,
            destinations: Vec::new(),
            state: false,
            input_mask: 0,
            memory: 0,
        }
    }
}

struct Circuit {
    modules: Vec<Module>,
    broadcaster_idx: usize,
    name_to_idx: std::collections::HashMap<String, usize>,
}

impl Circuit {
    fn parse(filename: &str) -> Self {
        let content = fs::read_to_string(filename).expect("Failed to read input file");
        let mut name_to_idx: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
        let mut modules: Vec<Module> = Vec::with_capacity(MAX_MODULES);
        let mut broadcaster_idx = 0;

        // First pass: assign indices to all module names
        let mut next_idx = 0usize;
        for line in content.trim().lines() {
            let parts: Vec<&str> = line.split(" -> ").collect();
            let name_part = parts[0];
            let dest_part = parts[1];

            let name = if name_part == "broadcaster" {
                "broadcaster"
            } else {
                &name_part[1..]
            };

            if !name_to_idx.contains_key(name) {
                name_to_idx.insert(name.to_string(), next_idx);
                next_idx += 1;
            }

            // Also assign indices to destinations
            for dest in dest_part.split(", ") {
                if !name_to_idx.contains_key(dest) {
                    name_to_idx.insert(dest.to_string(), next_idx);
                    next_idx += 1;
                }
            }
        }

        // Initialize all modules
        for _ in 0..next_idx {
            modules.push(Module::new(ModuleType::Output));
        }

        // Second pass: set up module types and destinations
        for line in content.trim().lines() {
            let parts: Vec<&str> = line.split(" -> ").collect();
            let name_part = parts[0];
            let dest_part = parts[1];

            let (name, module_type) = if name_part == "broadcaster" {
                ("broadcaster", ModuleType::Broadcaster)
            } else if name_part.starts_with('%') {
                (&name_part[1..], ModuleType::FlipFlop)
            } else {
                (&name_part[1..], ModuleType::Conjunction)
            };

            let idx = name_to_idx[name];
            if name == "broadcaster" {
                broadcaster_idx = idx;
            }

            modules[idx].module_type = module_type;

            for dest in dest_part.split(", ") {
                let dest_idx = name_to_idx[dest];
                modules[idx].destinations.push(dest_idx);
            }
        }

        // Third pass: set up conjunction input masks
        for src_idx in 0..modules.len() {
            let dests: Vec<usize> = modules[src_idx].destinations.clone();
            for &dest_idx in &dests {
                if modules[dest_idx].module_type == ModuleType::Conjunction {
                    modules[dest_idx].input_mask |= 1u64 << src_idx;
                }
            }
        }

        Circuit {
            modules,
            broadcaster_idx,
            name_to_idx,
        }
    }

    fn reset_state(&mut self) {
        for module in &mut self.modules {
            module.state = false;
            module.memory = 0;
        }
    }

    fn simulate_button_press(&mut self, watch_mask: u64) -> (u64, u64, u64) {
        let mut low_count: u64 = 0;
        let mut high_count: u64 = 0;
        let mut high_senders: u64 = 0;

        // Queue: (source_idx, dest_idx, is_high_pulse)
        let mut queue: VecDeque<(usize, usize, bool)> = VecDeque::with_capacity(256);
        queue.push_back((usize::MAX, self.broadcaster_idx, false)); // usize::MAX = button

        while let Some((source, dest, pulse)) = queue.pop_front() {
            if pulse {
                high_count += 1;
            } else {
                low_count += 1;
            }

            // Track if watched nodes send high pulses
            if watch_mask != 0 && pulse && source < 64 {
                let source_bit = 1u64 << source;
                if watch_mask & source_bit != 0 {
                    high_senders |= source_bit;
                }
            }

            let module = &mut self.modules[dest];
            match module.module_type {
                ModuleType::Broadcaster => {
                    for &next_dest in &module.destinations.clone() {
                        queue.push_back((dest, next_dest, pulse));
                    }
                }
                ModuleType::FlipFlop => {
                    if !pulse {
                        module.state = !module.state;
                        let new_pulse = module.state;
                        for &next_dest in &module.destinations.clone() {
                            queue.push_back((dest, next_dest, new_pulse));
                        }
                    }
                }
                ModuleType::Conjunction => {
                    let source_bit = 1u64 << source;
                    if pulse {
                        module.memory |= source_bit;
                    } else {
                        module.memory &= !source_bit;
                    }
                    // Send low if all inputs are high, otherwise send high
                    let all_high = module.memory == module.input_mask;
                    let output = !all_high;
                    for &next_dest in &module.destinations.clone() {
                        queue.push_back((dest, next_dest, output));
                    }
                }
                ModuleType::Output => {
                    // Does nothing
                }
            }
        }

        (low_count, high_count, high_senders)
    }
}

fn part1(circuit: &mut Circuit) -> u64 {
    circuit.reset_state();

    let mut total_low: u64 = 0;
    let mut total_high: u64 = 0;

    for _ in 0..1000 {
        let (low, high, _) = circuit.simulate_button_press(0);
        total_low += low;
        total_high += high;
    }

    total_low * total_high
}

fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 { a } else { gcd(b, a % b) }
}

fn lcm(a: u64, b: u64) -> u64 {
    a / gcd(a, b) * b
}

fn part2(circuit: &mut Circuit) -> u64 {
    circuit.reset_state();

    // Find the module that feeds into rx
    let rx_idx = match circuit.name_to_idx.get("rx") {
        Some(&idx) => idx,
        None => return 0,
    };

    let mut rx_input_idx: Option<usize> = None;
    for (idx, module) in circuit.modules.iter().enumerate() {
        if module.destinations.contains(&rx_idx) {
            rx_input_idx = Some(idx);
            break;
        }
    }

    let rx_input_idx = match rx_input_idx {
        Some(idx) => idx,
        None => return 0,
    };

    // Get the input mask for the conjunction feeding rx
    let watch_mask = circuit.modules[rx_input_idx].input_mask;
    if watch_mask == 0 {
        return 0;
    }

    let watch_count = watch_mask.count_ones() as usize;
    let mut cycle_lengths: Vec<u64> = Vec::new();
    let mut found_mask: u64 = 0;

    let mut button_press: u64 = 0;
    while cycle_lengths.len() < watch_count {
        button_press += 1;
        let (_, _, high_senders) = circuit.simulate_button_press(watch_mask);

        // Check for newly found high senders
        let new_found = high_senders & !found_mask;
        if new_found != 0 {
            // For each bit set in new_found, record the cycle length
            let new_count = new_found.count_ones() as usize;
            for _ in 0..new_count {
                cycle_lengths.push(button_press);
            }
            found_mask |= new_found;
        }
    }

    // LCM of all cycle lengths
    let mut result: u64 = 1;
    for &length in &cycle_lengths {
        result = lcm(result, length);
    }

    result
}

fn main() {
    let mut circuit = Circuit::parse("../input.txt");
    println!("Part 1: {}", part1(&mut circuit));
    println!("Part 2: {}", part2(&mut circuit));
}
