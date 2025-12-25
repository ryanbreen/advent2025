#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <algorithm>

struct Gate {
    std::string in1;
    std::string op;
    std::string in2;
    std::string out;
};

std::pair<std::map<std::string, int>, std::vector<Gate>> parse_input(const std::string& filename) {
    std::ifstream file(filename);
    std::string content((std::istreambuf_iterator<char>(file)),
                        std::istreambuf_iterator<char>());

    std::map<std::string, int> wires;
    std::vector<Gate> gates;

    size_t blank_line = content.find("\n\n");
    std::string initial_section = content.substr(0, blank_line);
    std::string gates_section = content.substr(blank_line + 2);

    // Parse initial values
    std::istringstream iss(initial_section);
    std::string line;
    while (std::getline(iss, line)) {
        size_t colon = line.find(": ");
        std::string name = line.substr(0, colon);
        int value = std::stoi(line.substr(colon + 2));
        wires[name] = value;
    }

    // Parse gates
    std::istringstream gss(gates_section);
    while (std::getline(gss, line)) {
        std::istringstream lss(line);
        Gate gate;
        std::string arrow;
        lss >> gate.in1 >> gate.op >> gate.in2 >> arrow >> gate.out;
        gates.push_back(gate);
    }

    return {wires, gates};
}

std::map<std::string, int> simulate(std::map<std::string, int> wires,
                                      const std::vector<Gate>& gates) {
    std::vector<Gate> remaining = gates;

    while (!remaining.empty()) {
        bool made_progress = false;
        std::vector<Gate> new_remaining;

        for (const auto& gate : remaining) {
            if (wires.count(gate.in1) && wires.count(gate.in2)) {
                int v1 = wires[gate.in1];
                int v2 = wires[gate.in2];
                int result;

                if (gate.op == "AND") {
                    result = v1 & v2;
                } else if (gate.op == "OR") {
                    result = v1 | v2;
                } else if (gate.op == "XOR") {
                    result = v1 ^ v2;
                }

                wires[gate.out] = result;
                made_progress = true;
            } else {
                new_remaining.push_back(gate);
            }
        }

        remaining = new_remaining;
        if (!made_progress && !remaining.empty()) {
            throw std::runtime_error("Circuit stuck - missing inputs");
        }
    }

    return wires;
}

long long get_z_value(const std::map<std::string, int>& wires) {
    std::vector<std::string> z_wires;
    for (const auto& [name, value] : wires) {
        if (name[0] == 'z') {
            z_wires.push_back(name);
        }
    }
    std::sort(z_wires.rbegin(), z_wires.rend());

    long long result = 0;
    for (const auto& z : z_wires) {
        result = (result << 1) | wires.at(z);
    }
    return result;
}

long long part1(const std::map<std::string, int>& wires,
                const std::vector<Gate>& gates) {
    auto final_wires = simulate(wires, gates);
    return get_z_value(final_wires);
}

std::string part2(const std::vector<Gate>& gates) {
    std::set<std::string> swapped;

    // Build lookup: output -> (in1, op, in2)
    std::map<std::string, std::tuple<std::string, std::string, std::string>> gate_by_output;
    for (const auto& gate : gates) {
        gate_by_output[gate.out] = {gate.in1, gate.op, gate.in2};
    }

    // Build lookup: (inputs_set, op) -> output
    std::map<std::pair<std::set<std::string>, std::string>, std::string> gate_by_inputs_op;
    for (const auto& gate : gates) {
        std::set<std::string> inputs = {gate.in1, gate.in2};
        gate_by_inputs_op[{inputs, gate.op}] = gate.out;
    }

    // Find the highest bit number
    int max_bit = 0;
    for (const auto& gate : gates) {
        if (gate.out[0] == 'z') {
            int bit = std::stoi(gate.out.substr(1));
            max_bit = std::max(max_bit, bit);
        }
    }

    std::string max_z = "z";
    if (max_bit < 10) max_z += "0";
    max_z += std::to_string(max_bit);

    for (const auto& gate : gates) {
        // Rule: XOR gates that don't take x,y as input should output to z
        if (gate.op == "XOR") {
            bool is_xy_xor = ((gate.in1[0] == 'x' || gate.in1[0] == 'y') &&
                             (gate.in2[0] == 'x' || gate.in2[0] == 'y'));
            if (!is_xy_xor) {
                if (gate.out[0] != 'z') {
                    swapped.insert(gate.out);
                }
            }
        }

        // Rule: z outputs (except highest bit) should come from XOR
        if (gate.out[0] == 'z' && gate.out != max_z) {
            if (gate.op != "XOR") {
                swapped.insert(gate.out);
            }
        }

        // Rule: AND gates (except x00 AND y00) should feed into OR
        if (gate.op == "AND") {
            bool is_first_bit = ((gate.in1 == "x00" && gate.in2 == "y00") ||
                                (gate.in1 == "y00" && gate.in2 == "x00"));
            if (!is_first_bit) {
                bool used_by_or = false;
                for (const auto& g2 : gates) {
                    if (g2.op == "OR" && (gate.out == g2.in1 || gate.out == g2.in2)) {
                        used_by_or = true;
                        break;
                    }
                }
                if (!used_by_or) {
                    swapped.insert(gate.out);
                }
            }
        }

        // Rule: XOR of x,y should feed into another XOR and AND
        if (gate.op == "XOR") {
            bool is_xy_xor = ((gate.in1[0] == 'x' || gate.in1[0] == 'y') &&
                             (gate.in2[0] == 'x' || gate.in2[0] == 'y'));
            bool is_z00 = ((gate.in1 == "x00" && gate.in2 == "y00") ||
                          (gate.in1 == "y00" && gate.in2 == "x00"));
            if (is_xy_xor && !is_z00) {
                bool used_by_xor = false;
                bool used_by_and = false;
                for (const auto& g2 : gates) {
                    if (gate.out == g2.in1 || gate.out == g2.in2) {
                        if (g2.op == "XOR") {
                            used_by_xor = true;
                        } else if (g2.op == "AND") {
                            used_by_and = true;
                        }
                    }
                }
                if (!(used_by_xor && used_by_and)) {
                    swapped.insert(gate.out);
                }
            }
        }
    }

    std::vector<std::string> result(swapped.begin(), swapped.end());
    std::sort(result.begin(), result.end());

    std::string output;
    for (size_t i = 0; i < result.size(); i++) {
        if (i > 0) output += ",";
        output += result[i];
    }
    return output;
}

int main() {
    auto [wires, gates] = parse_input("../input.txt");

    std::cout << "Part 1: " << part1(wires, gates) << std::endl;
    std::cout << "Part 2: " << part2(gates) << std::endl;

    return 0;
}
