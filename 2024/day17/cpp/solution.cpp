// Day 17: Chronospatial Computer - 3-bit VM emulator
#include <cstdint>
#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <string>
#include <vector>

struct State {
    int64_t a, b, c;
    std::vector<int> program;
};

State parse_input(const std::string& filename) {
    std::ifstream file(filename);
    State state{};
    std::string line;

    // Parse Register A
    std::getline(file, line);
    state.a = std::stoll(line.substr(line.find(':') + 2));

    // Parse Register B
    std::getline(file, line);
    state.b = std::stoll(line.substr(line.find(':') + 2));

    // Parse Register C
    std::getline(file, line);
    state.c = std::stoll(line.substr(line.find(':') + 2));

    // Skip blank line
    std::getline(file, line);

    // Parse Program
    std::getline(file, line);
    std::string prog_str = line.substr(line.find(':') + 2);
    std::stringstream ss(prog_str);
    std::string token;
    while (std::getline(ss, token, ',')) {
        state.program.push_back(std::stoi(token));
    }

    return state;
}

std::vector<int> run_program(int64_t a, int64_t b, int64_t c,
                              const std::vector<int>& program) {
    std::vector<int> output;
    size_t ip = 0;

    auto combo = [&](int operand) -> int64_t {
        switch (operand) {
            case 0: case 1: case 2: case 3:
                return operand;
            case 4:
                return a;
            case 5:
                return b;
            case 6:
                return c;
            default:
                throw std::runtime_error("Invalid combo operand");
        }
    };

    while (ip < program.size()) {
        int opcode = program[ip];
        int operand = program[ip + 1];

        switch (opcode) {
            case 0:  // adv - A = A >> combo
                a = a >> combo(operand);
                break;
            case 1:  // bxl - B = B XOR literal
                b = b ^ operand;
                break;
            case 2:  // bst - B = combo % 8
                b = combo(operand) & 7;
                break;
            case 3:  // jnz - jump if A != 0
                if (a != 0) {
                    ip = operand;
                    continue;
                }
                break;
            case 4:  // bxc - B = B XOR C
                b = b ^ c;
                break;
            case 5:  // out - output combo % 8
                output.push_back(combo(operand) & 7);
                break;
            case 6:  // bdv - B = A >> combo
                b = a >> combo(operand);
                break;
            case 7:  // cdv - C = A >> combo
                c = a >> combo(operand);
                break;
        }
        ip += 2;
    }

    return output;
}

std::string part1(const State& state) {
    auto output = run_program(state.a, state.b, state.c, state.program);
    std::string result;
    for (size_t i = 0; i < output.size(); ++i) {
        if (i > 0) result += ',';
        result += std::to_string(output[i]);
    }
    return result;
}

std::optional<int64_t> find_a(int target_idx, int64_t current_a, int64_t b,
                               int64_t c, const std::vector<int>& program) {
    if (target_idx < 0) {
        return current_a;
    }

    // Try all 8 possible 3-bit values for this position
    for (int bits = 0; bits < 8; ++bits) {
        int64_t candidate_a = (current_a << 3) | bits;

        // A can't be 0 at start (would halt immediately without output)
        if (candidate_a == 0 && target_idx == static_cast<int>(program.size()) - 1) {
            continue;
        }

        auto output = run_program(candidate_a, b, c, program);

        // Check if output matches the suffix of the program
        size_t suffix_len = program.size() - target_idx;
        if (output.size() == suffix_len) {
            bool matches = true;
            for (size_t i = 0; i < suffix_len; ++i) {
                if (output[i] != program[target_idx + i]) {
                    matches = false;
                    break;
                }
            }
            if (matches) {
                auto result = find_a(target_idx - 1, candidate_a, b, c, program);
                if (result.has_value()) {
                    return result;
                }
            }
        }
    }

    return std::nullopt;
}

int64_t part2(const State& state) {
    auto result = find_a(static_cast<int>(state.program.size()) - 1, 0,
                         state.b, state.c, state.program);
    return result.value_or(-1);
}

int main() {
    auto state = parse_input("../input.txt");

    std::cout << "Part 1: " << part1(state) << std::endl;
    std::cout << "Part 2: " << part2(state) << std::endl;

    return 0;
}
