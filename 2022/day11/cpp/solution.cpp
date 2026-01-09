#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>
#include <cstdint>
#include <regex>
#include <deque>
#include <numeric>

struct Monkey {
    std::deque<uint64_t> items;
    char op;           // '+' or '*'
    std::string operand; // "old" or a number
    uint64_t divisor;
    int if_true;
    int if_false;
    uint64_t inspections = 0;
};

std::vector<Monkey> parseMonkeys(const std::string& text) {
    std::vector<Monkey> monkeys;
    std::istringstream iss(text);
    std::string line;

    while (std::getline(iss, line)) {
        if (line.find("Monkey") != std::string::npos) {
            Monkey monkey;

            // Parse starting items
            std::getline(iss, line);
            std::regex num_regex(R"(\d+)");
            std::sregex_iterator it(line.begin(), line.end(), num_regex);
            std::sregex_iterator end;
            while (it != end) {
                monkey.items.push_back(std::stoull(it->str()));
                ++it;
            }

            // Parse operation
            std::getline(iss, line);
            std::regex op_regex(R"(new = old ([+*]) (\w+))");
            std::smatch match;
            if (std::regex_search(line, match, op_regex)) {
                monkey.op = match[1].str()[0];
                monkey.operand = match[2].str();
            }

            // Parse divisor
            std::getline(iss, line);
            std::regex div_regex(R"(\d+)");
            if (std::regex_search(line, match, div_regex)) {
                monkey.divisor = std::stoull(match[0].str());
            }

            // Parse if true
            std::getline(iss, line);
            if (std::regex_search(line, match, div_regex)) {
                monkey.if_true = std::stoi(match[0].str());
            }

            // Parse if false
            std::getline(iss, line);
            if (std::regex_search(line, match, div_regex)) {
                monkey.if_false = std::stoi(match[0].str());
            }

            monkeys.push_back(monkey);
        }
    }

    return monkeys;
}

uint64_t applyOperation(uint64_t old, char op, const std::string& operand) {
    uint64_t val = (operand == "old") ? old : std::stoull(operand);
    if (op == '+') {
        return old + val;
    } else {
        return old * val;
    }
}

void simulate(std::vector<Monkey>& monkeys, int rounds, int reliefDivisor, bool useModulo) {
    // Calculate product of all divisors for modulo
    uint64_t modValue = 1;
    if (useModulo) {
        for (const auto& m : monkeys) {
            modValue *= m.divisor;
        }
    }

    for (int round = 0; round < rounds; ++round) {
        for (auto& monkey : monkeys) {
            while (!monkey.items.empty()) {
                uint64_t item = monkey.items.front();
                monkey.items.pop_front();
                monkey.inspections++;

                // Apply operation
                uint64_t newVal = applyOperation(item, monkey.op, monkey.operand);

                // Apply relief
                if (reliefDivisor > 1) {
                    newVal /= reliefDivisor;
                }

                // Apply modulo to prevent overflow
                if (useModulo) {
                    newVal %= modValue;
                }

                // Test and throw
                if (newVal % monkey.divisor == 0) {
                    monkeys[monkey.if_true].items.push_back(newVal);
                } else {
                    monkeys[monkey.if_false].items.push_back(newVal);
                }
            }
        }
    }
}

uint64_t monkeyBusiness(const std::vector<Monkey>& monkeys) {
    std::vector<uint64_t> inspections;
    for (const auto& m : monkeys) {
        inspections.push_back(m.inspections);
    }
    std::sort(inspections.begin(), inspections.end(), std::greater<uint64_t>());
    return inspections[0] * inspections[1];
}

uint64_t part1(const std::string& text) {
    auto monkeys = parseMonkeys(text);
    simulate(monkeys, 20, 3, false);
    return monkeyBusiness(monkeys);
}

uint64_t part2(const std::string& text) {
    auto monkeys = parseMonkeys(text);
    simulate(monkeys, 10000, 1, true);
    return monkeyBusiness(monkeys);
}

int main() {
    std::ifstream file("../input.txt");
    if (!file.is_open()) {
        std::cerr << "Error: Could not open input file" << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string text = buffer.str();
    file.close();

    std::cout << "Part 1: " << part1(text) << std::endl;
    std::cout << "Part 2: " << part2(text) << std::endl;

    return 0;
}
