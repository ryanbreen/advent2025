#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <array>
#include <algorithm>
#include <utility>

// HASH algorithm: for each char, current = ((current + ASCII) * 17) % 256
int hashAlgorithm(const std::string& s) {
    int current = 0;
    for (char c : s) {
        current = ((current + static_cast<int>(c)) * 17) % 256;
    }
    return current;
}

// Part 1: Sum of HASH values for all comma-separated steps
long long part1(const std::vector<std::string>& steps) {
    long long total = 0;
    for (const auto& step : steps) {
        total += hashAlgorithm(step);
    }
    return total;
}

// Part 2: HASHMAP procedure with 256 boxes
long long part2(const std::vector<std::string>& steps) {
    // Each box contains ordered list of (label, focal_length) pairs
    std::array<std::vector<std::pair<std::string, int>>, 256> boxes;

    for (const auto& step : steps) {
        auto eqPos = step.find('=');
        if (eqPos != std::string::npos) {
            // label=N operation: add or replace lens
            std::string label = step.substr(0, eqPos);
            int focal = std::stoi(step.substr(eqPos + 1));
            int boxNum = hashAlgorithm(label);

            // Check if lens with this label already exists
            auto& box = boxes[boxNum];
            auto it = std::find_if(box.begin(), box.end(),
                [&label](const std::pair<std::string, int>& lens) {
                    return lens.first == label;
                });

            if (it != box.end()) {
                // Replace existing lens
                it->second = focal;
            } else {
                // Add new lens at end
                box.emplace_back(label, focal);
            }
        } else {
            // label- operation: remove lens
            std::string label = step.substr(0, step.length() - 1);
            int boxNum = hashAlgorithm(label);

            auto& box = boxes[boxNum];
            box.erase(
                std::remove_if(box.begin(), box.end(),
                    [&label](const std::pair<std::string, int>& lens) {
                        return lens.first == label;
                    }),
                box.end()
            );
        }
    }

    // Calculate focusing power
    long long total = 0;
    for (int boxNum = 0; boxNum < 256; ++boxNum) {
        const auto& box = boxes[boxNum];
        for (size_t slot = 0; slot < box.size(); ++slot) {
            total += static_cast<long long>(boxNum + 1) * (slot + 1) * box[slot].second;
        }
    }
    return total;
}

std::vector<std::string> parseInput(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error: Could not open " << filename << std::endl;
        return {};
    }

    std::string content;
    std::string line;
    while (std::getline(file, line)) {
        content += line;
    }

    std::vector<std::string> steps;
    std::stringstream ss(content);
    std::string step;
    while (std::getline(ss, step, ',')) {
        if (!step.empty()) {
            steps.push_back(step);
        }
    }

    return steps;
}

int main() {
    auto steps = parseInput("../input.txt");
    if (steps.empty()) {
        return 1;
    }

    std::cout << "Part 1: " << part1(steps) << std::endl;
    std::cout << "Part 2: " << part2(steps) << std::endl;

    return 0;
}
