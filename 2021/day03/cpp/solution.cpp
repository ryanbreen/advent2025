#include <iostream>
#include <fstream>
#include <vector>
#include <string>

std::vector<std::string> parseInput() {
    std::vector<std::string> numbers;
    std::ifstream file("../input.txt");
    std::string line;

    while (std::getline(file, line)) {
        if (!line.empty()) {
            numbers.push_back(line);
        }
    }

    return numbers;
}

int part1(const std::vector<std::string>& numbers) {
    int numBits = numbers[0].length();
    int gamma = 0;

    for (int pos = 0; pos < numBits; pos++) {
        int ones = 0;
        for (const auto& n : numbers) {
            if (n[pos] == '1') {
                ones++;
            }
        }
        int zeros = numbers.size() - ones;

        if (ones >= zeros) {
            gamma |= (1 << (numBits - 1 - pos));
        }
    }

    // epsilon is bitwise NOT of gamma (within numBits)
    int epsilon = gamma ^ ((1 << numBits) - 1);

    return gamma * epsilon;
}

int findRating(const std::vector<std::string>& numbers, bool useMostCommon) {
    int numBits = numbers[0].length();
    std::vector<std::string> candidates = numbers;

    for (int pos = 0; pos < numBits; pos++) {
        if (candidates.size() == 1) {
            break;
        }

        int ones = 0;
        for (const auto& n : candidates) {
            if (n[pos] == '1') {
                ones++;
            }
        }
        int zeros = candidates.size() - ones;

        char target;
        if (useMostCommon) {
            target = (ones >= zeros) ? '1' : '0';
        } else {
            target = (zeros <= ones) ? '0' : '1';
        }

        std::vector<std::string> filtered;
        for (const auto& n : candidates) {
            if (n[pos] == target) {
                filtered.push_back(n);
            }
        }
        candidates = filtered;
    }

    // Convert binary string to int
    int result = 0;
    for (char c : candidates[0]) {
        result = (result << 1) | (c - '0');
    }
    return result;
}

int part2(const std::vector<std::string>& numbers) {
    int oxygen = findRating(numbers, true);
    int co2 = findRating(numbers, false);
    return oxygen * co2;
}

int main() {
    std::vector<std::string> numbers = parseInput();

    std::cout << "Part 1: " << part1(numbers) << std::endl;
    std::cout << "Part 2: " << part2(numbers) << std::endl;

    return 0;
}
