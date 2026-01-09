#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <set>
#include <algorithm>

std::vector<std::string> parseInput(const std::string& filename) {
    std::vector<std::string> rucksacks;
    std::ifstream file(filename);
    std::string line;
    while (std::getline(file, line)) {
        if (!line.empty()) {
            rucksacks.push_back(line);
        }
    }
    return rucksacks;
}

int priority(char c) {
    if (c >= 'a' && c <= 'z') {
        return c - 'a' + 1;
    } else {
        return c - 'A' + 27;
    }
}

int part1(const std::vector<std::string>& rucksacks) {
    int total = 0;
    for (const auto& rucksack : rucksacks) {
        size_t mid = rucksack.size() / 2;
        std::set<char> first(rucksack.begin(), rucksack.begin() + mid);
        std::set<char> second(rucksack.begin() + mid, rucksack.end());

        std::set<char> common;
        std::set_intersection(first.begin(), first.end(),
                              second.begin(), second.end(),
                              std::inserter(common, common.begin()));

        if (!common.empty()) {
            total += priority(*common.begin());
        }
    }
    return total;
}

int part2(const std::vector<std::string>& rucksacks) {
    int total = 0;
    for (size_t i = 0; i < rucksacks.size(); i += 3) {
        std::set<char> set1(rucksacks[i].begin(), rucksacks[i].end());
        std::set<char> set2(rucksacks[i + 1].begin(), rucksacks[i + 1].end());
        std::set<char> set3(rucksacks[i + 2].begin(), rucksacks[i + 2].end());

        std::set<char> common12;
        std::set_intersection(set1.begin(), set1.end(),
                              set2.begin(), set2.end(),
                              std::inserter(common12, common12.begin()));

        std::set<char> common;
        std::set_intersection(common12.begin(), common12.end(),
                              set3.begin(), set3.end(),
                              std::inserter(common, common.begin()));

        if (!common.empty()) {
            total += priority(*common.begin());
        }
    }
    return total;
}

int main() {
    std::string inputFile = "../input.txt";
    std::vector<std::string> rucksacks = parseInput(inputFile);

    std::cout << "Part 1: " << part1(rucksacks) << std::endl;
    std::cout << "Part 2: " << part2(rucksacks) << std::endl;

    return 0;
}
