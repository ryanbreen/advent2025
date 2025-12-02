// Compile: g++ -std=c++17 -O2 -o solution solution.cpp

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <filesystem>
#include <sstream>
#include <utility>
#include <numeric>
#include <algorithm>

namespace fs = std::filesystem;

// Parse ranges from input file
std::vector<std::pair<long long, long long>> parseInput(const std::string& filename) {
    std::vector<std::pair<long long, long long>> ranges;
    std::ifstream file(filename);

    if (!file.is_open()) {
        std::cerr << "Error opening file: " << filename << std::endl;
        return ranges;
    }

    std::string content;
    std::string line;
    while (std::getline(file, line)) {
        content += line;
    }

    // Parse comma-separated ranges
    std::stringstream ss(content);
    std::string rangeStr;

    while (std::getline(ss, rangeStr, ',')) {
        // Trim whitespace
        rangeStr.erase(0, rangeStr.find_first_not_of(" \t\n\r"));
        rangeStr.erase(rangeStr.find_last_not_of(" \t\n\r") + 1);

        if (rangeStr.empty()) continue;

        // Parse start-end
        size_t dashPos = rangeStr.find('-', 1); // Start from 1 to handle negative numbers if any
        if (dashPos != std::string::npos) {
            long long start = std::stoll(rangeStr.substr(0, dashPos));
            long long end = std::stoll(rangeStr.substr(dashPos + 1));
            ranges.emplace_back(start, end);
        }
    }

    return ranges;
}

// Check if a number is invalid (pattern repeated exactly twice)
bool isInvalidID(long long num) {
    std::string str = std::to_string(num);
    size_t len = str.length();

    // Must be even length to be repeated twice
    if (len % 2 != 0) return false;

    size_t halfLen = len / 2;
    std::string firstHalf = str.substr(0, halfLen);
    std::string secondHalf = str.substr(halfLen);

    // Check if both halves are identical
    return firstHalf == secondHalf;
}

// Check if a number is invalid (pattern repeated at least twice)
bool isInvalidIDPart2(long long num) {
    std::string str = std::to_string(num);
    size_t len = str.length();

    // Try all possible pattern lengths from 1 to len/2
    for (size_t patternLen = 1; patternLen <= len / 2; ++patternLen) {
        // Check if the string can be divided evenly by this pattern length
        if (len % patternLen == 0) {
            std::string pattern = str.substr(0, patternLen);
            size_t repetitions = len / patternLen;

            // Check if repeating the pattern creates the full string
            if (repetitions >= 2) {
                std::string repeated;
                repeated.reserve(len);
                for (size_t i = 0; i < repetitions; ++i) {
                    repeated += pattern;
                }
                if (repeated == str) {
                    return true;
                }
            }
        }
    }

    return false;
}

// Part 1: Sum of invalid IDs where pattern repeated exactly twice
long long part1(const std::vector<std::pair<long long, long long>>& ranges) {
    return std::accumulate(ranges.begin(), ranges.end(), 0LL,
        [](long long sum, const auto& range) {
            long long rangeSum = 0;
            for (long long id = range.first; id <= range.second; ++id) {
                if (isInvalidID(id)) {
                    rangeSum += id;
                }
            }
            return sum + rangeSum;
        });
}

// Part 2: Sum of invalid IDs where pattern repeated at least twice
long long part2(const std::vector<std::pair<long long, long long>>& ranges) {
    return std::accumulate(ranges.begin(), ranges.end(), 0LL,
        [](long long sum, const auto& range) {
            long long rangeSum = 0;
            for (long long id = range.first; id <= range.second; ++id) {
                if (isInvalidIDPart2(id)) {
                    rangeSum += id;
                }
            }
            return sum + rangeSum;
        });
}

int main() {
    // Get the path to input.txt relative to the executable
    fs::path execPath = fs::current_path();
    fs::path inputPath = execPath / ".." / "input.txt";

    // If that doesn't exist, try relative to the source file location
    if (!fs::exists(inputPath)) {
        inputPath = "../input.txt";
    }

    auto ranges = parseInput(inputPath.string());

    if (ranges.empty()) {
        std::cerr << "No ranges parsed from input file" << std::endl;
        return 1;
    }

    std::cout << "Part 1: " << part1(ranges) << std::endl;
    std::cout << "Part 2: " << part2(ranges) << std::endl;

    return 0;
}
