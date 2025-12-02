// Compile: g++ -std=c++17 -O2 -o solution solution.cpp

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <filesystem>
#include <cmath>

namespace fs = std::filesystem;

// Parse rotations from input file
std::vector<std::pair<char, int>> parseInput(const std::string& filename) {
    std::vector<std::pair<char, int>> rotations;
    std::ifstream file(filename);

    if (!file.is_open()) {
        std::cerr << "Error opening file: " << filename << std::endl;
        return rotations;
    }

    std::string line;
    while (std::getline(file, line)) {
        if (!line.empty()) {
            char direction = line[0];
            int distance = std::stoi(line.substr(1));
            rotations.emplace_back(direction, distance);
        }
    }

    return rotations;
}

// Part 1: Count how many times dial ends at 0 after each rotation
int part1(const std::vector<std::pair<char, int>>& rotations) {
    int position = 50;
    int zerosCount = 0;

    for (const auto& [direction, distance] : rotations) {
        if (direction == 'L') {
            // Moving left (toward lower numbers)
            position = (position - distance) % 100;
            if (position < 0) position += 100;
        } else {
            // Moving right (toward higher numbers)
            position = (position + distance) % 100;
        }

        // Check if we landed on 0
        if (position == 0) {
            zerosCount++;
        }
    }

    return zerosCount;
}

// Part 2: Count how many times dial points at 0 during any click
int part2(const std::vector<std::pair<char, int>>& rotations) {
    int position = 50;
    int zerosCount = 0;

    for (const auto& [direction, distance] : rotations) {
        if (direction == 'L') {
            // Moving left (toward lower numbers)
            if (distance >= position && position > 0) {
                // We will pass through 0 at least once
                zerosCount++;

                // After the first crossing, how many more times?
                int remainingAfterFirstZero = distance - position;
                zerosCount += remainingAfterFirstZero / 100;
            } else if (position == 0 && distance > 0) {
                // Starting at 0, going left means we immediately leave 0
                // We come back to 0 every 100 clicks
                zerosCount += distance / 100;
            }

            // Calculate final position
            position = (position - distance) % 100;
            if (position < 0) position += 100;
        } else {
            // Moving right (toward higher numbers)
            if (distance >= 100 - position && position > 0) {
                // We will pass through 0 at least once
                zerosCount++;

                // After the first crossing, how many more times?
                int remainingAfterFirstZero = distance - (100 - position);
                zerosCount += remainingAfterFirstZero / 100;
            } else if (position == 0 && distance > 0) {
                // Starting at 0, going right means we immediately leave 0
                // We come back to 0 every 100 clicks
                zerosCount += distance / 100;
            }

            // Calculate final position
            position = (position + distance) % 100;
        }
    }

    return zerosCount;
}

int main() {
    // Get the path to input.txt relative to the executable
    fs::path execPath = fs::current_path();
    fs::path inputPath = execPath / ".." / "input.txt";

    // If that doesn't exist, try relative to the source file location
    if (!fs::exists(inputPath)) {
        inputPath = "../input.txt";
    }

    auto rotations = parseInput(inputPath.string());

    if (rotations.empty()) {
        std::cerr << "No rotations parsed from input file" << std::endl;
        return 1;
    }

    std::cout << "Part 1: " << part1(rotations) << std::endl;
    std::cout << "Part 2: " << part2(rotations) << std::endl;

    return 0;
}
