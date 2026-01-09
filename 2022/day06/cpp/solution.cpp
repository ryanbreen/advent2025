#include <iostream>
#include <fstream>
#include <string>
#include <unordered_set>

int find_marker(const std::string& data, int window_size) {
    for (size_t i = window_size; i <= data.length(); ++i) {
        std::unordered_set<char> window(data.begin() + i - window_size, data.begin() + i);
        if (window.size() == static_cast<size_t>(window_size)) {
            return static_cast<int>(i);
        }
    }
    return -1;
}

int part1(const std::string& data) {
    return find_marker(data, 4);
}

int part2(const std::string& data) {
    return find_marker(data, 14);
}

int main() {
    std::ifstream file("../input.txt");
    if (!file.is_open()) {
        std::cerr << "Error: Could not open input file" << std::endl;
        return 1;
    }

    std::string data;
    std::getline(file, data);
    file.close();

    std::cout << "Part 1: " << part1(data) << std::endl;
    std::cout << "Part 2: " << part2(data) << std::endl;

    return 0;
}
