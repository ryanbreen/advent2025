#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <unordered_map>

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Failed to open input.txt" << std::endl;
        return 1;
    }

    std::vector<int> left, right;
    int l, r;

    // Read input
    while (file >> l >> r) {
        left.push_back(l);
        right.push_back(r);
    }
    file.close();

    // Part 1: Sort both lists and calculate total distance
    std::vector<int> left_sorted = left;
    std::vector<int> right_sorted = right;

    std::sort(left_sorted.begin(), left_sorted.end());
    std::sort(right_sorted.begin(), right_sorted.end());

    long long part1_sum = 0;
    for (size_t i = 0; i < left_sorted.size(); i++) {
        part1_sum += std::abs(left_sorted[i] - right_sorted[i]);
    }

    // Part 2: Calculate similarity score
    // Build frequency map for right list
    std::unordered_map<int, int> right_freq;
    for (int num : right) {
        right_freq[num]++;
    }

    long long part2_sum = 0;
    for (int num : left) {
        part2_sum += (long long)num * right_freq[num];
    }

    std::cout << "Part 1: " << part1_sum << std::endl;
    std::cout << "Part 2: " << part2_sum << std::endl;

    return 0;
}
