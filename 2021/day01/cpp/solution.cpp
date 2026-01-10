#include <fstream>
#include <iostream>
#include <vector>

int main() {
    std::ifstream file("../input.txt");
    std::vector<int> depths;
    int depth;

    while (file >> depth) {
        depths.push_back(depth);
    }

    // Part 1: Count depth increases
    int part1 = 0;
    for (size_t i = 1; i < depths.size(); ++i) {
        if (depths[i] > depths[i - 1]) {
            ++part1;
        }
    }

    // Part 2: Count sliding window sum increases
    // Window sum increases when depths[i+3] > depths[i]
    // because: (a+b+c) < (b+c+d) simplifies to a < d
    int part2 = 0;
    for (size_t i = 0; i + 3 < depths.size() + 1; ++i) {
        if (depths[i + 3] > depths[i]) {
            ++part2;
        }
    }

    std::cout << "Part 1: " << part1 << std::endl;
    std::cout << "Part 2: " << part2 << std::endl;

    return 0;
}
