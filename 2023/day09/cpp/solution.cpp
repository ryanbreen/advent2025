#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <sstream>
#include <string>
#include <vector>

using Sequence = std::vector<long long>;
using Histories = std::vector<Sequence>;

Histories parse_input(const std::string& filename) {
    Histories histories;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) continue;

        Sequence seq;
        std::istringstream iss(line);
        long long num;
        while (iss >> num) {
            seq.push_back(num);
        }
        histories.push_back(std::move(seq));
    }

    return histories;
}

Sequence get_differences(const Sequence& seq) {
    Sequence diff;
    diff.reserve(seq.size() - 1);

    for (size_t i = 0; i < seq.size() - 1; ++i) {
        diff.push_back(seq[i + 1] - seq[i]);
    }

    return diff;
}

bool all_zeros(const Sequence& seq) {
    return std::all_of(seq.begin(), seq.end(), [](long long x) { return x == 0; });
}

long long extrapolate_next(const Sequence& seq) {
    std::vector<Sequence> sequences;
    sequences.push_back(seq);

    while (!all_zeros(sequences.back())) {
        sequences.push_back(get_differences(sequences.back()));
    }

    // Work backwards, summing the last element of each level
    long long next_val = 0;
    for (auto it = sequences.rbegin(); it != sequences.rend(); ++it) {
        next_val += it->back();
    }

    return next_val;
}

long long part1(const Histories& histories) {
    return std::accumulate(histories.begin(), histories.end(), 0LL,
        [](long long sum, const Sequence& seq) {
            return sum + extrapolate_next(seq);
        });
}

long long part2(const Histories& histories) {
    // Part 2: extrapolate backwards = reverse sequence and extrapolate forwards
    return std::accumulate(histories.begin(), histories.end(), 0LL,
        [](long long sum, const Sequence& seq) {
            Sequence reversed(seq.rbegin(), seq.rend());
            return sum + extrapolate_next(reversed);
        });
}

int main() {
    const auto histories = parse_input("../input.txt");

    std::cout << "Part 1: " << part1(histories) << '\n';
    std::cout << "Part 2: " << part2(histories) << '\n';

    return 0;
}
