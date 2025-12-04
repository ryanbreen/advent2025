#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <cmath>

using namespace std;

bool is_safe_report(const vector<int>& levels) {
    if (levels.size() < 2) return true;

    bool increasing = levels[1] > levels[0];

    for (size_t i = 0; i < levels.size() - 1; i++) {
        int diff = levels[i + 1] - levels[i];
        int abs_diff = abs(diff);

        // Check if difference is in valid range [1, 3]
        if (abs_diff < 1 || abs_diff > 3) {
            return false;
        }

        // Check if direction is consistent
        if ((increasing && diff <= 0) || (!increasing && diff >= 0)) {
            return false;
        }
    }

    return true;
}

bool is_safe_with_dampener(const vector<int>& levels) {
    // First check if already safe
    if (is_safe_report(levels)) {
        return true;
    }

    // Try removing each level one at a time
    for (size_t skip = 0; skip < levels.size(); skip++) {
        vector<int> temp;
        for (size_t i = 0; i < levels.size(); i++) {
            if (i != skip) {
                temp.push_back(levels[i]);
            }
        }

        if (is_safe_report(temp)) {
            return true;
        }
    }

    return false;
}

int main() {
    ifstream file("../input.txt");
    if (!file.is_open()) {
        cerr << "Error opening file" << endl;
        return 1;
    }

    vector<vector<int>> reports;
    string line;

    // Read and parse input
    while (getline(file, line)) {
        vector<int> levels;
        istringstream iss(line);
        int num;

        while (iss >> num) {
            levels.push_back(num);
        }

        if (!levels.empty()) {
            reports.push_back(levels);
        }
    }

    file.close();

    // Part 1: Count safe reports
    int safe_count = 0;
    for (const auto& report : reports) {
        if (is_safe_report(report)) {
            safe_count++;
        }
    }

    cout << "Part 1: " << safe_count << endl;

    // Part 2: Count safe reports with dampener
    int safe_with_dampener = 0;
    for (const auto& report : reports) {
        if (is_safe_with_dampener(report)) {
            safe_with_dampener++;
        }
    }

    cout << "Part 2: " << safe_with_dampener << endl;

    return 0;
}
