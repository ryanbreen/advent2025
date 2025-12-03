#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <cstdint>

using namespace std;

// Part 1: Select 2 batteries per bank to maximize 2-digit number
int part1(const vector<string>& lines) {
    int total = 0;

    for (const string& line : lines) {
        int n = line.length();

        // Precompute max suffix: max_suffix[i] = max digit from position i to end
        vector<int> max_suffix(n);
        max_suffix[n - 1] = line[n - 1] - '0';
        for (int i = n - 2; i >= 0; i--) {
            max_suffix[i] = max(line[i] - '0', max_suffix[i + 1]);
        }

        int max_joltage = 0;
        // For each possible first battery position
        for (int i = 0; i < n - 1; i++) {
            int first_digit = line[i] - '0';
            // The maximum second digit is the max from position i+1 onwards
            int max_second = max_suffix[i + 1];
            int joltage = first_digit * 10 + max_second;
            max_joltage = max(max_joltage, joltage);
        }

        total += max_joltage;
    }

    return total;
}

// Part 2: Select 12 batteries per bank to maximize 12-digit number
uint64_t part2(const vector<string>& lines) {
    uint64_t total = 0;

    for (const string& line : lines) {
        int n = line.length();
        int k = 12;  // Select exactly 12 batteries

        // Greedy algorithm to select k digits that form the maximum number
        string result = "";
        int current_pos = 0;

        for (int i = 0; i < k; i++) {
            // How many digits we still need to select after this one
            int remaining_needed = k - i - 1;
            // Latest position we can start searching from
            int search_end = n - remaining_needed;

            // Find the maximum digit in the valid range
            int max_digit = -1;
            int max_pos = current_pos;
            for (int j = current_pos; j < search_end; j++) {
                int digit = line[j] - '0';
                if (digit > max_digit) {
                    max_digit = digit;
                    max_pos = j;
                }
            }

            result += to_string(max_digit);
            current_pos = max_pos + 1;
        }

        // Convert the 12-digit string to uint64_t
        uint64_t joltage = stoull(result);
        total += joltage;
    }

    return total;
}

int main() {
    // Read input file
    ifstream input("../input.txt");
    if (!input.is_open()) {
        cerr << "Error: Could not open input.txt" << endl;
        return 1;
    }

    vector<string> lines;
    string line;
    while (getline(input, line)) {
        if (!line.empty()) {
            lines.push_back(line);
        }
    }
    input.close();

    // Solve both parts
    cout << "Part 1: " << part1(lines) << endl;
    cout << "Part 2: " << part2(lines) << endl;

    return 0;
}
