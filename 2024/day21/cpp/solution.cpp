/**
 * Day 21: Keypad Conundrum - Robot chain control with shortest path optimization
 *
 * Algorithm:
 * 1. Two keypads: numeric (7-9, 4-6, 1-3, gap-0-A) and directional (gap-^-A, <-v->)
 * 2. Find shortest paths between buttons avoiding gaps
 * 3. Use memoization: min_presses_for_move(from, to, depth, is_numeric)
 * 4. Part 1: depth=2, Part 2: depth=25
 * 5. Complexity = sequence_length * numeric_part_of_code
 */

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <tuple>
#include <algorithm>
#include <limits>

using namespace std;

// Position type
using Pos = pair<int, int>;

// Keypad layouts
map<char, Pos> NUMERIC = {
    {'7', {0, 0}}, {'8', {0, 1}}, {'9', {0, 2}},
    {'4', {1, 0}}, {'5', {1, 1}}, {'6', {1, 2}},
    {'1', {2, 0}}, {'2', {2, 1}}, {'3', {2, 2}},
    {'0', {3, 1}}, {'A', {3, 2}}
};
Pos NUMERIC_GAP = {3, 0};

map<char, Pos> DIRECTIONAL = {
    {'^', {0, 1}}, {'A', {0, 2}},
    {'<', {1, 0}}, {'v', {1, 1}}, {'>', {1, 2}}
};
Pos DIRECTIONAL_GAP = {0, 0};

// Memoization cache
map<tuple<char, char, int, bool>, long long> memo;

/**
 * Find all shortest paths from start to end, avoiding gap.
 * Uses DFS to explore only shortest paths (Manhattan distance).
 */
vector<string> shortest_paths(const map<char, Pos>& keypad, Pos gap, char start, char end) {
    Pos sp = keypad.at(start);
    Pos ep = keypad.at(end);

    vector<string> paths;

    function<void(int, int, string)> dfs = [&](int r, int c, string path) {
        // Avoid gap position
        if (make_pair(r, c) == gap) {
            return;
        }

        // Reached target
        if (make_pair(r, c) == ep) {
            paths.push_back(path);
            return;
        }

        // Move vertically toward target
        if (r < ep.first) {
            dfs(r + 1, c, path + 'v');
        } else if (r > ep.first) {
            dfs(r - 1, c, path + '^');
        }

        // Move horizontally toward target
        if (c < ep.second) {
            dfs(r, c + 1, path + '>');
        } else if (c > ep.second) {
            dfs(r, c - 1, path + '<');
        }
    };

    dfs(sp.first, sp.second, "");

    // Return empty path if start == end
    if (paths.empty()) {
        paths.push_back("");
    }

    return paths;
}

/**
 * Compute minimum presses needed to move from from_char to to_char and press,
 * at given depth in the robot chain.
 */
long long min_presses_for_move(char from_char, char to_char, int depth, bool is_numeric) {
    // Check memo cache
    auto key = make_tuple(from_char, to_char, depth, is_numeric);
    if (memo.count(key)) {
        return memo[key];
    }

    // Select keypad
    const map<char, Pos>& keypad = is_numeric ? NUMERIC : DIRECTIONAL;
    Pos gap = is_numeric ? NUMERIC_GAP : DIRECTIONAL_GAP;

    // Find all shortest paths
    vector<string> paths = shortest_paths(keypad, gap, from_char, to_char);

    long long result;

    if (depth == 0) {
        // At human level, just return minimum path length + 1 for 'A' press
        size_t min_len = numeric_limits<size_t>::max();
        for (const string& p : paths) {
            min_len = min(min_len, p.size());
        }
        result = min_len + 1;
    } else {
        // Try each path and find minimum cost
        long long best = numeric_limits<long long>::max();

        for (const string& path : paths) {
            // Need to type path + 'A' on the directional keypad above
            string sequence = path + 'A';
            long long cost = 0;
            char current = 'A';

            for (char ch : sequence) {
                cost += min_presses_for_move(current, ch, depth - 1, false);
                current = ch;
            }

            best = min(best, cost);
        }

        result = best;
    }

    // Store in cache
    memo[key] = result;
    return result;
}

/**
 * Compute minimum presses to type code on numeric keypad with given robot depth.
 */
long long solve_code(const string& code, int depth) {
    long long total = 0;
    char current = 'A';

    for (char ch : code) {
        total += min_presses_for_move(current, ch, depth, true);
        current = ch;
    }

    return total;
}

/**
 * Compute complexity: length * numeric part of code.
 */
long long complexity(const string& code, long long length) {
    // Extract numeric part (strip trailing 'A')
    string numeric_str = code.substr(0, code.find('A'));
    long long numeric_part = stoll(numeric_str);
    return length * numeric_part;
}

/**
 * Part 1: 2 intermediate robots (depth = 2).
 */
long long part1(const vector<string>& codes) {
    long long total = 0;
    for (const string& code : codes) {
        long long length = solve_code(code, 2);
        total += complexity(code, length);
    }
    return total;
}

/**
 * Part 2: 25 intermediate robots (depth = 25).
 */
long long part2(const vector<string>& codes) {
    long long total = 0;
    for (const string& code : codes) {
        long long length = solve_code(code, 25);
        total += complexity(code, length);
    }
    return total;
}

int main() {
    // Read input
    ifstream input("../input.txt");
    if (!input) {
        cerr << "Error: Cannot open input.txt" << endl;
        return 1;
    }

    vector<string> codes;
    string line;
    while (getline(input, line)) {
        if (!line.empty()) {
            codes.push_back(line);
        }
    }
    input.close();

    // Solve both parts
    cout << "Part 1: " << part1(codes) << endl;

    // Clear memo for part 2 (optional, but ensures fresh start)
    // memo.clear();  // Actually not needed, memo is shared and valid

    cout << "Part 2: " << part2(codes) << endl;

    return 0;
}
