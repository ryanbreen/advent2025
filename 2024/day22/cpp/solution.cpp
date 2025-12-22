#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <cstdint>
#include <algorithm>

using namespace std;

constexpr int SECRET_COUNT = 2000;
constexpr int32_t PRUNE_MASK = 0xFFFFFF;  // 2^24 - 1

int32_t next_secret(int32_t secret) {
    // Step 1: multiply by 64 (shift left 6), mix (XOR), prune (mask)
    secret ^= (secret << 6);
    secret &= PRUNE_MASK;

    // Step 2: divide by 32 (shift right 5), mix, prune
    secret ^= (secret >> 5);
    secret &= PRUNE_MASK;

    // Step 3: multiply by 2048 (shift left 11), mix, prune
    secret ^= (secret << 11);
    secret &= PRUNE_MASK;

    return secret;
}

// Pack 4 changes into a 32-bit key for efficient hashing
// Each change is -9 to 9, add 9 to make it 0 to 18, fits in 5 bits
inline int32_t pack_sequence(int8_t c1, int8_t c2, int8_t c3, int8_t c4) {
    return ((c1 + 9) << 15) | ((c2 + 9) << 10) | ((c3 + 9) << 5) | (c4 + 9);
}

int64_t part1(const vector<int32_t>& initial_secrets) {
    int64_t total = 0;

    for (int32_t initial : initial_secrets) {
        int32_t secret = initial;
        for (int i = 0; i < SECRET_COUNT; i++) {
            secret = next_secret(secret);
        }
        total += secret;
    }

    return total;
}

int32_t part2(const vector<int32_t>& initial_secrets) {
    // Map from packed sequence -> total bananas
    unordered_map<int32_t, int32_t> sequence_totals;
    sequence_totals.reserve(200000);  // Pre-allocate for better performance

    for (int32_t initial : initial_secrets) {
        int32_t secret = initial;

        // Generate prices and changes on-the-fly (no storage of all secrets)
        int8_t prices[SECRET_COUNT + 1];
        int8_t changes[SECRET_COUNT];

        prices[0] = secret % 10;
        for (int i = 0; i < SECRET_COUNT; i++) {
            secret = next_secret(secret);
            prices[i + 1] = secret % 10;
            changes[i] = prices[i + 1] - prices[i];
        }

        // Track first occurrence of each 4-change sequence for this buyer
        unordered_set<int32_t> seen;
        seen.reserve(2000);  // Pre-allocate

        for (int i = 0; i < SECRET_COUNT - 3; i++) {
            int32_t seq = pack_sequence(changes[i], changes[i+1], changes[i+2], changes[i+3]);

            if (seen.find(seq) == seen.end()) {
                seen.insert(seq);
                // Price we get is after these 4 changes (at index i+4)
                sequence_totals[seq] += prices[i + 4];
            }
        }
    }

    // Find maximum total
    int32_t max_bananas = 0;
    for (const auto& pair : sequence_totals) {
        max_bananas = max(max_bananas, pair.second);
    }

    return max_bananas;
}

int main() {
    // Read input file
    ifstream infile("../input.txt");
    if (!infile) {
        cerr << "Error: Could not open input file" << endl;
        return 1;
    }

    vector<int32_t> initial_secrets;
    string line;
    while (getline(infile, line)) {
        if (!line.empty()) {
            initial_secrets.push_back(stoi(line));
        }
    }
    infile.close();

    cout << "Part 1: " << part1(initial_secrets) << endl;
    cout << "Part 2: " << part2(initial_secrets) << endl;

    return 0;
}
