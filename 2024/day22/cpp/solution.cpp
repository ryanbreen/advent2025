#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <tuple>
#include <algorithm>

using namespace std;

// Custom hash for tuple<int, int, int, int>
struct TupleHash {
    size_t operator()(const tuple<int, int, int, int>& t) const {
        size_t h1 = hash<int>{}(get<0>(t));
        size_t h2 = hash<int>{}(get<1>(t));
        size_t h3 = hash<int>{}(get<2>(t));
        size_t h4 = hash<int>{}(get<3>(t));
        return h1 ^ (h2 << 1) ^ (h3 << 2) ^ (h4 << 3);
    }
};

long long next_secret(long long secret) {
    // Step 1: multiply by 64 (shift left 6), mix (XOR), prune (mod 16777216)
    secret ^= (secret << 6);
    secret &= 0xFFFFFF;  // Same as % 16777216 (2^24)

    // Step 2: divide by 32 (shift right 5), mix (XOR), prune
    secret ^= (secret >> 5);
    secret &= 0xFFFFFF;

    // Step 3: multiply by 2048 (shift left 11), mix (XOR), prune
    secret ^= (secret << 11);
    secret &= 0xFFFFFF;

    return secret;
}

vector<long long> generate_secrets(long long initial, int count) {
    vector<long long> secrets;
    secrets.reserve(count + 1);
    secrets.push_back(initial);

    long long secret = initial;
    for (int i = 0; i < count; i++) {
        secret = next_secret(secret);
        secrets.push_back(secret);
    }

    return secrets;
}

long long part1(const vector<long long>& initial_secrets) {
    long long total = 0;

    for (long long initial : initial_secrets) {
        long long secret = initial;
        for (int i = 0; i < 2000; i++) {
            secret = next_secret(secret);
        }
        total += secret;
    }

    return total;
}

long long part2(const vector<long long>& initial_secrets) {
    // Map from (change1, change2, change3, change4) -> total bananas
    unordered_map<tuple<int, int, int, int>, long long, TupleHash> sequence_totals;

    for (long long initial : initial_secrets) {
        // Generate 2001 secrets (initial + 2000 new)
        vector<long long> secrets = generate_secrets(initial, 2000);

        // Calculate prices (last digit of each secret)
        vector<int> prices;
        prices.reserve(secrets.size());
        for (long long s : secrets) {
            prices.push_back(s % 10);
        }

        // Calculate changes
        vector<int> changes;
        changes.reserve(prices.size() - 1);
        for (size_t i = 0; i < prices.size() - 1; i++) {
            changes.push_back(prices[i + 1] - prices[i]);
        }

        // Track first occurrence of each 4-change sequence for this buyer
        unordered_set<tuple<int, int, int, int>, TupleHash> seen;

        for (size_t i = 0; i < changes.size() - 3; i++) {
            auto seq = make_tuple(changes[i], changes[i+1], changes[i+2], changes[i+3]);

            if (seen.find(seq) == seen.end()) {
                seen.insert(seq);
                // Price we get is after these 4 changes
                sequence_totals[seq] += prices[i + 4];
            }
        }
    }

    // Find maximum total
    long long max_bananas = 0;
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

    vector<long long> initial_secrets;
    string line;
    while (getline(infile, line)) {
        if (!line.empty()) {
            initial_secrets.push_back(stoll(line));
        }
    }
    infile.close();

    cout << "Part 1: " << part1(initial_secrets) << endl;
    cout << "Part 2: " << part2(initial_secrets) << endl;

    return 0;
}
