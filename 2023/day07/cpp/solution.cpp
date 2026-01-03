#include <iostream>
#include <fstream>
#include <cstdio>
#include <cstring>
#include <vector>
#include <algorithm>

constexpr int HAND_SIZE = 5;
constexpr int NUM_CARDS = 13;

// Hand type rankings (higher value = stronger hand)
enum HandType {
    HIGH_CARD = 0,
    ONE_PAIR = 1,
    TWO_PAIR = 2,
    THREE_OF_A_KIND = 3,
    FULL_HOUSE = 4,
    FOUR_OF_A_KIND = 5,
    FIVE_OF_A_KIND = 6
};

struct Hand {
    char cards[HAND_SIZE + 1];
    int bid;
    int type;
    int card_values[HAND_SIZE];
};

// Card strength order for Part 1 (higher index = stronger)
static const char* CARD_STRENGTH = "23456789TJQKA";
// Card strength order for Part 2 (J is weakest)
static const char* CARD_STRENGTH_JOKER = "J23456789TQKA";

inline int cardValue(char c, const char* strength_order) {
    const char* p = std::strchr(strength_order, c);
    return p ? static_cast<int>(p - strength_order) : -1;
}

int getHandType(const char* hand) {
    int counts[NUM_CARDS] = {0};

    // Count occurrences of each card
    for (int i = 0; i < HAND_SIZE; i++) {
        int idx = cardValue(hand[i], CARD_STRENGTH);
        if (idx >= 0) counts[idx]++;
    }

    // Get sorted counts (descending)
    int sorted_counts[5] = {0};
    int num_counts = 0;
    for (int i = 0; i < NUM_CARDS; i++) {
        if (counts[i] > 0) {
            sorted_counts[num_counts++] = counts[i];
        }
    }

    // Simple bubble sort (small array)
    for (int i = 0; i < num_counts - 1; i++) {
        for (int j = i + 1; j < num_counts; j++) {
            if (sorted_counts[j] > sorted_counts[i]) {
                std::swap(sorted_counts[i], sorted_counts[j]);
            }
        }
    }

    // Determine hand type based on sorted counts
    if (num_counts == 1) return FIVE_OF_A_KIND;
    if (num_counts == 2 && sorted_counts[0] == 4) return FOUR_OF_A_KIND;
    if (num_counts == 2 && sorted_counts[0] == 3) return FULL_HOUSE;
    if (num_counts == 3 && sorted_counts[0] == 3) return THREE_OF_A_KIND;
    if (num_counts == 3 && sorted_counts[0] == 2) return TWO_PAIR;
    if (num_counts == 4) return ONE_PAIR;
    return HIGH_CARD;
}

int getHandTypeWithJokers(const char* hand) {
    int joker_count = 0;
    for (int i = 0; i < HAND_SIZE; i++) {
        if (hand[i] == 'J') joker_count++;
    }

    if (joker_count == 0) return getHandType(hand);
    if (joker_count == 5) return FIVE_OF_A_KIND;

    // Count non-joker cards
    int counts[NUM_CARDS] = {0};
    for (int i = 0; i < HAND_SIZE; i++) {
        if (hand[i] != 'J') {
            int idx = cardValue(hand[i], CARD_STRENGTH);
            if (idx >= 0) counts[idx]++;
        }
    }

    // Get sorted counts (descending)
    int sorted_counts[5] = {0};
    int num_counts = 0;
    for (int i = 0; i < NUM_CARDS; i++) {
        if (counts[i] > 0) {
            sorted_counts[num_counts++] = counts[i];
        }
    }

    // Simple bubble sort
    for (int i = 0; i < num_counts - 1; i++) {
        for (int j = i + 1; j < num_counts; j++) {
            if (sorted_counts[j] > sorted_counts[i]) {
                std::swap(sorted_counts[i], sorted_counts[j]);
            }
        }
    }

    // Add jokers to highest count
    sorted_counts[0] += joker_count;

    // Determine hand type based on sorted counts
    if (num_counts == 1 || sorted_counts[0] == 5) return FIVE_OF_A_KIND;
    if (sorted_counts[0] == 4) return FOUR_OF_A_KIND;
    if (sorted_counts[0] == 3 && sorted_counts[1] == 2) return FULL_HOUSE;
    if (sorted_counts[0] == 3) return THREE_OF_A_KIND;
    if (sorted_counts[0] == 2 && sorted_counts[1] == 2) return TWO_PAIR;
    if (sorted_counts[0] == 2) return ONE_PAIR;
    return HIGH_CARD;
}

bool compareHands(const Hand& a, const Hand& b) {
    // Compare by type first
    if (a.type != b.type) {
        return a.type < b.type;
    }

    // Then by card values left to right
    for (int i = 0; i < HAND_SIZE; i++) {
        if (a.card_values[i] != b.card_values[i]) {
            return a.card_values[i] < b.card_values[i];
        }
    }
    return false;
}

long long solve(std::vector<Hand>& hands, bool part2) {
    // Precompute type and card values for each hand
    const char* strength_order = part2 ? CARD_STRENGTH_JOKER : CARD_STRENGTH;

    for (auto& hand : hands) {
        hand.type = part2 ? getHandTypeWithJokers(hand.cards) : getHandType(hand.cards);
        for (int j = 0; j < HAND_SIZE; j++) {
            hand.card_values[j] = cardValue(hand.cards[j], strength_order);
        }
    }

    // Sort hands
    std::sort(hands.begin(), hands.end(), compareHands);

    // Calculate total winnings
    long long total = 0;
    int rank = 1;
    for (const auto& hand : hands) {
        total += static_cast<long long>(rank++) * hand.bid;
    }

    return total;
}

int main() {
    FILE* fp = std::fopen("../input.txt", "r");
    if (!fp) {
        std::cerr << "Could not open input.txt\n";
        return 1;
    }

    std::vector<Hand> hands1;
    std::vector<Hand> hands2;
    hands1.reserve(1024);
    hands2.reserve(1024);

    char line[256];
    while (std::fgets(line, sizeof(line), fp)) {
        Hand hand;
        if (std::sscanf(line, "%5s %d", hand.cards, &hand.bid) == 2) {
            hands1.push_back(hand);
            hands2.push_back(hand);
        }
    }
    std::fclose(fp);

    long long part1 = solve(hands1, false);
    long long part2_result = solve(hands2, true);

    std::cout << "Part 1: " << part1 << '\n';
    std::cout << "Part 2: " << part2_result << '\n';

    return 0;
}
