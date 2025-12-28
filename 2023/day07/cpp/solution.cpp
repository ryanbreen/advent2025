#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <algorithm>
#include <array>

// Card strength order (higher index = stronger)
const std::string CARD_STRENGTH = "23456789TJQKA";
const std::string CARD_STRENGTH_JOKER = "J23456789TQKA";  // J is weakest in Part 2

struct Hand {
    std::string cards;
    int bid;
};

int getCardStrength(char c, const std::string& order) {
    return order.find(c);
}

int getHandType(const std::string& hand) {
    // Count occurrences of each card
    std::array<int, 256> counts{};
    for (char c : hand) {
        counts[static_cast<unsigned char>(c)]++;
    }

    // Collect non-zero counts and sort descending
    std::vector<int> sortedCounts;
    for (int count : counts) {
        if (count > 0) {
            sortedCounts.push_back(count);
        }
    }
    std::sort(sortedCounts.begin(), sortedCounts.end(), std::greater<int>());

    // Determine hand type
    if (sortedCounts == std::vector<int>{5}) {
        return 6;  // Five of a kind
    } else if (sortedCounts == std::vector<int>{4, 1}) {
        return 5;  // Four of a kind
    } else if (sortedCounts == std::vector<int>{3, 2}) {
        return 4;  // Full house
    } else if (sortedCounts == std::vector<int>{3, 1, 1}) {
        return 3;  // Three of a kind
    } else if (sortedCounts == std::vector<int>{2, 2, 1}) {
        return 2;  // Two pair
    } else if (sortedCounts == std::vector<int>{2, 1, 1, 1}) {
        return 1;  // One pair
    } else {
        return 0;  // High card
    }
}

int getHandTypeWithJokers(const std::string& hand) {
    int jokerCount = std::count(hand.begin(), hand.end(), 'J');

    if (jokerCount == 0) {
        return getHandType(hand);
    }
    if (jokerCount == 5) {
        return 6;  // Five of a kind
    }

    // Count non-joker cards
    std::array<int, 256> counts{};
    for (char c : hand) {
        if (c != 'J') {
            counts[static_cast<unsigned char>(c)]++;
        }
    }

    // Collect non-zero counts and sort descending
    std::vector<int> sortedCounts;
    for (int count : counts) {
        if (count > 0) {
            sortedCounts.push_back(count);
        }
    }
    std::sort(sortedCounts.begin(), sortedCounts.end(), std::greater<int>());

    // Add jokers to the highest count
    sortedCounts[0] += jokerCount;

    // Determine hand type
    if (sortedCounts == std::vector<int>{5}) {
        return 6;  // Five of a kind
    } else if (sortedCounts == std::vector<int>{4, 1}) {
        return 5;  // Four of a kind
    } else if (sortedCounts == std::vector<int>{3, 2}) {
        return 4;  // Full house
    } else if (sortedCounts == std::vector<int>{3, 1, 1}) {
        return 3;  // Three of a kind
    } else if (sortedCounts == std::vector<int>{2, 2, 1}) {
        return 2;  // Two pair
    } else if (sortedCounts == std::vector<int>{2, 1, 1, 1}) {
        return 1;  // One pair
    } else {
        return 0;  // High card
    }
}

bool compareHandsPart1(const Hand& a, const Hand& b) {
    int typeA = getHandType(a.cards);
    int typeB = getHandType(b.cards);

    if (typeA != typeB) {
        return typeA < typeB;
    }

    // Compare card by card
    for (size_t i = 0; i < 5; ++i) {
        int strengthA = getCardStrength(a.cards[i], CARD_STRENGTH);
        int strengthB = getCardStrength(b.cards[i], CARD_STRENGTH);
        if (strengthA != strengthB) {
            return strengthA < strengthB;
        }
    }
    return false;
}

bool compareHandsPart2(const Hand& a, const Hand& b) {
    int typeA = getHandTypeWithJokers(a.cards);
    int typeB = getHandTypeWithJokers(b.cards);

    if (typeA != typeB) {
        return typeA < typeB;
    }

    // Compare card by card (with J as weakest)
    for (size_t i = 0; i < 5; ++i) {
        int strengthA = getCardStrength(a.cards[i], CARD_STRENGTH_JOKER);
        int strengthB = getCardStrength(b.cards[i], CARD_STRENGTH_JOKER);
        if (strengthA != strengthB) {
            return strengthA < strengthB;
        }
    }
    return false;
}

long long part1(std::vector<Hand> hands) {
    std::sort(hands.begin(), hands.end(), compareHandsPart1);

    long long total = 0;
    for (size_t rank = 0; rank < hands.size(); ++rank) {
        total += (rank + 1) * hands[rank].bid;
    }
    return total;
}

long long part2(std::vector<Hand> hands) {
    std::sort(hands.begin(), hands.end(), compareHandsPart2);

    long long total = 0;
    for (size_t rank = 0; rank < hands.size(); ++rank) {
        total += (rank + 1) * hands[rank].bid;
    }
    return total;
}

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Error: Could not open input.txt" << std::endl;
        return 1;
    }

    std::vector<Hand> hands;
    std::string line;
    while (std::getline(file, line)) {
        if (line.empty()) continue;

        std::istringstream iss(line);
        Hand hand;
        iss >> hand.cards >> hand.bid;
        hands.push_back(hand);
    }

    std::cout << "Part 1: " << part1(hands) << std::endl;
    std::cout << "Part 2: " << part2(hands) << std::endl;

    return 0;
}
