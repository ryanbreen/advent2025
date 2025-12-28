<cfscript>
/**
 * Advent of Code 2023 - Day 7: Camel Cards
 * ColdFusion (CFML) Solution
 */

// Read input file
// Get the directory of the current script and go up one level
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
inputText = trim(fileRead(inputPath));
lines = listToArray(inputText, chr(10));

// Card strength mappings (higher index = stronger)
CARD_STRENGTH = "23456789TJQKA";
CARD_STRENGTH_JOKER = "J23456789TQKA";  // J is weakest in Part 2

/**
 * Get the hand type as an integer (higher = stronger)
 * 6 = Five of a kind, 5 = Four of a kind, 4 = Full house,
 * 3 = Three of a kind, 2 = Two pair, 1 = One pair, 0 = High card
 */
function getHandType(hand) {
    var counts = {};

    // Count each card
    for (var i = 1; i <= len(hand); i++) {
        var card = mid(hand, i, 1);
        if (!structKeyExists(counts, card)) {
            counts[card] = 0;
        }
        counts[card]++;
    }

    // Get sorted counts (descending)
    var countValues = structValueArray(counts);
    arraySort(countValues, "numeric", "desc");

    // Determine hand type based on pattern
    if (arrayLen(countValues) == 1 && countValues[1] == 5) {
        return 6;  // Five of a kind
    } else if (arrayLen(countValues) == 2 && countValues[1] == 4) {
        return 5;  // Four of a kind
    } else if (arrayLen(countValues) == 2 && countValues[1] == 3 && countValues[2] == 2) {
        return 4;  // Full house
    } else if (arrayLen(countValues) == 3 && countValues[1] == 3) {
        return 3;  // Three of a kind
    } else if (arrayLen(countValues) == 3 && countValues[1] == 2 && countValues[2] == 2) {
        return 2;  // Two pair
    } else if (arrayLen(countValues) == 4 && countValues[1] == 2) {
        return 1;  // One pair
    } else {
        return 0;  // High card
    }
}

/**
 * Get hand type with joker rules (J acts as wildcard)
 */
function getHandTypeWithJokers(hand) {
    var jokerCount = 0;

    // Count jokers
    for (var i = 1; i <= len(hand); i++) {
        if (mid(hand, i, 1) == "J") {
            jokerCount++;
        }
    }

    // No jokers - use regular hand type
    if (jokerCount == 0) {
        return getHandType(hand);
    }

    // All jokers = five of a kind
    if (jokerCount == 5) {
        return 6;
    }

    // Count non-joker cards
    var counts = {};
    for (var i = 1; i <= len(hand); i++) {
        var card = mid(hand, i, 1);
        if (card != "J") {
            if (!structKeyExists(counts, card)) {
                counts[card] = 0;
            }
            counts[card]++;
        }
    }

    // Get sorted counts (descending)
    var countValues = structValueArray(counts);
    arraySort(countValues, "numeric", "desc");

    // Add jokers to highest count
    countValues[1] += jokerCount;

    // Determine hand type based on pattern
    if (arrayLen(countValues) == 1 && countValues[1] == 5) {
        return 6;  // Five of a kind
    } else if (arrayLen(countValues) == 2 && countValues[1] == 4) {
        return 5;  // Four of a kind
    } else if (arrayLen(countValues) == 2 && countValues[1] == 3 && countValues[2] == 2) {
        return 4;  // Full house
    } else if (arrayLen(countValues) == 3 && countValues[1] == 3) {
        return 3;  // Three of a kind
    } else if (arrayLen(countValues) == 3 && countValues[1] == 2 && countValues[2] == 2) {
        return 2;  // Two pair
    } else if (arrayLen(countValues) == 4 && countValues[1] == 2) {
        return 1;  // One pair
    } else {
        return 0;  // High card
    }
}

/**
 * Get card strength value (for tiebreakers)
 */
function getCardStrength(card, useJokerRules) {
    var strengthStr = useJokerRules ? CARD_STRENGTH_JOKER : CARD_STRENGTH;
    return find(card, strengthStr);
}

/**
 * Compare two hands for sorting
 * Returns: -1 if hand1 < hand2, 0 if equal, 1 if hand1 > hand2
 */
function compareHands(hand1, hand2, useJokerRules) {
    // Get hand types
    var type1 = useJokerRules ? getHandTypeWithJokers(hand1) : getHandType(hand1);
    var type2 = useJokerRules ? getHandTypeWithJokers(hand2) : getHandType(hand2);

    // Compare by type first
    if (type1 < type2) return -1;
    if (type1 > type2) return 1;

    // Same type - compare card by card
    for (var i = 1; i <= 5; i++) {
        var strength1 = getCardStrength(mid(hand1, i, 1), useJokerRules);
        var strength2 = getCardStrength(mid(hand2, i, 1), useJokerRules);

        if (strength1 < strength2) return -1;
        if (strength1 > strength2) return 1;
    }

    return 0;  // Hands are equal
}

/**
 * Bubble sort hands array (CFML array sorting with custom comparator is limited)
 */
function sortHands(handsArray, useJokerRules) {
    var n = arrayLen(handsArray);
    var swapped = true;

    while (swapped) {
        swapped = false;
        for (var i = 1; i < n; i++) {
            if (compareHands(handsArray[i].hand, handsArray[i+1].hand, useJokerRules) > 0) {
                // Swap
                var temp = handsArray[i];
                handsArray[i] = handsArray[i+1];
                handsArray[i+1] = temp;
                swapped = true;
            }
        }
        n--;
    }

    return handsArray;
}

/**
 * Parse input into array of hand/bid structs
 */
function parseInput() {
    var hands = [];

    for (var line in lines) {
        line = trim(line);
        if (len(line) > 0) {
            var parts = listToArray(line, " ");
            arrayAppend(hands, {
                "hand": parts[1],
                "bid": val(parts[2])
            });
        }
    }

    return hands;
}

/**
 * Calculate total winnings
 */
function calculateWinnings(sortedHands) {
    var total = 0;

    for (var rank = 1; rank <= arrayLen(sortedHands); rank++) {
        total += rank * sortedHands[rank].bid;
    }

    return total;
}

/**
 * Part 1: Standard hand ranking
 */
function part1() {
    var hands = parseInput();
    hands = sortHands(hands, false);
    return calculateWinnings(hands);
}

/**
 * Part 2: Joker rules (J is wildcard but weakest for tiebreakers)
 */
function part2() {
    var hands = parseInput();
    hands = sortHands(hands, true);
    return calculateWinnings(hands);
}

// Execute and output results
writeOutput("Part 1: " & part1() & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
