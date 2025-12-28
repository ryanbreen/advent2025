package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
)

// Card strength orders (higher index = stronger)
const cardStrength = "23456789TJQKA"
const cardStrengthJoker = "J23456789TQKA" // J is weakest in Part 2

type Hand struct {
	cards string
	bid   int
}

// getHandType returns the hand type as an integer (higher = stronger)
// 6 = Five of a kind, 5 = Four of a kind, 4 = Full house
// 3 = Three of a kind, 2 = Two pair, 1 = One pair, 0 = High card
func getHandType(hand string) int {
	counts := make(map[rune]int)
	for _, c := range hand {
		counts[c]++
	}

	// Get sorted counts (descending)
	var values []int
	for _, v := range counts {
		values = append(values, v)
	}
	sort.Sort(sort.Reverse(sort.IntSlice(values)))

	// Classify hand type
	switch {
	case len(values) == 1: // Five of a kind [5]
		return 6
	case values[0] == 4: // Four of a kind [4, 1]
		return 5
	case values[0] == 3 && values[1] == 2: // Full house [3, 2]
		return 4
	case values[0] == 3: // Three of a kind [3, 1, 1]
		return 3
	case values[0] == 2 && values[1] == 2: // Two pair [2, 2, 1]
		return 2
	case values[0] == 2: // One pair [2, 1, 1, 1]
		return 1
	default: // High card [1, 1, 1, 1, 1]
		return 0
	}
}

// getHandTypeWithJokers returns hand type with J as wildcards
func getHandTypeWithJokers(hand string) int {
	jokerCount := strings.Count(hand, "J")
	if jokerCount == 0 {
		return getHandType(hand)
	}
	if jokerCount == 5 {
		return 6 // Five of a kind
	}

	// Count non-joker cards
	counts := make(map[rune]int)
	for _, c := range hand {
		if c != 'J' {
			counts[c]++
		}
	}

	// Get sorted counts (descending)
	var values []int
	for _, v := range counts {
		values = append(values, v)
	}
	sort.Sort(sort.Reverse(sort.IntSlice(values)))

	// Add jokers to the highest count
	values[0] += jokerCount

	// Classify hand type
	switch {
	case values[0] == 5: // Five of a kind
		return 6
	case values[0] == 4: // Four of a kind
		return 5
	case values[0] == 3 && len(values) > 1 && values[1] == 2: // Full house
		return 4
	case values[0] == 3: // Three of a kind
		return 3
	case values[0] == 2 && len(values) > 1 && values[1] == 2: // Two pair
		return 2
	case values[0] == 2: // One pair
		return 1
	default: // High card
		return 0
	}
}

// handKey returns a sort key for comparing hands
func handKey(hand string, strengthOrder string) []int {
	key := make([]int, len(hand))
	for i, c := range hand {
		key[i] = strings.IndexRune(strengthOrder, c)
	}
	return key
}

// compareHands compares two hands for sorting (returns true if a < b)
func compareHands(a, b Hand, useJokers bool) bool {
	var typeA, typeB int
	var strengthOrder string

	if useJokers {
		typeA = getHandTypeWithJokers(a.cards)
		typeB = getHandTypeWithJokers(b.cards)
		strengthOrder = cardStrengthJoker
	} else {
		typeA = getHandType(a.cards)
		typeB = getHandType(b.cards)
		strengthOrder = cardStrength
	}

	if typeA != typeB {
		return typeA < typeB
	}

	// Compare card by card
	keyA := handKey(a.cards, strengthOrder)
	keyB := handKey(b.cards, strengthOrder)
	for i := range keyA {
		if keyA[i] != keyB[i] {
			return keyA[i] < keyB[i]
		}
	}
	return false
}

func parseInput(filename string) ([]Hand, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var hands []Hand
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		parts := strings.Fields(line)
		bid, _ := strconv.Atoi(parts[1])
		hands = append(hands, Hand{cards: parts[0], bid: bid})
	}
	return hands, scanner.Err()
}

func part1(hands []Hand) int {
	// Make a copy to avoid modifying original
	sorted := make([]Hand, len(hands))
	copy(sorted, hands)

	sort.Slice(sorted, func(i, j int) bool {
		return compareHands(sorted[i], sorted[j], false)
	})

	total := 0
	for rank, hand := range sorted {
		total += (rank + 1) * hand.bid
	}
	return total
}

func part2(hands []Hand) int {
	// Make a copy to avoid modifying original
	sorted := make([]Hand, len(hands))
	copy(sorted, hands)

	sort.Slice(sorted, func(i, j int) bool {
		return compareHands(sorted[i], sorted[j], true)
	})

	total := 0
	for rank, hand := range sorted {
		total += (rank + 1) * hand.bid
	}
	return total
}

func main() {
	// Get the directory of the current executable/source file
	execPath, _ := os.Executable()
	dir := filepath.Dir(execPath)

	// Try multiple possible input locations
	inputPaths := []string{
		filepath.Join(dir, "..", "input.txt"),
		"../input.txt",
		filepath.Join(filepath.Dir(os.Args[0]), "..", "input.txt"),
	}

	var hands []Hand
	var err error

	for _, path := range inputPaths {
		hands, err = parseInput(path)
		if err == nil {
			break
		}
	}

	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(hands))
	fmt.Printf("Part 2: %d\n", part2(hands))
}
