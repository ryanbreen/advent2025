package main

import (
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
)

func parseInput(text string) [][]int {
	lines := strings.Split(strings.TrimSpace(text), "\n")
	histories := make([][]int, len(lines))

	for i, line := range lines {
		parts := strings.Fields(line)
		seq := make([]int, len(parts))
		for j, p := range parts {
			num, err := strconv.Atoi(p)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error parsing number %q: %v\n", p, err)
				os.Exit(1)
			}
			seq[j] = num
		}
		histories[i] = seq
	}

	return histories
}

func allZero(seq []int) bool {
	for _, v := range seq {
		if v != 0 {
			return false
		}
	}
	return true
}

// extrapolateNext computes the next value in the sequence by building
// difference pyramids and propagating the extrapolated value upward.
func extrapolateNext(seq []int) int {
	if allZero(seq) {
		return 0
	}

	// Compute differences
	diffs := make([]int, len(seq)-1)
	for i := range diffs {
		diffs[i] = seq[i+1] - seq[i]
	}

	// Recursively extrapolate and add to last element
	return seq[len(seq)-1] + extrapolateNext(diffs)
}

func reverse(seq []int) {
	for i, j := 0, len(seq)-1; i < j; i, j = i+1, j-1 {
		seq[i], seq[j] = seq[j], seq[i]
	}
}

func part1(histories [][]int) int {
	sum := 0
	for _, h := range histories {
		sum += extrapolateNext(slices.Clone(h))
	}
	return sum
}

func part2(histories [][]int) int {
	sum := 0
	for _, h := range histories {
		// Reverse the sequence and extrapolate forward
		// This is equivalent to extrapolating backward on the original
		reversed := slices.Clone(h)
		reverse(reversed)
		sum += extrapolateNext(reversed)
	}
	return sum
}

func main() {
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	histories := parseInput(string(data))

	fmt.Printf("Part 1: %d\n", part1(histories))
	fmt.Printf("Part 2: %d\n", part2(histories))
}
