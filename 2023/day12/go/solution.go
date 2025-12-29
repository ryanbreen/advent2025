package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// MemoKey represents the state for memoization
type MemoKey struct {
	pos        int
	groupIdx   int
	currentRun int
}

// countArrangements counts valid arrangements for a pattern and groups
func countArrangements(pattern string, groups []int) int64 {
	memo := make(map[MemoKey]int64)

	var dp func(pos, groupIdx, currentRun int) int64
	dp = func(pos, groupIdx, currentRun int) int64 {
		key := MemoKey{pos, groupIdx, currentRun}
		if val, ok := memo[key]; ok {
			return val
		}

		// Base case: reached end of pattern
		if pos == len(pattern) {
			// Valid if we've matched all groups and no partial run
			if groupIdx == len(groups) && currentRun == 0 {
				return 1
			}
			// Or if we're on the last group and the run matches
			if groupIdx == len(groups)-1 && groups[groupIdx] == currentRun {
				return 1
			}
			return 0
		}

		var result int64
		char := pattern[pos]

		// Option 1: Place operational spring (.)
		if char == '.' || char == '?' {
			if currentRun == 0 {
				// No active run, just move forward
				result += dp(pos+1, groupIdx, 0)
			} else if groupIdx < len(groups) && groups[groupIdx] == currentRun {
				// End current run if it matches expected group size
				result += dp(pos+1, groupIdx+1, 0)
			}
			// Otherwise invalid (run doesn't match group)
		}

		// Option 2: Place damaged spring (#)
		if char == '#' || char == '?' {
			if groupIdx < len(groups) && currentRun < groups[groupIdx] {
				// Can extend current run
				result += dp(pos+1, groupIdx, currentRun+1)
			}
			// Otherwise invalid (exceeds group size or no more groups)
		}

		memo[key] = result
		return result
	}

	return dp(0, 0, 0)
}

// parseLine parses a line into pattern and groups
func parseLine(line string) (string, []int) {
	parts := strings.Fields(line)
	pattern := parts[0]

	groupStrs := strings.Split(parts[1], ",")
	groups := make([]int, len(groupStrs))
	for i, s := range groupStrs {
		var err error
		groups[i], err = strconv.Atoi(s)
		if err != nil {
			panic(fmt.Sprintf("invalid group number: %s", s))
		}
	}

	return pattern, groups
}

// unfold repeats pattern (joined by '?') and groups 5 times
func unfold(pattern string, groups []int) (string, []int) {
	patterns := make([]string, 5)
	for i := 0; i < 5; i++ {
		patterns[i] = pattern
	}
	unfoldedPattern := strings.Join(patterns, "?")

	unfoldedGroups := make([]int, len(groups)*5)
	for i := 0; i < 5; i++ {
		copy(unfoldedGroups[i*len(groups):], groups)
	}

	return unfoldedPattern, unfoldedGroups
}

func part1(lines []string) int64 {
	var total int64
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		pattern, groups := parseLine(line)
		total += countArrangements(pattern, groups)
	}
	return total
}

func part2(lines []string) int64 {
	var total int64
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		pattern, groups := parseLine(line)
		unfoldedPattern, unfoldedGroups := unfold(pattern, groups)
		total += countArrangements(unfoldedPattern, unfoldedGroups)
	}
	return total
}

func main() {
	file, err := os.Open("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(lines))
	fmt.Printf("Part 2: %d\n", part2(lines))
}
