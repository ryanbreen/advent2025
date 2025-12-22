package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

// Position represents a (row, col) coordinate
type Position struct {
	row, col int
}

// CacheKey for memoization
type CacheKey struct {
	from      rune
	to        rune
	depth     int
	isNumeric bool
}

var (
	// Numeric keypad layout
	numeric = map[rune]Position{
		'7': {0, 0}, '8': {0, 1}, '9': {0, 2},
		'4': {1, 0}, '5': {1, 1}, '6': {1, 2},
		'1': {2, 0}, '2': {2, 1}, '3': {2, 2},
		'0': {3, 1}, 'A': {3, 2},
	}
	numericGap = Position{3, 0}

	// Directional keypad layout
	directional = map[rune]Position{
		'^': {0, 1}, 'A': {0, 2},
		'<': {1, 0}, 'v': {1, 1}, '>': {1, 2},
	}
	directionalGap = Position{0, 0}

	// Cache for memoization
	cache = make(map[CacheKey]int)
)

// shortestPaths finds all shortest paths from start to end, avoiding gap
func shortestPaths(keypad map[rune]Position, gap Position, start, end rune) []string {
	startPos := keypad[start]
	endPos := keypad[end]

	var paths []string

	var dfs func(r, c int, path string)
	dfs = func(r, c int, path string) {
		if r == gap.row && c == gap.col {
			return
		}
		if r == endPos.row && c == endPos.col {
			paths = append(paths, path)
			return
		}
		// Move vertically toward target
		if r < endPos.row {
			dfs(r+1, c, path+"v")
		} else if r > endPos.row {
			dfs(r-1, c, path+"^")
		}
		// Move horizontally toward target
		if c < endPos.col {
			dfs(r, c+1, path+">")
		} else if c > endPos.col {
			dfs(r, c-1, path+"<")
		}
	}

	dfs(startPos.row, startPos.col, "")

	if len(paths) == 0 {
		return []string{""}
	}
	return paths
}

// minPressesForMove computes minimum presses needed to move from fromChar to toChar and press
func minPressesForMove(fromChar, toChar rune, depth int, isNumeric bool) int {
	key := CacheKey{fromChar, toChar, depth, isNumeric}
	if val, ok := cache[key]; ok {
		return val
	}

	var keypad map[rune]Position
	var gap Position
	if isNumeric {
		keypad = numeric
		gap = numericGap
	} else {
		keypad = directional
		gap = directionalGap
	}

	paths := shortestPaths(keypad, gap, fromChar, toChar)

	var result int
	if depth == 0 {
		// At human level, just return path length + 1 for 'A' press
		minLen := len(paths[0])
		for _, p := range paths {
			if len(p) < minLen {
				minLen = len(p)
			}
		}
		result = minLen + 1
	} else {
		best := int(^uint(0) >> 1) // Max int
		for _, path := range paths {
			// Need to type path + 'A' on the directional keypad above
			sequence := path + "A"
			cost := 0
			current := 'A'
			for _, char := range sequence {
				cost += minPressesForMove(current, char, depth-1, false)
				current = char
			}
			if cost < best {
				best = cost
			}
		}
		result = best
	}

	cache[key] = result
	return result
}

// solveCode computes minimum presses to type code on numeric keypad with given robot depth
func solveCode(code string, depth int) int {
	total := 0
	current := 'A'
	for _, char := range code {
		total += minPressesForMove(current, char, depth, true)
		current = char
	}
	return total
}

// complexity computes the complexity: length * numeric part of code
func complexity(code string, length int) int {
	// Remove trailing 'A' and convert to int
	numericPart, _ := strconv.Atoi(strings.TrimRight(code, "A"))
	return length * numericPart
}

// part1 solves part 1: 2 intermediate robots (depth = 2)
func part1(codes []string) int {
	total := 0
	for _, code := range codes {
		length := solveCode(code, 2)
		total += complexity(code, length)
	}
	return total
}

// part2 solves part 2: 25 intermediate robots (depth = 25)
func part2(codes []string) int {
	total := 0
	for _, code := range codes {
		length := solveCode(code, 25)
		total += complexity(code, length)
	}
	return total
}

func main() {
	// Read input file
	exePath, _ := os.Executable()
	exeDir := filepath.Dir(exePath)
	inputPath := filepath.Join(exeDir, "..", "input.txt")

	// Try current directory if executable path doesn't work
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		inputPath = "../input.txt"
	}

	file, err := os.Open(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening input file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var codes []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line != "" {
			codes = append(codes, line)
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Part 1:", part1(codes))

	// Clear cache between parts for fresh calculation
	cache = make(map[CacheKey]int)

	fmt.Println("Part 2:", part2(codes))
}
