package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

type Pair struct {
	a1, b1, a2, b2 int
}

func parseInput(filename string) ([]Pair, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var pairs []Pair
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}

		parts := strings.Split(line, ",")
		left := strings.Split(parts[0], "-")
		right := strings.Split(parts[1], "-")

		a1, _ := strconv.Atoi(left[0])
		b1, _ := strconv.Atoi(left[1])
		a2, _ := strconv.Atoi(right[0])
		b2, _ := strconv.Atoi(right[1])

		pairs = append(pairs, Pair{a1, b1, a2, b2})
	}

	return pairs, scanner.Err()
}

// fullyContains checks if one range fully contains the other
func fullyContains(p Pair) bool {
	return (p.a1 <= p.a2 && p.b1 >= p.b2) || (p.a2 <= p.a1 && p.b2 >= p.b1)
}

// overlaps checks if ranges overlap at all
func overlaps(p Pair) bool {
	return p.a1 <= p.b2 && p.a2 <= p.b1
}

func part1(pairs []Pair) int {
	count := 0
	for _, p := range pairs {
		if fullyContains(p) {
			count++
		}
	}
	return count
}

func part2(pairs []Pair) int {
	count := 0
	for _, p := range pairs {
		if overlaps(p) {
			count++
		}
	}
	return count
}

func main() {
	exePath, _ := os.Executable()
	exeDir := filepath.Dir(exePath)
	inputFile := filepath.Join(exeDir, "..", "input.txt")

	// If running with go run, use the source file directory
	if _, err := os.Stat(inputFile); os.IsNotExist(err) {
		inputFile = filepath.Join(".", "..", "input.txt")
	}

	pairs, err := parseInput(inputFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Part 1:", part1(pairs))
	fmt.Println("Part 2:", part2(pairs))
}
