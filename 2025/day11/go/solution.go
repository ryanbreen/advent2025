package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// parseInput reads the graph from input file into an adjacency list
func parseInput(filename string) (map[string][]string, error) {
	graph := make(map[string][]string)

	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}

		parts := strings.Split(line, ": ")
		node := parts[0]

		var neighbors []string
		if len(parts) > 1 && parts[1] != "" {
			neighbors = strings.Fields(parts[1])
		}

		graph[node] = neighbors
	}

	return graph, scanner.Err()
}

// part1 counts all paths from 'you' to 'out' using memoization
func part1(graph map[string][]string) uint64 {
	memo := make(map[string]uint64)

	var countPaths func(node string) uint64
	countPaths = func(node string) uint64 {
		if node == "out" {
			return 1
		}

		// Check memo
		if count, exists := memo[node]; exists {
			return count
		}

		// If node not in graph, no paths
		neighbors, exists := graph[node]
		if !exists {
			return 0
		}

		// Count paths through all neighbors
		total := uint64(0)
		for _, neighbor := range neighbors {
			total += countPaths(neighbor)
		}

		memo[node] = total
		return total
	}

	return countPaths("you")
}

// part2 counts paths from 'svr' to 'out' that visit both 'dac' and 'fft'
func part2(graph map[string][]string) uint64 {
	// Create a function that counts paths from any node to a specific target
	countPathsToTarget := func(target string) func(string) uint64 {
		memo := make(map[string]uint64)

		var count func(node string) uint64
		count = func(node string) uint64 {
			if node == target {
				return 1
			}

			// Check memo
			if c, exists := memo[node]; exists {
				return c
			}

			// If node not in graph, no paths
			neighbors, exists := graph[node]
			if !exists {
				return 0
			}

			// Count paths through all neighbors
			total := uint64(0)
			for _, neighbor := range neighbors {
				total += count(neighbor)
			}

			memo[node] = total
			return total
		}

		return count
	}

	pathsToOut := countPathsToTarget("out")
	pathsToDac := countPathsToTarget("dac")
	pathsToFft := countPathsToTarget("fft")

	// Paths that visit dac before fft: svr -> dac -> fft -> out
	dacBeforeFft := pathsToDac("svr") * pathsToFft("dac") * pathsToOut("fft")

	// Paths that visit fft before dac: svr -> fft -> dac -> out
	fftBeforeDac := pathsToFft("svr") * pathsToDac("fft") * pathsToOut("dac")

	return dacBeforeFft + fftBeforeDac
}

func main() {
	inputFile := "../input.txt"
	if len(os.Args) > 1 {
		inputFile = os.Args[1]
	}

	graph, err := parseInput(inputFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(graph))
	fmt.Printf("Part 2: %d\n", part2(graph))
}
