package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	depths := parseInput("../input.txt")

	fmt.Printf("Part 1: %d\n", part1(depths))
	fmt.Printf("Part 2: %d\n", part2(depths))
}

func parseInput(filename string) []int {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var depths []int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		depth, err := strconv.Atoi(scanner.Text())
		if err != nil {
			panic(err)
		}
		depths = append(depths, depth)
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}

	return depths
}

// part1 counts the number of times a depth measurement increases from the previous
func part1(depths []int) int {
	count := 0
	for i := 1; i < len(depths); i++ {
		if depths[i] > depths[i-1] {
			count++
		}
	}
	return count
}

// part2 counts increases in 3-measurement sliding window sums
func part2(depths []int) int {
	// Create sliding window sums of 3 consecutive measurements
	windowSums := make([]int, len(depths)-2)
	for i := 0; i < len(depths)-2; i++ {
		windowSums[i] = depths[i] + depths[i+1] + depths[i+2]
	}

	// Count how many times the sum increases
	count := 0
	for i := 1; i < len(windowSums); i++ {
		if windowSums[i] > windowSums[i-1] {
			count++
		}
	}
	return count
}
