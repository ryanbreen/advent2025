package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"sort"
	"strconv"
	"strings"
)

func readInput() ([]int, []int, error) {
	file, err := os.Open("../input.txt")
	if err != nil {
		return nil, nil, err
	}
	defer file.Close()

	var leftList, rightList []int
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Fields(line)
		if len(parts) != 2 {
			continue
		}

		left, err := strconv.Atoi(parts[0])
		if err != nil {
			return nil, nil, err
		}

		right, err := strconv.Atoi(parts[1])
		if err != nil {
			return nil, nil, err
		}

		leftList = append(leftList, left)
		rightList = append(rightList, right)
	}

	return leftList, rightList, scanner.Err()
}

func part1(leftList, rightList []int) int {
	// Create copies to avoid modifying originals
	left := make([]int, len(leftList))
	right := make([]int, len(rightList))
	copy(left, leftList)
	copy(right, rightList)

	// Sort both lists
	sort.Ints(left)
	sort.Ints(right)

	// Calculate total distance
	totalDistance := 0
	for i := 0; i < len(left); i++ {
		totalDistance += int(math.Abs(float64(left[i] - right[i])))
	}

	return totalDistance
}

func part2(leftList, rightList []int) int {
	// Count occurrences in right list
	rightCounts := make(map[int]int)
	for _, num := range rightList {
		rightCounts[num]++
	}

	// Calculate similarity score
	similarityScore := 0
	for _, num := range leftList {
		similarityScore += num * rightCounts[num]
	}

	return similarityScore
}

func main() {
	leftList, rightList, err := readInput()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(leftList, rightList))
	fmt.Printf("Part 2: %d\n", part2(leftList, rightList))
}
