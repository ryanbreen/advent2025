package main

import (
	"bufio"
	"fmt"
	"os"
)

type point struct {
	row, col int
}

func parseGrid(lines []string) []point {
	var galaxies []point
	for r, line := range lines {
		for c, ch := range line {
			if ch == '#' {
				galaxies = append(galaxies, point{r, c})
			}
		}
	}
	return galaxies
}

func findEmptyRowsAndCols(lines []string, galaxies []point) ([]int, []int) {
	rows := len(lines)
	cols := 0
	if rows > 0 {
		cols = len(lines[0])
	}

	// Use boolean arrays to track empty rows/cols
	emptyRows := make([]bool, rows)
	emptyCols := make([]bool, cols)
	for i := range emptyRows {
		emptyRows[i] = true
	}
	for i := range emptyCols {
		emptyCols[i] = true
	}

	// Mark rows and columns containing galaxies as not empty
	for _, g := range galaxies {
		emptyRows[g.row] = false
		emptyCols[g.col] = false
	}

	// Build prefix sums for O(1) range queries
	prefixRows := make([]int, rows+1)
	prefixCols := make([]int, cols+1)

	for r := 0; r < rows; r++ {
		prefixRows[r+1] = prefixRows[r]
		if emptyRows[r] {
			prefixRows[r+1]++
		}
	}

	for c := 0; c < cols; c++ {
		prefixCols[c+1] = prefixCols[c]
		if emptyCols[c] {
			prefixCols[c+1]++
		}
	}

	return prefixRows, prefixCols
}

func calculateDistances(galaxies []point, prefixRows, prefixCols []int, expansionFactor int64) int64 {
	var total int64

	for i := 0; i < len(galaxies); i++ {
		for j := i + 1; j < len(galaxies); j++ {
			r1, c1 := galaxies[i].row, galaxies[i].col
			r2, c2 := galaxies[j].row, galaxies[j].col

			// Calculate row distance with expansion using prefix sums
			minR, maxR := min(r1, r2), max(r1, r2)
			emptyRowCount := prefixRows[maxR] - prefixRows[minR]
			rowDist := int64(maxR-minR) + int64(emptyRowCount)*(expansionFactor-1)

			// Calculate column distance with expansion using prefix sums
			minC, maxC := min(c1, c2), max(c1, c2)
			emptyColCount := prefixCols[maxC] - prefixCols[minC]
			colDist := int64(maxC-minC) + int64(emptyColCount)*(expansionFactor-1)

			total += rowDist + colDist
		}
	}

	return total
}

func solve(galaxies []point, prefixRows, prefixCols []int) (int64, int64) {
	part1 := calculateDistances(galaxies, prefixRows, prefixCols, 2)
	part2 := calculateDistances(galaxies, prefixRows, prefixCols, 1000000)
	return part1, part2
}

func main() {
	inputFile := "../input.txt"
	if len(os.Args) > 1 {
		inputFile = os.Args[1]
	}

	file, err := os.Open(inputFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line != "" {
			lines = append(lines, line)
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}

	// Parse once, use for both parts
	galaxies := parseGrid(lines)
	prefixRows, prefixCols := findEmptyRowsAndCols(lines, galaxies)

	p1, p2 := solve(galaxies, prefixRows, prefixCols)
	fmt.Println("Part 1:", p1)
	fmt.Println("Part 2:", p2)
}
