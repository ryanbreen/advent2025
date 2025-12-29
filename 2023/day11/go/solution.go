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

func findEmptyRowsAndCols(lines []string) (map[int]struct{}, map[int]struct{}) {
	rows := len(lines)
	cols := 0
	if rows > 0 {
		cols = len(lines[0])
	}

	emptyRows := make(map[int]struct{})
	emptyCols := make(map[int]struct{})

	// Find empty rows
	for r, line := range lines {
		hasGalaxy := false
		for _, ch := range line {
			if ch == '#' {
				hasGalaxy = true
				break
			}
		}
		if !hasGalaxy {
			emptyRows[r] = struct{}{}
		}
	}

	// Find empty columns
	for c := 0; c < cols; c++ {
		hasGalaxy := false
		for r := 0; r < rows; r++ {
			if lines[r][c] == '#' {
				hasGalaxy = true
				break
			}
		}
		if !hasGalaxy {
			emptyCols[c] = struct{}{}
		}
	}

	return emptyRows, emptyCols
}

func calculateDistances(galaxies []point, emptyRows, emptyCols map[int]struct{}, expansionFactor int) int64 {
	var total int64

	for i := 0; i < len(galaxies); i++ {
		for j := i + 1; j < len(galaxies); j++ {
			r1, c1 := galaxies[i].row, galaxies[i].col
			r2, c2 := galaxies[j].row, galaxies[j].col

			// Calculate row distance with expansion
			minR, maxR := min(r1, r2), max(r1, r2)
			rowDist := int64(maxR - minR)
			for r := minR; r < maxR; r++ {
				if _, ok := emptyRows[r]; ok {
					rowDist += int64(expansionFactor - 1)
				}
			}

			// Calculate column distance with expansion
			minC, maxC := min(c1, c2), max(c1, c2)
			colDist := int64(maxC - minC)
			for c := minC; c < maxC; c++ {
				if _, ok := emptyCols[c]; ok {
					colDist += int64(expansionFactor - 1)
				}
			}

			total += rowDist + colDist
		}
	}

	return total
}

func solve(lines []string, galaxies []point, emptyRows, emptyCols map[int]struct{}) (int64, int64) {
	part1 := calculateDistances(galaxies, emptyRows, emptyCols, 2)
	part2 := calculateDistances(galaxies, emptyRows, emptyCols, 1000000)
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
	emptyRows, emptyCols := findEmptyRowsAndCols(lines)

	p1, p2 := solve(lines, galaxies, emptyRows, emptyCols)
	fmt.Println("Part 1:", p1)
	fmt.Println("Part 2:", p2)
}
