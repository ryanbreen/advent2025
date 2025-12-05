package main

import (
	"fmt"
	"os"
	"strings"
)

type Direction struct {
	dr, dc int
}

var directions = []Direction{
	{0, 1},   // right
	{0, -1},  // left
	{1, 0},   // down
	{-1, 0},  // up
	{1, 1},   // down-right
	{1, -1},  // down-left
	{-1, 1},  // up-right
	{-1, -1}, // up-left
}

func main() {
	// Read input file
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		panic(err)
	}

	inputText := strings.TrimSpace(string(data))
	grid := strings.Split(inputText, "\n")

	fmt.Printf("Part 1: %d\n", part1(grid))
	fmt.Printf("Part 2: %d\n", part2(grid))
}

func part1(grid []string) int {
	rows := len(grid)
	cols := len(grid[0])
	target := "XMAS"
	count := 0

	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			// Try each direction from this position
			for _, dir := range directions {
				// Check if XMAS fits in this direction
				found := true
				for i := 0; i < len(target); i++ {
					nr := r + dir.dr*i
					nc := c + dir.dc*i

					if nr < 0 || nr >= rows || nc < 0 || nc >= cols {
						found = false
						break
					}
					if grid[nr][nc] != target[i] {
						found = false
						break
					}
				}
				if found {
					count++
				}
			}
		}
	}

	return count
}

func part2(grid []string) int {
	// Find X-MAS patterns: two MAS strings forming an X with A in the center
	// Each diagonal can be MAS or SAM
	rows := len(grid)
	cols := len(grid[0])
	count := 0

	// Check each possible center point (A must be in the middle)
	for r := 1; r < rows-1; r++ {
		for c := 1; c < cols-1; c++ {
			if grid[r][c] != 'A' {
				continue
			}

			// Get the four corners
			topLeft := grid[r-1][c-1]
			topRight := grid[r-1][c+1]
			bottomLeft := grid[r+1][c-1]
			bottomRight := grid[r+1][c+1]

			// Check diagonal 1 (top-left to bottom-right): MAS or SAM
			diag1Ok := (topLeft == 'M' && bottomRight == 'S') || (topLeft == 'S' && bottomRight == 'M')

			// Check diagonal 2 (top-right to bottom-left): MAS or SAM
			diag2Ok := (topRight == 'M' && bottomLeft == 'S') || (topRight == 'S' && bottomLeft == 'M')

			if diag1Ok && diag2Ok {
				count++
			}
		}
	}

	return count
}
