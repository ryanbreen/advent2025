package main

import (
	"bufio"
	"fmt"
	"os"
)

// 8 directions: N, NE, E, SE, S, SW, W, NW
var directions = [][2]int{
	{-1, 0},  // N
	{-1, 1},  // NE
	{0, 1},   // E
	{1, 1},   // SE
	{1, 0},   // S
	{1, -1},  // SW
	{0, -1},  // W
	{-1, -1}, // NW
}

func readInput(filename string) ([][]rune, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var grid [][]rune
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		grid = append(grid, []rune(line))
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return grid, nil
}

func countNeighbors(grid [][]rune, row, col int) int {
	count := 0
	rows := len(grid)
	cols := len(grid[0])

	for _, dir := range directions {
		newRow := row + dir[0]
		newCol := col + dir[1]

		if newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols {
			if grid[newRow][newCol] == '@' {
				count++
			}
		}
	}

	return count
}

func part1(grid [][]rune) int {
	count := 0
	for row := 0; row < len(grid); row++ {
		for col := 0; col < len(grid[row]); col++ {
			if grid[row][col] == '@' {
				neighbors := countNeighbors(grid, row, col)
				if neighbors < 4 {
					count++
				}
			}
		}
	}
	return count
}

func findAccessibleRolls(grid [][]rune) [][2]int {
	var accessible [][2]int
	for row := 0; row < len(grid); row++ {
		for col := 0; col < len(grid[row]); col++ {
			if grid[row][col] == '@' {
				neighbors := countNeighbors(grid, row, col)
				if neighbors < 4 {
					accessible = append(accessible, [2]int{row, col})
				}
			}
		}
	}
	return accessible
}

func part2(grid [][]rune) int {
	// Make a deep copy of the grid
	gridCopy := make([][]rune, len(grid))
	for i := range grid {
		gridCopy[i] = make([]rune, len(grid[i]))
		copy(gridCopy[i], grid[i])
	}

	rows := len(gridCopy)
	cols := len(gridCopy[0])

	// Precompute neighbor counts for all rolls
	neighborCount := make([][]int, rows)
	for i := range neighborCount {
		neighborCount[i] = make([]int, cols)
	}

	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if gridCopy[r][c] == '@' {
				neighborCount[r][c] = countNeighbors(gridCopy, r, c)
			}
		}
	}

	// Initialize queue with all accessible rolls (< 4 neighbors)
	queue := make([][2]int, 0, rows*cols)
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if gridCopy[r][c] == '@' && neighborCount[r][c] < 4 {
				queue = append(queue, [2]int{r, c})
			}
		}
	}

	totalRemoved := 0

	// Process queue
	for len(queue) > 0 {
		pos := queue[0]
		queue = queue[1:]
		r, c := pos[0], pos[1]

		// Skip if already removed
		if gridCopy[r][c] != '@' {
			continue
		}

		// Remove this roll
		gridCopy[r][c] = '.'
		totalRemoved++

		// Decrement neighbor counts for all adjacent rolls
		for _, dir := range directions {
			nr := r + dir[0]
			nc := c + dir[1]

			if nr >= 0 && nr < rows && nc >= 0 && nc < cols && gridCopy[nr][nc] == '@' {
				neighborCount[nr][nc]--
				// If this neighbor just became accessible, add to queue
				if neighborCount[nr][nc] == 3 {
					queue = append(queue, [2]int{nr, nc})
				}
			}
		}
	}

	return totalRemoved
}

func main() {
	grid, err := readInput("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(grid))
	fmt.Printf("Part 2: %d\n", part2(grid))
}
