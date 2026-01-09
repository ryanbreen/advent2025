package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
)

func parseGrid(lines []string) [][]int {
	grid := make([][]int, len(lines))
	for i, line := range lines {
		grid[i] = make([]int, len(line))
		for j, c := range line {
			grid[i][j] = int(c - '0')
		}
	}
	return grid
}

func isVisible(grid [][]int, row, col int) bool {
	rows, cols := len(grid), len(grid[0])
	height := grid[row][col]

	// Check from left
	visibleLeft := true
	for c := 0; c < col; c++ {
		if grid[row][c] >= height {
			visibleLeft = false
			break
		}
	}
	if visibleLeft {
		return true
	}

	// Check from right
	visibleRight := true
	for c := col + 1; c < cols; c++ {
		if grid[row][c] >= height {
			visibleRight = false
			break
		}
	}
	if visibleRight {
		return true
	}

	// Check from top
	visibleTop := true
	for r := 0; r < row; r++ {
		if grid[r][col] >= height {
			visibleTop = false
			break
		}
	}
	if visibleTop {
		return true
	}

	// Check from bottom
	visibleBottom := true
	for r := row + 1; r < rows; r++ {
		if grid[r][col] >= height {
			visibleBottom = false
			break
		}
	}
	return visibleBottom
}

func scenicScore(grid [][]int, row, col int) int {
	rows, cols := len(grid), len(grid[0])
	height := grid[row][col]

	// Count trees visible in each direction
	// Left
	left := 0
	for c := col - 1; c >= 0; c-- {
		left++
		if grid[row][c] >= height {
			break
		}
	}

	// Right
	right := 0
	for c := col + 1; c < cols; c++ {
		right++
		if grid[row][c] >= height {
			break
		}
	}

	// Up
	up := 0
	for r := row - 1; r >= 0; r-- {
		up++
		if grid[r][col] >= height {
			break
		}
	}

	// Down
	down := 0
	for r := row + 1; r < rows; r++ {
		down++
		if grid[r][col] >= height {
			break
		}
	}

	return left * right * up * down
}

func part1(grid [][]int) int {
	rows, cols := len(grid), len(grid[0])
	count := 0
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if isVisible(grid, r, c) {
				count++
			}
		}
	}
	return count
}

func part2(grid [][]int) int {
	rows, cols := len(grid), len(grid[0])
	maxScore := 0
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			score := scenicScore(grid, r, c)
			if score > maxScore {
				maxScore = score
			}
		}
	}
	return maxScore
}

func main() {
	execPath, _ := os.Executable()
	dir := filepath.Dir(execPath)
	inputFile := filepath.Join(dir, "..", "input.txt")

	// Try relative path if executable path doesn't work
	if _, err := os.Stat(inputFile); os.IsNotExist(err) {
		inputFile = filepath.Join(".", "..", "input.txt")
	}

	file, err := os.Open(inputFile)
	if err != nil {
		panic(err)
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

	grid := parseGrid(lines)

	fmt.Println("Part 1:", part1(grid))
	fmt.Println("Part 2:", part2(grid))
}
