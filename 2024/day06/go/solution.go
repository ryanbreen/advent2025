package main

import (
	"bufio"
	"fmt"
	"os"
)

type Position struct {
	row, col int
}

type State struct {
	pos Position
	dir Direction
}

type Direction int

const (
	Up Direction = iota
	Right
	Down
	Left
)

func main() {
	fmt.Println("Part 1:", part1())
	fmt.Println("Part 2:", part2())
}

func part1() int {
	grid, startRow, startCol, startDir := parseInput()

	// Track visited positions
	visited := make(map[Position]bool)

	// Current position and direction
	row, col := startRow, startCol
	dir := startDir

	// Mark starting position as visited
	visited[Position{row, col}] = true

	// Simulate guard movement
	for {
		// Calculate next position based on current direction
		nextRow, nextCol := getNextPosition(row, col, dir)

		// Check if guard leaves the map
		if nextRow < 0 || nextRow >= len(grid) || nextCol < 0 || nextCol >= len(grid[0]) {
			break
		}

		// Check if there's an obstacle ahead
		if grid[nextRow][nextCol] == '#' {
			// Turn right 90 degrees
			dir = turnRight(dir)
		} else {
			// Move forward
			row, col = nextRow, nextCol
			visited[Position{row, col}] = true
		}
	}

	return len(visited)
}

func parseInput() ([][]rune, int, int, Direction) {
	file, err := os.Open("../input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var grid [][]rune
	var startRow, startCol int
	var startDir Direction

	scanner := bufio.NewScanner(file)
	row := 0
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}

		gridRow := []rune(line)

		// Find starting position
		for col, ch := range gridRow {
			switch ch {
			case '^':
				startRow, startCol = row, col
				startDir = Up
				gridRow[col] = '.' // Replace with empty space
			case 'v':
				startRow, startCol = row, col
				startDir = Down
				gridRow[col] = '.' // Replace with empty space
			case '<':
				startRow, startCol = row, col
				startDir = Left
				gridRow[col] = '.' // Replace with empty space
			case '>':
				startRow, startCol = row, col
				startDir = Right
				gridRow[col] = '.' // Replace with empty space
			}
		}

		grid = append(grid, gridRow)
		row++
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}

	return grid, startRow, startCol, startDir
}

func getNextPosition(row, col int, dir Direction) (int, int) {
	switch dir {
	case Up:
		return row - 1, col
	case Down:
		return row + 1, col
	case Left:
		return row, col - 1
	case Right:
		return row, col + 1
	}
	return row, col
}

func turnRight(dir Direction) Direction {
	return (dir + 1) % 4
}

func checkLoop(grid [][]rune, startRow, startCol int, startDir Direction) bool {
	rows := len(grid)
	cols := len(grid[0])

	// Track states (position, direction) to detect loops
	states := make(map[State]bool)
	row, col := startRow, startCol
	dir := startDir

	for {
		// If we've seen this state before, we're in a loop
		state := State{Position{row, col}, dir}
		if states[state] {
			return true
		}
		states[state] = true

		// Calculate next position
		nextRow, nextCol := getNextPosition(row, col, dir)

		// Check if guard would leave the map
		if nextRow < 0 || nextRow >= rows || nextCol < 0 || nextCol >= cols {
			return false
		}

		// Check if there's an obstacle ahead
		if grid[nextRow][nextCol] == '#' {
			// Turn right
			dir = turnRight(dir)
		} else {
			// Move forward
			row, col = nextRow, nextCol
		}
	}
}

func part2() int {
	grid, startRow, startCol, startDir := parseInput()
	rows := len(grid)
	cols := len(grid[0])

	count := 0

	// Try placing an obstruction at each position
	for row := 0; row < rows; row++ {
		for col := 0; col < cols; col++ {
			// Skip if there's already an obstacle or if it's the starting position
			if grid[row][col] == '#' || (row == startRow && col == startCol) {
				continue
			}

			// Place temporary obstruction
			grid[row][col] = '#'

			// Check if this creates a loop
			if checkLoop(grid, startRow, startCol, startDir) {
				count++
			}

			// Remove the obstruction
			grid[row][col] = '.'
		}
	}

	return count
}
