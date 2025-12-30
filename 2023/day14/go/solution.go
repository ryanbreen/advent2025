package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func readGrid(filename string) [][]byte {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var grid [][]byte
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if len(line) > 0 {
			grid = append(grid, []byte(line))
		}
	}
	return grid
}

func copyGrid(grid [][]byte) [][]byte {
	newGrid := make([][]byte, len(grid))
	for i, row := range grid {
		newGrid[i] = make([]byte, len(row))
		copy(newGrid[i], row)
	}
	return newGrid
}

func tiltNorth(grid [][]byte) {
	rows := len(grid)
	cols := len(grid[0])

	for col := 0; col < cols; col++ {
		writePos := 0
		for row := 0; row < rows; row++ {
			switch grid[row][col] {
			case 'O':
				if writePos != row {
					grid[writePos][col] = 'O'
					grid[row][col] = '.'
				}
				writePos++
			case '#':
				writePos = row + 1
			}
		}
	}
}

func tiltSouth(grid [][]byte) {
	rows := len(grid)
	cols := len(grid[0])

	for col := 0; col < cols; col++ {
		writePos := rows - 1
		for row := rows - 1; row >= 0; row-- {
			switch grid[row][col] {
			case 'O':
				if writePos != row {
					grid[writePos][col] = 'O'
					grid[row][col] = '.'
				}
				writePos--
			case '#':
				writePos = row - 1
			}
		}
	}
}

func tiltWest(grid [][]byte) {
	rows := len(grid)
	cols := len(grid[0])

	for row := 0; row < rows; row++ {
		writePos := 0
		for col := 0; col < cols; col++ {
			switch grid[row][col] {
			case 'O':
				if writePos != col {
					grid[row][writePos] = 'O'
					grid[row][col] = '.'
				}
				writePos++
			case '#':
				writePos = col + 1
			}
		}
	}
}

func tiltEast(grid [][]byte) {
	rows := len(grid)
	cols := len(grid[0])

	for row := 0; row < rows; row++ {
		writePos := cols - 1
		for col := cols - 1; col >= 0; col-- {
			switch grid[row][col] {
			case 'O':
				if writePos != col {
					grid[row][writePos] = 'O'
					grid[row][col] = '.'
				}
				writePos--
			case '#':
				writePos = col - 1
			}
		}
	}
}

func spinCycle(grid [][]byte) {
	tiltNorth(grid)
	tiltWest(grid)
	tiltSouth(grid)
	tiltEast(grid)
}

func calculateLoad(grid [][]byte) int {
	rows := len(grid)
	load := 0
	for row := 0; row < rows; row++ {
		for col := 0; col < len(grid[row]); col++ {
			if grid[row][col] == 'O' {
				load += rows - row
			}
		}
	}
	return load
}

func gridToString(grid [][]byte) string {
	var sb strings.Builder
	for _, row := range grid {
		sb.Write(row)
		sb.WriteByte('\n')
	}
	return sb.String()
}

func part1(grid [][]byte) int {
	g := copyGrid(grid)
	tiltNorth(g)
	return calculateLoad(g)
}

func part2(grid [][]byte) int {
	g := copyGrid(grid)
	seen := make(map[string]int)
	targetCycles := 1000000000

	for cycle := 0; cycle < targetCycles; cycle++ {
		key := gridToString(g)
		if prevCycle, found := seen[key]; found {
			cycleLength := cycle - prevCycle
			remaining := (targetCycles - cycle) % cycleLength
			for i := 0; i < remaining; i++ {
				spinCycle(g)
			}
			return calculateLoad(g)
		}
		seen[key] = cycle
		spinCycle(g)
	}

	return calculateLoad(g)
}

func main() {
	grid := readGrid("../input.txt")

	fmt.Println("Part 1:", part1(grid))
	fmt.Println("Part 2:", part2(grid))
}
