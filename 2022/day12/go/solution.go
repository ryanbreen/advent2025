package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
)

type Point struct {
	row, col int
}

type State struct {
	row, col, dist int
}

func parseGrid(lines []string) ([][]byte, Point, Point) {
	grid := make([][]byte, len(lines))
	var start, end Point

	for r, line := range lines {
		grid[r] = []byte(line)
		for c, ch := range grid[r] {
			if ch == 'S' {
				start = Point{r, c}
				grid[r][c] = 'a'
			} else if ch == 'E' {
				end = Point{r, c}
				grid[r][c] = 'z'
			}
		}
	}

	return grid, start, end
}

func bfs(grid [][]byte, starts []Point, end Point) int {
	rows := len(grid)
	cols := len(grid[0])
	visited := make(map[Point]bool)
	queue := make([]State, 0)

	for _, start := range starts {
		queue = append(queue, State{start.row, start.col, 0})
		visited[start] = true
	}

	directions := []Point{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		if current.row == end.row && current.col == end.col {
			return current.dist
		}

		currentHeight := grid[current.row][current.col]

		for _, d := range directions {
			nr, nc := current.row+d.row, current.col+d.col

			if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
				nextPoint := Point{nr, nc}
				if !visited[nextPoint] {
					nextHeight := grid[nr][nc]
					// Can move if destination is at most 1 higher
					if nextHeight <= currentHeight+1 {
						visited[nextPoint] = true
						queue = append(queue, State{nr, nc, current.dist + 1})
					}
				}
			}
		}
	}

	return -1 // No path found
}

func part1(lines []string) int {
	grid, start, end := parseGrid(lines)
	return bfs(grid, []Point{start}, end)
}

func part2(lines []string) int {
	grid, _, end := parseGrid(lines)

	// Find all cells with elevation 'a'
	var starts []Point
	for r, row := range grid {
		for c, ch := range row {
			if ch == 'a' {
				starts = append(starts, Point{r, c})
			}
		}
	}

	return bfs(grid, starts, end)
}

func main() {
	// Get the directory of the executable
	execPath, _ := os.Executable()
	execDir := filepath.Dir(execPath)

	// Try multiple paths for input file
	inputPaths := []string{
		filepath.Join(execDir, "..", "input.txt"),
		"../input.txt",
	}

	var file *os.File
	var err error

	for _, path := range inputPaths {
		file, err = os.Open(path)
		if err == nil {
			break
		}
	}

	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening input file: %v\n", err)
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
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Part 1:", part1(lines))
	fmt.Println("Part 2:", part2(lines))
}
