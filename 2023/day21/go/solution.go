package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
)

type Point struct {
	row, col int
}

func parseInput(filename string) ([]string, Point) {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var grid []string
	var start Point

	scanner := bufio.NewScanner(file)
	row := 0
	for scanner.Scan() {
		line := scanner.Text()
		grid = append(grid, line)
		for col, ch := range line {
			if ch == 'S' {
				start = Point{row, col}
			}
		}
		row++
	}

	return grid, start
}

// countReachable counts cells reachable in exactly 'steps' steps (bounded grid)
func countReachable(grid []string, start Point, steps int) int64 {
	rows, cols := len(grid), len(grid[0])

	// BFS to find minimum steps to each cell
	visited := make(map[Point]int)
	queue := []struct {
		p    Point
		dist int
	}{{start, 0}}
	visited[start] = 0

	dirs := []Point{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}

	for len(queue) > 0 {
		curr := queue[0]
		queue = queue[1:]

		if curr.dist >= steps {
			continue
		}

		for _, d := range dirs {
			nr, nc := curr.p.row+d.row, curr.p.col+d.col
			if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
				np := Point{nr, nc}
				if grid[nr][nc] != '#' {
					if _, exists := visited[np]; !exists {
						visited[np] = curr.dist + 1
						queue = append(queue, struct {
							p    Point
							dist int
						}{np, curr.dist + 1})
					}
				}
			}
		}
	}

	// Count cells reachable in exactly 'steps' steps
	// A cell reachable in d steps can be reached in d+2, d+4, ... steps
	targetParity := steps % 2
	var count int64 = 0
	for _, d := range visited {
		if d <= steps && d%2 == targetParity {
			count++
		}
	}

	return count
}

// countReachableInfiniteBFS does BFS on infinite tiled grid for small step counts
func countReachableInfiniteBFS(grid []string, start Point, steps int) int64 {
	rows, cols := len(grid), len(grid[0])

	visited := make(map[Point]int)
	queue := []struct {
		p    Point
		dist int
	}{{start, 0}}
	visited[start] = 0

	dirs := []Point{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}

	for len(queue) > 0 {
		curr := queue[0]
		queue = queue[1:]

		if curr.dist >= steps {
			continue
		}

		for _, d := range dirs {
			nr, nc := curr.p.row+d.row, curr.p.col+d.col
			np := Point{nr, nc}

			// Map to grid coordinates (infinite tiling)
			gr := ((nr % rows) + rows) % rows
			gc := ((nc % cols) + cols) % cols

			if grid[gr][gc] != '#' {
				if _, exists := visited[np]; !exists {
					visited[np] = curr.dist + 1
					queue = append(queue, struct {
						p    Point
						dist int
					}{np, curr.dist + 1})
				}
			}
		}
	}

	targetParity := steps % 2
	var count int64 = 0
	for _, d := range visited {
		if d <= steps && d%2 == targetParity {
			count++
		}
	}

	return count
}

// countReachableInfinite counts cells reachable on an infinite tiled grid
func countReachableInfinite(grid []string, start Point, steps int) int64 {
	rows := len(grid)
	size := rows
	half := size / 2

	// For small step counts, use direct BFS
	if steps <= size*2 {
		return countReachableInfiniteBFS(grid, start, steps)
	}

	// The number of full grid widths we travel
	n := int64((steps - half) / size)

	// Calculate reachable counts for n=0, 1, 2 to determine the quadratic
	y0 := countReachableInfiniteBFS(grid, start, half)
	y1 := countReachableInfiniteBFS(grid, start, half+size)
	y2 := countReachableInfiniteBFS(grid, start, half+2*size)

	// Solve for a, b, c using Lagrange interpolation
	// f(x) = y0 + (y1-y0)*x + (y2-2*y1+y0)*x*(x-1)/2
	a := (y2 - 2*y1 + y0) / 2
	b := y1 - y0 - a
	c := y0

	return a*n*n + b*n + c
}

func part1(grid []string, start Point) int64 {
	return countReachable(grid, start, 64)
}

func part2(grid []string, start Point) int64 {
	return countReachableInfinite(grid, start, 26501365)
}

func main() {
	// Get the directory of the executable
	_, filename, _, _ := runtime.Caller(0)
	dir := filepath.Dir(filename)
	inputPath := filepath.Join(dir, "..", "input.txt")

	grid, start := parseInput(inputPath)

	fmt.Printf("Part 1: %d\n", part1(grid, start))
	fmt.Printf("Part 2: %d\n", part2(grid, start))
}
