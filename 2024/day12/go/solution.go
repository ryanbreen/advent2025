package main

import (
	"fmt"
	"os"
	"strings"
)

type Point struct {
	r, c int
}

var grid [][]rune
var rows, cols int

func main() {
	// Read input
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		panic(err)
	}

	// Parse grid
	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	grid = make([][]rune, len(lines))
	for i, line := range lines {
		grid[i] = []rune(line)
	}
	rows = len(grid)
	cols = len(grid[0])

	fmt.Printf("Part 1: %d\n", part1())
	fmt.Printf("Part 2: %d\n", part2())
}

func findRegions() []map[Point]bool {
	visited := make(map[Point]bool)
	regions := []map[Point]bool{}

	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			p := Point{r, c}
			if visited[p] {
				continue
			}

			// BFS to find all cells in this region
			plant := grid[r][c]
			region := make(map[Point]bool)
			queue := []Point{p}

			for len(queue) > 0 {
				curr := queue[0]
				queue = queue[1:]

				if visited[curr] {
					continue
				}
				if curr.r < 0 || curr.r >= rows || curr.c < 0 || curr.c >= cols {
					continue
				}
				if grid[curr.r][curr.c] != plant {
					continue
				}

				visited[curr] = true
				region[curr] = true

				// Check all 4 neighbors
				directions := []Point{{0, 1}, {0, -1}, {1, 0}, {-1, 0}}
				for _, d := range directions {
					next := Point{curr.r + d.r, curr.c + d.c}
					if !visited[next] {
						queue = append(queue, next)
					}
				}
			}

			regions = append(regions, region)
		}
	}

	return regions
}

func calculatePerimeter(region map[Point]bool) int {
	perimeter := 0
	directions := []Point{{0, 1}, {0, -1}, {1, 0}, {-1, 0}}

	for p := range region {
		for _, d := range directions {
			neighbor := Point{p.r + d.r, p.c + d.c}
			if !region[neighbor] {
				perimeter++
			}
		}
	}

	return perimeter
}

func part1() int {
	regions := findRegions()
	total := 0

	for _, region := range regions {
		area := len(region)
		perimeter := calculatePerimeter(region)
		total += area * perimeter
	}

	return total
}

func countSides(region map[Point]bool) int {
	corners := 0

	for p := range region {
		// Check all 8 neighbors for corner detection
		up := region[Point{p.r - 1, p.c}]
		down := region[Point{p.r + 1, p.c}]
		left := region[Point{p.r, p.c - 1}]
		right := region[Point{p.r, p.c + 1}]
		upLeft := region[Point{p.r - 1, p.c - 1}]
		upRight := region[Point{p.r - 1, p.c + 1}]
		downLeft := region[Point{p.r + 1, p.c - 1}]
		downRight := region[Point{p.r + 1, p.c + 1}]

		// Top-left corner
		if !up && !left { // convex
			corners++
		} else if up && left && !upLeft { // concave
			corners++
		}

		// Top-right corner
		if !up && !right { // convex
			corners++
		} else if up && right && !upRight { // concave
			corners++
		}

		// Bottom-left corner
		if !down && !left { // convex
			corners++
		} else if down && left && !downLeft { // concave
			corners++
		}

		// Bottom-right corner
		if !down && !right { // convex
			corners++
		} else if down && right && !downRight { // concave
			corners++
		}
	}

	return corners
}

func part2() int {
	regions := findRegions()
	total := 0

	for _, region := range regions {
		area := len(region)
		sides := countSides(region)
		total += area * sides
	}

	return total
}
