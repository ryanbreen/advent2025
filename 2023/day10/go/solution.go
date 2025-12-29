package main

import (
	"fmt"
	"os"
	"strings"
)

// Delta represents a directional offset in the grid.
type Delta struct {
	dr, dc int
}

// Point represents a position in the grid.
type Point struct {
	r, c int
}

// Direction deltas: N=(-1,0), S=(1,0), E=(0,1), W=(0,-1)
var pipeConnections = map[byte][]Delta{
	'|': {{-1, 0}, {1, 0}},  // N, S
	'-': {{0, -1}, {0, 1}},  // W, E
	'L': {{-1, 0}, {0, 1}},  // N, E
	'J': {{-1, 0}, {0, -1}}, // N, W
	'7': {{1, 0}, {0, -1}},  // S, W
	'F': {{1, 0}, {0, 1}},   // S, E
}

var cardinalDirections = []Delta{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}

func findStart(grid []string) Point {
	for r, row := range grid {
		for c, ch := range row {
			if ch == 'S' {
				return Point{r, c}
			}
		}
	}
	return Point{-1, -1}
}

func getNeighbors(grid []string, pos Point) []Point {
	r, c := pos.r, pos.c
	rows, cols := len(grid), len(grid[0])
	ch := grid[r][c]

	var neighbors []Point

	if ch == 'S' {
		// S can connect to any adjacent pipe that connects back to it
		for _, d := range cardinalDirections {
			nr, nc := r+d.dr, c+d.dc
			if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
				adjCh := grid[nr][nc]
				if dirs, ok := pipeConnections[adjCh]; ok {
					// Check if adjacent pipe connects back to S
					for _, adjD := range dirs {
						if nr+adjD.dr == r && nc+adjD.dc == c {
							neighbors = append(neighbors, Point{nr, nc})
							break
						}
					}
				}
			}
		}
	} else if dirs, ok := pipeConnections[ch]; ok {
		for _, d := range dirs {
			nr, nc := r+d.dr, c+d.dc
			if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
				neighbors = append(neighbors, Point{nr, nc})
			}
		}
	}

	return neighbors
}

func findLoop(grid []string, start Point) map[Point]int {
	distances := make(map[Point]int)
	distances[start] = 0

	// Use index-based queue for efficiency
	queue := make([]Point, 0, 256)
	queue = append(queue, start)
	head := 0

	for head < len(queue) {
		pos := queue[head]
		head++

		for _, neighbor := range getNeighbors(grid, pos) {
			if _, exists := distances[neighbor]; !exists {
				distances[neighbor] = distances[pos] + 1
				queue = append(queue, neighbor)
			}
		}
	}

	return distances
}

func determineStartPipe(grid []string, start Point, loopPositions map[Point]int) byte {
	r, c := start.r, start.c
	rows, cols := len(grid), len(grid[0])

	connections := make(map[Delta]bool)

	for _, d := range cardinalDirections {
		nr, nc := r+d.dr, c+d.dc
		neighbor := Point{nr, nc}
		if _, inLoop := loopPositions[neighbor]; inLoop {
			if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
				adjCh := grid[nr][nc]
				if dirs, ok := pipeConnections[adjCh]; ok {
					for _, adjD := range dirs {
						if nr+adjD.dr == r && nc+adjD.dc == c {
							connections[d] = true
							break
						}
					}
				}
			}
		}
	}

	for pipe, dirs := range pipeConnections {
		if len(dirs) == len(connections) {
			match := true
			for _, d := range dirs {
				if !connections[d] {
					match = false
					break
				}
			}
			if match {
				return pipe
			}
		}
	}

	return 'S'
}

func part1(grid []string) int {
	start := findStart(grid)
	distances := findLoop(grid, start)

	maxDist := 0
	for _, d := range distances {
		if d > maxDist {
			maxDist = d
		}
	}
	return maxDist
}

func part2(grid []string) int {
	start := findStart(grid)
	distances := findLoop(grid, start)

	// Replace S with its actual pipe type
	startPipe := determineStartPipe(grid, start, distances)
	modifiedGrid := make([][]byte, len(grid))
	for i, row := range grid {
		modifiedGrid[i] = []byte(row)
	}
	modifiedGrid[start.r][start.c] = startPipe

	rows, cols := len(modifiedGrid), len(modifiedGrid[0])
	enclosed := 0

	for r := 0; r < rows; r++ {
		inside := false
		for c := 0; c < cols; c++ {
			pos := Point{r, c}
			if _, inLoop := distances[pos]; inLoop {
				ch := modifiedGrid[r][c]
				// Count pipes that have a north connection: |, L, J
				if ch == '|' || ch == 'L' || ch == 'J' {
					inside = !inside
				}
			} else {
				if inside {
					enclosed++
				}
			}
		}
	}

	return enclosed
}

func main() {
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	lines := strings.Split(strings.TrimSpace(string(data)), "\n")

	fmt.Printf("Part 1: %d\n", part1(lines))
	fmt.Printf("Part 2: %d\n", part2(lines))
}
