package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strings"
)

type Point struct {
	r, c int
}

type Edge struct {
	to   int
	dist int
}

var directions = [4]Point{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}

var slopeDirs = [256]Point{
	'^': {-1, 0},
	'v': {1, 0},
	'<': {0, -1},
	'>': {0, 1},
}

var isSlope = [256]bool{
	'^': true,
	'v': true,
	'<': true,
	'>': true,
}

func parseInput(filename string) []string {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var grid []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line != "" {
			grid = append(grid, line)
		}
	}
	return grid
}

func findJunctions(grid []string) []Point {
	rows, cols := len(grid), len(grid[0])
	var junctions []Point

	// Start and end points
	start := Point{0, strings.Index(grid[0], ".")}
	end := Point{rows - 1, strings.Index(grid[rows-1], ".")}
	junctions = append(junctions, start)
	junctions = append(junctions, end)

	// Find intersections (cells with 3+ walkable neighbors)
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if grid[r][c] == '#' {
				continue
			}
			// Skip start and end (already added)
			if (r == start.r && c == start.c) || (r == end.r && c == end.c) {
				continue
			}
			neighbors := 0
			for _, d := range directions {
				nr, nc := r+d.r, c+d.c
				if nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#' {
					neighbors++
				}
			}
			if neighbors >= 3 {
				junctions = append(junctions, Point{r, c})
			}
		}
	}

	return junctions
}

func buildGraph(grid []string, junctions []Point, respectSlopes bool) [][]Edge {
	rows, cols := len(grid), len(grid[0])

	// Create a lookup map from Point to junction index
	junctionIndex := make(map[Point]int, len(junctions))
	for i, j := range junctions {
		junctionIndex[j] = i
	}

	graph := make([][]Edge, len(junctions))

	for startIdx, startJunction := range junctions {
		// DFS from each junction to find reachable junctions
		type State struct {
			r, c int
			dist int
		}
		stack := make([]State, 0, 256)
		stack = append(stack, State{startJunction.r, startJunction.c, 0})

		// Use a 2D visited array for graph building (faster than map for grid traversal)
		visited := make([][]bool, rows)
		for i := range visited {
			visited[i] = make([]bool, cols)
		}
		visited[startJunction.r][startJunction.c] = true

		for len(stack) > 0 {
			// Pop from stack
			current := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			r, c, dist := current.r, current.c, current.dist

			if dist > 0 {
				if idx, ok := junctionIndex[Point{r, c}]; ok {
					// Found another junction
					graph[startIdx] = append(graph[startIdx], Edge{idx, dist})
					continue
				}
			}

			// Explore neighbors
			for di := 0; di < 4; di++ {
				d := directions[di]
				nr, nc := r+d.r, c+d.c
				if nr < 0 || nr >= rows || nc < 0 || nc >= cols {
					continue
				}
				if grid[nr][nc] == '#' {
					continue
				}
				if visited[nr][nc] {
					continue
				}

				// Check slope constraints for Part 1
				if respectSlopes {
					cell := grid[r][c]
					if isSlope[cell] {
						reqDir := slopeDirs[cell]
						if d.r != reqDir.r || d.c != reqDir.c {
							continue
						}
					}
				}

				visited[nr][nc] = true
				stack = append(stack, State{nr, nc, dist + 1})
			}
		}
	}

	return graph
}

func longestPathDFS(graph [][]Edge, startIdx, endIdx int) int {
	// Use bitmask for visited (works up to 64 nodes, sufficient for this problem)
	var dfs func(node int, visited uint64) int
	dfs = func(node int, visited uint64) int {
		if node == endIdx {
			return 0
		}

		visited |= 1 << node
		maxDist := math.MinInt32

		for _, edge := range graph[node] {
			if (visited & (1 << edge.to)) == 0 {
				result := dfs(edge.to, visited)
				if result != math.MinInt32 {
					if edge.dist+result > maxDist {
						maxDist = edge.dist + result
					}
				}
			}
		}

		return maxDist
	}

	return dfs(startIdx, 0)
}

func solve(grid []string, respectSlopes bool) int {
	junctions := findJunctions(grid)
	graph := buildGraph(grid, junctions, respectSlopes)

	// Start is index 0, end is index 1 (by construction in findJunctions)
	return longestPathDFS(graph, 0, 1)
}

func part1(grid []string) int {
	return solve(grid, true)
}

func part2(grid []string) int {
	return solve(grid, false)
}

func main() {
	grid := parseInput("../input.txt")
	fmt.Printf("Part 1: %d\n", part1(grid))
	fmt.Printf("Part 2: %d\n", part2(grid))
}
