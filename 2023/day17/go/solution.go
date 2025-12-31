package main

import (
	"container/heap"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// State represents a position and movement state in the grid
type State struct {
	row, col  int
	dir       int // 0=right, 1=down, 2=left, 3=up, -1=none
	consec    int
}

// Item represents an item in the priority queue
type Item struct {
	heat  int
	state State
}

// PriorityQueue implements heap.Interface for min-heap
type PriorityQueue []Item

func (pq PriorityQueue) Len() int           { return len(pq) }
func (pq PriorityQueue) Less(i, j int) bool { return pq[i].heat < pq[j].heat }
func (pq PriorityQueue) Swap(i, j int)      { pq[i], pq[j] = pq[j], pq[i] }

func (pq *PriorityQueue) Push(x interface{}) {
	*pq = append(*pq, x.(Item))
}

func (pq *PriorityQueue) Pop() interface{} {
	old := *pq
	n := len(old)
	item := old[n-1]
	*pq = old[:n-1]
	return item
}

func parseInput(filename string) [][]int {
	data, err := os.ReadFile(filename)
	if err != nil {
		panic(err)
	}

	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	grid := make([][]int, len(lines))

	for i, line := range lines {
		grid[i] = make([]int, len(line))
		for j, c := range line {
			grid[i][j] = int(c - '0')
		}
	}

	return grid
}

func dijkstra(grid [][]int, minStraight, maxStraight int) int {
	rows, cols := len(grid), len(grid[0])

	// Direction offsets: right, down, left, up
	dr := []int{0, 1, 0, -1}
	dc := []int{1, 0, -1, 0}

	// Priority queue with starting state
	pq := &PriorityQueue{Item{heat: 0, state: State{0, 0, -1, 0}}}
	heap.Init(pq)

	// Visited states
	visited := make(map[State]bool)

	for pq.Len() > 0 {
		item := heap.Pop(pq).(Item)
		heat := item.heat
		s := item.state

		// Check if we reached the goal
		if s.row == rows-1 && s.col == cols-1 {
			if minStraight == 0 || s.consec >= minStraight {
				return heat
			}
		}

		// Skip if already visited
		if visited[s] {
			continue
		}
		visited[s] = true

		// Try all four directions
		for nd := 0; nd < 4; nd++ {
			// Can't reverse direction
			if s.dir != -1 && nd == (s.dir+2)%4 {
				continue
			}

			nr, nc := s.row+dr[nd], s.col+dc[nd]

			// Bounds check
			if nr < 0 || nr >= rows || nc < 0 || nc >= cols {
				continue
			}

			var newConsec int
			if nd == s.dir {
				// Continuing in same direction
				newConsec = s.consec + 1
				if newConsec > maxStraight {
					continue
				}
			} else {
				// Turning - must have gone minStraight in previous direction first
				if s.dir != -1 && s.consec < minStraight {
					continue
				}
				newConsec = 1
			}

			newHeat := heat + grid[nr][nc]
			newState := State{nr, nc, nd, newConsec}

			if !visited[newState] {
				heap.Push(pq, Item{heat: newHeat, state: newState})
			}
		}
	}

	return -1 // No path found
}

func part1(grid [][]int) int {
	return dijkstra(grid, 0, 3)
}

func part2(grid [][]int) int {
	return dijkstra(grid, 4, 10)
}

func main() {
	exePath, _ := os.Executable()
	dir := filepath.Dir(exePath)
	inputPath := filepath.Join(dir, "..", "input.txt")

	// Fallback for running with go run
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		inputPath = filepath.Join(".", "..", "input.txt")
	}

	grid := parseInput(inputPath)

	fmt.Printf("Part 1: %d\n", part1(grid))
	fmt.Printf("Part 2: %d\n", part2(grid))
}
