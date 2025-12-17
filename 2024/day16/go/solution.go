package main

import (
	"container/heap"
	"fmt"
	"os"
	"strings"
)

// Directions: 0=East, 1=South, 2=West, 3=North
var dx = [4]int{1, 0, -1, 0}
var dy = [4]int{0, 1, 0, -1}

// State represents a position and direction in the maze
type State struct {
	x, y, dir int
}

// PQItem represents an item in the priority queue
type PQItem struct {
	cost int
	x, y int
	dir  int
	index int
}

// PriorityQueue implements heap.Interface
type PriorityQueue []*PQItem

func (pq PriorityQueue) Len() int { return len(pq) }

func (pq PriorityQueue) Less(i, j int) bool {
	return pq[i].cost < pq[j].cost
}

func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].index = i
	pq[j].index = j
}

func (pq *PriorityQueue) Push(x interface{}) {
	n := len(*pq)
	item := x.(*PQItem)
	item.index = n
	*pq = append(*pq, item)
}

func (pq *PriorityQueue) Pop() interface{} {
	old := *pq
	n := len(old)
	item := old[n-1]
	old[n-1] = nil
	item.index = -1
	*pq = old[0 : n-1]
	return item
}

// parseInput parses the maze and finds start/end positions
func parseInput(text string) ([][]byte, [2]int, [2]int) {
	lines := strings.Split(strings.TrimSpace(text), "\n")
	grid := make([][]byte, len(lines))
	var start, end [2]int

	for y, line := range lines {
		grid[y] = []byte(line)
		for x, cell := range grid[y] {
			if cell == 'S' {
				start = [2]int{x, y}
			} else if cell == 'E' {
				end = [2]int{x, y}
			}
		}
	}

	return grid, start, end
}

// dijkstraForward runs Dijkstra from start facing East
func dijkstraForward(grid [][]byte, start [2]int) map[State]int {
	pq := &PriorityQueue{}
	heap.Init(pq)
	heap.Push(pq, &PQItem{cost: 0, x: start[0], y: start[1], dir: 0})

	dist := make(map[State]int)
	height := len(grid)
	width := len(grid[0])

	for pq.Len() > 0 {
		item := heap.Pop(pq).(*PQItem)
		cost, x, y, d := item.cost, item.x, item.y, item.dir

		state := State{x, y, d}
		if _, visited := dist[state]; visited {
			continue
		}
		dist[state] = cost

		// Move forward
		nx, ny := x+dx[d], y+dy[d]
		if ny >= 0 && ny < height && nx >= 0 && nx < width && grid[ny][nx] != '#' {
			heap.Push(pq, &PQItem{cost: cost + 1, x: nx, y: ny, dir: d})
		}

		// Turn left
		heap.Push(pq, &PQItem{cost: cost + 1000, x: x, y: y, dir: (d - 1 + 4) % 4})
		// Turn right
		heap.Push(pq, &PQItem{cost: cost + 1000, x: x, y: y, dir: (d + 1) % 4})
	}

	return dist
}

// dijkstraBackward runs Dijkstra backward from end (all directions)
func dijkstraBackward(grid [][]byte, end [2]int) map[State]int {
	pq := &PriorityQueue{}
	heap.Init(pq)

	// At end, we can arrive facing any direction
	for d := 0; d < 4; d++ {
		heap.Push(pq, &PQItem{cost: 0, x: end[0], y: end[1], dir: d})
	}

	dist := make(map[State]int)
	height := len(grid)
	width := len(grid[0])

	for pq.Len() > 0 {
		item := heap.Pop(pq).(*PQItem)
		cost, x, y, d := item.cost, item.x, item.y, item.dir

		state := State{x, y, d}
		if _, visited := dist[state]; visited {
			continue
		}
		dist[state] = cost

		// Reverse of "move forward": come from behind
		px, py := x-dx[d], y-dy[d]
		if py >= 0 && py < height && px >= 0 && px < width && grid[py][px] != '#' {
			heap.Push(pq, &PQItem{cost: cost + 1, x: px, y: py, dir: d})
		}

		// Reverse of turn: came from same position with different direction
		heap.Push(pq, &PQItem{cost: cost + 1000, x: x, y: y, dir: (d - 1 + 4) % 4})
		heap.Push(pq, &PQItem{cost: cost + 1000, x: x, y: y, dir: (d + 1) % 4})
	}

	return dist
}

// part1 finds the lowest score path from start to end
func part1(grid [][]byte, start, end [2]int) int {
	dist := dijkstraForward(grid, start)

	minCost := int(^uint(0) >> 1) // Max int
	for d := 0; d < 4; d++ {
		state := State{end[0], end[1], d}
		if cost, ok := dist[state]; ok && cost < minCost {
			minCost = cost
		}
	}

	return minCost
}

// part2 counts tiles that are part of any optimal path
func part2(grid [][]byte, start, end [2]int, bestScore int) int {
	distFromStart := dijkstraForward(grid, start)
	distToEnd := dijkstraBackward(grid, end)

	tilesOnBestPath := make(map[[2]int]bool)
	height := len(grid)
	width := len(grid[0])

	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			if grid[y][x] == '#' {
				continue
			}
			// Check if this tile is on any optimal path
			for d := 0; d < 4; d++ {
				state := State{x, y, d}
				fromStart, hasFromStart := distFromStart[state]
				toEnd, hasToEnd := distToEnd[state]

				if hasFromStart && hasToEnd && fromStart+toEnd == bestScore {
					tilesOnBestPath[[2]int{x, y}] = true
					break
				}
			}
		}
	}

	return len(tilesOnBestPath)
}

func main() {
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	grid, start, end := parseInput(string(data))

	answer1 := part1(grid, start, end)
	fmt.Printf("Part 1: %d\n", answer1)

	answer2 := part2(grid, start, end, answer1)
	fmt.Printf("Part 2: %d\n", answer2)
}
