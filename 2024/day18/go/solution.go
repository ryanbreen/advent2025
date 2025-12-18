package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	x, y int
}

type QueueItem struct {
	point Point
	steps int
}

func parseInput(filename string) ([]Point, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var positions []Point
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		parts := strings.Split(line, ",")
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		positions = append(positions, Point{x, y})
	}
	return positions, scanner.Err()
}

func bfs(corrupted map[Point]bool, size int) int {
	start := Point{0, 0}
	goal := Point{size - 1, size - 1}

	if corrupted[start] || corrupted[goal] {
		return -1
	}

	queue := []QueueItem{{start, 0}}
	visited := make(map[Point]bool)
	visited[start] = true

	directions := []Point{{0, 1}, {0, -1}, {1, 0}, {-1, 0}}

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		if current.point == goal {
			return current.steps
		}

		for _, d := range directions {
			next := Point{current.point.x + d.x, current.point.y + d.y}
			if next.x >= 0 && next.x < size && next.y >= 0 && next.y < size &&
				!visited[next] && !corrupted[next] {
				visited[next] = true
				queue = append(queue, QueueItem{next, current.steps + 1})
			}
		}
	}

	return -1
}

func part1(positions []Point, numBytes, size int) int {
	corrupted := make(map[Point]bool)
	for i := 0; i < numBytes && i < len(positions); i++ {
		corrupted[positions[i]] = true
	}
	return bfs(corrupted, size)
}

func part2(positions []Point, size int) string {
	left, right := 0, len(positions)

	for left < right {
		mid := (left + right) / 2
		corrupted := make(map[Point]bool)
		for i := 0; i <= mid; i++ {
			corrupted[positions[i]] = true
		}
		if bfs(corrupted, size) == -1 {
			right = mid
		} else {
			left = mid + 1
		}
	}

	blockingPos := positions[left]
	return fmt.Sprintf("%d,%d", blockingPos.x, blockingPos.y)
}

func main() {
	positions, err := parseInput("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Part 1:", part1(positions, 1024, 71))
	fmt.Println("Part 2:", part2(positions, 71))
}
