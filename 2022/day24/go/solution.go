package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

type Blizzard struct {
	r, c      int
	direction rune
}

type State struct {
	timeModPeriod int
	r, c          int
}

type QueueItem struct {
	time int
	r, c int
}

func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func lcm(a, b int) int {
	return a * b / gcd(a, b)
}

func parseInput(text string) ([]Blizzard, int, int, int, int, [2]int, [2]int) {
	lines := strings.Split(strings.TrimSpace(text), "\n")
	height := len(lines)
	width := len(lines[0])

	innerH := height - 2
	innerW := width - 2

	var blizzards []Blizzard
	for r, line := range lines {
		for c, ch := range line {
			if ch == '^' || ch == 'v' || ch == '<' || ch == '>' {
				blizzards = append(blizzards, Blizzard{r, c, ch})
			}
		}
	}

	start := [2]int{0, strings.Index(lines[0], ".")}
	end := [2]int{height - 1, strings.Index(lines[height-1], ".")}

	return blizzards, height, width, innerH, innerW, start, end
}

func getBlizzardPositions(blizzards []Blizzard, innerH, innerW, time int) map[[2]int]bool {
	positions := make(map[[2]int]bool)

	for _, b := range blizzards {
		ir := b.r - 1
		ic := b.c - 1

		var nr, nc int

		switch b.direction {
		case '^':
			nr = ((ir-time)%innerH + innerH) % innerH
			nc = ic
		case 'v':
			nr = (ir + time) % innerH
			nc = ic
		case '<':
			nr = ir
			nc = ((ic-time)%innerW + innerW) % innerW
		case '>':
			nr = ir
			nc = (ic + time) % innerW
		}

		positions[[2]int{nr + 1, nc + 1}] = true
	}

	return positions
}

func bfs(blizzards []Blizzard, height, width, innerH, innerW int, start, end [2]int, startTime int) int {
	period := lcm(innerH, innerW)

	// Precompute blizzard positions for all times in one period
	blizzardCache := make([]map[[2]int]bool, period)
	for t := 0; t < period; t++ {
		blizzardCache[t] = getBlizzardPositions(blizzards, innerH, innerW, t)
	}

	// BFS with queue
	queue := []QueueItem{{startTime, start[0], start[1]}}
	visited := make(map[State]bool)
	visited[State{startTime % period, start[0], start[1]}] = true

	// Directions: wait, up, down, left, right
	directions := [][2]int{{0, 0}, {-1, 0}, {1, 0}, {0, -1}, {0, 1}}

	for len(queue) > 0 {
		item := queue[0]
		queue = queue[1:]

		time, r, c := item.time, item.r, item.c

		if r == end[0] && c == end[1] {
			return time
		}

		nextTime := time + 1
		nextBlizzards := blizzardCache[nextTime%period]

		for _, d := range directions {
			nr, nc := r+d[0], c+d[1]

			// Check bounds
			if nr == start[0] && nc == start[1] {
				// Start position is always valid
			} else if nr == end[0] && nc == end[1] {
				// End position is always valid
			} else if nr <= 0 || nr >= height-1 || nc <= 0 || nc >= width-1 {
				continue // Wall
			}

			// Check blizzards
			if nextBlizzards[[2]int{nr, nc}] {
				continue
			}

			state := State{nextTime % period, nr, nc}
			if !visited[state] {
				visited[state] = true
				queue = append(queue, QueueItem{nextTime, nr, nc})
			}
		}
	}

	return -1 // No path found
}

func part1(text string) int {
	blizzards, height, width, innerH, innerW, start, end := parseInput(text)
	return bfs(blizzards, height, width, innerH, innerW, start, end, 0)
}

func part2(text string) int {
	blizzards, height, width, innerH, innerW, start, end := parseInput(text)

	// Trip 1: start to end
	t1 := bfs(blizzards, height, width, innerH, innerW, start, end, 0)

	// Trip 2: end to start
	t2 := bfs(blizzards, height, width, innerH, innerW, end, start, t1)

	// Trip 3: start to end again
	t3 := bfs(blizzards, height, width, innerH, innerW, start, end, t2)

	return t3
}

func main() {
	_, currentFile, _, _ := runtime.Caller(0)
	dir := filepath.Dir(currentFile)
	inputPath := filepath.Join(dir, "..", "input.txt")

	file, err := os.Open(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var sb strings.Builder
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		sb.WriteString(scanner.Text())
		sb.WriteString("\n")
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}

	text := sb.String()

	fmt.Println("Part 1:", part1(text))
	fmt.Println("Part 2:", part2(text))
}
