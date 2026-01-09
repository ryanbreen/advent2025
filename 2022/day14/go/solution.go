package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

type Point struct {
	x, y int
}

func parsePaths(text string) map[Point]bool {
	rocks := make(map[Point]bool)
	lines := strings.Split(strings.TrimSpace(text), "\n")

	for _, line := range lines {
		parts := strings.Split(line, " -> ")
		for i := 0; i < len(parts)-1; i++ {
			p1 := parsePoint(parts[i])
			p2 := parsePoint(parts[i+1])

			// Draw line from p1 to p2
			if p1.x == p2.x {
				// vertical line
				minY, maxY := min(p1.y, p2.y), max(p1.y, p2.y)
				for y := minY; y <= maxY; y++ {
					rocks[Point{p1.x, y}] = true
				}
			} else {
				// horizontal line
				minX, maxX := min(p1.x, p2.x), max(p1.x, p2.x)
				for x := minX; x <= maxX; x++ {
					rocks[Point{x, p1.y}] = true
				}
			}
		}
	}
	return rocks
}

func parsePoint(s string) Point {
	parts := strings.Split(s, ",")
	x, _ := strconv.Atoi(parts[0])
	y, _ := strconv.Atoi(parts[1])
	return Point{x, y}
}

func simulateSand(blocked map[Point]bool, maxY int, floor bool) (Point, bool) {
	x, y := 500, 0

	for {
		// Check if sand has fallen below all rocks (into abyss)
		if !floor && y > maxY {
			return Point{}, false
		}

		// Try to move down
		if floor && y+1 == maxY+2 {
			// Hit the floor
			return Point{x, y}, true
		} else if !blocked[Point{x, y + 1}] {
			y++
		} else if !blocked[Point{x - 1, y + 1}] {
			// Try to move down-left
			x--
			y++
		} else if !blocked[Point{x + 1, y + 1}] {
			// Try to move down-right
			x++
			y++
		} else {
			// Sand comes to rest
			return Point{x, y}, true
		}
	}
}

func findMaxY(rocks map[Point]bool) int {
	maxY := 0
	for p := range rocks {
		if p.y > maxY {
			maxY = p.y
		}
	}
	return maxY
}

func part1(text string) int {
	rocks := parsePaths(text)
	maxY := findMaxY(rocks)

	// Copy rocks to blocked
	blocked := make(map[Point]bool)
	for p := range rocks {
		blocked[p] = true
	}

	count := 0
	for {
		pos, ok := simulateSand(blocked, maxY, false)
		if !ok {
			break
		}
		blocked[pos] = true
		count++
	}
	return count
}

func part2(text string) int {
	rocks := parsePaths(text)
	maxY := findMaxY(rocks)

	// Copy rocks to blocked
	blocked := make(map[Point]bool)
	for p := range rocks {
		blocked[p] = true
	}

	count := 0
	source := Point{500, 0}
	for {
		pos, _ := simulateSand(blocked, maxY, true)
		blocked[pos] = true
		count++
		if pos == source {
			break
		}
	}
	return count
}

func main() {
	// Get the directory of the executable
	execPath, _ := os.Executable()
	dir := filepath.Dir(execPath)

	// Try to find input.txt in parent directory
	inputPath := filepath.Join(dir, "..", "input.txt")
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		// Fallback: try current working directory
		cwd, _ := os.Getwd()
		inputPath = filepath.Join(cwd, "..", "input.txt")
	}

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

	text := sb.String()

	fmt.Println("Part 1:", part1(text))
	fmt.Println("Part 2:", part2(text))
}
