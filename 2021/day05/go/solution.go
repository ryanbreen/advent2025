package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

type Line struct {
	x1, y1, x2, y2 int
}

type Point struct {
	x, y int
}

func parseInput() ([]Line, error) {
	exePath, err := os.Executable()
	if err != nil {
		exePath, _ = os.Getwd()
	} else {
		exePath = filepath.Dir(exePath)
	}

	// Try multiple paths
	paths := []string{
		filepath.Join(exePath, "..", "input.txt"),
		filepath.Join(".", "..", "input.txt"),
		"../input.txt",
	}

	var file *os.File
	for _, p := range paths {
		file, err = os.Open(p)
		if err == nil {
			break
		}
	}
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var lines []Line
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}

		parts := strings.Split(line, " -> ")
		start := strings.Split(parts[0], ",")
		end := strings.Split(parts[1], ",")

		x1, _ := strconv.Atoi(start[0])
		y1, _ := strconv.Atoi(start[1])
		x2, _ := strconv.Atoi(end[0])
		y2, _ := strconv.Atoi(end[1])

		lines = append(lines, Line{x1, y1, x2, y2})
	}

	return lines, scanner.Err()
}

func sign(x int) int {
	if x > 0 {
		return 1
	} else if x < 0 {
		return -1
	}
	return 0
}

func countOverlaps(lines []Line, includeDiagonals bool) int {
	grid := make(map[Point]int)

	for _, l := range lines {
		dx := sign(l.x2 - l.x1)
		dy := sign(l.y2 - l.y1)

		// Skip diagonals in part 1
		if !includeDiagonals && dx != 0 && dy != 0 {
			continue
		}

		x, y := l.x1, l.y1
		for {
			grid[Point{x, y}]++
			if x == l.x2 && y == l.y2 {
				break
			}
			x += dx
			y += dy
		}
	}

	count := 0
	for _, v := range grid {
		if v >= 2 {
			count++
		}
	}
	return count
}

func part1(lines []Line) int {
	return countOverlaps(lines, false)
}

func part2(lines []Line) int {
	return countOverlaps(lines, true)
}

func main() {
	lines, err := parseInput()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(lines))
	fmt.Printf("Part 2: %d\n", part2(lines))
}
