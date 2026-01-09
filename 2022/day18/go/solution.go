package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

// Point3D represents a 3D coordinate
type Point3D struct {
	x, y, z int
}

// 6 directions: +x, -x, +y, -y, +z, -z
var directions = []Point3D{
	{1, 0, 0}, {-1, 0, 0},
	{0, 1, 0}, {0, -1, 0},
	{0, 0, 1}, {0, 0, -1},
}

func parseInput(text string) map[Point3D]bool {
	cubes := make(map[Point3D]bool)
	scanner := bufio.NewScanner(strings.NewReader(text))

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		parts := strings.Split(line, ",")
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		z, _ := strconv.Atoi(parts[2])
		cubes[Point3D{x, y, z}] = true
	}

	return cubes
}

func part1(text string) int {
	cubes := parseInput(text)
	surfaceArea := 0

	for cube := range cubes {
		for _, dir := range directions {
			neighbor := Point3D{cube.x + dir.x, cube.y + dir.y, cube.z + dir.z}
			if !cubes[neighbor] {
				surfaceArea++
			}
		}
	}

	return surfaceArea
}

func part2(text string) int {
	cubes := parseInput(text)

	// Find bounding box with 1 unit padding
	minX, maxX := 1<<30, -(1 << 30)
	minY, maxY := 1<<30, -(1 << 30)
	minZ, maxZ := 1<<30, -(1 << 30)

	for cube := range cubes {
		if cube.x < minX {
			minX = cube.x
		}
		if cube.x > maxX {
			maxX = cube.x
		}
		if cube.y < minY {
			minY = cube.y
		}
		if cube.y > maxY {
			maxY = cube.y
		}
		if cube.z < minZ {
			minZ = cube.z
		}
		if cube.z > maxZ {
			maxZ = cube.z
		}
	}

	// Add padding
	minX--
	maxX++
	minY--
	maxY++
	minZ--
	maxZ++

	// BFS to find all exterior air cells
	exterior := make(map[Point3D]bool)
	start := Point3D{minX, minY, minZ}
	queue := []Point3D{start}
	exterior[start] = true

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		for _, dir := range directions {
			next := Point3D{current.x + dir.x, current.y + dir.y, current.z + dir.z}

			// Stay within bounds
			if next.x < minX || next.x > maxX ||
				next.y < minY || next.y > maxY ||
				next.z < minZ || next.z > maxZ {
				continue
			}

			// Skip cubes and already visited
			if cubes[next] || exterior[next] {
				continue
			}

			exterior[next] = true
			queue = append(queue, next)
		}
	}

	// Count faces touching exterior air
	surfaceArea := 0
	for cube := range cubes {
		for _, dir := range directions {
			neighbor := Point3D{cube.x + dir.x, cube.y + dir.y, cube.z + dir.z}
			if exterior[neighbor] {
				surfaceArea++
			}
		}
	}

	return surfaceArea
}

func main() {
	execPath, _ := os.Executable()
	dir := filepath.Dir(execPath)
	inputPath := filepath.Join(dir, "..", "input.txt")

	// Fallback for running with go run
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		inputPath = filepath.Join(".", "..", "input.txt")
	}

	data, err := os.ReadFile(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	text := string(data)

	fmt.Println("Part 1:", part1(text))
	fmt.Println("Part 2:", part2(text))
}
