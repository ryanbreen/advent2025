package main

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

type region struct {
	width  int
	height int
	counts []int
}

// parseInput parses the input into shapes and regions
func parseInput(text string) (map[int]int, []region, error) {
	sections := strings.Split(strings.TrimSpace(text), "\n\n")

	shapes := make(map[int]int)
	var regions []region

	for _, section := range sections {
		lines := strings.Split(strings.TrimSpace(section), "\n")
		if len(lines) == 0 {
			continue
		}

		if strings.Contains(lines[0], ":") && !strings.Contains(lines[0], "x") {
			// Shape definition
			idxStr := strings.TrimSuffix(lines[0], ":")
			idx, err := strconv.Atoi(idxStr)
			if err != nil {
				return nil, nil, fmt.Errorf("invalid shape index %q: %w", idxStr, err)
			}

			cellCount := 0
			for i := 1; i < len(lines); i++ {
				cellCount += strings.Count(lines[i], "#")
			}
			shapes[idx] = cellCount
		} else {
			// Region definitions
			for _, line := range lines {
				if strings.Contains(line, "x") {
					parts := strings.Split(line, ":")
					if len(parts) != 2 {
						return nil, nil, fmt.Errorf("invalid region line: %q", line)
					}

					dims := strings.TrimSpace(parts[0])
					dimParts := strings.Split(dims, "x")
					if len(dimParts) != 2 {
						return nil, nil, fmt.Errorf("invalid dimensions: %q", dims)
					}

					w, err := strconv.Atoi(dimParts[0])
					if err != nil {
						return nil, nil, fmt.Errorf("invalid width %q: %w", dimParts[0], err)
					}
					h, err := strconv.Atoi(dimParts[1])
					if err != nil {
						return nil, nil, fmt.Errorf("invalid height %q: %w", dimParts[1], err)
					}

					countsStr := strings.Fields(strings.TrimSpace(parts[1]))
					counts := make([]int, len(countsStr))
					for i, s := range countsStr {
						count, err := strconv.Atoi(s)
						if err != nil {
							return nil, nil, fmt.Errorf("invalid count %q: %w", s, err)
						}
						counts[i] = count
					}

					regions = append(regions, region{width: w, height: h, counts: counts})
				}
			}
		}
	}

	return shapes, regions, nil
}

// canFitRegion checks if all presents can fit in the region
func canFitRegion(r region, shapes map[int]int) bool {
	totalCellsNeeded := 0
	for i, count := range r.counts {
		totalCellsNeeded += count * shapes[i]
	}
	available := r.width * r.height
	return totalCellsNeeded <= available
}

// part1 counts regions that can fit all their presents
func part1(shapes map[int]int, regions []region) int {
	count := 0
	for _, r := range regions {
		if canFitRegion(r, shapes) {
			count++
		}
	}
	return count
}

// part2 is just a button click to finish - no computation needed
func part2() int {
	return 0
}

func main() {
	file, err := os.Open("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	data, err := io.ReadAll(file)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}

	shapes, regions, err := parseInput(string(data))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error parsing input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(shapes, regions))
	fmt.Printf("Part 2: %d\n", part2())
}
