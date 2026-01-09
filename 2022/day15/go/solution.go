package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
)

type Sensor struct {
	sx, sy, bx, by, dist int
}

type Range struct {
	start, end int
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func parseSensors(filename string) []Sensor {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var sensors []Sensor
	pattern := regexp.MustCompile(`Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)`)

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		matches := pattern.FindStringSubmatch(line)
		if matches == nil {
			continue
		}

		sx, _ := strconv.Atoi(matches[1])
		sy, _ := strconv.Atoi(matches[2])
		bx, _ := strconv.Atoi(matches[3])
		by, _ := strconv.Atoi(matches[4])
		dist := abs(sx-bx) + abs(sy-by)

		sensors = append(sensors, Sensor{sx, sy, bx, by, dist})
	}

	return sensors
}

func mergeRanges(ranges []Range) []Range {
	if len(ranges) == 0 {
		return ranges
	}

	sort.Slice(ranges, func(i, j int) bool {
		return ranges[i].start < ranges[j].start
	})

	merged := []Range{ranges[0]}

	for i := 1; i < len(ranges); i++ {
		last := &merged[len(merged)-1]
		if ranges[i].start <= last.end+1 {
			if ranges[i].end > last.end {
				last.end = ranges[i].end
			}
		} else {
			merged = append(merged, ranges[i])
		}
	}

	return merged
}

func getCoverageAtRow(sensors []Sensor, row int) []Range {
	var ranges []Range

	for _, s := range sensors {
		rowDist := abs(s.sy - row)
		if rowDist > s.dist {
			continue
		}

		xSpread := s.dist - rowDist
		ranges = append(ranges, Range{s.sx - xSpread, s.sx + xSpread})
	}

	return mergeRanges(ranges)
}

func part1(sensors []Sensor) int {
	targetRow := 2000000
	ranges := getCoverageAtRow(sensors, targetRow)

	total := 0
	for _, r := range ranges {
		total += r.end - r.start + 1
	}

	// Subtract beacons on this row
	beaconsOnRow := make(map[int]bool)
	for _, s := range sensors {
		if s.by == targetRow {
			beaconsOnRow[s.bx] = true
		}
	}

	return total - len(beaconsOnRow)
}

func part2(sensors []Sensor) int {
	maxCoord := 4000000

	for row := 0; row <= maxCoord; row++ {
		ranges := getCoverageAtRow(sensors, row)

		// Clip ranges to search area
		var clipped []Range
		for _, r := range ranges {
			if r.end < 0 || r.start > maxCoord {
				continue
			}
			start := r.start
			end := r.end
			if start < 0 {
				start = 0
			}
			if end > maxCoord {
				end = maxCoord
			}
			clipped = append(clipped, Range{start, end})
		}

		clipped = mergeRanges(clipped)

		// Check if full row is covered
		if len(clipped) == 1 && clipped[0].start == 0 && clipped[0].end == maxCoord {
			continue
		}

		// Found a gap
		var x int
		if len(clipped) > 1 {
			x = clipped[0].end + 1
		} else if clipped[0].start > 0 {
			x = 0
		} else {
			x = clipped[0].end + 1
		}

		return x*4000000 + row
	}

	return -1
}

func main() {
	exePath, _ := os.Executable()
	dir := filepath.Dir(exePath)
	inputFile := filepath.Join(dir, "..", "input.txt")

	// If running with go run, use relative path from source
	if _, err := os.Stat(inputFile); os.IsNotExist(err) {
		inputFile = filepath.Join(".", "..", "input.txt")
	}

	sensors := parseSensors(inputFile)

	fmt.Println("Part 1:", part1(sensors))
	fmt.Println("Part 2:", part2(sensors))
}
