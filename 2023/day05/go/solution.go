// Package main solves Advent of Code 2023 Day 5: If You Give A Seed A Fertilizer.
//
// The puzzle involves mapping seed numbers through a series of category
// transformations (seed -> soil -> fertilizer -> water -> light -> temperature
// -> humidity -> location) to find the lowest location number.
package main

import (
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
)

// MapRange represents a single mapping range with destination start, source start, and length.
type MapRange struct {
	dstStart int
	srcStart int
	length   int
}

// srcEnd returns the exclusive end of the source range.
func (m MapRange) srcEnd() int {
	return m.srcStart + m.length
}

// offset returns the difference between destination and source starts.
func (m MapRange) offset() int {
	return m.dstStart - m.srcStart
}

// Range represents a half-open interval [start, end).
type Range struct {
	start int
	end   int
}

// parseInput parses the puzzle input into seeds and mapping layers.
func parseInput(text string) (seeds []int, maps [][]MapRange) {
	sections := strings.Split(strings.TrimSpace(text), "\n\n")

	// Parse seeds
	seedStrs := strings.Fields(strings.Split(sections[0], ": ")[1])
	seeds = make([]int, len(seedStrs))
	for i, s := range seedStrs {
		seeds[i], _ = strconv.Atoi(s)
	}

	// Parse maps
	maps = make([][]MapRange, len(sections)-1)
	for i, section := range sections[1:] {
		lines := strings.Split(strings.TrimSpace(section), "\n")[1:] // Skip header
		maps[i] = make([]MapRange, len(lines))
		for j, line := range lines {
			parts := strings.Fields(line)
			dst, _ := strconv.Atoi(parts[0])
			src, _ := strconv.Atoi(parts[1])
			length, _ := strconv.Atoi(parts[2])
			maps[i][j] = MapRange{dst, src, length}
		}
	}

	return seeds, maps
}

// applyMap transforms a value through a single mapping layer.
func applyMap(value int, ranges []MapRange) int {
	for _, r := range ranges {
		if value >= r.srcStart && value < r.srcEnd() {
			return value + r.offset()
		}
	}
	return value
}

// seedToLocation transforms a seed number through all mapping layers to get its location.
func seedToLocation(seed int, maps [][]MapRange) int {
	value := seed
	for _, layer := range maps {
		value = applyMap(value, layer)
	}
	return value
}

// part1 finds the minimum location for individual seed values.
func part1(seeds []int, maps [][]MapRange) int {
	locations := make([]int, len(seeds))
	for i, seed := range seeds {
		locations[i] = seedToLocation(seed, maps)
	}
	return slices.Min(locations)
}

// applyMapToRanges transforms a set of ranges through a single mapping layer.
// Each input range may be split into multiple output ranges based on how it
// overlaps with the mapping ranges.
func applyMapToRanges(inputRanges []Range, mapRanges []MapRange) []Range {
	var result []Range

	for _, input := range inputRanges {
		// Track unmapped portions as we process each map range
		unmapped := []Range{input}

		for _, mr := range mapRanges {
			var stillUnmapped []Range

			for _, r := range unmapped {
				// Calculate overlap between range r and map range mr
				overlapStart := max(r.start, mr.srcStart)
				overlapEnd := min(r.end, mr.srcEnd())

				if overlapStart < overlapEnd {
					// This portion maps to destination
					result = append(result, Range{
						start: overlapStart + mr.offset(),
						end:   overlapEnd + mr.offset(),
					})

					// Keep unmapped portions (before and after overlap)
					if r.start < overlapStart {
						stillUnmapped = append(stillUnmapped, Range{r.start, overlapStart})
					}
					if r.end > overlapEnd {
						stillUnmapped = append(stillUnmapped, Range{overlapEnd, r.end})
					}
				} else {
					// No overlap, entire range remains unmapped
					stillUnmapped = append(stillUnmapped, r)
				}
			}

			unmapped = stillUnmapped
		}

		// Any remaining unmapped portions pass through unchanged
		result = append(result, unmapped...)
	}

	return result
}

// part2 finds the minimum location when seeds are interpreted as ranges.
// Seed pairs (start, length) define contiguous ranges of seed values.
func part2(seeds []int, maps [][]MapRange) int {
	// Convert seed pairs to ranges
	ranges := make([]Range, 0, len(seeds)/2)
	for i := 0; i < len(seeds); i += 2 {
		ranges = append(ranges, Range{seeds[i], seeds[i] + seeds[i+1]})
	}

	// Apply each mapping layer
	for _, layer := range maps {
		ranges = applyMapToRanges(ranges, layer)
	}

	// Find minimum start value across all resulting ranges
	starts := make([]int, len(ranges))
	for i, r := range ranges {
		starts[i] = r.start
	}
	return slices.Min(starts)
}

func main() {
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	seeds, maps := parseInput(string(data))

	fmt.Println("Part 1:", part1(seeds, maps))
	fmt.Println("Part 2:", part2(seeds, maps))
}
