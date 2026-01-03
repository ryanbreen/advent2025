package main

import (
	"fmt"
	"os"
)

func main() {
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input file: %v\n", err)
		os.Exit(1)
	}

	part1Sum := 0
	part2Sum := 0

	i := 0
	n := len(data)

	for i < n {
		// Skip "Game "
		i += 5

		// Parse game ID
		gameID := 0
		for i < n && data[i] >= '0' && data[i] <= '9' {
			gameID = gameID*10 + int(data[i]-'0')
			i++
		}

		// Skip ": "
		i += 2

		// Track minimums for part 2 and max seen for part 1
		minRed, minGreen, minBlue := 0, 0, 0
		possible := true

		// Parse draws until end of line
		for i < n && data[i] != '\n' {
			// Parse count
			count := 0
			for i < n && data[i] >= '0' && data[i] <= '9' {
				count = count*10 + int(data[i]-'0')
				i++
			}

			// Skip space
			i++

			// Determine color by first character
			switch data[i] {
			case 'r': // red
				if count > minRed {
					minRed = count
				}
				if count > 12 {
					possible = false
				}
				i += 3 // skip "red"
			case 'g': // green
				if count > minGreen {
					minGreen = count
				}
				if count > 13 {
					possible = false
				}
				i += 5 // skip "green"
			case 'b': // blue
				if count > minBlue {
					minBlue = count
				}
				if count > 14 {
					possible = false
				}
				i += 4 // skip "blue"
			}

			// Skip separator (, or ;) and space if present
			if i < n && (data[i] == ',' || data[i] == ';') {
				i += 2 // skip ", " or "; "
			}
		}

		// Skip newline
		if i < n && data[i] == '\n' {
			i++
		}

		// Part 1: sum IDs of possible games
		if possible {
			part1Sum += gameID
		}

		// Part 2: sum power of minimum cubes
		part2Sum += minRed * minGreen * minBlue
	}

	fmt.Printf("Part 1: %d\n", part1Sum)
	fmt.Printf("Part 2: %d\n", part2Sum)
}
