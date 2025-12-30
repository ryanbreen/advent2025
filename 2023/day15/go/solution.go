package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

// Lens represents a lens with a label and focal length
type Lens struct {
	label string
	focal int
}

// hash runs the HASH algorithm on a string
func hash(s string) int {
	current := 0
	for i := 0; i < len(s); i++ {
		current = ((current + int(s[i])) * 17) % 256
	}
	return current
}

// part1 calculates the sum of HASH values for all steps
func part1(steps []string) int {
	total := 0
	for _, step := range steps {
		total += hash(step)
	}
	return total
}

// part2 runs the HASHMAP procedure and calculates focusing power
func part2(steps []string) int {
	boxes := make([][]Lens, 256)
	for i := range boxes {
		boxes[i] = []Lens{}
	}

	for _, step := range steps {
		if strings.Contains(step, "=") {
			parts := strings.Split(step, "=")
			label := parts[0]
			focal, _ := strconv.Atoi(parts[1])
			boxNum := hash(label)

			// Try to find and replace existing lens
			found := false
			for i, lens := range boxes[boxNum] {
				if lens.label == label {
					boxes[boxNum][i].focal = focal
					found = true
					break
				}
			}
			// If not found, append new lens
			if !found {
				boxes[boxNum] = append(boxes[boxNum], Lens{label, focal})
			}
		} else {
			// '-' operation - remove lens
			label := step[:len(step)-1]
			boxNum := hash(label)

			// Remove lens with matching label
			newBox := []Lens{}
			for _, lens := range boxes[boxNum] {
				if lens.label != label {
					newBox = append(newBox, lens)
				}
			}
			boxes[boxNum] = newBox
		}
	}

	// Calculate focusing power
	total := 0
	for boxNum, box := range boxes {
		for slot, lens := range box {
			total += (boxNum + 1) * (slot + 1) * lens.focal
		}
	}
	return total
}

func main() {
	// Get the directory of the executable
	execPath, _ := os.Executable()
	dir := filepath.Dir(execPath)
	inputPath := filepath.Join(dir, "..", "input.txt")

	// Try relative path first (for go run)
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		inputPath = "../input.txt"
	}

	data, err := os.ReadFile(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	// Parse input - remove newlines and split by comma
	text := strings.TrimSpace(string(data))
	text = strings.ReplaceAll(text, "\n", "")
	steps := strings.Split(text, ",")

	fmt.Printf("Part 1: %d\n", part1(steps))
	fmt.Printf("Part 2: %d\n", part2(steps))
}
