package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
)

func parseInput(filename string) []string {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var rucksacks []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line != "" {
			rucksacks = append(rucksacks, line)
		}
	}
	return rucksacks
}

func priority(char rune) int {
	if char >= 'a' && char <= 'z' {
		return int(char-'a') + 1
	}
	return int(char-'A') + 27
}

func part1(rucksacks []string) int {
	total := 0
	for _, rucksack := range rucksacks {
		mid := len(rucksack) / 2
		first := make(map[rune]bool)
		for _, c := range rucksack[:mid] {
			first[c] = true
		}
		for _, c := range rucksack[mid:] {
			if first[c] {
				total += priority(c)
				break
			}
		}
	}
	return total
}

func part2(rucksacks []string) int {
	total := 0
	for i := 0; i < len(rucksacks); i += 3 {
		first := make(map[rune]bool)
		second := make(map[rune]bool)

		for _, c := range rucksacks[i] {
			first[c] = true
		}
		for _, c := range rucksacks[i+1] {
			if first[c] {
				second[c] = true
			}
		}
		for _, c := range rucksacks[i+2] {
			if second[c] {
				total += priority(c)
				break
			}
		}
	}
	return total
}

func main() {
	exePath, _ := os.Executable()
	exeDir := filepath.Dir(exePath)
	inputFile := filepath.Join(exeDir, "..", "input.txt")

	// If running with go run, use the source file's directory instead
	if _, err := os.Stat(inputFile); os.IsNotExist(err) {
		inputFile = filepath.Join(".", "..", "input.txt")
	}

	rucksacks := parseInput(inputFile)

	fmt.Println("Part 1:", part1(rucksacks))
	fmt.Println("Part 2:", part2(rucksacks))
}
