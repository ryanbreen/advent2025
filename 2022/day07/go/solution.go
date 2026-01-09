package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

func parseFilesystem(lines []string) map[string]int {
	path := []string{}
	dirSizes := make(map[string]int)

	for _, line := range lines {
		if strings.HasPrefix(line, "$ cd") {
			target := line[5:]
			if target == "/" {
				path = []string{"/"}
			} else if target == ".." {
				path = path[:len(path)-1]
			} else {
				path = append(path, target)
			}
		} else if strings.HasPrefix(line, "$ ls") {
			continue
		} else if strings.HasPrefix(line, "dir ") {
			continue
		} else {
			// It's a file with size
			parts := strings.SplitN(line, " ", 2)
			size, _ := strconv.Atoi(parts[0])
			// Add size to current directory and all parent directories
			for i := range path {
				var dirPath string
				if i == 0 {
					dirPath = "/"
				} else {
					dirPath = strings.Join(path[:i+1], "/")
				}
				dirSizes[dirPath] += size
			}
		}
	}

	return dirSizes
}

func part1(dirSizes map[string]int) int {
	sum := 0
	for _, size := range dirSizes {
		if size <= 100000 {
			sum += size
		}
	}
	return sum
}

func part2(dirSizes map[string]int) int {
	totalSpace := 70000000
	neededSpace := 30000000
	usedSpace := dirSizes["/"]
	freeSpace := totalSpace - usedSpace
	needToFree := neededSpace - freeSpace

	minSize := usedSpace // Start with root size as upper bound
	for _, size := range dirSizes {
		if size >= needToFree && size < minSize {
			minSize = size
		}
	}
	return minSize
}

func main() {
	exePath, _ := os.Executable()
	exeDir := filepath.Dir(exePath)
	inputFile := filepath.Join(exeDir, "..", "input.txt")

	// Try relative path first (for go run)
	if _, err := os.Stat(inputFile); os.IsNotExist(err) {
		inputFile = "../input.txt"
	}

	file, err := os.Open(inputFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	dirSizes := parseFilesystem(lines)

	fmt.Println("Part 1:", part1(dirSizes))
	fmt.Println("Part 2:", part2(dirSizes))
}
