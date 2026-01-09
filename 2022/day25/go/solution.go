package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
)

// snafuToDecimal converts a SNAFU number string to decimal.
func snafuToDecimal(s string) int64 {
	digitValues := map[rune]int64{
		'2': 2,
		'1': 1,
		'0': 0,
		'-': -1,
		'=': -2,
	}

	var result int64
	for _, char := range s {
		result = result*5 + digitValues[char]
	}
	return result
}

// decimalToSnafu converts a decimal number to SNAFU string.
func decimalToSnafu(n int64) string {
	if n == 0 {
		return "0"
	}

	var digits []byte
	for n != 0 {
		remainder := n % 5
		if remainder <= 2 {
			digits = append(digits, byte('0'+remainder))
			n /= 5
		} else if remainder == 3 {
			digits = append(digits, '=')
			n = n/5 + 1
		} else { // remainder == 4
			digits = append(digits, '-')
			n = n/5 + 1
		}
	}

	// Reverse the digits
	for i, j := 0, len(digits)-1; i < j; i, j = i+1, j-1 {
		digits[i], digits[j] = digits[j], digits[i]
	}

	return string(digits)
}

func part1(lines []string) string {
	var total int64
	for _, line := range lines {
		total += snafuToDecimal(line)
	}
	return decimalToSnafu(total)
}

func main() {
	// Get the directory of the executable
	execPath, err := os.Executable()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error getting executable path: %v\n", err)
		os.Exit(1)
	}
	execDir := filepath.Dir(execPath)
	inputPath := filepath.Join(execDir, "..", "input.txt")

	file, err := os.Open(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening input file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line != "" {
			lines = append(lines, line)
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Part 1:", part1(lines))
	fmt.Println("Part 2: No Part 2 on Day 25!")
}
