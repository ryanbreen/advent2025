package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
)

// nextSecret generates the next secret number using mix and prune operations
func nextSecret(secret int) int {
	// Step 1: multiply by 64, mix, prune
	secret ^= (secret << 6) // * 64 = << 6
	secret &= 0xFFFFFF      // % 16777216 = & (2^24 - 1)

	// Step 2: divide by 32, mix, prune
	secret ^= (secret >> 5) // / 32 = >> 5
	secret &= 0xFFFFFF

	// Step 3: multiply by 2048, mix, prune
	secret ^= (secret << 11) // * 2048 = << 11
	secret &= 0xFFFFFF

	return secret
}

// generateSecrets generates a sequence of secret numbers
func generateSecrets(initial int, count int) []int {
	secrets := make([]int, count+1)
	secrets[0] = initial
	secret := initial
	for i := 1; i <= count; i++ {
		secret = nextSecret(secret)
		secrets[i] = secret
	}
	return secrets
}

// part1 calculates the sum of the 2000th secret number for each buyer
func part1(initialSecrets []int) int {
	total := 0
	for _, initial := range initialSecrets {
		secret := initial
		for i := 0; i < 2000; i++ {
			secret = nextSecret(secret)
		}
		total += secret
	}
	return total
}

// sequence represents a 4-change sequence
type sequence struct {
	c1, c2, c3, c4 int
}

// part2 finds the best sequence of 4 price changes to maximize bananas
func part2(initialSecrets []int) int {
	// Map from sequence to total bananas
	sequenceTotals := make(map[sequence]int)

	for _, initial := range initialSecrets {
		// Generate 2001 secrets (initial + 2000 new)
		secrets := generateSecrets(initial, 2000)

		// Calculate prices (last digit of each secret)
		prices := make([]int, len(secrets))
		for i, s := range secrets {
			prices[i] = s % 10
		}

		// Calculate changes
		changes := make([]int, len(prices)-1)
		for i := 0; i < len(prices)-1; i++ {
			changes[i] = prices[i+1] - prices[i]
		}

		// Track first occurrence of each 4-change sequence for this buyer
		seen := make(map[sequence]bool)
		for i := 0; i <= len(changes)-4; i++ {
			seq := sequence{
				c1: changes[i],
				c2: changes[i+1],
				c3: changes[i+2],
				c4: changes[i+3],
			}
			if !seen[seq] {
				seen[seq] = true
				// Price we get is after these 4 changes
				sequenceTotals[seq] += prices[i+4]
			}
		}
	}

	// Find maximum total
	maxBananas := 0
	for _, total := range sequenceTotals {
		if total > maxBananas {
			maxBananas = total
		}
	}

	return maxBananas
}

func main() {
	// Read input from ../input.txt
	execPath, err := os.Executable()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error getting executable path: %v\n", err)
		os.Exit(1)
	}
	inputPath := filepath.Join(filepath.Dir(execPath), "..", "input.txt")

	file, err := os.Open(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening input file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var initialSecrets []int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		num, err := strconv.Atoi(line)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error parsing number: %v\n", err)
			os.Exit(1)
		}
		initialSecrets = append(initialSecrets, num)
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(initialSecrets))
	fmt.Printf("Part 2: %d\n", part2(initialSecrets))
}
