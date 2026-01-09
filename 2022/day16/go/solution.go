package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
)

// Valve represents a valve with flow rate
type Valve struct {
	name     string
	flowRate int
	tunnels  []string
}

func parseInput(text string) map[string]Valve {
	valves := make(map[string]Valve)
	pattern := regexp.MustCompile(`Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)`)

	scanner := bufio.NewScanner(strings.NewReader(text))
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		matches := pattern.FindStringSubmatch(line)
		if matches == nil {
			continue
		}
		name := matches[1]
		flowRate, _ := strconv.Atoi(matches[2])
		tunnels := strings.Split(matches[3], ", ")
		valves[name] = Valve{name: name, flowRate: flowRate, tunnels: tunnels}
	}
	return valves
}

func computeDistances(valves map[string]Valve) map[string]map[string]int {
	// Only care about valves with flow > 0 plus starting valve AA
	var relevant []string
	relevant = append(relevant, "AA")
	for name, valve := range valves {
		if valve.flowRate > 0 {
			relevant = append(relevant, name)
		}
	}

	distances := make(map[string]map[string]int)
	relevantSet := make(map[string]bool)
	for _, v := range relevant {
		relevantSet[v] = true
	}

	for _, start := range relevant {
		distances[start] = make(map[string]int)

		// BFS from start
		type queueItem struct {
			node string
			dist int
		}
		queue := []queueItem{{start, 0}}
		visited := make(map[string]bool)
		visited[start] = true

		for len(queue) > 0 {
			curr := queue[0]
			queue = queue[1:]

			if relevantSet[curr.node] && curr.node != start {
				distances[start][curr.node] = curr.dist
			}

			for _, neighbor := range valves[curr.node].tunnels {
				if !visited[neighbor] {
					visited[neighbor] = true
					queue = append(queue, queueItem{neighbor, curr.dist + 1})
				}
			}
		}
	}

	return distances
}

// DFS with memoization for Part 1
func part1(text string) int {
	valves := parseInput(text)
	distances := computeDistances(valves)

	// List of valuable valves (flow > 0)
	var valuable []string
	for name, valve := range valves {
		if valve.flowRate > 0 {
			valuable = append(valuable, name)
		}
	}

	// Create a mapping from valve name to bit index
	valveIndex := make(map[string]int)
	for i, v := range valuable {
		valveIndex[v] = i
	}

	// Memoization: key is (position_index, time_left, opened_bitmask)
	type memoKey struct {
		pos      string
		timeLeft int
		opened   int
	}
	memo := make(map[memoKey]int)

	var dfs func(pos string, timeLeft int, opened int) int
	dfs = func(pos string, timeLeft int, opened int) int {
		if timeLeft <= 0 {
			return 0
		}

		key := memoKey{pos, timeLeft, opened}
		if val, ok := memo[key]; ok {
			return val
		}

		best := 0
		for _, nextValve := range valuable {
			idx := valveIndex[nextValve]
			if opened&(1<<idx) != 0 {
				continue // Already opened
			}

			timeCost := distances[pos][nextValve] + 1
			if timeCost < timeLeft {
				newTime := timeLeft - timeCost
				pressure := valves[nextValve].flowRate * newTime
				result := pressure + dfs(nextValve, newTime, opened|(1<<idx))
				if result > best {
					best = result
				}
			}
		}

		memo[key] = best
		return best
	}

	return dfs("AA", 30, 0)
}

// Part 2: Find max pressure with elephant helper (26 minutes each)
func part2(text string) int {
	valves := parseInput(text)
	distances := computeDistances(valves)

	// List of valuable valves (flow > 0)
	var valuable []string
	for name, valve := range valves {
		if valve.flowRate > 0 {
			valuable = append(valuable, name)
		}
	}
	n := len(valuable)

	// Create a mapping from valve name to bit index
	valveIndex := make(map[string]int)
	for i, v := range valuable {
		valveIndex[v] = i
	}

	// Compute max pressure for each subset of valves
	maxPressureForSubset := func(subset int) int {
		type memoKey struct {
			pos      string
			timeLeft int
			opened   int
		}
		memo := make(map[memoKey]int)

		var dfs func(pos string, timeLeft int, opened int) int
		dfs = func(pos string, timeLeft int, opened int) int {
			if timeLeft <= 0 {
				return 0
			}

			key := memoKey{pos, timeLeft, opened}
			if val, ok := memo[key]; ok {
				return val
			}

			best := 0
			for _, nextValve := range valuable {
				idx := valveIndex[nextValve]
				// Check if this valve is in our allowed subset and not yet opened
				if subset&(1<<idx) == 0 {
					continue // Not in our subset
				}
				if opened&(1<<idx) != 0 {
					continue // Already opened
				}

				timeCost := distances["AA"][nextValve]
				if pos != "AA" {
					timeCost = distances[pos][nextValve]
				}
				timeCost++ // +1 for opening

				if timeCost < timeLeft {
					newTime := timeLeft - timeCost
					pressure := valves[nextValve].flowRate * newTime
					result := pressure + dfs(nextValve, newTime, opened|(1<<idx))
					if result > best {
						best = result
					}
				}
			}

			memo[key] = best
			return best
		}

		return dfs("AA", 26, 0)
	}

	// Compute max scores for all subsets
	maxScores := make([]int, 1<<n)
	for mask := 0; mask < (1 << n); mask++ {
		maxScores[mask] = maxPressureForSubset(mask)
	}

	// Find best partition where you and elephant open disjoint sets
	best := 0
	fullMask := (1 << n) - 1
	for mask := 0; mask < (1 << n); mask++ {
		complement := fullMask ^ mask
		if mask <= complement { // Avoid counting same partition twice
			total := maxScores[mask] + maxScores[complement]
			if total > best {
				best = total
			}
		}
	}

	return best
}

func main() {
	// Get the directory of the executable
	execPath, _ := os.Executable()
	dir := filepath.Dir(execPath)
	inputFile := filepath.Join(dir, "..", "input.txt")

	// Try to read from the expected location
	content, err := os.ReadFile(inputFile)
	if err != nil {
		// Fall back to relative path from current working directory
		inputFile = filepath.Join(".", "..", "input.txt")
		content, err = os.ReadFile(inputFile)
		if err != nil {
			// Try from go subdirectory
			inputFile = filepath.Join("input.txt")
			content, err = os.ReadFile("../input.txt")
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error reading input file: %v\n", err)
				os.Exit(1)
			}
		}
	}

	text := string(content)

	fmt.Println("Part 1:", part1(text))
	fmt.Println("Part 2:", part2(text))
}
