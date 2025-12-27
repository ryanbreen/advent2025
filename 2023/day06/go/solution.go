package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

func parseRaces(lines []string) ([]int, []int) {
	timeParts := strings.Fields(strings.Split(lines[0], ":")[1])
	distParts := strings.Fields(strings.Split(lines[1], ":")[1])

	times := make([]int, len(timeParts))
	distances := make([]int, len(distParts))

	// Parse errors ignored: AoC input is guaranteed well-formed
	for i, t := range timeParts {
		times[i], _ = strconv.Atoi(t)
	}
	for i, d := range distParts {
		distances[i], _ = strconv.Atoi(d)
	}

	return times, distances
}

func countWaysToWin(time, record int) int {
	// If we hold the button for t ms, we travel t * (time - t) mm.
	// We need: t * (time - t) > record
	// Solving: -t^2 + time*t - record > 0
	// Roots: t = (time +/- sqrt(time^2 - 4*record)) / 2

	t := float64(time)
	r := float64(record)

	discriminant := t*t - 4*r
	if discriminant <= 0 {
		return 0
	}

	sqrtD := math.Sqrt(discriminant)
	tLow := (t - sqrtD) / 2
	tHigh := (t + sqrtD) / 2

	// We need integer values strictly between the roots
	first := int(math.Floor(tLow)) + 1
	last := int(math.Ceil(tHigh)) - 1

	if last < first {
		return 0
	}
	return last - first + 1
}

func part1(times, distances []int) int {
	result := 1
	for i, t := range times {
		ways := countWaysToWin(t, distances[i])
		result *= ways
	}
	return result
}

func part2(times, distances []int) int {
	// Concatenate all times and distances into single numbers
	var timeStr, distStr strings.Builder
	for _, t := range times {
		timeStr.WriteString(strconv.Itoa(t))
	}
	for _, d := range distances {
		distStr.WriteString(strconv.Itoa(d))
	}

	// Parse errors ignored: concatenated digits are guaranteed valid
	time, _ := strconv.Atoi(timeStr.String())
	dist, _ := strconv.Atoi(distStr.String())

	return countWaysToWin(time, dist)
}

func main() {
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	content := strings.TrimSpace(string(data))
	lines := strings.Split(content, "\n")

	times, distances := parseRaces(lines)

	fmt.Printf("Part 1: %d\n", part1(times, distances))
	fmt.Printf("Part 2: %d\n", part2(times, distances))
}
