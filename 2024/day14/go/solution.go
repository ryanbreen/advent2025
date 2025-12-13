package main

import (
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

const (
	WIDTH  = 101
	HEIGHT = 103
)

type Robot struct {
	px, py, vx, vy int
}

func parseRobots(text string) []Robot {
	var robots []Robot
	re := regexp.MustCompile(`p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)`)

	lines := strings.Split(strings.TrimSpace(text), "\n")
	for _, line := range lines {
		matches := re.FindStringSubmatch(line)
		if matches != nil {
			px, _ := strconv.Atoi(matches[1])
			py, _ := strconv.Atoi(matches[2])
			vx, _ := strconv.Atoi(matches[3])
			vy, _ := strconv.Atoi(matches[4])
			robots = append(robots, Robot{px, py, vx, vy})
		}
	}

	return robots
}

// mod handles negative modulo correctly
func mod(a, b int) int {
	result := a % b
	if result < 0 {
		result += b
	}
	return result
}

type Position struct {
	x, y int
}

func simulate(robots []Robot, seconds int) []Position {
	positions := make([]Position, len(robots))

	for i, robot := range robots {
		// Position after 'seconds' time, with wrapping
		newX := mod(robot.px+robot.vx*seconds, WIDTH)
		newY := mod(robot.py+robot.vy*seconds, HEIGHT)
		positions[i] = Position{newX, newY}
	}

	return positions
}

func countQuadrants(positions []Position) (int, int, int, int) {
	midX := WIDTH / 2   // 50
	midY := HEIGHT / 2  // 51

	q1, q2, q3, q4 := 0, 0, 0, 0

	for _, pos := range positions {
		if pos.x == midX || pos.y == midY {
			continue // Skip robots on middle lines
		}

		if pos.x < midX && pos.y < midY {
			q1++ // Top-left
		} else if pos.x > midX && pos.y < midY {
			q2++ // Top-right
		} else if pos.x < midX && pos.y > midY {
			q3++ // Bottom-left
		} else {
			q4++ // Bottom-right
		}
	}

	return q1, q2, q3, q4
}

func part1(robots []Robot) int {
	positions := simulate(robots, 100)
	q1, q2, q3, q4 := countQuadrants(positions)
	return q1 * q2 * q3 * q4
}

func part2(robots []Robot) int {
	// The Christmas tree appears when robots cluster together
	// Look for a frame with a long horizontal line of robots (tree base/border)
	for seconds := 1; seconds <= WIDTH*HEIGHT; seconds++ {
		positions := simulate(robots, seconds)

		// Create a set of positions
		posSet := make(map[Position]bool)
		for _, pos := range positions {
			posSet[pos] = true
		}

		// Look for a horizontal line of at least 20 consecutive robots
		for y := 0; y < HEIGHT; y++ {
			maxConsecutive := 0
			consecutive := 0

			for x := 0; x < WIDTH; x++ {
				if posSet[Position{x, y}] {
					consecutive++
					if consecutive > maxConsecutive {
						maxConsecutive = consecutive
					}
				} else {
					consecutive = 0
				}
			}

			if maxConsecutive >= 20 {
				return seconds
			}
		}
	}

	return -1
}

func main() {
	input, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	robots := parseRobots(string(input))

	fmt.Printf("Part 1: %d\n", part1(robots))
	fmt.Printf("Part 2: %d\n", part2(robots))
}
