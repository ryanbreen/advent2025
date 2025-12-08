package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Point struct {
	x, y, z int
}

type UnionFind struct {
	parent []int
	rank   []int
	size   []int
}

func NewUnionFind(n int) *UnionFind {
	uf := &UnionFind{
		parent: make([]int, n),
		rank:   make([]int, n),
		size:   make([]int, n),
	}
	for i := 0; i < n; i++ {
		uf.parent[i] = i
		uf.size[i] = 1
	}
	return uf
}

func (uf *UnionFind) Find(x int) int {
	if uf.parent[x] != x {
		uf.parent[x] = uf.Find(uf.parent[x]) // Path compression
	}
	return uf.parent[x]
}

func (uf *UnionFind) Union(x, y int) bool {
	px, py := uf.Find(x), uf.Find(y)
	if px == py {
		return false // Already in same set
	}

	// Union by rank
	if uf.rank[px] < uf.rank[py] {
		px, py = py, px
	}
	uf.parent[py] = px
	uf.size[px] += uf.size[py]
	if uf.rank[px] == uf.rank[py] {
		uf.rank[px]++
	}
	return true
}

func (uf *UnionFind) GetComponentSizes() []int {
	sizes := []int{}
	for i := 0; i < len(uf.parent); i++ {
		if uf.parent[i] == i { // Root of a component
			sizes = append(sizes, uf.size[i])
		}
	}
	return sizes
}

type Pair struct {
	distSq int
	i, j   int
}

func euclideanDistanceSq(p1, p2 Point) int {
	dx := p1.x - p2.x
	dy := p1.y - p2.y
	dz := p1.z - p2.z
	return dx*dx + dy*dy + dz*dz
}

func parseInput(filename string) ([]Point, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	points := []Point{}
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		parts := strings.Split(line, ",")
		if len(parts) != 3 {
			continue
		}
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		z, _ := strconv.Atoi(parts[2])
		points = append(points, Point{x, y, z})
	}
	return points, scanner.Err()
}

func part1(points []Point, numConnections int) int {
	n := len(points)

	// Generate all pairs with distances
	pairs := []Pair{}
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			distSq := euclideanDistanceSq(points[i], points[j])
			pairs = append(pairs, Pair{distSq, i, j})
		}
	}

	// Sort by distance
	sort.Slice(pairs, func(i, j int) bool {
		return pairs[i].distSq < pairs[j].distSq
	})

	// Union-Find to connect closest pairs
	uf := NewUnionFind(n)
	connections := 0
	for _, pair := range pairs {
		uf.Union(pair.i, pair.j)
		connections++
		if connections == numConnections {
			break
		}
	}

	// Get component sizes and find the 3 largest
	sizes := uf.GetComponentSizes()
	sort.Slice(sizes, func(i, j int) bool {
		return sizes[i] > sizes[j]
	})

	// Multiply the 3 largest
	return sizes[0] * sizes[1] * sizes[2]
}

func part2(points []Point) int {
	n := len(points)

	// Generate all pairs with distances
	pairs := []Pair{}
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			distSq := euclideanDistanceSq(points[i], points[j])
			pairs = append(pairs, Pair{distSq, i, j})
		}
	}

	// Sort by distance
	sort.Slice(pairs, func(i, j int) bool {
		return pairs[i].distSq < pairs[j].distSq
	})

	// Union-Find to connect until all in one circuit
	uf := NewUnionFind(n)
	numComponents := n

	for _, pair := range pairs {
		if uf.Union(pair.i, pair.j) { // Actually merged two components
			numComponents--
			if numComponents == 1 {
				// This was the last connection - all in one circuit now
				return points[pair.i].x * points[pair.j].x // Product of X coordinates
			}
		}
	}

	return 0
}

func main() {
	inputFile := "../input.txt"
	if len(os.Args) > 1 {
		inputFile = os.Args[1]
	}

	points, err := parseInput(inputFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(points, 1000))
	fmt.Printf("Part 2: %d\n", part2(points))
}
