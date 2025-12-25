package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)

type Graph map[string]map[string]bool

// parseInput reads the network connections and builds an adjacency map
func parseInput(filename string) (Graph, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	graph := make(Graph)
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		parts := strings.Split(line, "-")
		if len(parts) != 2 {
			continue
		}
		a, b := parts[0], parts[1]

		// Add bidirectional edges
		if graph[a] == nil {
			graph[a] = make(map[string]bool)
		}
		if graph[b] == nil {
			graph[b] = make(map[string]bool)
		}
		graph[a][b] = true
		graph[b][a] = true
	}

	return graph, scanner.Err()
}

// findTriangles finds all sets of 3 interconnected nodes
func findTriangles(graph Graph) [][]string {
	trianglesMap := make(map[string]bool)

	for a := range graph {
		for b := range graph[a] {
			if a < b { // Only process each edge once
				// Find common neighbors of a and b
				for c := range graph[a] {
					if graph[b][c] {
						// a, b, c form a triangle
						tri := []string{a, b, c}
						sort.Strings(tri)
						key := strings.Join(tri, ",")
						trianglesMap[key] = true
					}
				}
			}
		}
	}

	// Convert map to slice
	triangles := make([][]string, 0, len(trianglesMap))
	for key := range trianglesMap {
		triangles = append(triangles, strings.Split(key, ","))
	}

	return triangles
}

// part1 counts triangles containing at least one node starting with 't'
func part1(graph Graph) int {
	triangles := findTriangles(graph)
	count := 0

	for _, tri := range triangles {
		for _, node := range tri {
			if strings.HasPrefix(node, "t") {
				count++
				break
			}
		}
	}

	return count
}

// setIntersection returns the intersection of two sets
func setIntersection(a, b map[string]bool) map[string]bool {
	result := make(map[string]bool)
	for k := range a {
		if b[k] {
			result[k] = true
		}
	}
	return result
}

// setUnion returns the union of two sets
func setUnion(a, b map[string]bool) map[string]bool {
	result := make(map[string]bool)
	for k := range a {
		result[k] = true
	}
	for k := range b {
		result[k] = true
	}
	return result
}

// setDifference returns a - b
func setDifference(a, b map[string]bool) map[string]bool {
	result := make(map[string]bool)
	for k := range a {
		if !b[k] {
			result[k] = true
		}
	}
	return result
}

// copySet creates a copy of a set
func copySet(s map[string]bool) map[string]bool {
	result := make(map[string]bool)
	for k := range s {
		result[k] = true
	}
	return result
}

// bronKerbosch implements the Bron-Kerbosch algorithm with pivoting
func bronKerbosch(graph Graph, r, p, x map[string]bool, cliques *[]map[string]bool) {
	if len(p) == 0 && len(x) == 0 {
		// Found a maximal clique
		clique := copySet(r)
		*cliques = append(*cliques, clique)
		return
	}

	// Choose pivot from P ∪ X with maximum degree in P
	pivot := ""
	maxDegree := -1
	pUnionX := setUnion(p, x)
	for v := range pUnionX {
		degree := len(setIntersection(graph[v], p))
		if degree > maxDegree {
			maxDegree = degree
			pivot = v
		}
	}

	// Iterate over P \ N(pivot)
	pivotNeighbors := make(map[string]bool)
	if pivot != "" {
		pivotNeighbors = graph[pivot]
	}

	candidates := setDifference(p, pivotNeighbors)
	for v := range candidates {
		newR := copySet(r)
		newR[v] = true

		newP := setIntersection(p, graph[v])
		newX := setIntersection(x, graph[v])

		bronKerbosch(graph, newR, newP, newX, cliques)

		delete(p, v)
		x[v] = true
	}
}

// part2 finds the largest clique and returns the password
func part2(graph Graph) string {
	cliques := []map[string]bool{}

	// Initialize P with all nodes
	p := make(map[string]bool)
	for node := range graph {
		p[node] = true
	}

	r := make(map[string]bool)
	x := make(map[string]bool)

	bronKerbosch(graph, r, p, x, &cliques)

	// Find the largest clique
	var largest map[string]bool
	maxSize := 0
	for _, clique := range cliques {
		if len(clique) > maxSize {
			maxSize = len(clique)
			largest = clique
		}
	}

	// Convert to sorted slice
	nodes := make([]string, 0, len(largest))
	for node := range largest {
		nodes = append(nodes, node)
	}
	sort.Strings(nodes)

	return strings.Join(nodes, ",")
}

func main() {
	graph, err := parseInput("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Part 1:", part1(graph))
	fmt.Println("Part 2:", part2(graph))
}
