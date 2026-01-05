package main

import (
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"sort"
	"strings"
)

// Graph represents an adjacency list
type Graph map[string]map[string]bool

// Edge represents an undirected edge (sorted)
type Edge struct {
	a, b string
}

func newEdge(a, b string) Edge {
	if a > b {
		a, b = b, a
	}
	return Edge{a, b}
}

// parseInput reads the input file and builds the graph
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

		parts := strings.Split(line, ": ")
		left := parts[0]
		neighbors := strings.Fields(parts[1])

		if graph[left] == nil {
			graph[left] = make(map[string]bool)
		}

		for _, neighbor := range neighbors {
			graph[left][neighbor] = true
			if graph[neighbor] == nil {
				graph[neighbor] = make(map[string]bool)
			}
			graph[neighbor][left] = true
		}
	}

	return graph, scanner.Err()
}

// bfsComponentSize computes the size of the component starting from 'start',
// excluding the given edges
func bfsComponentSize(graph Graph, start string, excludedEdges map[Edge]bool) int {
	visited := make(map[string]bool)
	visited[start] = true
	queue := []string{start}

	for len(queue) > 0 {
		node := queue[0]
		queue = queue[1:]

		for neighbor := range graph[node] {
			edge := newEdge(node, neighbor)
			if !visited[neighbor] && !excludedEdges[edge] {
				visited[neighbor] = true
				queue = append(queue, neighbor)
			}
		}
	}

	return len(visited)
}

// computeEdgeBetweenness calculates approximate edge betweenness centrality
func computeEdgeBetweenness(graph Graph, sampleNodes int) map[Edge]float64 {
	edgeCount := make(map[Edge]float64)

	// Get all nodes
	nodes := make([]string, 0, len(graph))
	for node := range graph {
		nodes = append(nodes, node)
	}

	// Sample nodes if requested
	if sampleNodes > 0 && len(nodes) > sampleNodes {
		rand.Seed(42)
		rand.Shuffle(len(nodes), func(i, j int) {
			nodes[i], nodes[j] = nodes[j], nodes[i]
		})
		nodes = nodes[:sampleNodes]
	}

	for _, source := range nodes {
		// BFS to find shortest paths
		dist := make(map[string]int)
		pred := make(map[string][]string)
		dist[source] = 0
		queue := []string{source}

		for len(queue) > 0 {
			node := queue[0]
			queue = queue[1:]

			for neighbor := range graph[node] {
				if _, ok := dist[neighbor]; !ok {
					dist[neighbor] = dist[node] + 1
					pred[neighbor] = []string{node}
					queue = append(queue, neighbor)
				} else if dist[neighbor] == dist[node]+1 {
					pred[neighbor] = append(pred[neighbor], node)
				}
			}
		}

		// Count number of shortest paths to each node
		numPaths := make(map[string]float64)
		numPaths[source] = 1.0

		// Process nodes in order of distance from source
		sortedNodes := make([]string, 0, len(dist))
		for node := range dist {
			sortedNodes = append(sortedNodes, node)
		}
		sort.Slice(sortedNodes, func(i, j int) bool {
			return dist[sortedNodes[i]] < dist[sortedNodes[j]]
		})

		for _, node := range sortedNodes {
			for _, p := range pred[node] {
				numPaths[node] += numPaths[p]
			}
		}

		// Accumulate edge betweenness (reverse order)
		dependency := make(map[string]float64)

		for i := len(sortedNodes) - 1; i >= 0; i-- {
			node := sortedNodes[i]
			for _, p := range pred[node] {
				edge := newEdge(p, node)
				frac := numPaths[p] / numPaths[node]
				contrib := frac * (1 + dependency[node])
				edgeCount[edge] += contrib
				dependency[p] += contrib
			}
		}
	}

	return edgeCount
}

// findCutEdges finds the 3 edges to cut and returns the product of component sizes
func findCutEdges(graph Graph) int {
	// Compute edge betweenness with sampling
	edgeBetweenness := computeEdgeBetweenness(graph, 100)

	// Sort edges by betweenness (highest first)
	type edgeScore struct {
		edge  Edge
		score float64
	}
	var scores []edgeScore
	for edge, score := range edgeBetweenness {
		scores = append(scores, edgeScore{edge, score})
	}
	sort.Slice(scores, func(i, j int) bool {
		return scores[i].score > scores[j].score
	})

	totalNodes := len(graph)

	// Try combinations of top candidate edges
	topEdges := make([]Edge, 0, 20)
	for i := 0; i < 20 && i < len(scores); i++ {
		topEdges = append(topEdges, scores[i].edge)
	}

	// Try all combinations of 3 edges from the top candidates
	for i := 0; i < len(topEdges); i++ {
		for j := i + 1; j < len(topEdges); j++ {
			for k := j + 1; k < len(topEdges); k++ {
				excluded := make(map[Edge]bool)
				excluded[topEdges[i]] = true
				excluded[topEdges[j]] = true
				excluded[topEdges[k]] = true

				// Pick any node to start from
				var start string
				for node := range graph {
					start = node
					break
				}

				size1 := bfsComponentSize(graph, start, excluded)

				if size1 < totalNodes {
					// Graph is disconnected!
					size2 := totalNodes - size1
					return size1 * size2
				}
			}
		}
	}

	return 0
}

func part1(filename string) (int, error) {
	graph, err := parseInput(filename)
	if err != nil {
		return 0, err
	}
	return findCutEdges(graph), nil
}

func part2() string {
	return "Push the big red button!"
}

func main() {
	inputFile := "../input.txt"

	result1, err := part1(inputFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", result1)
	fmt.Printf("Part 2: %s\n", part2())
}
