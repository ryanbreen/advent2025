#!/usr/bin/env ruby

require 'set'

def parse_input(filename)
  # Parse the network connections into an adjacency hash of sets
  graph = Hash.new { |h, k| h[k] = Set.new }

  File.readlines(filename).each do |line|
    a, b = line.strip.split('-')
    graph[a].add(b)
    graph[b].add(a)
  end

  graph
end

def find_triangles(graph)
  # Find all triangles (sets of 3 interconnected nodes)
  triangles = Set.new

  graph.each do |a, neighbors_a|
    neighbors_a.each do |b|
      next unless a < b  # Only process each edge once

      # Find common neighbors
      common = graph[a] & graph[b]
      common.each do |c|
        # Use sorted array as set identifier to avoid duplicates
        tri = [a, b, c].sort
        triangles.add(tri)
      end
    end
  end

  triangles
end

def part1(graph)
  # Count triangles containing at least one node starting with 't'
  triangles = find_triangles(graph)

  triangles.count do |tri|
    tri.any? { |node| node.start_with?('t') }
  end
end

def bron_kerbosch(graph, r, p, x, cliques)
  # Bron-Kerbosch algorithm to find all maximal cliques
  if p.empty? && x.empty?
    cliques << r.dup
    return
  end

  # Use pivot to reduce branching
  pivot = (p | x).max_by { |v| (graph[v] & p).size }

  # Iterate over candidates not connected to pivot
  (p - graph[pivot]).each do |v|
    bron_kerbosch(
      graph,
      r | Set[v],
      p & graph[v],
      x & graph[v],
      cliques
    )
    p = p - Set[v]
    x = x | Set[v]
  end
end

def part2(graph)
  # Find the largest clique (fully connected subgraph)
  cliques = []
  all_nodes = Set.new(graph.keys)

  bron_kerbosch(graph, Set.new, all_nodes, Set.new, cliques)

  # Find the largest clique
  largest = cliques.max_by(&:size)

  # Return sorted, comma-joined password
  largest.sort.join(',')
end

def main
  graph = parse_input('../input.txt')

  puts "Part 1: #{part1(graph)}"
  puts "Part 2: #{part2(graph)}"
end

main if __FILE__ == $PROGRAM_NAME
