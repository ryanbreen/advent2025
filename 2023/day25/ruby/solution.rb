#!/usr/bin/env ruby
# frozen_string_literal: true

# Advent of Code 2023 - Day 25: Snowverload
# Find the minimum cut of 3 edges that divides the graph into two components.
#
# Uses edge betweenness centrality: edges that form the cut between two large
# components will have high betweenness (many shortest paths pass through them).

require 'set'

# Parse the input file into an adjacency list representation
def parse_input(filename)
  graph = Hash.new { |h, k| h[k] = Set.new }
  File.readlines(filename, chomp: true).each do |line|
    next if line.empty?

    left, right = line.split(': ')
    neighbors = right.split
    neighbors.each do |neighbor|
      graph[left].add(neighbor)
      graph[neighbor].add(left)
    end
  end
  graph
end

# BFS to find component size, ignoring excluded edges
def bfs_component_size(graph, start, excluded_edges)
  visited = Set.new([start])
  queue = [start]

  until queue.empty?
    node = queue.shift
    graph[node].each do |neighbor|
      edge = [node, neighbor].sort
      next if visited.include?(neighbor) || excluded_edges.include?(edge)

      visited.add(neighbor)
      queue.push(neighbor)
    end
  end

  visited.size
end

# Compute approximate edge betweenness centrality.
# Higher values indicate edges that many shortest paths pass through.
def compute_edge_betweenness(graph, sample_nodes = nil)
  edge_count = Hash.new(0.0)
  nodes = graph.keys

  # Sample nodes for efficiency
  if sample_nodes && nodes.size > sample_nodes
    nodes = nodes.sample(sample_nodes, random: Random.new(42))
  end

  nodes.each do |source|
    # BFS to find shortest paths
    dist = { source => 0 }
    pred = Hash.new { |h, k| h[k] = [] }  # Predecessors on shortest paths
    queue = [source]

    until queue.empty?
      node = queue.shift
      graph[node].each do |neighbor|
        if !dist.key?(neighbor)
          dist[neighbor] = dist[node] + 1
          pred[neighbor] << node
          queue.push(neighbor)
        elsif dist[neighbor] == dist[node] + 1
          pred[neighbor] << node
        end
      end
    end

    # Backtrack to count edge usage
    # Number of shortest paths to each node
    num_paths = Hash.new(0.0)
    num_paths[source] = 1.0
    dist.keys.sort_by { |n| dist[n] }.each do |node|
      pred[node].each do |p|
        num_paths[node] += num_paths[p]
      end
    end

    # Accumulate edge betweenness (reverse BFS order)
    dependency = Hash.new(0.0)
    dist.keys.sort_by { |n| -dist[n] }.each do |node|
      pred[node].each do |p|
        edge = [p, node].sort
        # Weight by path fraction
        frac = num_paths[p] / num_paths[node]
        contrib = frac * (1 + dependency[node])
        edge_count[edge] += contrib
        dependency[p] += contrib
      end
    end
  end

  edge_count
end

# Find the 3 edges to cut using edge betweenness
def find_cut_edges(graph)
  # Compute edge betweenness with sampling for speed
  edge_betweenness = compute_edge_betweenness(graph, 100)

  # Sort edges by betweenness (highest first)
  sorted_edges = edge_betweenness.sort_by { |_e, v| -v }

  total_nodes = graph.size

  # Try removing top candidate edges
  # We need to find 3 edges that disconnect the graph
  top_edges = sorted_edges.first(20).map { |e, _v| e }

  (0...top_edges.size).each do |i|
    ((i + 1)...top_edges.size).each do |j|
      ((j + 1)...top_edges.size).each do |k|
        excluded = Set.new([top_edges[i], top_edges[j], top_edges[k]])
        start = graph.keys.first
        size1 = bfs_component_size(graph, start, excluded)

        if size1 < total_nodes
          # Graph is disconnected!
          size2 = total_nodes - size1
          return size1 * size2
        end
      end
    end
  end

  nil
end

# Solve Part 1: Find the 3-edge cut and return product of component sizes
def part1(filename)
  graph = parse_input(filename)
  find_cut_edges(graph)
end

# Part 2: Day 25 Part 2 is traditionally unlocked by having 49 stars.
# There's no computation needed - just push the button!
def part2(_filename)
  'Push the big red button!'
end

if __FILE__ == $PROGRAM_NAME
  input_file = File.join(File.dirname(__FILE__), '..', 'input.txt')

  puts "Part 1: #{part1(input_file)}"
  puts "Part 2: #{part2(input_file)}"
end
