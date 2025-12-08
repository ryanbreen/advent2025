#!/usr/bin/env ruby

class UnionFind
  def initialize(n)
    @parent = (0...n).to_a
    @rank = Array.new(n, 0)
    @size = Array.new(n, 1)
  end

  def find(x)
    if @parent[x] != x
      @parent[x] = find(@parent[x])  # Path compression
    end
    @parent[x]
  end

  def union(x, y)
    px = find(x)
    py = find(y)

    return false if px == py  # Already in same set

    # Union by rank
    if @rank[px] < @rank[py]
      px, py = py, px
    end

    @parent[py] = px
    @size[px] += @size[py]

    if @rank[px] == @rank[py]
      @rank[px] += 1
    end

    true
  end

  def component_sizes
    sizes = []
    @parent.each_with_index do |parent, i|
      if parent == i  # Root of a component
        sizes << @size[i]
      end
    end
    sizes
  end
end

def parse_input(filename)
  points = []
  File.readlines(filename).each do |line|
    line = line.strip
    next if line.empty?

    x, y, z = line.split(',').map(&:to_i)
    points << [x, y, z]
  end
  points
end

def euclidean_distance_sq(p1, p2)
  (p1[0] - p2[0])**2 + (p1[1] - p2[1])**2 + (p1[2] - p2[2])**2
end

def part1(points, num_connections = 1000)
  n = points.length

  # Generate all pairs with distances
  pairs = []
  (0...n).each do |i|
    ((i + 1)...n).each do |j|
      dist_sq = euclidean_distance_sq(points[i], points[j])
      pairs << [dist_sq, i, j]
    end
  end

  # Sort by distance
  pairs.sort!

  # Union-Find to connect closest pairs
  uf = UnionFind.new(n)
  connections = 0

  pairs.each do |dist_sq, i, j|
    uf.union(i, j)
    connections += 1
    break if connections == num_connections
  end

  # Get component sizes and find the 3 largest
  sizes = uf.component_sizes.sort.reverse

  # Multiply the 3 largest
  sizes[0] * sizes[1] * sizes[2]
end

def part2(points)
  n = points.length

  # Generate all pairs with distances
  pairs = []
  (0...n).each do |i|
    ((i + 1)...n).each do |j|
      dist_sq = euclidean_distance_sq(points[i], points[j])
      pairs << [dist_sq, i, j]
    end
  end

  # Sort by distance
  pairs.sort!

  # Union-Find to connect until all in one circuit
  uf = UnionFind.new(n)
  num_components = n

  pairs.each do |dist_sq, i, j|
    if uf.union(i, j)  # Actually merged two components
      num_components -= 1
      if num_components == 1
        # This was the last connection - all in one circuit now
        return points[i][0] * points[j][0]  # Product of X coordinates
      end
    end
  end

  0
end

def main
  input_file = ARGV[0] || '../input.txt'
  points = parse_input(input_file)

  puts "Part 1: #{part1(points)}"
  puts "Part 2: #{part2(points)}"
end

main if __FILE__ == $PROGRAM_NAME
