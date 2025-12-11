#!/usr/bin/env ruby

def parse_input(filename)
  # Parse input into a graph (adjacency list)
  graph = {}
  File.readlines(filename).each do |line|
    line = line.strip
    next if line.empty?

    parts = line.split(': ')
    node = parts[0]
    neighbors = parts.length > 1 ? parts[1].split(' ') : []
    graph[node] = neighbors
  end
  graph
end

def part1(graph)
  # Count all paths from 'you' to 'out' using memoization
  memo = {}

  count_paths = lambda do |node|
    return memo[node] if memo.key?(node)

    if node == 'out'
      return 1
    end

    if !graph.key?(node)
      return 0
    end

    result = graph[node].sum { |neighbor| count_paths.call(neighbor) }
    memo[node] = result
    result
  end

  count_paths.call('you')
end

def part2(graph)
  # Count paths from 'svr' to 'out' that visit both 'dac' and 'fft'
  # Since it's a DAG, paths either visit dac before fft, or fft before dac

  # Helper to count paths from each node to a target
  count_paths_to_target = lambda do |target|
    memo = {}

    count = lambda do |node|
      return memo[node] if memo.key?(node)

      if node == target
        return 1
      end

      if !graph.key?(node)
        return 0
      end

      result = graph[node].sum { |neighbor| count.call(neighbor) }
      memo[node] = result
      result
    end

    count
  end

  paths_to_out = count_paths_to_target.call('out')
  paths_to_dac = count_paths_to_target.call('dac')
  paths_to_fft = count_paths_to_target.call('fft')

  # Paths that visit dac before fft: svr -> dac -> fft -> out
  dac_before_fft = paths_to_dac.call('svr') * paths_to_fft.call('dac') * paths_to_out.call('fft')

  # Paths that visit fft before dac: svr -> fft -> dac -> out
  fft_before_dac = paths_to_fft.call('svr') * paths_to_dac.call('fft') * paths_to_out.call('dac')

  dac_before_fft + fft_before_dac
end

# Main
input_file = ARGV[0] || '../input.txt'
graph = parse_input(input_file)

puts "Part 1: #{part1(graph)}"
puts "Part 2: #{part2(graph)}"
