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
  memo = Hash.new do |h, node|
    h[node] = if node == 'out'
                1
              elsif !graph.key?(node)
                0
              else
                graph[node].sum { |neighbor| h[neighbor] }
              end
  end

  memo['you']
end

def part2(graph)
  # Count paths from 'svr' to 'out' that visit both 'dac' and 'fft'
  # Since it's a DAG, paths either visit dac before fft, or fft before dac

  # Helper to create a memoized path counter for a specific target
  make_path_counter = lambda do |target|
    Hash.new do |h, node|
      h[node] = if node == target
                  1
                elsif !graph.key?(node)
                  0
                else
                  graph[node].sum { |neighbor| h[neighbor] }
                end
    end
  end

  paths_to_out = make_path_counter.call('out')
  paths_to_dac = make_path_counter.call('dac')
  paths_to_fft = make_path_counter.call('fft')

  # Paths that visit dac before fft: svr -> dac -> fft -> out
  dac_before_fft = paths_to_dac['svr'] * paths_to_fft['dac'] * paths_to_out['fft']

  # Paths that visit fft before dac: svr -> fft -> dac -> out
  fft_before_dac = paths_to_fft['svr'] * paths_to_dac['fft'] * paths_to_out['dac']

  dac_before_fft + fft_before_dac
end

# Main
input_file = ARGV[0] || '../input.txt'
graph = parse_input(input_file)

puts "Part 1: #{part1(graph)}"
puts "Part 2: #{part2(graph)}"
