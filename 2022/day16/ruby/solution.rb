#!/usr/bin/env ruby

def parse_input(text)
  valves = {}
  tunnels = {}

  pattern = /Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)/
  text.strip.split("\n").each do |line|
    match = line.match(pattern)
    name = match[1]
    rate = match[2].to_i
    neighbors = match[3].split(', ')
    valves[name] = rate
    tunnels[name] = neighbors
  end

  [valves, tunnels]
end

def compute_distances(valves, tunnels)
  # Only care about valves with flow > 0 plus starting valve AA
  relevant = ['AA'] + valves.keys.select { |v| valves[v] > 0 }
  distances = {}

  relevant.each do |start|
    distances[start] = {}
    queue = [[start, 0]]
    visited = { start => true }

    until queue.empty?
      curr, dist = queue.shift
      if relevant.include?(curr) && curr != start
        distances[start][curr] = dist
      end

      tunnels[curr].each do |neighbor|
        unless visited[neighbor]
          visited[neighbor] = true
          queue.push([neighbor, dist + 1])
        end
      end
    end
  end

  distances
end

def part1(text)
  valves, tunnels = parse_input(text)
  distances = compute_distances(valves, tunnels)

  # Only consider valves with positive flow
  valuable = valves.keys.select { |v| valves[v] > 0 }

  memo = {}

  dfs = lambda do |pos, time_left, opened|
    return 0 if time_left <= 0

    key = [pos, time_left, opened]
    return memo[key] if memo.key?(key)

    best = 0
    valuable.each do |next_valve|
      next if opened.include?(next_valve)

      # Time to move there and open it
      time_cost = distances[pos][next_valve] + 1
      if time_cost < time_left
        new_time = time_left - time_cost
        pressure = valves[next_valve] * new_time
        new_opened = (opened + [next_valve]).sort
        best = [best, pressure + dfs.call(next_valve, new_time, new_opened)].max
      end
    end

    memo[key] = best
    best
  end

  dfs.call('AA', 30, [])
end

def part2(text)
  valves, tunnels = parse_input(text)
  distances = compute_distances(valves, tunnels)

  valuable = valves.keys.select { |v| valves[v] > 0 }
  n = valuable.length

  # Compute max pressure for each subset of valves (using bitmask)
  max_pressure_for_subset = lambda do |mask|
    subset = (0...n).select { |i| (mask & (1 << i)) != 0 }.map { |i| valuable[i] }

    memo = {}

    dfs = lambda do |pos, time_left, opened|
      return 0 if time_left <= 0

      key = [pos, time_left, opened]
      return memo[key] if memo.key?(key)

      best = 0
      subset.each do |next_valve|
        next if opened.include?(next_valve)

        time_cost = distances[pos][next_valve] + 1
        if time_cost < time_left
          new_time = time_left - time_cost
          pressure = valves[next_valve] * new_time
          new_opened = (opened + [next_valve]).sort
          best = [best, pressure + dfs.call(next_valve, new_time, new_opened)].max
        end
      end

      memo[key] = best
      best
    end

    dfs.call('AA', 26, [])
  end

  # Generate all subsets and compute max pressure
  max_scores = {}
  (0...(1 << n)).each do |mask|
    max_scores[mask] = max_pressure_for_subset.call(mask)
  end

  # Find best partition where you and elephant open disjoint sets
  best = 0
  full_mask = (1 << n) - 1
  (0...(1 << n)).each do |mask|
    complement = full_mask ^ mask
    if mask <= complement
      best = [best, max_scores[mask] + max_scores[complement]].max
    end
  end

  best
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  text = File.read(input_file)

  puts "Part 1: #{part1(text)}"
  puts "Part 2: #{part2(text)}"
end

main
