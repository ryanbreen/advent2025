use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;

fn parse_input(text: &str) -> (HashMap<String, i32>, HashMap<String, Vec<String>>) {
    let mut valves = HashMap::new();
    let mut tunnels = HashMap::new();

    for line in text.trim().lines() {
        // Parse: "Valve XX has flow rate=N; tunnels lead to valves A, B, C"
        let parts: Vec<&str> = line.split_whitespace().collect();
        let name = parts[1].to_string();
        let rate: i32 = parts[4]
            .trim_end_matches(';')
            .split('=')
            .nth(1)
            .unwrap()
            .parse()
            .unwrap();

        let neighbors: Vec<String> = parts[9..]
            .iter()
            .map(|s| s.trim_end_matches(',').to_string())
            .collect();

        valves.insert(name.clone(), rate);
        tunnels.insert(name, neighbors);
    }

    (valves, tunnels)
}

fn compute_distances(
    valves: &HashMap<String, i32>,
    tunnels: &HashMap<String, Vec<String>>,
) -> HashMap<String, HashMap<String, i32>> {
    // Only care about valves with flow > 0 plus starting valve AA
    let mut relevant: Vec<String> = vec!["AA".to_string()];
    relevant.extend(valves.iter().filter(|(_, &v)| v > 0).map(|(k, _)| k.clone()));

    let mut distances: HashMap<String, HashMap<String, i32>> = HashMap::new();

    for start in &relevant {
        let mut dist_map = HashMap::new();
        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();

        queue.push_back((start.clone(), 0));
        visited.insert(start.clone());

        while let Some((curr, dist)) = queue.pop_front() {
            if relevant.contains(&curr) && &curr != start {
                dist_map.insert(curr.clone(), dist);
            }

            if let Some(neighbors) = tunnels.get(&curr) {
                for neighbor in neighbors {
                    if !visited.contains(neighbor) {
                        visited.insert(neighbor.clone());
                        queue.push_back((neighbor.clone(), dist + 1));
                    }
                }
            }
        }

        distances.insert(start.clone(), dist_map);
    }

    distances
}

fn part1(text: &str) -> i32 {
    let (valves, tunnels) = parse_input(text);
    let distances = compute_distances(&valves, &tunnels);

    // Get valuable valves (flow > 0) and assign indices
    let valuable: Vec<String> = valves
        .iter()
        .filter(|(_, &v)| v > 0)
        .map(|(k, _)| k.clone())
        .collect();

    let n = valuable.len();
    let aa_idx = n; // Use n as index for AA

    // Build distance matrix (including AA)
    let mut dist_matrix = vec![vec![0i32; n + 1]; n + 1];
    for (i, v) in valuable.iter().enumerate() {
        for (j, u) in valuable.iter().enumerate() {
            if i != j {
                dist_matrix[i][j] = *distances.get(v).unwrap().get(u).unwrap();
            }
        }
        dist_matrix[i][aa_idx] = *distances.get(v).unwrap().get("AA").unwrap();
        dist_matrix[aa_idx][i] = *distances.get("AA").unwrap().get(v).unwrap();
    }

    let flow_rates: Vec<i32> = valuable.iter().map(|v| valves[v]).collect();

    // DFS with memoization using bitmask for opened valves
    let mut memo: HashMap<(usize, i32, u64), i32> = HashMap::new();

    fn dfs(
        pos: usize,
        time_left: i32,
        opened: u64,
        n: usize,
        aa_idx: usize,
        dist_matrix: &Vec<Vec<i32>>,
        flow_rates: &Vec<i32>,
        memo: &mut HashMap<(usize, i32, u64), i32>,
    ) -> i32 {
        if time_left <= 0 {
            return 0;
        }

        let key = (pos, time_left, opened);
        if let Some(&cached) = memo.get(&key) {
            return cached;
        }

        let mut best = 0;
        for next in 0..n {
            if opened & (1 << next) != 0 {
                continue;
            }

            let time_cost = dist_matrix[pos][next] + 1;
            if time_cost < time_left {
                let new_time = time_left - time_cost;
                let pressure = flow_rates[next] * new_time;
                let result = pressure
                    + dfs(
                        next,
                        new_time,
                        opened | (1 << next),
                        n,
                        aa_idx,
                        dist_matrix,
                        flow_rates,
                        memo,
                    );
                best = best.max(result);
            }
        }

        memo.insert(key, best);
        best
    }

    dfs(
        aa_idx,
        30,
        0,
        n,
        aa_idx,
        &dist_matrix,
        &flow_rates,
        &mut memo,
    )
}

fn part2(text: &str) -> i32 {
    let (valves, tunnels) = parse_input(text);
    let distances = compute_distances(&valves, &tunnels);

    // Get valuable valves (flow > 0) and assign indices
    let valuable: Vec<String> = valves
        .iter()
        .filter(|(_, &v)| v > 0)
        .map(|(k, _)| k.clone())
        .collect();

    let n = valuable.len();
    let aa_idx = n;

    // Build distance matrix
    let mut dist_matrix = vec![vec![0i32; n + 1]; n + 1];
    for (i, v) in valuable.iter().enumerate() {
        for (j, u) in valuable.iter().enumerate() {
            if i != j {
                dist_matrix[i][j] = *distances.get(v).unwrap().get(u).unwrap();
            }
        }
        dist_matrix[i][aa_idx] = *distances.get(v).unwrap().get("AA").unwrap();
        dist_matrix[aa_idx][i] = *distances.get("AA").unwrap().get(v).unwrap();
    }

    let flow_rates: Vec<i32> = valuable.iter().map(|v| valves[v]).collect();

    // Compute max pressure for each subset of valves
    fn max_pressure_for_subset(
        subset_mask: u64,
        n: usize,
        aa_idx: usize,
        dist_matrix: &Vec<Vec<i32>>,
        flow_rates: &Vec<i32>,
    ) -> i32 {
        let mut memo: HashMap<(usize, i32, u64), i32> = HashMap::new();

        fn dfs(
            pos: usize,
            time_left: i32,
            opened: u64,
            subset_mask: u64,
            n: usize,
            aa_idx: usize,
            dist_matrix: &Vec<Vec<i32>>,
            flow_rates: &Vec<i32>,
            memo: &mut HashMap<(usize, i32, u64), i32>,
        ) -> i32 {
            if time_left <= 0 {
                return 0;
            }

            let key = (pos, time_left, opened);
            if let Some(&cached) = memo.get(&key) {
                return cached;
            }

            let mut best = 0;
            for next in 0..n {
                // Only consider valves in the subset that aren't opened
                if subset_mask & (1 << next) == 0 || opened & (1 << next) != 0 {
                    continue;
                }

                let time_cost = dist_matrix[pos][next] + 1;
                if time_cost < time_left {
                    let new_time = time_left - time_cost;
                    let pressure = flow_rates[next] * new_time;
                    let result = pressure
                        + dfs(
                            next,
                            new_time,
                            opened | (1 << next),
                            subset_mask,
                            n,
                            aa_idx,
                            dist_matrix,
                            flow_rates,
                            memo,
                        );
                    best = best.max(result);
                }
            }

            memo.insert(key, best);
            best
        }

        dfs(
            aa_idx,
            26,
            0,
            subset_mask,
            n,
            aa_idx,
            dist_matrix,
            flow_rates,
            &mut memo,
        )
    }

    // Compute max scores for all subsets
    let total_subsets = 1u64 << n;
    let mut max_scores: Vec<i32> = vec![0; total_subsets as usize];

    for mask in 0..total_subsets {
        max_scores[mask as usize] =
            max_pressure_for_subset(mask, n, aa_idx, &dist_matrix, &flow_rates);
    }

    // Find best partition where you and elephant open disjoint sets
    let mut best = 0;
    let full_mask = total_subsets - 1;
    for mask in 0..total_subsets {
        let complement = full_mask ^ mask;
        if mask <= complement {
            let score = max_scores[mask as usize] + max_scores[complement as usize];
            best = best.max(score);
        }
    }

    best
}

fn main() {
    let input_path = std::env::current_dir()
        .unwrap()
        .join("../input.txt");
    let text = fs::read_to_string(input_path).expect("Failed to read input file");

    println!("Part 1: {}", part1(&text));
    println!("Part 2: {}", part2(&text));
}
