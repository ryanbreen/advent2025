use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;

type Graph = HashMap<String, HashSet<String>>;
type Edge = (String, String);

fn parse_input(filename: &str) -> Graph {
    let content = fs::read_to_string(filename).expect("Failed to read input file");
    let mut graph: Graph = HashMap::new();

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let parts: Vec<&str> = line.split(": ").collect();
        let left = parts[0].to_string();
        let neighbors: Vec<&str> = parts[1].split_whitespace().collect();

        for neighbor in neighbors {
            let neighbor_str = neighbor.to_string();
            graph.entry(left.clone())
                .or_insert_with(HashSet::new)
                .insert(neighbor_str.clone());
            graph.entry(neighbor_str)
                .or_insert_with(HashSet::new)
                .insert(left.clone());
        }
    }

    graph
}

fn normalize_edge(a: &str, b: &str) -> Edge {
    if a < b {
        (a.to_string(), b.to_string())
    } else {
        (b.to_string(), a.to_string())
    }
}

fn bfs_component_size(graph: &Graph, start: &str, excluded_edges: &HashSet<Edge>) -> usize {
    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();

    visited.insert(start.to_string());
    queue.push_back(start.to_string());

    while let Some(node) = queue.pop_front() {
        if let Some(neighbors) = graph.get(&node) {
            for neighbor in neighbors {
                let edge = normalize_edge(&node, neighbor);
                if !visited.contains(neighbor) && !excluded_edges.contains(&edge) {
                    visited.insert(neighbor.clone());
                    queue.push_back(neighbor.clone());
                }
            }
        }
    }

    visited.len()
}

fn compute_edge_betweenness(graph: &Graph, sample_size: usize) -> HashMap<Edge, f64> {
    let mut edge_count: HashMap<Edge, f64> = HashMap::new();
    let nodes: Vec<String> = graph.keys().cloned().collect();

    // Sample nodes for efficiency
    let sample_nodes = if nodes.len() > sample_size {
        use std::collections::hash_map::RandomState;
        use std::hash::{BuildHasher, Hash, Hasher};

        // Simple deterministic sampling using hash
        let mut sampled = Vec::new();
        let hasher = RandomState::new();
        for (i, node) in nodes.iter().enumerate() {
            let mut h = hasher.build_hasher();
            node.hash(&mut h);
            if sampled.len() < sample_size || (h.finish() % (i as u64 + 1)) < sample_size as u64 {
                if sampled.len() < sample_size {
                    sampled.push(node.clone());
                } else {
                    let idx = (h.finish() % sample_size as u64) as usize;
                    sampled[idx] = node.clone();
                }
            }
        }
        sampled
    } else {
        nodes.clone()
    };

    for source in &sample_nodes {
        // BFS to find shortest paths
        let mut dist: HashMap<String, usize> = HashMap::new();
        let mut pred: HashMap<String, Vec<String>> = HashMap::new();
        let mut queue = VecDeque::new();

        dist.insert(source.clone(), 0);
        queue.push_back(source.clone());

        while let Some(node) = queue.pop_front() {
            if let Some(neighbors) = graph.get(&node) {
                for neighbor in neighbors {
                    let current_dist = *dist.get(&node).unwrap();
                    if !dist.contains_key(neighbor) {
                        dist.insert(neighbor.clone(), current_dist + 1);
                        pred.entry(neighbor.clone())
                            .or_insert_with(Vec::new)
                            .push(node.clone());
                        queue.push_back(neighbor.clone());
                    } else if *dist.get(neighbor).unwrap() == current_dist + 1 {
                        pred.entry(neighbor.clone())
                            .or_insert_with(Vec::new)
                            .push(node.clone());
                    }
                }
            }
        }

        // Count paths to each node
        let mut num_paths: HashMap<String, f64> = HashMap::new();
        num_paths.insert(source.clone(), 1.0);

        let mut dist_vec: Vec<_> = dist.iter().collect();
        dist_vec.sort_by_key(|(_, d)| *d);

        for (node, _) in &dist_vec {
            if let Some(predecessors) = pred.get(*node) {
                for p in predecessors {
                    let path_count = *num_paths.get(p).unwrap_or(&0.0);
                    *num_paths.entry((*node).clone()).or_insert(0.0) += path_count;
                }
            }
        }

        // Accumulate edge betweenness (reverse order)
        let mut dependency: HashMap<String, f64> = HashMap::new();
        dist_vec.reverse();

        for (node, _) in &dist_vec {
            if let Some(predecessors) = pred.get(*node) {
                for p in predecessors {
                    let edge = normalize_edge(p, node);
                    let node_paths = *num_paths.get(*node).unwrap_or(&1.0);
                    let p_paths = *num_paths.get(p).unwrap_or(&0.0);
                    let frac = if node_paths > 0.0 { p_paths / node_paths } else { 0.0 };
                    let node_dep = *dependency.get(*node).unwrap_or(&0.0);
                    let contrib = frac * (1.0 + node_dep);
                    *edge_count.entry(edge).or_insert(0.0) += contrib;
                    *dependency.entry(p.clone()).or_insert(0.0) += contrib;
                }
            }
        }
    }

    edge_count
}

fn find_cut_edges(graph: &Graph) -> Option<usize> {
    // Compute edge betweenness with sampling
    let edge_betweenness = compute_edge_betweenness(graph, 100);

    // Sort edges by betweenness (highest first)
    let mut sorted_edges: Vec<_> = edge_betweenness.iter().collect();
    sorted_edges.sort_by(|a, b| b.1.partial_cmp(a.1).unwrap());

    let total_nodes = graph.len();

    // Try top 20 candidates
    let top_edges: Vec<Edge> = sorted_edges.iter()
        .take(20)
        .map(|(e, _)| (*e).clone())
        .collect();

    // Try all combinations of 3 edges
    for i in 0..top_edges.len() {
        for j in (i + 1)..top_edges.len() {
            for k in (j + 1)..top_edges.len() {
                let mut excluded = HashSet::new();
                excluded.insert(top_edges[i].clone());
                excluded.insert(top_edges[j].clone());
                excluded.insert(top_edges[k].clone());

                let start = graph.keys().next().unwrap();
                let size1 = bfs_component_size(graph, start, &excluded);

                if size1 < total_nodes {
                    // Graph is disconnected!
                    let size2 = total_nodes - size1;
                    return Some(size1 * size2);
                }
            }
        }
    }

    None
}

fn part1(filename: &str) -> usize {
    let graph = parse_input(filename);
    find_cut_edges(&graph).expect("Could not find valid cut")
}

fn part2() -> &'static str {
    "Push the big red button!"
}

fn main() {
    let input_file = "../input.txt";

    println!("Part 1: {}", part1(input_file));
    println!("Part 2: {}", part2());
}
