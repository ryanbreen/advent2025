use std::collections::{HashMap, HashSet};
use std::fs;

fn parse_input(filename: &str) -> HashMap<String, HashSet<String>> {
    let content = fs::read_to_string(filename).expect("Failed to read input file");
    let mut graph: HashMap<String, HashSet<String>> = HashMap::new();

    for line in content.lines() {
        let parts: Vec<&str> = line.trim().split('-').collect();
        if parts.len() == 2 {
            let a = parts[0].to_string();
            let b = parts[1].to_string();

            graph.entry(a.clone()).or_insert_with(HashSet::new).insert(b.clone());
            graph.entry(b).or_insert_with(HashSet::new).insert(a);
        }
    }

    graph
}

fn find_triangles(graph: &HashMap<String, HashSet<String>>) -> HashSet<Vec<String>> {
    let mut triangles = HashSet::new();

    for a in graph.keys() {
        if let Some(neighbors_a) = graph.get(a) {
            for b in neighbors_a {
                if a < b {  // Only process each edge once
                    // Find common neighbors
                    if let Some(neighbors_b) = graph.get(b) {
                        let common: HashSet<_> = neighbors_a.intersection(neighbors_b).collect();
                        for c in common {
                            // Create sorted triangle to avoid duplicates
                            let mut tri = vec![a.clone(), b.clone(), (*c).clone()];
                            tri.sort();
                            triangles.insert(tri);
                        }
                    }
                }
            }
        }
    }

    triangles
}

fn part1(graph: &HashMap<String, HashSet<String>>) -> usize {
    let triangles = find_triangles(graph);

    triangles.iter()
        .filter(|tri| tri.iter().any(|node| node.starts_with('t')))
        .count()
}

fn bron_kerbosch(
    graph: &HashMap<String, HashSet<String>>,
    r: HashSet<String>,
    mut p: HashSet<String>,
    mut x: HashSet<String>,
    cliques: &mut Vec<HashSet<String>>,
) {
    if p.is_empty() && x.is_empty() {
        cliques.push(r);
        return;
    }

    // Use pivot to reduce branching
    let union: HashSet<_> = p.union(&x).cloned().collect();
    let pivot = union.iter()
        .max_by_key(|v| {
            graph.get(*v)
                .map(|neighbors| neighbors.intersection(&p).count())
                .unwrap_or(0)
        })
        .cloned();

    // Get pivot's neighbors
    let pivot_neighbors = if let Some(piv) = pivot {
        graph.get(&piv).cloned().unwrap_or_default()
    } else {
        HashSet::new()
    };

    // Process vertices not in pivot's neighborhood
    let candidates: Vec<_> = p.difference(&pivot_neighbors).cloned().collect();

    for v in candidates {
        let mut new_r = r.clone();
        new_r.insert(v.clone());

        let neighbors = graph.get(&v).cloned().unwrap_or_default();
        let new_p: HashSet<_> = p.intersection(&neighbors).cloned().collect();
        let new_x: HashSet<_> = x.intersection(&neighbors).cloned().collect();

        bron_kerbosch(graph, new_r, new_p, new_x, cliques);

        p.remove(&v);
        x.insert(v);
    }
}

fn part2(graph: &HashMap<String, HashSet<String>>) -> String {
    let mut cliques = Vec::new();
    let all_nodes: HashSet<String> = graph.keys().cloned().collect();

    bron_kerbosch(
        graph,
        HashSet::new(),
        all_nodes,
        HashSet::new(),
        &mut cliques
    );

    // Find the largest clique
    let largest = cliques.iter()
        .max_by_key(|c| c.len())
        .expect("No cliques found");

    // Return sorted, comma-joined password
    let mut nodes: Vec<_> = largest.iter().cloned().collect();
    nodes.sort();
    nodes.join(",")
}

fn main() {
    let graph = parse_input("../input.txt");

    println!("Part 1: {}", part1(&graph));
    println!("Part 2: {}", part2(&graph));
}
