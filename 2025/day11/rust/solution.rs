use std::collections::HashMap;
use std::fs;
use std::io::{self, BufRead};

type Graph = HashMap<String, Vec<String>>;
type Cache = HashMap<String, u64>;

fn parse_input(filename: &str) -> io::Result<Graph> {
    let file = fs::File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut graph = HashMap::new();

    for line in reader.lines() {
        let line = line?;
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let parts: Vec<&str> = line.split(": ").collect();
        let node = parts[0].to_string();
        let neighbors = if parts.len() > 1 {
            parts[1].split_whitespace()
                .map(|s| s.to_string())
                .collect()
        } else {
            Vec::new()
        };

        graph.insert(node, neighbors);
    }

    Ok(graph)
}

fn count_paths_to(graph: &Graph, target: &str, cache: &mut Cache, node: &str) -> u64 {
    if node == target {
        return 1;
    }

    if let Some(&cached) = cache.get(node) {
        return cached;
    }

    let count = if let Some(neighbors) = graph.get(node) {
        neighbors.iter()
            .map(|neighbor| count_paths_to(graph, target, cache, neighbor))
            .sum()
    } else {
        0
    };

    cache.insert(node.to_string(), count);
    count
}

fn part1(graph: &Graph) -> u64 {
    let mut cache = HashMap::new();
    count_paths_to(graph, "out", &mut cache, "you")
}

fn part2(graph: &Graph) -> u64 {
    // Count paths from each node to 'out'
    let mut cache_to_out = HashMap::new();
    let paths_to_out = |node: &str, cache: &mut Cache| -> u64 {
        count_paths_to(graph, "out", cache, node)
    };

    // Count paths from each node to 'dac'
    let mut cache_to_dac = HashMap::new();
    let paths_to_dac = |node: &str, cache: &mut Cache| -> u64 {
        count_paths_to(graph, "dac", cache, node)
    };

    // Count paths from each node to 'fft'
    let mut cache_to_fft = HashMap::new();
    let paths_to_fft = |node: &str, cache: &mut Cache| -> u64 {
        count_paths_to(graph, "fft", cache, node)
    };

    // Paths that visit dac before fft: svr -> dac -> fft -> out
    let svr_to_dac = paths_to_dac("svr", &mut cache_to_dac);
    let dac_to_fft = paths_to_fft("dac", &mut cache_to_fft);
    let fft_to_out = paths_to_out("fft", &mut cache_to_out);
    let dac_before_fft = svr_to_dac * dac_to_fft * fft_to_out;

    // Paths that visit fft before dac: svr -> fft -> dac -> out
    let svr_to_fft = paths_to_fft("svr", &mut cache_to_fft);
    let fft_to_dac = paths_to_dac("fft", &mut cache_to_dac);
    let dac_to_out = paths_to_out("dac", &mut cache_to_out);
    let fft_before_dac = svr_to_fft * fft_to_dac * dac_to_out;

    dac_before_fft + fft_before_dac
}

fn main() -> io::Result<()> {
    let input_file = "../input.txt";
    let graph = parse_input(input_file)?;

    println!("Part 1: {}", part1(&graph));
    println!("Part 2: {}", part2(&graph));

    Ok(())
}
