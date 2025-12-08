use std::fs;

#[derive(Debug, Clone)]
struct Point {
    x: i32,
    y: i32,
    z: i32,
}

struct UnionFind {
    parent: Vec<usize>,
    rank: Vec<usize>,
    size: Vec<usize>,
}

impl UnionFind {
    fn new(n: usize) -> Self {
        UnionFind {
            parent: (0..n).collect(),
            rank: vec![0; n],
            size: vec![1; n],
        }
    }

    fn find(&mut self, x: usize) -> usize {
        if self.parent[x] != x {
            self.parent[x] = self.find(self.parent[x]); // Path compression
        }
        self.parent[x]
    }

    fn union(&mut self, x: usize, y: usize) -> bool {
        let mut px = self.find(x);
        let mut py = self.find(y);

        if px == py {
            return false; // Already in same set
        }

        // Union by rank
        if self.rank[px] < self.rank[py] {
            std::mem::swap(&mut px, &mut py);
        }

        self.parent[py] = px;
        self.size[px] += self.size[py];

        if self.rank[px] == self.rank[py] {
            self.rank[px] += 1;
        }

        true
    }

    fn get_component_sizes(&self) -> Vec<usize> {
        let mut sizes = Vec::new();
        for i in 0..self.parent.len() {
            if self.parent[i] == i {
                sizes.push(self.size[i]);
            }
        }
        sizes
    }
}

fn parse_input(filename: &str) -> Vec<Point> {
    let content = fs::read_to_string(filename).expect("Failed to read input file");
    content
        .lines()
        .filter_map(|line| {
            let line = line.trim();
            if line.is_empty() {
                return None;
            }
            let parts: Vec<&str> = line.split(',').collect();
            if parts.len() == 3 {
                Some(Point {
                    x: parts[0].parse().unwrap(),
                    y: parts[1].parse().unwrap(),
                    z: parts[2].parse().unwrap(),
                })
            } else {
                None
            }
        })
        .collect()
}

fn euclidean_distance_sq(p1: &Point, p2: &Point) -> i64 {
    let dx = (p1.x - p2.x) as i64;
    let dy = (p1.y - p2.y) as i64;
    let dz = (p1.z - p2.z) as i64;
    dx * dx + dy * dy + dz * dz
}

fn part1(points: &[Point], num_connections: usize) -> usize {
    let n = points.len();

    // Generate all pairs with distances
    let mut pairs: Vec<(i64, usize, usize)> = Vec::new();
    for i in 0..n {
        for j in (i + 1)..n {
            let dist_sq = euclidean_distance_sq(&points[i], &points[j]);
            pairs.push((dist_sq, i, j));
        }
    }

    // Sort by distance
    pairs.sort_by_key(|&(dist, _, _)| dist);

    // Union-Find to connect closest pairs
    let mut uf = UnionFind::new(n);
    let mut connections = 0;

    for (_, i, j) in pairs.iter() {
        uf.union(*i, *j);
        connections += 1;
        if connections == num_connections {
            break;
        }
    }

    // Get component sizes and find the 3 largest
    let mut sizes = uf.get_component_sizes();
    sizes.sort_by(|a, b| b.cmp(a)); // Sort in descending order

    // Multiply the 3 largest
    sizes[0] * sizes[1] * sizes[2]
}

fn part2(points: &[Point]) -> i64 {
    let n = points.len();

    // Generate all pairs with distances
    let mut pairs: Vec<(i64, usize, usize)> = Vec::new();
    for i in 0..n {
        for j in (i + 1)..n {
            let dist_sq = euclidean_distance_sq(&points[i], &points[j]);
            pairs.push((dist_sq, i, j));
        }
    }

    // Sort by distance
    pairs.sort_by_key(|&(dist, _, _)| dist);

    // Union-Find to connect until all in one circuit
    let mut uf = UnionFind::new(n);
    let mut num_components = n;

    for (_, i, j) in pairs.iter() {
        if uf.union(*i, *j) {
            // Actually merged two components
            num_components -= 1;
            if num_components == 1 {
                // This was the last connection - all in one circuit now
                return (points[*i].x as i64) * (points[*j].x as i64);
            }
        }
    }

    0
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let input_file = if args.len() > 1 {
        &args[1]
    } else {
        "../input.txt"
    };

    let points = parse_input(input_file);

    println!("Part 1: {}", part1(&points, 1000));
    println!("Part 2: {}", part2(&points));
}
