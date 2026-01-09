# Day 7: No Space Left On Device

## Problem Summary

The Elves' device is out of disk space and needs a system update. You browse the filesystem using terminal commands (`cd`, `ls`) and need to analyze directory sizes to find what to delete.

**Part 1**: Find all directories with total size at most 100,000 and sum their sizes.

**Part 2**: The filesystem has 70,000,000 bytes total, and the update needs 30,000,000 bytes free. Find the smallest directory to delete that would free enough space.

## Input Format

Terminal output showing `cd` and `ls` commands with their results:
```
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
...
```

## Algorithmic Approach

### Key Insight

Instead of building a full tree and then computing sizes recursively, we can compute directory sizes in a single pass by maintaining a path stack. When we encounter a file, we add its size to ALL directories in the current path (from root to current directory).

### Algorithm

1. **Maintain a path stack** tracking current directory location
2. **Parse commands**:
   - `cd /` → reset path to root
   - `cd ..` → pop from path stack
   - `cd <dir>` → push directory onto path stack
   - `ls` → ignore (output follows)
3. **For each file encountered** (lines starting with a number):
   - Parse the file size
   - Add size to ALL directories in current path (current dir and all ancestors)
4. **Part 1**: Sum all directory sizes ≤ 100,000
5. **Part 2**: Calculate `need_to_free = 30000000 - (70000000 - root_size)`, find smallest directory ≥ `need_to_free`

### Data Structures

- **Path Stack**: Array/list tracking current directory path
- **Directory Sizes**: Hash map from path string to accumulated size

### Time Complexity

- O(n × d) where n = number of lines, d = average path depth
- Each file size is added to O(d) directories

### Space Complexity

- O(m × d) where m = number of unique directories, d = average path length for storage

## Programming Techniques Highlighted

- **Stack-based path tracking**: Classic technique for tree traversal without recursion
- **Single-pass accumulation**: Computing derived values during parsing
- **String parsing**: Handling command-line style input
- **Hash maps**: Efficient lookup and accumulation by path

## Language-Specific Notes

### Fast Implementations (5-6ms)
- **Zig**: Fastest with efficient string handling and hash maps
- **ARM64**: Direct memory manipulation with fixed-size arrays
- **C++**: `unordered_map` with string keys
- **C**: Custom hash table or linear array search
- **Rust**: `HashMap` with efficient string interning

### Scripting Languages (18-36ms)
- **Perl**: Native hash support, excellent string handling
- **Python**: `defaultdict(int)` for clean accumulation
- **Common Lisp**: Hash tables with `equal` test for string keys
- **Bash**: Surprisingly competitive using associative arrays

### Interpreted/VM Languages (45-68ms)
- **Node.js**: `Map` for directory sizes
- **Java**: `HashMap<String, Long>` with merge operations
- **Ruby**: `Hash.new(0)` for default values
- **PHP**: Associative arrays with string keys
- **Go**: `map[string]int` with simple syntax

### Heavy Runtimes (386-2445ms)
- **Clojure**: Immutable data structures add overhead
- **ColdFusion**: String operations and struct handling are costly

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 5.1          | 1.9         |
| ARM64 asm   | 5.4          | 1.9         |
| C++         | 5.9          | 1.9         |
| C           | 6.1          | 1.9         |
| Rust        | 6.1          | 1.9         |
| Perl        | 17.9         | 5.9         |
| Python      | 21.9         | 14.8        |
| Common Lisp | 24.5         | 41.0        |
| Bash        | 35.5         | 7.0         |
| Node.js     | 45.2         | 38.4        |
| Java        | 49.6         | 50.3        |
| Ruby        | 53.8         | 28.0        |
| PHP         | 59.3         | 25.6        |
| Go          | 68.2         | 26.7        |
| Clojure     | 385.9        | 131.3       |
| ColdFusion  | 2,444.6      | 1,212.6     |

## Answers

- Part 1: 1908462
- Part 2: 3979145
