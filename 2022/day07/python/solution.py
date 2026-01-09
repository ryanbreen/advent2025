#!/usr/bin/env python3
import os
from collections import defaultdict

def parse_filesystem(lines):
    """Parse terminal output and return directory sizes."""
    # Track current path and directory sizes
    path = []
    dir_sizes = defaultdict(int)

    for line in lines:
        if line.startswith('$ cd'):
            target = line[5:]
            if target == '/':
                path = ['/']
            elif target == '..':
                path.pop()
            else:
                path.append(target)
        elif line.startswith('$ ls'):
            continue
        elif line.startswith('dir '):
            continue
        else:
            # It's a file with size
            size = int(line.split()[0])
            # Add size to current directory and all parent directories
            for i in range(len(path)):
                dir_path = '/'.join(path[:i+1]) or '/'
                dir_sizes[dir_path] += size

    return dir_sizes

def part1(dir_sizes):
    """Sum of sizes of directories with total size <= 100000."""
    return sum(size for size in dir_sizes.values() if size <= 100000)

def part2(dir_sizes):
    """Find smallest directory to delete to free enough space."""
    total_space = 70000000
    needed_space = 30000000
    used_space = dir_sizes['/']
    free_space = total_space - used_space
    need_to_free = needed_space - free_space

    # Find smallest directory >= need_to_free
    candidates = [size for size in dir_sizes.values() if size >= need_to_free]
    return min(candidates)

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        lines = f.read().strip().split('\n')

    dir_sizes = parse_filesystem(lines)

    print('Part 1:', part1(dir_sizes))
    print('Part 2:', part2(dir_sizes))

if __name__ == '__main__':
    main()
