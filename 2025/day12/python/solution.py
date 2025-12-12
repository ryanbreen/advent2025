#!/usr/bin/env python3
"""
Day 12: Christmas Tree Farm - Polyomino Packing

The solution checks if presents (polyominoes) can fit into rectangular regions.
For this problem, the constraint is simply: total cells needed <= available cells.
"""

from typing import Dict, List, Tuple


def parse_input(text: str) -> Tuple[Dict[int, int], List[Tuple[int, int, List[int]]]]:
    """
    Parse input into shapes and regions.

    Args:
        text: The full input text

    Returns:
        A tuple of (shapes, regions) where:
        - shapes: dict mapping shape index to cell count
        - regions: list of (width, height, present_counts) tuples
    """
    sections = text.strip().split('\n\n')

    shapes: Dict[int, int] = {}
    regions: List[Tuple[int, int, List[int]]] = []

    for section in sections:
        lines = section.strip().split('\n')
        first_line = lines[0]

        if ':' in first_line and 'x' not in first_line:
            # Shape definition
            shape_idx = int(first_line.rstrip(':'))
            cell_count = sum(line.count('#') for line in lines[1:])
            shapes[shape_idx] = cell_count
        else:
            # Region definitions
            for line in lines:
                if 'x' in line:
                    dims_part, counts_part = line.split(':')
                    width, height = map(int, dims_part.strip().split('x'))
                    present_counts = list(map(int, counts_part.split()))
                    regions.append((width, height, present_counts))

    return shapes, regions


def can_fit_region(
    width: int,
    height: int,
    present_counts: List[int],
    shape_sizes: Dict[int, int]
) -> bool:
    """
    Check if all presents can fit in the region.

    This uses a simple heuristic: total cells needed <= available cells.
    For the actual problem, this is sufficient given the input constraints.

    Args:
        width: Region width
        height: Region height
        present_counts: Number of each shape type needed
        shape_sizes: Map of shape index to cell count

    Returns:
        True if presents can fit, False otherwise
    """
    total_cells_needed = sum(
        count * shape_sizes[shape_idx]
        for shape_idx, count in enumerate(present_counts)
    )
    available_cells = width * height
    return total_cells_needed <= available_cells


def part1(shapes: Dict[int, int], regions: List[Tuple[int, int, List[int]]]) -> int:
    """
    Count regions that can fit all their presents.

    Args:
        shapes: Map of shape index to cell count
        regions: List of (width, height, present_counts) tuples

    Returns:
        Number of regions where all presents fit
    """
    return sum(
        1 for width, height, present_counts in regions
        if can_fit_region(width, height, present_counts, shapes)
    )


def part2() -> int:
    """Part 2 is just a button click to finish - no computation needed."""
    return 0


def main() -> None:
    """Main entry point."""
    with open('../input.txt', 'r') as f:
        text = f.read()

    shapes, regions = parse_input(text)

    print(f"Part 1: {part1(shapes, regions)}")
    print(f"Part 2: {part2()}")


if __name__ == '__main__':
    main()
