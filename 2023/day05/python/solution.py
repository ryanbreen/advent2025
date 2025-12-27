"""Advent of Code 2023 - Day 5: If You Give A Seed A Fertilizer

This module solves the almanac seed-to-location mapping puzzle. Seeds must be
mapped through a series of transformation maps (seed->soil->fertilizer->etc.)
to find their final location numbers.

Part 1: Find the minimum location for a list of individual seed numbers.
Part 2: Interpret seeds as ranges and find the minimum location efficiently
        using interval arithmetic rather than brute force iteration.
"""

from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True, slots=True)
class MapRange:
    """A single mapping range that transforms source values to destination values.

    Attributes:
        destination_start: The starting value of the destination range.
        source_start: The starting value of the source range.
        length: The number of values in the range.
    """
    destination_start: int
    source_start: int
    length: int

    @property
    def source_end(self) -> int:
        """Return the exclusive end of the source range."""
        return self.source_start + self.length

    @property
    def offset(self) -> int:
        """Return the offset to add when mapping source to destination."""
        return self.destination_start - self.source_start

    def contains(self, value: int) -> bool:
        """Check if a value falls within the source range."""
        return self.source_start <= value < self.source_end

    def transform(self, value: int) -> int:
        """Transform a source value to its destination value.

        Assumes the value is within the source range.
        """
        return value + self.offset


@dataclass(frozen=True, slots=True)
class Interval:
    """A half-open interval [start, end) representing a range of values.

    Attributes:
        start: The inclusive start of the interval.
        end: The exclusive end of the interval.
    """
    start: int
    end: int

    def is_valid(self) -> bool:
        """Check if this interval is non-empty."""
        return self.start < self.end

    def intersect(self, other: 'Interval') -> 'Interval':
        """Return the intersection of two intervals."""
        return Interval(max(self.start, other.start), min(self.end, other.end))


def parse_input(text: str) -> tuple[list[int], list[list[MapRange]]]:
    """Parse the puzzle input into seeds and transformation maps.

    Args:
        text: The raw puzzle input text.

    Returns:
        A tuple containing:
        - A list of seed numbers
        - A list of maps, where each map is a list of MapRange objects
    """
    sections = text.strip().split('\n\n')

    # Parse seeds from the first line
    seeds = list(map(int, sections[0].split(': ')[1].split()))

    # Parse each transformation map
    maps: list[list[MapRange]] = []
    for section in sections[1:]:
        lines = section.strip().split('\n')
        ranges: list[MapRange] = []
        for line in lines[1:]:  # Skip the header line
            destination_start, source_start, length = map(int, line.split())
            ranges.append(MapRange(destination_start, source_start, length))
        maps.append(ranges)

    return seeds, maps


def apply_map(value: int, map_ranges: list[MapRange]) -> int:
    """Apply a transformation map to a single value.

    Searches through the map ranges to find one that contains the value.
    If found, transforms the value; otherwise returns it unchanged.

    Args:
        value: The input value to transform.
        map_ranges: The list of MapRange objects defining the transformation.

    Returns:
        The transformed value, or the original if no range matches.
    """
    for map_range in map_ranges:
        if map_range.contains(value):
            return map_range.transform(value)
    return value


def seed_to_location(seed: int, maps: list[list[MapRange]]) -> int:
    """Convert a seed number through all maps to get its location.

    Applies each transformation map in sequence, passing the result
    of each transformation to the next map.

    Args:
        seed: The initial seed number.
        maps: The list of transformation maps to apply in order.

    Returns:
        The final location number for this seed.
    """
    value = seed
    for map_ranges in maps:
        value = apply_map(value, map_ranges)
    return value


def part1(seeds: list[int], maps: list[list[MapRange]]) -> int:
    """Solve Part 1: Find the minimum location for individual seeds.

    Args:
        seeds: List of individual seed numbers.
        maps: List of transformation maps.

    Returns:
        The lowest location number among all seeds.
    """
    return min(seed_to_location(seed, maps) for seed in seeds)


def apply_map_to_intervals(
    input_intervals: list[Interval],
    map_ranges: list[MapRange]
) -> list[Interval]:
    """Apply a transformation map to a list of intervals.

    This is the key optimization for Part 2. Instead of checking each
    individual seed value, we transform entire intervals at once.

    Each input interval may be split into multiple output intervals:
    - Parts that overlap with a MapRange are transformed
    - Parts that don't overlap with any MapRange pass through unchanged

    Args:
        input_intervals: List of input intervals to transform.
        map_ranges: The transformation map to apply.

    Returns:
        List of transformed output intervals.
    """
    result: list[Interval] = []

    for interval in input_intervals:
        # Track parts of the interval not yet mapped
        unmapped: list[Interval] = [interval]

        for map_range in map_ranges:
            source_interval = Interval(map_range.source_start, map_range.source_end)
            next_unmapped: list[Interval] = []

            for remaining in unmapped:
                # Check for overlap with this map range
                overlap = remaining.intersect(source_interval)

                if overlap.is_valid():
                    # The overlapping portion gets transformed
                    transformed = Interval(
                        overlap.start + map_range.offset,
                        overlap.end + map_range.offset
                    )
                    result.append(transformed)

                    # The non-overlapping portions remain unmapped
                    before = Interval(remaining.start, overlap.start)
                    if before.is_valid():
                        next_unmapped.append(before)

                    after = Interval(overlap.end, remaining.end)
                    if after.is_valid():
                        next_unmapped.append(after)
                else:
                    # No overlap, this portion remains unmapped
                    next_unmapped.append(remaining)

            unmapped = next_unmapped

        # Any remaining unmapped portions pass through unchanged
        result.extend(unmapped)

    return result


def part2(seeds: list[int], maps: list[list[MapRange]]) -> int:
    """Solve Part 2: Find the minimum location for seed ranges.

    In Part 2, the seeds list is interpreted as pairs of (start, length),
    defining ranges of seed values. We use interval arithmetic to
    efficiently transform these ranges without iterating over individual seeds.

    Args:
        seeds: List of seed range definitions (alternating start and length).
        maps: List of transformation maps.

    Returns:
        The lowest location number among all seeds in all ranges.
    """
    # Convert seed pairs into intervals
    intervals: list[Interval] = []
    for i in range(0, len(seeds), 2):
        seed_start, seed_length = seeds[i], seeds[i + 1]
        intervals.append(Interval(seed_start, seed_start + seed_length))

    # Apply each transformation map to the intervals
    for map_ranges in maps:
        intervals = apply_map_to_intervals(intervals, map_ranges)

    # Find the minimum start value among all resulting intervals
    return min(interval.start for interval in intervals)


def main() -> None:
    """Main entry point: parse input and solve both parts."""
    input_path = Path(__file__).parent.parent / 'input.txt'
    text = input_path.read_text()

    seeds, maps = parse_input(text)

    print('Part 1:', part1(seeds, maps))
    print('Part 2:', part2(seeds, maps))


if __name__ == '__main__':
    main()
