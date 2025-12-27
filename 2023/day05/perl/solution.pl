#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;
use List::Util qw(min);

# =============================================================================
# Advent of Code 2023 - Day 5: If You Give A Seed A Fertilizer
#
# This puzzle involves mapping seeds through a series of transformations
# (seed -> soil -> fertilizer -> water -> light -> temperature -> humidity -> location)
# to find the minimum location number.
#
# Part 1: Apply mappings to individual seed numbers
# Part 2: Apply mappings to ranges of seed numbers (interval arithmetic)
# =============================================================================

# Read input file
my $script_dir = dirname(__FILE__);
my $input_path = File::Spec->catfile($script_dir, '..', 'input.txt');
open my $fh, '<', $input_path or die "Cannot open $input_path: $!";
my $text = do { local $/; <$fh> };
close $fh;

# Parse input into sections separated by blank lines
my @sections = split /\n\n/, $text;

# Extract seed numbers from the first line
my ($seed_line) = $sections[0] =~ /seeds:\s+(.+)/;
my @seeds = split /\s+/, $seed_line;

# Parse each mapping section into arrays of [destination_start, source_start, length]
my @almanac_maps;
for my $section_index (1 .. $#sections) {
    my @lines = split /\n/, $sections[$section_index];
    shift @lines;  # Skip the header line (e.g., "seed-to-soil map:")

    my @mapping_rules;
    for my $line (@lines) {
        next unless $line =~ /\S/;  # Skip empty lines
        my ($dest_start, $src_start, $range_length) = split /\s+/, $line;
        push @mapping_rules, [$dest_start, $src_start, $range_length];
    }
    push @almanac_maps, \@mapping_rules;
}

# =============================================================================
# Part 1: Single Value Mapping
# =============================================================================

# Apply a single mapping stage to a value
# Returns the mapped value, or the original if no rule matches
sub apply_single_map {
    my ($value, $mapping_rules) = @_;

    for my $rule (@$mapping_rules) {
        my ($dest_start, $src_start, $range_length) = @$rule;
        my $src_end = $src_start + $range_length;

        # Check if value falls within this mapping's source range
        if ($value >= $src_start && $value < $src_end) {
            my $offset = $value - $src_start;
            return $dest_start + $offset;
        }
    }

    # No mapping matched - value passes through unchanged
    return $value;
}

# Transform a seed number through all mapping stages to get final location
sub seed_to_location {
    my ($seed, $maps) = @_;
    my $current_value = $seed;

    for my $mapping_rules (@$maps) {
        $current_value = apply_single_map($current_value, $mapping_rules);
    }

    return $current_value;
}

sub part1 {
    my ($seeds, $maps) = @_;

    my @locations = map { seed_to_location($_, $maps) } @$seeds;
    return min(@locations);
}

# =============================================================================
# Part 2: Range-Based Mapping (Interval Arithmetic)
#
# Instead of mapping individual seeds (which would be billions of values),
# we map intervals/ranges through each transformation stage.
#
# Each range is represented as [start, end) - inclusive start, exclusive end.
#
# When a range overlaps with a mapping rule, we split it into up to 3 parts:
#   1. The portion BEFORE the rule's source range (passes through unmapped)
#   2. The portion WITHIN the rule's source range (gets transformed)
#   3. The portion AFTER the rule's source range (passes through unmapped)
# =============================================================================

# Apply a mapping stage to a collection of ranges
# Returns a new array of transformed ranges
sub apply_map_to_ranges {
    my ($input_ranges, $mapping_rules) = @_;
    my @mapped_ranges;

    for my $input_range (@$input_ranges) {
        my ($range_start, $range_end) = @$input_range;

        # Track portions of this range that haven't been mapped yet
        my @unmapped_portions = ([$range_start, $range_end]);

        # Try each mapping rule - portions that match get transformed,
        # portions that don't match remain in @unmapped_portions
        for my $rule (@$mapping_rules) {
            my ($dest_start, $src_start, $range_length) = @$rule;
            my $src_end = $src_start + $range_length;
            my @still_unmapped;

            for my $portion (@unmapped_portions) {
                my ($portion_start, $portion_end) = @$portion;

                # CASE 1: Portion entirely before mapping source range
                # [portion_start...portion_end) ... [src_start...src_end)
                # This portion doesn't overlap - keep it for next rule
                if ($portion_end <= $src_start) {
                    push @still_unmapped, $portion;
                    next;
                }

                # CASE 2: Portion entirely after mapping source range
                # [src_start...src_end) ... [portion_start...portion_end)
                # This portion doesn't overlap - keep it for next rule
                if ($portion_start >= $src_end) {
                    push @still_unmapped, $portion;
                    next;
                }

                # CASE 3: Portion overlaps with mapping source range
                # We may need to split into up to 3 pieces

                # 3a: Part before the overlap (stays unmapped for now)
                if ($portion_start < $src_start) {
                    push @still_unmapped, [$portion_start, $src_start];
                }

                # 3b: The overlapping part (gets mapped/transformed)
                my $overlap_start = $portion_start > $src_start ? $portion_start : $src_start;
                my $overlap_end   = $portion_end   < $src_end   ? $portion_end   : $src_end;
                my $mapping_offset = $dest_start - $src_start;
                push @mapped_ranges, [$overlap_start + $mapping_offset,
                                      $overlap_end   + $mapping_offset];

                # 3c: Part after the overlap (stays unmapped for now)
                if ($portion_end > $src_end) {
                    push @still_unmapped, [$src_end, $portion_end];
                }
            }

            @unmapped_portions = @still_unmapped;
        }

        # Any portions that didn't match any rule pass through unchanged
        push @mapped_ranges, @unmapped_portions;
    }

    return \@mapped_ranges;
}

sub part2 {
    my ($seeds, $maps) = @_;

    # In Part 2, seeds are pairs: (start, length)
    # Convert to ranges: [start, start + length)
    my @seed_ranges;
    for (my $i = 0; $i < @$seeds; $i += 2) {
        my $start  = $seeds->[$i];
        my $length = $seeds->[$i + 1];
        push @seed_ranges, [$start, $start + $length];
    }

    # Transform ranges through each mapping stage
    my $current_ranges = \@seed_ranges;
    for my $mapping_rules (@$maps) {
        $current_ranges = apply_map_to_ranges($current_ranges, $mapping_rules);
    }

    # Find the minimum starting point among all final location ranges
    my @range_starts = map { $_->[0] } @$current_ranges;
    return min(@range_starts);
}

# =============================================================================
# Main Execution
# =============================================================================

print "Part 1: ", part1(\@seeds, \@almanac_maps), "\n";
print "Part 2: ", part2(\@seeds, \@almanac_maps), "\n";
