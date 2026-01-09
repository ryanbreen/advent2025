#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(reduce);

# Read input file
my $script_dir = $0 =~ s/[^\/]+$//r;
my $input_file = "${script_dir}../input.txt";

open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my $text = do { local $/; <$fh> };
close $fh;

sub parse_monkeys {
    my ($text) = @_;
    my @monkeys;

    for my $block (split /\n\n/, $text) {
        my @lines = split /\n/, $block;

        # Parse starting items
        my @items = ($lines[1] =~ /(\d+)/g);

        # Parse operation
        my ($operator, $operand) = ($lines[2] =~ /new = old ([+*]) (\w+)/);

        # Parse divisor and targets
        my ($divisor) = ($lines[3] =~ /(\d+)/);
        my ($if_true) = ($lines[4] =~ /(\d+)/);
        my ($if_false) = ($lines[5] =~ /(\d+)/);

        push @monkeys, {
            items => \@items,
            operator => $operator,
            operand => $operand,
            divisor => $divisor,
            if_true => $if_true,
            if_false => $if_false,
            inspections => 0
        };
    }

    return \@monkeys;
}

sub apply_operation {
    my ($old, $operator, $operand) = @_;
    my $val = $operand eq 'old' ? $old : $operand;

    if ($operator eq '+') {
        return $old + $val;
    } else {
        return $old * $val;
    }
}

sub simulate {
    my ($monkeys, $rounds, $relief_divisor, $use_modulo) = @_;
    $relief_divisor //= 3;
    $use_modulo //= 0;

    # For part 2, compute product of all divisors
    my $mod_value;
    if ($use_modulo) {
        $mod_value = 1;
        for my $monkey (@$monkeys) {
            $mod_value *= $monkey->{divisor};
        }
    }

    for my $round (1 .. $rounds) {
        for my $monkey (@$monkeys) {
            while (@{$monkey->{items}}) {
                my $item = shift @{$monkey->{items}};
                $monkey->{inspections}++;

                # Apply operation
                my $new_val = apply_operation($item, $monkey->{operator}, $monkey->{operand});

                # Apply relief
                if ($relief_divisor > 1) {
                    $new_val = int($new_val / $relief_divisor);
                }

                # Apply modulo to prevent overflow
                if ($mod_value) {
                    $new_val = $new_val % $mod_value;
                }

                # Test and throw
                if ($new_val % $monkey->{divisor} == 0) {
                    push @{$monkeys->[$monkey->{if_true}]{items}}, $new_val;
                } else {
                    push @{$monkeys->[$monkey->{if_false}]{items}}, $new_val;
                }
            }
        }
    }

    return $monkeys;
}

sub monkey_business {
    my ($monkeys) = @_;
    my @inspections = sort { $b <=> $a } map { $_->{inspections} } @$monkeys;
    return $inspections[0] * $inspections[1];
}

sub part1 {
    my ($text) = @_;
    my $monkeys = parse_monkeys($text);
    simulate($monkeys, 20, 3);
    return monkey_business($monkeys);
}

sub part2 {
    my ($text) = @_;
    my $monkeys = parse_monkeys($text);
    simulate($monkeys, 10000, 1, 1);
    return monkey_business($monkeys);
}

print "Part 1: ", part1($text), "\n";
print "Part 2: ", part2($text), "\n";
