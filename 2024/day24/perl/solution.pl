#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

sub parse_input {
    my ($filename) = @_;

    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    my $content = do { local $/; <$fh> };
    close $fh;

    my @parts = split /\n\n/, $content;

    # Parse initial wire values
    my %wires;
    for my $line (split /\n/, $parts[0]) {
        my ($name, $val) = split /: /, $line;
        $wires{$name} = int($val);
    }

    # Parse gates
    my @gates;
    for my $line (split /\n/, $parts[1]) {
        # Format: "x00 AND y00 -> z00"
        my @parts_line = split / /, $line;
        my $in1 = $parts_line[0];
        my $op = $parts_line[1];
        my $in2 = $parts_line[2];
        my $out = $parts_line[4];
        push @gates, [$in1, $op, $in2, $out];
    }

    return (\%wires, \@gates);
}

sub simulate {
    my ($wires_ref, $gates_ref) = @_;

    # Make a copy of wires
    my %wires = %$wires_ref;
    my @remaining = @$gates_ref;

    while (@remaining) {
        my $made_progress = 0;
        my @new_remaining;

        for my $gate (@remaining) {
            my ($in1, $op, $in2, $out) = @$gate;

            if (exists $wires{$in1} && exists $wires{$in2}) {
                my $v1 = $wires{$in1};
                my $v2 = $wires{$in2};

                if ($op eq 'AND') {
                    $wires{$out} = $v1 & $v2;
                } elsif ($op eq 'OR') {
                    $wires{$out} = $v1 | $v2;
                } elsif ($op eq 'XOR') {
                    $wires{$out} = $v1 ^ $v2;
                }

                $made_progress = 1;
            } else {
                push @new_remaining, $gate;
            }
        }

        @remaining = @new_remaining;
        if (!$made_progress && @remaining) {
            die "Circuit stuck - missing inputs";
        }
    }

    return \%wires;
}

sub get_z_value {
    my ($wires_ref) = @_;

    # Get all z wires and sort in reverse order
    my @z_wires = sort { $b cmp $a } grep { /^z/ } keys %$wires_ref;

    my $result = 0;
    for my $z (@z_wires) {
        $result = ($result << 1) | $wires_ref->{$z};
    }

    return $result;
}

sub part1 {
    my ($wires_ref, $gates_ref) = @_;

    my $final_wires = simulate($wires_ref, $gates_ref);
    return get_z_value($final_wires);
}

sub part2 {
    my ($gates_ref) = @_;

    # Build lookup: output -> [in1, op, in2]
    my %gate_by_output;
    for my $gate (@$gates_ref) {
        my ($in1, $op, $in2, $out) = @$gate;
        $gate_by_output{$out} = [$in1, $op, $in2];
    }

    # Build lookup: (inputs_set, op) -> output
    my %gate_by_inputs_op;
    for my $gate (@$gates_ref) {
        my ($in1, $op, $in2, $out) = @$gate;
        my $key = join('|', sort($in1, $in2)) . '|' . $op;
        $gate_by_inputs_op{$key} = $out;
    }

    # Find the highest bit number
    my $max_bit = 0;
    for my $gate (@$gates_ref) {
        my $out = $gate->[3];
        if ($out =~ /^z(\d+)$/) {
            $max_bit = $1 if $1 > $max_bit;
        }
    }

    my %swapped;

    for my $gate (@$gates_ref) {
        my ($in1, $op, $in2, $out) = @$gate;

        # Rule: XOR gates that don't take x,y as input should output to z
        if ($op eq 'XOR') {
            my $is_xy_xor = ($in1 =~ /^[xy]/ || $in2 =~ /^[xy]/) &&
                            ($in1 =~ /^[xy]/ || $in2 =~ /^[xy]/);

            if (!$is_xy_xor) {
                # This is a second-level XOR (sum XOR carry), should output to z
                if ($out !~ /^z/) {
                    $swapped{$out} = 1;
                }
            }
        }

        # Rule: z outputs (except the highest bit) should come from XOR
        if ($out =~ /^z/) {
            my $z_num = substr($out, 1);
            if ($z_num != $max_bit && $op ne 'XOR') {
                $swapped{$out} = 1;
            }
        }

        # Rule: AND gates (except x00 AND y00) should feed into OR
        if ($op eq 'AND') {
            my $is_first_bit = ($in1 eq 'x00' && $in2 eq 'y00') ||
                               ($in1 eq 'y00' && $in2 eq 'x00');

            if (!$is_first_bit) {
                # This AND output should be input to an OR gate
                my $used_by_or = 0;
                for my $gate2 (@$gates_ref) {
                    my ($in1b, $op2, $in2b, $out2) = @$gate2;
                    if ($op2 eq 'OR' && ($out eq $in1b || $out eq $in2b)) {
                        $used_by_or = 1;
                        last;
                    }
                }
                if (!$used_by_or) {
                    $swapped{$out} = 1;
                }
            }
        }

        # Rule: XOR of x,y should feed into another XOR (for z output) or AND (for carry)
        if ($op eq 'XOR') {
            my $is_xy_xor = ($in1 =~ /^[xy]/ && $in2 =~ /^[xy]/);
            my $is_z00 = ($in1 eq 'x00' && $in2 eq 'y00') ||
                         ($in1 eq 'y00' && $in2 eq 'x00');

            if ($is_xy_xor && !$is_z00) {
                # Should be used by XOR and AND
                my $used_by_xor = 0;
                my $used_by_and = 0;

                for my $gate2 (@$gates_ref) {
                    my ($in1b, $op2, $in2b, $out2) = @$gate2;
                    if ($out eq $in1b || $out eq $in2b) {
                        $used_by_xor = 1 if $op2 eq 'XOR';
                        $used_by_and = 1 if $op2 eq 'AND';
                    }
                }

                if (!($used_by_xor && $used_by_and)) {
                    $swapped{$out} = 1;
                }
            }
        }
    }

    return join(',', sort keys %swapped);
}

sub main {
    my ($wires_ref, $gates_ref) = parse_input('../input.txt');

    say 'Part 1: ', part1($wires_ref, $gates_ref);
    say 'Part 2: ', part2($gates_ref);
}

main();
