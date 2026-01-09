#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

result=$(awk '
BEGIN {
    # SNAFU digit to decimal value mapping
    digit_val["2"] = 2
    digit_val["1"] = 1
    digit_val["0"] = 0
    digit_val["-"] = -1
    digit_val["="] = -2

    # Decimal value to SNAFU digit mapping
    val_digit[0] = "0"
    val_digit[1] = "1"
    val_digit[2] = "2"
    val_digit[3] = "="
    val_digit[4] = "-"

    total = 0
}

# Convert each SNAFU line to decimal and accumulate
{
    n = length($0)
    decimal = 0
    for (i = 1; i <= n; i++) {
        c = substr($0, i, 1)
        decimal = decimal * 5 + digit_val[c]
    }
    total += decimal
}

END {
    # Convert total back to SNAFU
    if (total == 0) {
        print "0"
    } else {
        snafu = ""
        n = total
        while (n > 0) {
            remainder = n % 5
            snafu = val_digit[remainder] snafu
            if (remainder <= 2) {
                n = int(n / 5)
            } else {
                # remainder is 3 or 4, carry over
                n = int(n / 5) + 1
            }
        }
        print snafu
    }
}
' "$INPUT_FILE")

echo "Part 1: $result"
echo "Part 2: No Part 2 on Day 25!"
