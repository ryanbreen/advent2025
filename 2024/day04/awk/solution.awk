#!/usr/bin/awk -f

BEGIN {
    # 8 directions: right, left, down, up, and 4 diagonals
    dir[0, 0] = 0;  dir[0, 1] = 1;   # right
    dir[1, 0] = 0;  dir[1, 1] = -1;  # left
    dir[2, 0] = 1;  dir[2, 1] = 0;   # down
    dir[3, 0] = -1; dir[3, 1] = 0;   # up
    dir[4, 0] = 1;  dir[4, 1] = 1;   # down-right
    dir[5, 0] = 1;  dir[5, 1] = -1;  # down-left
    dir[6, 0] = -1; dir[6, 1] = 1;   # up-right
    dir[7, 0] = -1; dir[7, 1] = -1;  # up-left

    rows = 0
}

# Read the grid
{
    rows++
    for (i = 1; i <= length($0); i++) {
        grid[rows, i] = substr($0, i, 1)
    }
    cols = length($0)
}

END {
    print "Part 1: " part1()
    print "Part 2: " part2()
}

function part1(    r, c, d, count, target, found, i, ch, nr, nc, dr, dc) {
    target = "XMAS"
    count = 0

    for (r = 1; r <= rows; r++) {
        for (c = 1; c <= cols; c++) {
            # Try each direction from this position
            for (d = 0; d < 8; d++) {
                dr = dir[d, 0]
                dc = dir[d, 1]
                found = 1

                # Check if XMAS fits in this direction
                for (i = 0; i < length(target); i++) {
                    ch = substr(target, i + 1, 1)
                    nr = r + dr * i
                    nc = c + dc * i

                    if (nr < 1 || nr > rows || nc < 1 || nc > cols) {
                        found = 0
                        break
                    }
                    if (grid[nr, nc] != ch) {
                        found = 0
                        break
                    }
                }

                if (found) {
                    count++
                }
            }
        }
    }

    return count
}

function part2(    r, c, count, top_left, top_right, bottom_left, bottom_right, diag1_ok, diag2_ok) {
    count = 0

    # Check each possible center point (A must be in the middle)
    for (r = 2; r <= rows - 1; r++) {
        for (c = 2; c <= cols - 1; c++) {
            if (grid[r, c] != "A") {
                continue
            }

            # Get the four corners
            top_left = grid[r - 1, c - 1]
            top_right = grid[r - 1, c + 1]
            bottom_left = grid[r + 1, c - 1]
            bottom_right = grid[r + 1, c + 1]

            # Check diagonal 1 (top-left to bottom-right): MAS or SAM
            diag1_ok = (top_left == "M" && bottom_right == "S") || (top_left == "S" && bottom_right == "M")

            # Check diagonal 2 (top-right to bottom-left): MAS or SAM
            diag2_ok = (top_right == "M" && bottom_left == "S") || (top_right == "S" && bottom_left == "M")

            if (diag1_ok && diag2_ok) {
                count++
            }
        }
    }

    return count
}
