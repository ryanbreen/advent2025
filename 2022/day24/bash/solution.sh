#!/bin/bash

# Day 24: Blizzard Basin
# BFS through a grid with moving blizzards

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Use AWK for the heavy computation with precomputed blizzard positions
awk '
function gcd(a, b) {
    while (b != 0) {
        t = b
        b = a % b
        a = t
    }
    return a
}

function lcm(a, b) {
    return (a * b) / gcd(a, b)
}

function mod(a, m) {
    return ((a % m) + m) % m
}

# Precompute all blizzard positions for all times in one period
function precompute_blizzards(    i, r, c, d, ir, ic, nr, nc, t) {
    for (t = 0; t < period; t++) {
        for (i = 0; i < num_blizzards; i++) {
            r = bliz_r[i]
            c = bliz_c[i]
            d = bliz_d[i]

            # Adjust to inner coordinates (subtract 2 for 1-indexed walls)
            ir = r - 2
            ic = c - 2

            if (d == "^") {
                nr = mod(ir - t, inner_h)
                nc = ic
            } else if (d == "v") {
                nr = mod(ir + t, inner_h)
                nc = ic
            } else if (d == "<") {
                nr = ir
                nc = mod(ic - t, inner_w)
            } else if (d == ">") {
                nr = ir
                nc = mod(ic + t, inner_w)
            }

            # Store as "time,row,col" -> 1 (convert back to 1-indexed grid coords)
            bliz_cache[t, nr + 2, nc + 2] = 1
        }
    }
}

# BFS from (sr, sc) to (er, ec) starting at time start_t
function bfs(sr, sc, er, ec, start_t,    queue_t, queue_r, queue_c, head, tail, t, r, c, nt, nr, nc, di, tm) {
    # Directions: wait, up, down, left, right
    dr[0] = 0; dc[0] = 0
    dr[1] = -1; dc[1] = 0
    dr[2] = 1; dc[2] = 0
    dr[3] = 0; dc[3] = -1
    dr[4] = 0; dc[4] = 1

    delete visited
    head = 0
    tail = 0

    # Push start state
    queue_t[tail] = start_t
    queue_r[tail] = sr
    queue_c[tail] = sc
    tail++
    visited[mod(start_t, period), sr, sc] = 1

    while (head < tail) {
        t = queue_t[head]
        r = queue_r[head]
        c = queue_c[head]
        head++

        if (r == er && c == ec) {
            return t
        }

        nt = t + 1
        tm = mod(nt, period)

        for (di = 0; di < 5; di++) {
            nr = r + dr[di]
            nc = c + dc[di]

            # Check if valid position
            # Start/end positions are valid
            if ((nr == sr && nc == sc) || (nr == er && nc == ec)) {
                # Start or end position - always valid
            } else if (nr < 2 || nr > height - 1 || nc < 2 || nc > width - 1) {
                # Wall position (1-indexed: row 1 and row height are walls,
                # col 1 and col width are walls)
                continue
            }

            # Check if blizzard blocks this position
            if ((tm, nr, nc) in bliz_cache) {
                continue
            }

            if (!((tm, nr, nc) in visited)) {
                visited[tm, nr, nc] = 1
                queue_t[tail] = nt
                queue_r[tail] = nr
                queue_c[tail] = nc
                tail++
            }
        }
    }

    return -1
}

BEGIN {
    num_blizzards = 0
}

{
    lines[NR] = $0
    height = NR
    width = length($0)
}

END {
    inner_h = height - 2
    inner_w = width - 2
    period = lcm(inner_h, inner_w)

    # Parse blizzards
    for (r = 1; r <= height; r++) {
        n = split(lines[r], chars, "")
        for (c = 1; c <= n; c++) {
            ch = chars[c]
            if (ch == "^" || ch == "v" || ch == "<" || ch == ">") {
                bliz_r[num_blizzards] = r
                bliz_c[num_blizzards] = c
                bliz_d[num_blizzards] = ch
                num_blizzards++
            }
        }
    }

    # Precompute all blizzard positions
    precompute_blizzards()

    # Find start and end
    n = split(lines[1], chars, "")
    for (c = 1; c <= n; c++) {
        if (chars[c] == ".") {
            start_r = 1
            start_c = c
            break
        }
    }

    n = split(lines[height], chars, "")
    for (c = 1; c <= n; c++) {
        if (chars[c] == ".") {
            end_r = height
            end_c = c
            break
        }
    }

    # Part 1: start to end
    t1 = bfs(start_r, start_c, end_r, end_c, 0)
    print "Part 1:", t1

    # Part 2: start -> end -> start -> end
    t2 = bfs(end_r, end_c, start_r, start_c, t1)
    t3 = bfs(start_r, start_c, end_r, end_c, t2)
    print "Part 2:", t3
}
' "$INPUT_FILE"
