#!/usr/bin/env bash
# Day 21: Step Counter - Garden plot reachability
# Uses awk for performance-critical BFS operations

INPUT_FILE="${1:-../input.txt}"

# Part 1: BFS on finite grid for 64 steps
part1() {
    awk '
    BEGIN { rows = 0 }
    {
        grid[rows] = $0
        cols = length($0)
        for (c = 1; c <= cols; c++) {
            if (substr($0, c, 1) == "S") {
                start_r = rows
                start_c = c - 1
            }
        }
        rows++
    }
    END {
        steps = 64
        target_parity = steps % 2

        # BFS - track visited with distance
        # visited[r,c] = distance
        visited[start_r, start_c] = 0
        queue[0] = start_r "," start_c "," 0
        front = 0
        back = 1

        while (front < back) {
            split(queue[front], parts, ",")
            r = parts[1]
            c = parts[2]
            d = parts[3]
            front++

            if (d >= steps) continue

            # 4 directions
            dr[0] = -1; dc[0] = 0
            dr[1] = 1;  dc[1] = 0
            dr[2] = 0;  dc[2] = -1
            dr[3] = 0;  dc[3] = 1

            for (i = 0; i < 4; i++) {
                nr = r + dr[i]
                nc = c + dc[i]
                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                    ch = substr(grid[nr], nc + 1, 1)
                    if (ch != "#" && !((nr, nc) in visited)) {
                        visited[nr, nc] = d + 1
                        queue[back++] = nr "," nc "," (d + 1)
                    }
                }
            }
        }

        # Count cells with matching parity
        count = 0
        for (key in visited) {
            d = visited[key]
            if (d <= steps && d % 2 == target_parity) {
                count++
            }
        }
        print count
    }
    ' "$INPUT_FILE"
}

# BFS on infinite grid for given steps
bfs_infinite() {
    local steps=$1
    awk -v steps="$steps" '
    BEGIN { rows = 0 }
    {
        grid[rows] = $0
        cols = length($0)
        for (c = 1; c <= cols; c++) {
            if (substr($0, c, 1) == "S") {
                start_r = rows
                start_c = c - 1
            }
        }
        rows++
    }
    END {
        target_parity = steps % 2

        # BFS with infinite tiling
        visited[start_r, start_c] = 0
        queue[0] = start_r "," start_c "," 0
        front = 0
        back = 1

        while (front < back) {
            split(queue[front], parts, ",")
            r = int(parts[1])
            c = int(parts[2])
            d = int(parts[3])
            front++

            if (d >= steps) continue

            # 4 directions
            dr[0] = -1; dc[0] = 0
            dr[1] = 1;  dc[1] = 0
            dr[2] = 0;  dc[2] = -1
            dr[3] = 0;  dc[3] = 1

            for (i = 0; i < 4; i++) {
                nr = r + dr[i]
                nc = c + dc[i]

                # Map to grid coordinates (infinite tiling)
                gr = ((nr % rows) + rows) % rows
                gc = ((nc % cols) + cols) % cols
                ch = substr(grid[gr], gc + 1, 1)

                if (ch != "#" && !((nr, nc) in visited)) {
                    visited[nr, nc] = d + 1
                    queue[back++] = nr "," nc "," (d + 1)
                }
            }
        }

        # Count cells with matching parity
        count = 0
        for (key in visited) {
            d = visited[key]
            if (d <= steps && d % 2 == target_parity) {
                count++
            }
        }
        print count
    }
    ' "$INPUT_FILE"
}

# Part 2: Quadratic extrapolation for 26501365 steps
part2() {
    # Grid is 131x131, start at center (65, 65)
    # 26501365 = 65 + 202300 * 131
    local size=131
    local half=65
    local n=202300

    # Calculate y0, y1, y2 for quadratic interpolation
    local y0 y1 y2
    y0=$(bfs_infinite $half)
    y1=$(bfs_infinite $((half + size)))
    y2=$(bfs_infinite $((half + 2 * size)))

    # Use bc for large integer arithmetic
    # a = (y2 - 2*y1 + y0) / 2
    # b = y1 - y0 - a
    # c = y0
    # Result = a*n^2 + b*n + c
    echo "
        y0 = $y0
        y1 = $y1
        y2 = $y2
        n = $n
        a = (y2 - 2*y1 + y0) / 2
        b = y1 - y0 - a
        c = y0
        a * n * n + b * n + c
    " | bc
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
