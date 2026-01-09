#!/opt/homebrew/bin/bash

# Day 18: Boiling Boulders
# Part 1: Count all exposed cube faces
# Part 2: Count only exterior surface area (BFS flood fill)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Use associative arrays for cube lookup
declare -A cubes
declare -A exterior

# Directions: +x, -x, +y, -y, +z, -z
dx=(1 -1 0 0 0 0)
dy=(0 0 1 -1 0 0)
dz=(0 0 0 0 1 -1)

# Parse input and store cubes
min_x=999999 max_x=-999999
min_y=999999 max_y=-999999
min_z=999999 max_z=-999999

while IFS=',' read -r x y z; do
    cubes["$x,$y,$z"]=1
    ((x < min_x)) && min_x=$x
    ((x > max_x)) && max_x=$x
    ((y < min_y)) && min_y=$y
    ((y > max_y)) && max_y=$y
    ((z < min_z)) && min_z=$z
    ((z > max_z)) && max_z=$z
done < "$INPUT_FILE"

# Part 1: Count all exposed faces
part1=0
for key in "${!cubes[@]}"; do
    IFS=',' read -r x y z <<< "$key"
    for i in 0 1 2 3 4 5; do
        nx=$((x + dx[i]))
        ny=$((y + dy[i]))
        nz=$((z + dz[i]))
        if [[ -z "${cubes[$nx,$ny,$nz]}" ]]; then
            ((part1++))
        fi
    done
done

echo "Part 1: $part1"

# Part 2: BFS to find exterior air, then count faces touching exterior
# Expand bounding box by 1
((min_x--))
((max_x++))
((min_y--))
((max_y++))
((min_z--))
((max_z++))

# BFS queue (stored as array of "x,y,z" strings)
queue=("$min_x,$min_y,$min_z")
exterior["$min_x,$min_y,$min_z"]=1
head=0

while ((head < ${#queue[@]})); do
    IFS=',' read -r x y z <<< "${queue[head]}"
    ((head++))

    for i in 0 1 2 3 4 5; do
        nx=$((x + dx[i]))
        ny=$((y + dy[i]))
        nz=$((z + dz[i]))

        # Skip if out of bounds
        ((nx < min_x || nx > max_x)) && continue
        ((ny < min_y || ny > max_y)) && continue
        ((nz < min_z || nz > max_z)) && continue

        # Skip if cube or already visited
        [[ -n "${cubes[$nx,$ny,$nz]}" ]] && continue
        [[ -n "${exterior[$nx,$ny,$nz]}" ]] && continue

        exterior["$nx,$ny,$nz"]=1
        queue+=("$nx,$ny,$nz")
    done
done

# Count faces touching exterior air
part2=0
for key in "${!cubes[@]}"; do
    IFS=',' read -r x y z <<< "$key"
    for i in 0 1 2 3 4 5; do
        nx=$((x + dx[i]))
        ny=$((y + dy[i]))
        nz=$((z + dz[i]))
        if [[ -n "${exterior[$nx,$ny,$nz]}" ]]; then
            ((part2++))
        fi
    done
done

echo "Part 2: $part2"
