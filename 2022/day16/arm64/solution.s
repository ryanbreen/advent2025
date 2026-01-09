// Day 16: Proboscidea Volcanium - ARM64 Assembly (macOS)
//
// Part 1: Find max pressure release in 30 minutes
// Part 2: With elephant helper, find max pressure in 26 minutes each (partition valves)
//
// Algorithm:
// 1. Parse valves with flow rates and tunnels
// 2. BFS to compute shortest paths between all valuable valves (flow > 0)
// 3. DFS to find optimal valve opening order
// 4. Part 2: Enumerate all 2^n subsets, find best partition

.global _main
.align 4

// Constants
.equ MAX_VALVES, 64
.equ MAX_VALUABLE, 16
.equ MAX_INPUT, 8192
.equ MAX_QUEUE, 512

// Macro for loading addresses
.macro LOAD_ADDR reg, label
    adrp    \reg, \label@PAGE
    add     \reg, \reg, \label@PAGEOFF
.endm

// ============================================================================
// Data Section
// ============================================================================
.data

input_path:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"

.align 3
// Valve data
num_valves:     .quad 0                         // Total number of valves
num_valuable:   .quad 0                         // Valves with flow > 0
aa_index:       .quad 0                         // Index of valve AA

// Valve names as 2-byte codes (stored as 16-bit values)
valve_names:    .space MAX_VALVES * 2

// Flow rates for each valve
flow_rates:     .space MAX_VALVES * 4

// Adjacency list
adj_count:      .space MAX_VALVES * 4
adj_list:       .space MAX_VALVES * MAX_VALVES * 4

// Indices of valuable valves (flow > 0)
valuable_idx:   .space MAX_VALUABLE * 4

// Flow rates of valuable valves (for quick access)
valuable_flow:  .space MAX_VALUABLE * 4

// Distance matrix: dist[i][j] = shortest path
// Row 0 = from AA to each valuable valve
// Row 1..n = from valuable[i-1] to each valuable valve
// Column j = to valuable[j]
distances:      .space (MAX_VALUABLE + 1) * MAX_VALUABLE * 4

// BFS data
bfs_queue:      .space MAX_QUEUE * 4
bfs_dist:       .space MAX_VALVES * 4

// Memoization for Part 2
memo_scores:    .space 65536 * 4

// File buffer
file_buffer:    .space MAX_INPUT

// ============================================================================
// Code Section
// ============================================================================
.text

// ============================================================================
// Main entry point
// ============================================================================
_main:
    // Open input file
    LOAD_ADDR x0, input_path
    mov     x1, #0
    mov     x2, #0
    mov     x16, #5
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit
    mov     x19, x0

    // Read file
    mov     x0, x19
    LOAD_ADDR x1, file_buffer
    mov     x2, #MAX_INPUT
    mov     x16, #3
    svc     #0x80

    // Close file
    mov     x0, x19
    mov     x16, #6
    svc     #0x80

    // Parse input
    bl      parse_input

    // Compute distances
    bl      compute_distances

    // Solve Part 1
    mov     x0, #0                          // pos = 0 (start from AA)
    mov     x1, #30                         // time = 30
    mov     x2, #0                          // opened = 0
    bl      dfs_search
    mov     x21, x0

    // Solve Part 2
    bl      solve_part2
    mov     x22, x0

    // Print Part 1
    LOAD_ADDR x0, part1_msg
    bl      print_str
    mov     x0, x21
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Print Part 2
    LOAD_ADDR x0, part2_msg
    bl      print_str
    mov     x0, x22
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Exit
    mov     x0, #0
    mov     x16, #1
    svc     #0x80

error_exit:
    mov     x0, #1
    mov     x16, #1
    svc     #0x80

// ============================================================================
// parse_input: Parse the valve definitions
// ============================================================================
parse_input:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    LOAD_ADDR x19, file_buffer
    mov     x20, #0                         // Valve count

parse_line:
    ldrb    w0, [x19]
    cbz     w0, parse_done
    cmp     w0, #'\n'
    b.eq    skip_empty_line

    // Skip "Valve "
    add     x19, x19, #6

    // Read valve name (2 chars)
    ldrb    w21, [x19], #1
    ldrb    w22, [x19], #1
    lsl     w21, w21, #8
    orr     w21, w21, w22

    // Store valve name
    LOAD_ADDR x0, valve_names
    strh    w21, [x0, x20, lsl #1]

    // Check if this is AA
    mov     w0, #('A' << 8) | 'A'
    cmp     w21, w0
    b.ne    not_aa
    LOAD_ADDR x0, aa_index
    str     x20, [x0]
not_aa:

    // Skip " has flow rate="
    add     x19, x19, #15

    // Parse flow rate
    mov     x23, #0
parse_rate:
    ldrb    w0, [x19], #1
    cmp     w0, #';'
    b.eq    rate_done
    sub     w0, w0, #'0'
    mov     x1, #10
    mul     x23, x23, x1
    add     x23, x23, x0
    b       parse_rate

rate_done:
    // Store flow rate
    LOAD_ADDR x0, flow_rates
    str     w23, [x0, x20, lsl #2]

    // Skip to valves list - look for "valve " or "valves "
skip_to_valves:
    ldrb    w0, [x19], #1
    cmp     w0, #' '
    b.ne    skip_to_valves
    ldrb    w0, [x19]
    cmp     w0, #'v'
    b.ne    skip_to_valves
    // Skip "valve" (5 chars)
    add     x19, x19, #5
    // Check if next char is 's' (plural)
    ldrb    w0, [x19]
    cmp     w0, #'s'
    b.ne    skip_space_after_valve
    add     x19, x19, #1                    // Skip 's' in "valves"
skip_space_after_valve:
    add     x19, x19, #1                    // Skip space after valve(s)

    // Parse neighbor list
    mov     x24, #0                         // Neighbor count

parse_neighbors:
    ldrb    w25, [x19], #1
    ldrb    w26, [x19], #1
    lsl     w25, w25, #8
    orr     w25, w25, w26

    // Store neighbor name temporarily
    LOAD_ADDR x0, adj_list
    mov     x1, #MAX_VALVES
    mul     x1, x20, x1
    add     x1, x1, x24
    str     w25, [x0, x1, lsl #2]
    add     x24, x24, #1

    // Check for more neighbors
    ldrb    w0, [x19]
    cmp     w0, #','
    b.ne    neighbors_done
    add     x19, x19, #2
    b       parse_neighbors

neighbors_done:
    LOAD_ADDR x0, adj_count
    str     w24, [x0, x20, lsl #2]

skip_newline:
    ldrb    w0, [x19]
    cbz     w0, next_valve
    cmp     w0, #'\n'
    b.ne    skip_char
    add     x19, x19, #1
    b       next_valve
skip_char:
    add     x19, x19, #1
    b       skip_newline

skip_empty_line:
    add     x19, x19, #1
    b       parse_line

next_valve:
    add     x20, x20, #1
    b       parse_line

parse_done:
    LOAD_ADDR x0, num_valves
    str     x20, [x0]

    // Resolve neighbor names to indices
    bl      resolve_neighbors

    // Build list of valuable valves
    bl      build_valuable_list

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// resolve_neighbors: Convert neighbor names to indices
// ============================================================================
resolve_neighbors:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    LOAD_ADDR x0, num_valves
    ldr     x19, [x0]

    mov     x20, #0

resolve_valve_loop:
    cmp     x20, x19
    b.ge    resolve_done

    LOAD_ADDR x0, adj_count
    ldr     w21, [x0, x20, lsl #2]

    mov     x22, #0

resolve_neighbor_loop:
    cmp     x22, x21
    b.ge    next_resolve_valve

    LOAD_ADDR x0, adj_list
    mov     x1, #MAX_VALVES
    mul     x1, x20, x1
    add     x1, x1, x22
    ldr     w23, [x0, x1, lsl #2]

    mov     x24, #0
find_valve_loop:
    cmp     x24, x19
    b.ge    found_valve
    LOAD_ADDR x0, valve_names
    ldrh    w0, [x0, x24, lsl #1]
    cmp     w0, w23
    b.eq    found_valve
    add     x24, x24, #1
    b       find_valve_loop

found_valve:
    LOAD_ADDR x0, adj_list
    mov     x1, #MAX_VALVES
    mul     x1, x20, x1
    add     x1, x1, x22
    str     w24, [x0, x1, lsl #2]

    add     x22, x22, #1
    b       resolve_neighbor_loop

next_resolve_valve:
    add     x20, x20, #1
    b       resolve_valve_loop

resolve_done:
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// build_valuable_list: Build list of valves with flow > 0
// ============================================================================
build_valuable_list:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x0, num_valves
    ldr     x19, [x0]

    mov     x20, #0                         // valuable count
    mov     x21, #0                         // valve index

build_loop:
    cmp     x21, x19
    b.ge    build_done

    LOAD_ADDR x0, flow_rates
    ldr     w22, [x0, x21, lsl #2]
    cbz     w22, next_build

    // Store valve index
    LOAD_ADDR x0, valuable_idx
    str     w21, [x0, x20, lsl #2]

    // Store flow rate for quick access
    LOAD_ADDR x0, valuable_flow
    str     w22, [x0, x20, lsl #2]

    add     x20, x20, #1

next_build:
    add     x21, x21, #1
    b       build_loop

build_done:
    LOAD_ADDR x0, num_valuable
    str     x20, [x0]

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// compute_distances: BFS from AA and each valuable valve to all valuable valves
// ============================================================================
compute_distances:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x0, num_valuable
    ldr     x19, [x0]

    // BFS from AA (store in row 0)
    LOAD_ADDR x0, aa_index
    ldr     x0, [x0]
    mov     x1, #0
    bl      bfs_from_valve

    // BFS from each valuable valve
    mov     x20, #0
dist_loop:
    cmp     x20, x19
    b.ge    dist_done

    LOAD_ADDR x0, valuable_idx
    ldr     w0, [x0, x20, lsl #2]
    add     x1, x20, #1
    bl      bfs_from_valve

    add     x20, x20, #1
    b       dist_loop

dist_done:
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// bfs_from_valve: BFS from valve x0, store distances at row x1
// ============================================================================
bfs_from_valve:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0                         // Start valve
    mov     x20, x1                         // Row in distance matrix

    LOAD_ADDR x0, num_valves
    ldr     x21, [x0]
    LOAD_ADDR x0, num_valuable
    ldr     x22, [x0]

    // Initialize distances to -1 (unvisited)
    LOAD_ADDR x0, bfs_dist
    mov     x1, #0
    mov     w2, #-1
init_dist:
    cmp     x1, x21
    b.ge    init_done
    str     w2, [x0, x1, lsl #2]
    add     x1, x1, #1
    b       init_dist

init_done:
    // Initialize queue with start valve
    LOAD_ADDR x23, bfs_queue
    str     w19, [x23]
    mov     x24, #0                         // head
    mov     x25, #1                         // tail

    // Set start distance to 0
    LOAD_ADDR x0, bfs_dist
    str     wzr, [x0, x19, lsl #2]

bfs_loop:
    cmp     x24, x25
    b.ge    bfs_done

    // Dequeue
    ldr     w26, [x23, x24, lsl #2]
    add     x24, x24, #1

    // Get current distance
    LOAD_ADDR x0, bfs_dist
    ldr     w27, [x0, x26, lsl #2]

    // Visit neighbors
    LOAD_ADDR x0, adj_count
    ldr     w0, [x0, x26, lsl #2]
    mov     x1, #0

visit_neighbors:
    cmp     x1, x0
    b.ge    bfs_loop

    // Get neighbor
    stp     x0, x1, [sp, #-16]!
    LOAD_ADDR x2, adj_list
    mov     x3, #MAX_VALVES
    mul     x3, x26, x3
    add     x3, x3, x1
    ldr     w28, [x2, x3, lsl #2]
    ldp     x0, x1, [sp], #16

    // Check if visited
    LOAD_ADDR x2, bfs_dist
    ldr     w3, [x2, x28, lsl #2]
    cmp     w3, #0
    b.ge    next_neighbor                   // Already visited (dist >= 0)

    // Set distance
    add     w4, w27, #1
    str     w4, [x2, x28, lsl #2]

    // Enqueue
    str     w28, [x23, x25, lsl #2]
    add     x25, x25, #1

next_neighbor:
    add     x1, x1, #1
    b       visit_neighbors

bfs_done:
    // Store distances to valuable valves in distance matrix
    LOAD_ADDR x0, distances
    mov     x1, x22                         // num_valuable
    mul     x1, x20, x1                     // row * num_valuable
    add     x0, x0, x1, lsl #2              // &distances[row][0]

    mov     x1, #0
store_dist_loop:
    cmp     x1, x22
    b.ge    store_done

    LOAD_ADDR x2, valuable_idx
    ldr     w2, [x2, x1, lsl #2]            // valve index
    LOAD_ADDR x3, bfs_dist
    ldr     w3, [x3, x2, lsl #2]            // distance to this valve
    str     w3, [x0, x1, lsl #2]

    add     x1, x1, #1
    b       store_dist_loop

store_done:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// dfs_search: DFS to find max pressure
// x0 = position (0 = AA, 1..n = valuable valve index + 1)
// x1 = time left
// x2 = opened bitmask
// Returns: x0 = max pressure
// ============================================================================
dfs_search:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    mov     x19, x0                         // pos
    mov     x20, x1                         // time_left
    mov     x21, x2                         // opened

    // Base case
    cmp     x20, #0
    b.le    dfs_return_zero

    LOAD_ADDR x0, num_valuable
    ldr     x22, [x0]

    mov     x23, #0                         // best pressure

    // Try each unopened valve
    mov     x24, #0                         // valve index in valuable list
dfs_loop:
    cmp     x24, x22
    b.ge    dfs_done

    // Check if already opened
    mov     x0, #1
    lsl     x0, x0, x24
    tst     x0, x21
    b.ne    next_dfs_valve

    // Get distance from current position to this valve
    // distances[pos][valve]
    LOAD_ADDR x0, distances
    mul     x1, x19, x22                    // pos * num_valuable
    add     x1, x1, x24                     // + valve
    ldr     w25, [x0, x1, lsl #2]           // distance

    // Time cost = distance + 1 (for opening)
    add     x25, x25, #1

    // Check if we have enough time
    cmp     x25, x20
    b.ge    next_dfs_valve

    // Calculate pressure from this valve
    sub     x26, x20, x25                   // new_time = time_left - cost
    LOAD_ADDR x0, valuable_flow
    ldr     w0, [x0, x24, lsl #2]           // flow rate
    mul     x0, x26, x0                     // pressure = new_time * flow_rate

    // Recurse
    stp     x0, x23, [sp, #-16]!
    stp     x24, xzr, [sp, #-16]!

    add     x0, x24, #1                     // new pos = valve + 1
    mov     x1, x26                         // new time
    mov     x2, #1
    lsl     x2, x2, x24
    orr     x2, x21, x2                     // new opened
    bl      dfs_search

    ldp     x24, xzr, [sp], #16
    ldp     x1, x23, [sp], #16              // x1 = pressure from this valve
    add     x0, x0, x1                      // total

    // Update best
    cmp     x0, x23
    csel    x23, x0, x23, gt

next_dfs_valve:
    add     x24, x24, #1
    b       dfs_loop

dfs_done:
    mov     x0, x23
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

dfs_return_zero:
    mov     x0, #0
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_part2: Find max pressure with you and elephant (26 min each)
// ============================================================================
solve_part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    LOAD_ADDR x0, num_valuable
    ldr     x19, [x0]

    // Compute max pressure for each subset
    mov     x0, #1
    lsl     x20, x0, x19                    // 2^num_valuable
    mov     x21, #0                         // current mask

compute_mask_loop:
    cmp     x21, x20
    b.ge    find_best_partition

    // Compute max pressure for this subset
    mov     x0, #0                          // pos = AA
    mov     x1, #26                         // time = 26
    mov     x2, x21                         // allowed = subset mask
    bl      dfs_subset

    // Store result
    LOAD_ADDR x1, memo_scores
    str     w0, [x1, x21, lsl #2]

    add     x21, x21, #1
    b       compute_mask_loop

find_best_partition:
    // Find best partition
    mov     x22, #0                         // best
    sub     x23, x20, #1                    // full_mask
    mov     x21, #0

partition_loop:
    cmp     x21, x20
    b.ge    part2_done

    eor     x24, x23, x21                   // complement

    cmp     x21, x24
    b.gt    next_partition

    LOAD_ADDR x0, memo_scores
    ldr     w1, [x0, x21, lsl #2]
    ldr     w2, [x0, x24, lsl #2]
    add     x1, x1, x2

    cmp     x1, x22
    csel    x22, x1, x22, gt

next_partition:
    add     x21, x21, #1
    b       partition_loop

part2_done:
    mov     x0, x22

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// dfs_subset: DFS for a specific subset of valves (indicated by allowed mask)
// x0 = pos, x1 = time_left, x2 = allowed mask
// Only valves where bit is SET in allowed can be opened
// Returns: x0 = max pressure
// ============================================================================
dfs_subset:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0                         // pos
    mov     x20, x1                         // time_left
    mov     x27, x2                         // allowed (and remaining - we mark opened by clearing bits)

    cmp     x20, #0
    b.le    dfs_sub_zero

    LOAD_ADDR x0, num_valuable
    ldr     x22, [x0]

    mov     x23, #0                         // best

    mov     x24, #0                         // valve index
dfs_sub_loop:
    cmp     x24, x22
    b.ge    dfs_sub_done

    // Check if this valve is in allowed set (and not yet opened)
    mov     x0, #1
    lsl     x0, x0, x24
    tst     x0, x27
    b.eq    next_sub_valve                  // Not allowed or already opened

    // Get distance
    LOAD_ADDR x1, distances
    mul     x2, x19, x22
    add     x2, x2, x24
    ldr     w25, [x1, x2, lsl #2]
    add     x25, x25, #1

    // Check time
    cmp     x25, x20
    b.ge    next_sub_valve

    // Pressure
    sub     x26, x20, x25
    LOAD_ADDR x1, valuable_flow
    ldr     w1, [x1, x24, lsl #2]
    mul     x28, x26, x1

    // Recurse with this valve removed from allowed
    stp     x28, x23, [sp, #-16]!
    stp     x24, xzr, [sp, #-16]!

    add     x0, x24, #1                     // new pos
    mov     x1, x26                         // new time
    mov     x2, #1
    lsl     x2, x2, x24
    bic     x2, x27, x2                     // Remove this valve from allowed
    bl      dfs_subset

    ldp     x24, xzr, [sp], #16
    ldp     x28, x23, [sp], #16
    add     x0, x0, x28

    cmp     x0, x23
    csel    x23, x0, x23, gt

next_sub_valve:
    add     x24, x24, #1
    b       dfs_sub_loop

dfs_sub_done:
    mov     x0, x23
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

dfs_sub_zero:
    mov     x0, #0
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print null-terminated string
// ============================================================================
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0
    mov     x20, #0
1:  ldrb    w1, [x19, x20]
    cbz     w1, 2f
    add     x20, x20, #1
    b       1b

2:  mov     x0, #1
    mov     x1, x19
    mov     x2, x20
    mov     x16, #4
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_num: Print number
// ============================================================================
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    sub     sp, sp, #32

    mov     x19, x0
    add     x20, sp, #31
    strb    wzr, [x20]

    cbnz    x19, 1f
    sub     x20, x20, #1
    mov     w0, #'0'
    strb    w0, [x20]
    b       2f

1:  cbz     x19, 2f
    mov     x1, #10
    udiv    x2, x19, x1
    msub    x3, x2, x1, x19
    add     w3, w3, #'0'
    sub     x20, x20, #1
    strb    w3, [x20]
    mov     x19, x2
    b       1b

2:  mov     x0, x20
    bl      print_str

    add     sp, sp, #32
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
