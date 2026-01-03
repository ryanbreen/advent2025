// =============================================================================
// Advent of Code 2023 - Day 21: Step Counter
// ARM64 Assembly Solution for macOS
//
// Part 1: BFS from start to count cells reachable in exactly 64 steps
//         (cells where distance <= 64 and distance % 2 == 0)
// Part 2: Grid tiles infinitely. 26501365 = 65 + 202300*131
//         Use quadratic extrapolation: compute f(65), f(65+131), f(65+262)
//         then solve f(n) = an^2 + bn + c where n = 202300
// =============================================================================

.global _start
.align 4

// Constants - use smaller values that fit in immediates
.equ BUFFER_SIZE, 32768
.equ MAX_GRID_SIZE, 132           // 131x131 grid + padding
.equ QUEUE_SIZE, 400000           // Large queue for BFS
.equ VISITED_SIZE, 1048576        // 2^20 hash table for visited
.equ VISITED_MASK, 1048575        // VISITED_SIZE - 1
.equ GRID_DIM, 131                // Grid dimension

.data
path:           .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"

// Large constants that can't be immediates
n_value:        .quad 202300
fnv_prime:      .quad 1099511628211
fnv_offset:     .quad 14695981039346656037
visited_sz:     .quad VISITED_SIZE
visited_msk:    .quad VISITED_MASK
queue_sz:       .quad QUEUE_SIZE
empty_marker:   .quad 0x8000000080000000  // Empty marker (impossible key: min_int, min_int)

.bss
.align 4
buffer:         .skip BUFFER_SIZE
grid:           .skip MAX_GRID_SIZE * MAX_GRID_SIZE
rows:           .skip 8
cols:           .skip 8
start_r:        .skip 8
start_c:        .skip 8
queue:          .skip QUEUE_SIZE * 24    // (row, col, dist) each 8 bytes
visited:        .skip VISITED_SIZE * 16  // Hash table: (key, dist) pairs
output_buf:     .skip 32

.text

// =============================================================================
// _start: Program entry point
// =============================================================================
_start:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Open and read file
    adrp    x0, path@PAGE
    add     x0, x0, path@PAGEOFF
    mov     x1, #0                  // O_RDONLY
    bl      _open
    cmp     x0, #0
    b.lt    exit_error
    mov     x19, x0                 // Save fd

    adrp    x1, buffer@PAGE
    add     x1, x1, buffer@PAGEOFF
    mov     x2, #BUFFER_SIZE
    bl      _read
    mov     x20, x0                 // Save bytes read

    mov     x0, x19
    bl      _close

    // Null-terminate buffer
    adrp    x0, buffer@PAGE
    add     x0, x0, buffer@PAGEOFF
    strb    wzr, [x0, x20]

    // Parse grid
    bl      parse_grid

    // Part 1: count reachable in 64 steps
    adrp    x0, part1_msg@PAGE
    add     x0, x0, part1_msg@PAGEOFF
    bl      print_string

    mov     x0, #64
    bl      count_reachable
    bl      print_number

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    // Part 2: quadratic extrapolation
    adrp    x0, part2_msg@PAGE
    add     x0, x0, part2_msg@PAGEOFF
    bl      print_string

    bl      solve_part2
    bl      print_number

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    // Exit
    mov     x0, #0
    mov     x16, #1
    svc     #0x80

exit_error:
    mov     x0, #1
    mov     x16, #1
    svc     #0x80

// =============================================================================
// parse_grid: Parse input into grid array, find start position
// =============================================================================
parse_grid:
    stp     x29, x30, [sp, #-64]!
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    mov     x29, sp

    adrp    x19, buffer@PAGE
    add     x19, x19, buffer@PAGEOFF
    adrp    x20, grid@PAGE
    add     x20, x20, grid@PAGEOFF

    mov     x21, #0                 // row
    mov     x22, #0                 // col
    mov     x23, #0                 // max cols

parse_loop:
    ldrb    w0, [x19], #1
    cbz     w0, parse_done

    cmp     w0, #'\n'
    b.eq    parse_newline

    // Store in grid
    mov     x0, #MAX_GRID_SIZE
    mul     x1, x21, x0
    add     x1, x1, x22
    ldrb    w2, [x19, #-1]
    strb    w2, [x20, x1]

    // Check for 'S' (start position)
    cmp     w2, #'S'
    b.ne    parse_next_col

    adrp    x0, start_r@PAGE
    add     x0, x0, start_r@PAGEOFF
    str     x21, [x0]
    adrp    x0, start_c@PAGE
    add     x0, x0, start_c@PAGEOFF
    str     x22, [x0]

parse_next_col:
    add     x22, x22, #1
    cmp     x22, x23
    b.le    parse_loop
    mov     x23, x22
    b       parse_loop

parse_newline:
    add     x21, x21, #1
    mov     x22, #0
    b       parse_loop

parse_done:
    // Handle last row if no trailing newline
    cbz     x22, no_extra_row
    add     x21, x21, #1
no_extra_row:

    adrp    x0, rows@PAGE
    add     x0, x0, rows@PAGEOFF
    str     x21, [x0]
    adrp    x0, cols@PAGE
    add     x0, x0, cols@PAGEOFF
    str     x23, [x0]

    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x23, x24, [sp, #48]
    ldp     x29, x30, [sp], #64
    ret

// =============================================================================
// count_reachable: Count cells reachable in exactly 'steps' steps (Part 1)
// Input: x0 = steps
// Output: x0 = count
// =============================================================================
count_reachable:
    stp     x29, x30, [sp, #-96]!
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]
    mov     x29, sp

    mov     x19, x0                 // steps

    // Load grid info
    adrp    x0, rows@PAGE
    add     x0, x0, rows@PAGEOFF
    ldr     x20, [x0]               // rows
    adrp    x0, cols@PAGE
    add     x0, x0, cols@PAGEOFF
    ldr     x21, [x0]               // cols
    adrp    x0, start_r@PAGE
    add     x0, x0, start_r@PAGEOFF
    ldr     x22, [x0]               // start_r
    adrp    x0, start_c@PAGE
    add     x0, x0, start_c@PAGEOFF
    ldr     x23, [x0]               // start_c

    adrp    x24, grid@PAGE
    add     x24, x24, grid@PAGEOFF
    adrp    x25, queue@PAGE
    add     x25, x25, queue@PAGEOFF

    // Clear visited array (use empty marker as unvisited)
    adrp    x0, visited@PAGE
    add     x0, x0, visited@PAGEOFF
    adrp    x1, visited_sz@PAGE
    add     x1, x1, visited_sz@PAGEOFF
    ldr     x1, [x1]
    adrp    x2, empty_marker@PAGE
    add     x2, x2, empty_marker@PAGEOFF
    ldr     x2, [x2]
clear_visited:
    str     x2, [x0], #16
    subs    x1, x1, #1
    b.ne    clear_visited

    // Initialize BFS
    mov     x26, #0                 // queue head
    mov     x27, #1                 // queue tail

    // Enqueue start: (start_r, start_c, 0)
    str     x22, [x25]              // row
    str     x23, [x25, #8]          // col
    str     xzr, [x25, #16]         // dist = 0

    // Mark start as visited
    mov     x0, x22
    mov     x1, x23
    bl      hash_pos
    adrp    x1, visited@PAGE
    add     x1, x1, visited@PAGEOFF
    adrp    x2, visited_msk@PAGE
    add     x2, x2, visited_msk@PAGEOFF
    ldr     x2, [x2]
    and     x0, x0, x2
    lsl     x0, x0, #4
    add     x1, x1, x0
    // Store key and distance
    mov     x2, x22
    lsl     x2, x2, #32
    orr     x2, x2, x23
    str     x2, [x1]
    str     xzr, [x1, #8]           // dist = 0

bfs_loop:
    cmp     x26, x27
    b.ge    bfs_done

    // Dequeue
    mov     x0, #24
    mul     x0, x26, x0
    add     x0, x25, x0
    ldr     x1, [x0]                // row
    ldr     x2, [x0, #8]            // col
    ldr     x3, [x0, #16]           // dist
    add     x26, x26, #1

    // Skip if at max distance
    cmp     x3, x19
    b.ge    bfs_loop

    // Try 4 directions: up, down, left, right
    mov     x28, #0                 // direction index

try_direction:
    cmp     x28, #4
    b.ge    bfs_loop

    // Calculate new position
    mov     x4, x1                  // nr = r
    mov     x5, x2                  // nc = c

    cmp     x28, #0
    b.ne    not_up
    sub     x4, x4, #1              // up
    b       check_bounds
not_up:
    cmp     x28, #1
    b.ne    not_down
    add     x4, x4, #1              // down
    b       check_bounds
not_down:
    cmp     x28, #2
    b.ne    not_left
    sub     x5, x5, #1              // left
    b       check_bounds
not_left:
    add     x5, x5, #1              // right

check_bounds:
    // Check bounds
    cmp     x4, #0
    b.lt    next_dir
    cmp     x4, x20
    b.ge    next_dir
    cmp     x5, #0
    b.lt    next_dir
    cmp     x5, x21
    b.ge    next_dir

    // Check not wall
    mov     x0, #MAX_GRID_SIZE
    mul     x0, x4, x0
    add     x0, x0, x5
    ldrb    w6, [x24, x0]
    cmp     w6, #'#'
    b.eq    next_dir

    // Check if visited
    stp     x1, x2, [sp, #-32]!
    stp     x3, x4, [sp, #16]
    mov     x0, x4
    mov     x1, x5
    bl      hash_pos
    ldp     x3, x4, [sp, #16]
    ldp     x1, x2, [sp], #32

    adrp    x6, visited@PAGE
    add     x6, x6, visited@PAGEOFF
    adrp    x7, visited_msk@PAGE
    add     x7, x7, visited_msk@PAGEOFF
    ldr     x7, [x7]
    and     x0, x0, x7
    lsl     x7, x0, #4
    add     x6, x6, x7

    // Linear probe to find slot
    mov     x8, x4
    lsl     x8, x8, #32
    orr     x8, x8, x5              // key = (nr << 32) | nc

    mov     x9, #0                  // probe count
probe_loop:
    cmp     x9, #100                // Max probes
    b.ge    next_dir

    ldr     x10, [x6]
    cmp     x10, x8
    b.eq    next_dir                // Already visited

    // Check if empty (compare with empty_marker)
    adrp    x11, empty_marker@PAGE
    add     x11, x11, empty_marker@PAGEOFF
    ldr     x11, [x11]
    cmp     x10, x11
    b.eq    insert_visited

    // Move to next slot
    add     x6, x6, #16
    add     x9, x9, #1
    b       probe_loop

insert_visited:
    // Mark as visited with distance
    add     x10, x3, #1             // new_dist = dist + 1
    str     x8, [x6]                // key
    str     x10, [x6, #8]           // dist

    // Enqueue
    mov     x0, #24
    mul     x0, x27, x0
    add     x0, x25, x0
    str     x4, [x0]                // nr
    str     x5, [x0, #8]            // nc
    str     x10, [x0, #16]          // new_dist
    add     x27, x27, #1

next_dir:
    add     x28, x28, #1
    b       try_direction

bfs_done:
    // Count cells with correct parity
    and     x28, x19, #1            // target parity
    mov     x0, #0                  // count

    adrp    x1, visited@PAGE
    add     x1, x1, visited@PAGEOFF
    adrp    x2, visited_sz@PAGE
    add     x2, x2, visited_sz@PAGEOFF
    ldr     x2, [x2]

    // Load empty marker for comparison
    adrp    x6, empty_marker@PAGE
    add     x6, x6, empty_marker@PAGEOFF
    ldr     x6, [x6]

count_loop:
    cbz     x2, count_done

    ldr     x3, [x1]                // key
    cmp     x3, x6                  // Compare with empty marker
    b.eq    count_next

    ldr     x4, [x1, #8]            // dist
    cmp     x4, x19
    b.gt    count_next

    and     x5, x4, #1
    cmp     x5, x28
    b.ne    count_next

    add     x0, x0, #1

count_next:
    add     x1, x1, #16
    sub     x2, x2, #1
    b       count_loop

count_done:
    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x23, x24, [sp, #48]
    ldp     x25, x26, [sp, #64]
    ldp     x27, x28, [sp, #80]
    ldp     x29, x30, [sp], #96
    ret

// =============================================================================
// hash_pos: Simple hash for position
// Input: x0 = row, x1 = col
// Output: x0 = hash value
// =============================================================================
hash_pos:
    stp     x19, x20, [sp, #-16]!

    // FNV-1a style hash
    adrp    x2, fnv_offset@PAGE
    add     x2, x2, fnv_offset@PAGEOFF
    ldr     x2, [x2]
    adrp    x3, fnv_prime@PAGE
    add     x3, x3, fnv_prime@PAGEOFF
    ldr     x3, [x3]

    eor     x2, x2, x0
    mul     x2, x2, x3
    eor     x2, x2, x1
    mul     x2, x2, x3

    mov     x0, x2
    ldp     x19, x20, [sp], #16
    ret

// =============================================================================
// count_reachable_infinite: BFS on infinite tiled grid
// Input: x0 = steps
// Output: x0 = count
// =============================================================================
count_reachable_infinite:
    stp     x29, x30, [sp, #-112]!
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]
    str     x0, [sp, #96]           // Save steps
    mov     x29, sp

    mov     x19, x0                 // steps

    // Load grid info
    adrp    x0, rows@PAGE
    add     x0, x0, rows@PAGEOFF
    ldr     x20, [x0]               // rows
    adrp    x0, cols@PAGE
    add     x0, x0, cols@PAGEOFF
    ldr     x21, [x0]               // cols
    adrp    x0, start_r@PAGE
    add     x0, x0, start_r@PAGEOFF
    ldr     x22, [x0]               // start_r
    adrp    x0, start_c@PAGE
    add     x0, x0, start_c@PAGEOFF
    ldr     x23, [x0]               // start_c

    adrp    x24, grid@PAGE
    add     x24, x24, grid@PAGEOFF
    adrp    x25, queue@PAGE
    add     x25, x25, queue@PAGEOFF

    // Clear visited array
    adrp    x0, visited@PAGE
    add     x0, x0, visited@PAGEOFF
    adrp    x1, visited_sz@PAGE
    add     x1, x1, visited_sz@PAGEOFF
    ldr     x1, [x1]
    adrp    x2, empty_marker@PAGE
    add     x2, x2, empty_marker@PAGEOFF
    ldr     x2, [x2]
clear_inf_visited:
    str     x2, [x0], #16
    subs    x1, x1, #1
    b.ne    clear_inf_visited

    // Initialize BFS
    mov     x26, #0                 // queue head
    mov     x27, #1                 // queue tail

    // Enqueue start
    str     x22, [x25]
    str     x23, [x25, #8]
    str     xzr, [x25, #16]

    // Mark start as visited
    mov     x0, x22
    mov     x1, x23
    bl      hash_pos_signed
    adrp    x1, visited@PAGE
    add     x1, x1, visited@PAGEOFF
    adrp    x2, visited_msk@PAGE
    add     x2, x2, visited_msk@PAGEOFF
    ldr     x2, [x2]
    and     x0, x0, x2
    lsl     x0, x0, #4
    add     x1, x1, x0
    mov     x2, x22                 // row
    lsl     x2, x2, #32             // shift to upper 32 bits
    mov     x3, x23                 // col
    ubfx    x3, x3, #0, #32         // extract lower 32 bits
    orr     x2, x2, x3              // combine
    str     x2, [x1]
    str     xzr, [x1, #8]

inf_bfs_loop:
    cmp     x26, x27
    b.ge    inf_bfs_done

    // Check queue size limit
    adrp    x0, queue_sz@PAGE
    add     x0, x0, queue_sz@PAGEOFF
    ldr     x0, [x0]
    sub     x0, x0, #10
    cmp     x27, x0
    b.ge    inf_bfs_done

    // Dequeue
    mov     x0, #24
    mul     x0, x26, x0
    add     x0, x25, x0
    ldr     x1, [x0]                // row (signed)
    ldr     x2, [x0, #8]            // col (signed)
    ldr     x3, [x0, #16]           // dist
    add     x26, x26, #1

    cmp     x3, x19
    b.ge    inf_bfs_loop

    // Try 4 directions
    mov     x28, #0

inf_try_direction:
    cmp     x28, #4
    b.ge    inf_bfs_loop

    mov     x4, x1                  // nr
    mov     x5, x2                  // nc

    cmp     x28, #0
    b.ne    inf_not_up
    sub     x4, x4, #1
    b       inf_check_wall
inf_not_up:
    cmp     x28, #1
    b.ne    inf_not_down
    add     x4, x4, #1
    b       inf_check_wall
inf_not_down:
    cmp     x28, #2
    b.ne    inf_not_left
    sub     x5, x5, #1
    b       inf_check_wall
inf_not_left:
    add     x5, x5, #1

inf_check_wall:
    // Map to grid coordinates (handle negative modulo)
    stp     x1, x2, [sp, #-48]!
    stp     x3, x4, [sp, #16]
    str     x5, [sp, #32]

    // gr = ((nr % rows) + rows) % rows
    mov     x0, x4
    mov     x1, x20
    bl      mod_signed
    mov     x6, x0                  // gr

    ldr     x5, [sp, #32]
    mov     x0, x5
    mov     x1, x21
    bl      mod_signed
    mov     x7, x0                  // gc

    ldp     x3, x4, [sp, #16]
    ldr     x5, [sp, #32]
    ldp     x1, x2, [sp], #48

    // Check wall at (gr, gc)
    mov     x0, #MAX_GRID_SIZE
    mul     x0, x6, x0
    add     x0, x0, x7
    ldrb    w8, [x24, x0]
    cmp     w8, #'#'
    b.eq    inf_next_dir

    // Compute hash for indexing
    stp     x1, x2, [sp, #-48]!
    stp     x3, x4, [sp, #16]
    str     x5, [sp, #32]
    mov     x0, x4
    mov     x1, x5
    bl      hash_pos_signed
    ldp     x3, x4, [sp, #16]
    ldr     x5, [sp, #32]
    ldp     x1, x2, [sp], #48

    adrp    x6, visited@PAGE
    add     x6, x6, visited@PAGEOFF
    adrp    x7, visited_msk@PAGE
    add     x7, x7, visited_msk@PAGEOFF
    ldr     x7, [x7]
    and     x8, x0, x7
    lsl     x8, x8, #4
    add     x6, x6, x8              // x6 = starting slot address

    // Create key from signed coordinates (after hash call so x9 is preserved)
    // row in upper 32 bits, col in lower 32 bits
    mov     x9, x4                  // x9 = row (already 64-bit signed)
    lsl     x9, x9, #32             // shift row to upper 32 bits
    mov     x10, x5                 // x10 = col (64-bit signed)
    ubfx    x10, x10, #0, #32       // extract lower 32 bits
    orr     x9, x9, x10             // combine: key = (row << 32) | (col & 0xFFFFFFFF)

    mov     x10, #0
inf_probe_loop:
    cmp     x10, #1000
    b.ge    inf_next_dir

    ldr     x11, [x6]
    cmp     x11, x9
    b.eq    inf_next_dir            // Already visited

    // Check if empty (compare with empty_marker)
    adrp    x12, empty_marker@PAGE
    add     x12, x12, empty_marker@PAGEOFF
    ldr     x12, [x12]
    cmp     x11, x12
    b.eq    inf_insert_visited

    add     x6, x6, #16
    // Wrap around
    adrp    x12, visited@PAGE
    add     x12, x12, visited@PAGEOFF
    adrp    x13, visited_sz@PAGE
    add     x13, x13, visited_sz@PAGEOFF
    ldr     x13, [x13]
    lsl     x13, x13, #4
    add     x13, x12, x13
    cmp     x6, x13
    b.lt    inf_no_wrap
    mov     x6, x12
inf_no_wrap:
    add     x10, x10, #1
    b       inf_probe_loop

inf_insert_visited:
    add     x11, x3, #1
    str     x9, [x6]
    str     x11, [x6, #8]

    // Enqueue
    mov     x0, #24
    mul     x0, x27, x0
    add     x0, x25, x0
    str     x4, [x0]
    str     x5, [x0, #8]
    str     x11, [x0, #16]
    add     x27, x27, #1

inf_next_dir:
    add     x28, x28, #1
    b       inf_try_direction

inf_bfs_done:
    // Count cells with correct parity
    ldr     x19, [sp, #96]          // Reload steps
    and     x28, x19, #1
    mov     x0, #0

    adrp    x1, visited@PAGE
    add     x1, x1, visited@PAGEOFF
    adrp    x2, visited_sz@PAGE
    add     x2, x2, visited_sz@PAGEOFF
    ldr     x2, [x2]

    // Load empty marker for comparison
    adrp    x6, empty_marker@PAGE
    add     x6, x6, empty_marker@PAGEOFF
    ldr     x6, [x6]

inf_count_loop:
    cbz     x2, inf_count_done

    ldr     x3, [x1]
    cmp     x3, x6                  // Compare with empty marker
    b.eq    inf_count_next

    ldr     x4, [x1, #8]
    cmp     x4, x19
    b.gt    inf_count_next

    and     x5, x4, #1
    cmp     x5, x28
    b.ne    inf_count_next

    add     x0, x0, #1

inf_count_next:
    add     x1, x1, #16
    sub     x2, x2, #1
    b       inf_count_loop

inf_count_done:
    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x23, x24, [sp, #48]
    ldp     x25, x26, [sp, #64]
    ldp     x27, x28, [sp, #80]
    ldp     x29, x30, [sp], #112
    ret

// =============================================================================
// hash_pos_signed: Hash for signed position
// =============================================================================
hash_pos_signed:
    stp     x19, x20, [sp, #-16]!

    adrp    x2, fnv_offset@PAGE
    add     x2, x2, fnv_offset@PAGEOFF
    ldr     x2, [x2]
    adrp    x3, fnv_prime@PAGE
    add     x3, x3, fnv_prime@PAGEOFF
    ldr     x3, [x3]

    eor     x2, x2, x0
    mul     x2, x2, x3
    eor     x2, x2, x1
    mul     x2, x2, x3

    mov     x0, x2
    ldp     x19, x20, [sp], #16
    ret

// =============================================================================
// mod_signed: Compute ((x % m) + m) % m for signed x
// Input: x0 = value (signed), x1 = modulus
// Output: x0 = result (positive)
// =============================================================================
mod_signed:
    sdiv    x2, x0, x1
    msub    x0, x2, x1, x0          // x0 = x0 - (x2 * x1) = remainder
    cmp     x0, #0
    b.ge    mod_done
    add     x0, x0, x1
mod_done:
    ret

// =============================================================================
// solve_part2: Quadratic extrapolation for infinite grid
// Output: x0 = count for 26501365 steps
// =============================================================================
solve_part2:
    stp     x29, x30, [sp, #-80]!
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    mov     x29, sp

    // Grid size = 131, half = 65
    // 26501365 = 65 + 202300 * 131
    // n = 202300

    mov     x19, #GRID_DIM          // size = 131
    mov     x20, #65                // half = size / 2

    // y0 = count_reachable_infinite(65)
    mov     x0, x20
    bl      count_reachable_infinite
    mov     x21, x0                 // y0

    // y1 = count_reachable_infinite(65 + 131)
    add     x0, x20, x19
    bl      count_reachable_infinite
    mov     x22, x0                 // y1

    // y2 = count_reachable_infinite(65 + 262)
    add     x0, x20, x19, lsl #1
    bl      count_reachable_infinite
    mov     x23, x0                 // y2

    // Quadratic fitting: f(n) = an^2 + bn + c
    // Using finite differences:
    // a = (y2 - 2*y1 + y0) / 2
    // b = y1 - y0 - a
    // c = y0

    // a = (y2 - 2*y1 + y0) / 2
    lsl     x0, x22, #1             // 2*y1
    sub     x0, x23, x0             // y2 - 2*y1
    add     x0, x0, x21             // y2 - 2*y1 + y0
    asr     x24, x0, #1             // a = / 2

    // b = y1 - y0 - a
    sub     x25, x22, x21           // y1 - y0
    sub     x25, x25, x24           // b = y1 - y0 - a

    // c = y0
    mov     x26, x21

    // n = 202300
    adrp    x0, n_value@PAGE
    add     x0, x0, n_value@PAGEOFF
    ldr     x0, [x0]

    // result = a*n*n + b*n + c
    mul     x1, x0, x0              // n*n
    mul     x1, x24, x1             // a*n*n
    mul     x2, x25, x0             // b*n
    add     x0, x1, x2              // a*n*n + b*n
    add     x0, x0, x26             // + c

    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x23, x24, [sp, #48]
    ldp     x25, x26, [sp, #64]
    ldp     x29, x30, [sp], #80
    ret

// =============================================================================
// print_string: Write null-terminated string to stdout
// =============================================================================
print_string:
    stp     x29, x30, [sp, #-32]!
    stp     x19, x20, [sp, #16]

    mov     x19, x0
    mov     x20, #0

strlen_loop:
    ldrb    w0, [x19, x20]
    cbz     w0, strlen_done
    add     x20, x20, #1
    b       strlen_loop

strlen_done:
    mov     x0, #1
    mov     x1, x19
    mov     x2, x20
    mov     x16, #4
    svc     #0x80

    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// =============================================================================
// print_number: Print decimal number to stdout
// =============================================================================
print_number:
    stp     x29, x30, [sp, #-48]!
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]

    mov     x19, x0
    adrp    x20, output_buf@PAGE
    add     x20, x20, output_buf@PAGEOFF
    add     x20, x20, #31
    mov     x21, #0

    cbnz    x19, num_loop
    mov     w22, #'0'
    strb    w22, [x20, #-1]!
    mov     x21, #1
    b       num_print

num_loop:
    cbz     x19, num_print

    mov     x1, #10
    udiv    x2, x19, x1
    msub    x3, x2, x1, x19

    add     w3, w3, #'0'
    strb    w3, [x20, #-1]!
    add     x21, x21, #1

    mov     x19, x2
    b       num_loop

num_print:
    mov     x0, #1
    mov     x1, x20
    mov     x2, x21
    mov     x16, #4
    svc     #0x80

    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret
