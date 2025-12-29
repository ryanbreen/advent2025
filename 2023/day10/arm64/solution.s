// ============================================================================
// Day 10: Pipe Maze - ARM64 Assembly for macOS
// ============================================================================
//
// Algorithm Overview:
// ------------------
// This solution solves the Advent of Code Day 10 "Pipe Maze" puzzle where we
// navigate a grid of pipes starting from position 'S'.
//
// Part 1: Find the maximum distance from S in the pipe loop
//   - Use Breadth-First Search (BFS) starting from 'S'
//   - Track distances to each reachable pipe segment
//   - Return the maximum distance (farthest point in the loop)
//
// Part 2: Count tiles enclosed by the pipe loop
//   - First determine what pipe type 'S' actually represents
//   - Use ray casting algorithm: scan each row left-to-right
//   - Track "inside/outside" state by counting north-connecting pipes
//   - Pipes with north connection (|, L, J) toggle inside/outside state
//   - Count non-loop tiles that are in "inside" state
//
// Data Structures:
//   - grid[]: Character grid storing the pipe maze
//   - distances[]: BFS distance from start (-1 = not visited)
//   - queue[]: BFS queue storing (row, col) pairs
//
// ============================================================================

.global _start
.align 4

// ============================================================================
// Constants
// ============================================================================
.equ BUFFER_SIZE, 32768
.equ MAX_GRID, 150
.equ MAX_QUEUE, 25000

// System call numbers (macOS ARM64)
.equ SYS_EXIT, 1
.equ SYS_READ, 3
.equ SYS_WRITE, 4
.equ SYS_OPEN, 5
.equ SYS_CLOSE, 6

// ============================================================================
// Macros for address loading (reduces repetitive adrp+add patterns)
// ============================================================================

// Load address of a symbol into register
.macro load_addr reg, symbol
    adrp    \reg, \symbol@PAGE
    add     \reg, \reg, \symbol@PAGEOFF
.endm

// Load word value from a symbol into register
.macro load_word reg, symbol, tmp
    adrp    \tmp, \symbol@PAGE
    add     \tmp, \tmp, \symbol@PAGEOFF
    ldr     \reg, [\tmp]
.endm

// Load byte value from a symbol into register
.macro load_byte reg, symbol, tmp
    adrp    \tmp, \symbol@PAGE
    add     \tmp, \tmp, \symbol@PAGEOFF
    ldrb    \reg, [\tmp]
.endm

// Store word value to a symbol
.macro store_word reg, symbol, tmp
    adrp    \tmp, \symbol@PAGE
    add     \tmp, \tmp, \symbol@PAGEOFF
    str     \reg, [\tmp]
.endm

// Store byte value to a symbol
.macro store_byte reg, symbol, tmp
    adrp    \tmp, \symbol@PAGE
    add     \tmp, \tmp, \symbol@PAGEOFF
    strb    \reg, [\tmp]
.endm

// ============================================================================
// Data Section
// ============================================================================
.data
filename:       .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"

.align 4
buffer:         .space BUFFER_SIZE
grid:           .space MAX_GRID * MAX_GRID      // The grid (flattened)
distances:      .space MAX_GRID * MAX_GRID * 4  // Distance from start (4 bytes each, -1 = not visited)
queue:          .space MAX_QUEUE * 8            // BFS queue (row, col pairs)
num_buffer:     .space 32                       // For number to string conversion

// Grid dimensions and start position
rows:           .word 0
cols:           .word 0
start_row:      .word 0
start_col:      .word 0
start_pipe:     .byte 0
.align 4

// ============================================================================
// Text Section
// ============================================================================
.text

_start:
    // Open input file
    mov     x16, #SYS_OPEN
    load_addr x0, filename
    mov     x1, #0              // O_RDONLY
    mov     x2, #0
    svc     #0x80

    // Check for error
    cmp     x0, #0
    b.lt    exit_error

    mov     x19, x0             // Save file descriptor

    // Read file contents
    mov     x16, #SYS_READ
    mov     x0, x19
    load_addr x1, buffer
    mov     x2, #BUFFER_SIZE
    svc     #0x80

    mov     x20, x0             // Save bytes read

    // Close file
    mov     x16, #SYS_CLOSE
    mov     x0, x19
    svc     #0x80

    // Parse grid and find dimensions
    bl      parse_grid

    // Find start position 'S'
    bl      find_start

    // Part 1: BFS to find loop and max distance
    bl      find_loop
    mov     x21, x0             // Save part 1 result

    // Part 2: Determine start pipe type and count enclosed tiles
    bl      determine_start_pipe
    bl      count_enclosed
    mov     x22, x0             // Save part 2 result

    // Print Part 1 result
    load_addr x0, part1_msg
    mov     x1, #8
    bl      print_str

    mov     x0, x21
    bl      print_num

    load_addr x0, newline
    mov     x1, #1
    bl      print_str

    // Print Part 2 result
    load_addr x0, part2_msg
    mov     x1, #8
    bl      print_str

    mov     x0, x22
    bl      print_num

    load_addr x0, newline
    mov     x1, #1
    bl      print_str

    // Exit success
    mov     x0, #0
    mov     x16, #SYS_EXIT
    svc     #0x80

exit_error:
    mov     x0, #1
    mov     x16, #SYS_EXIT
    svc     #0x80

// ============================================================================
// parse_grid: Parse input buffer into grid array
// Sets: rows, cols global variables
// ============================================================================
parse_grid:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    load_addr x19, buffer
    load_addr x21, grid

    mov     x22, #0             // Current row
    mov     x23, #0             // Current col
    mov     x24, #0             // Max cols found
    mov     x25, x20            // Bytes to process

parse_grid_loop:
    cbz     x25, parse_grid_done
    ldrb    w0, [x19], #1
    sub     x25, x25, #1

    cmp     w0, #10             // newline
    b.eq    parse_grid_newline

    cmp     w0, #13             // carriage return - skip
    b.eq    parse_grid_loop

    // Store character using MAX_GRID stride
    mov     x1, x22             // row
    mov     x2, #MAX_GRID
    mul     x1, x1, x2
    add     x1, x1, x23         // + col
    strb    w0, [x21, x1]

    add     x23, x23, #1        // col++
    cmp     x23, x24
    csel    x24, x23, x24, gt   // max_cols = max(max_cols, col)

    b       parse_grid_loop

parse_grid_newline:
    cbz     x23, parse_grid_loop     // Skip empty lines
    add     x22, x22, #1             // row++
    mov     x23, #0                  // col = 0
    b       parse_grid_loop

parse_grid_done:
    // If last line didn't end with newline
    cbnz    x23, parse_grid_add_row
    b       parse_grid_store

parse_grid_add_row:
    add     x22, x22, #1

parse_grid_store:
    store_word w22, rows, x0
    store_word w24, cols, x0

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// find_start: Locate the 'S' position in the grid
// Sets: start_row, start_col global variables
// ============================================================================
find_start:
    stp     x29, x30, [sp, #-16]!

    load_addr x0, grid
    load_word w1, rows, x1
    load_word w2, cols, x2

    mov     x3, #0              // row

find_start_row_loop:
    cmp     w3, w1
    b.ge    find_start_done
    mov     x4, #0              // col

find_start_col_loop:
    cmp     w4, w2
    b.ge    find_start_next_row

    mov     x5, x3
    mov     x6, #MAX_GRID
    mul     x5, x5, x6
    add     x5, x5, x4
    ldrb    w6, [x0, x5]

    cmp     w6, #'S'
    b.ne    find_start_next_col

    // Found start position
    store_word w3, start_row, x7
    store_word w4, start_col, x7
    b       find_start_done

find_start_next_col:
    add     x4, x4, #1
    b       find_start_col_loop

find_start_next_row:
    add     x3, x3, #1
    b       find_start_row_loop

find_start_done:
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// get_char: Get character at grid position
// Input: x0 = row, x1 = col
// Output: x0 = character
// ============================================================================
get_char:
    load_addr x2, grid
    mov     x3, #MAX_GRID
    mul     x3, x0, x3
    add     x3, x3, x1
    ldrb    w0, [x2, x3]
    ret

// ============================================================================
// get_dist: Get distance at grid position
// Input: x0 = row, x1 = col
// Output: x0 = distance (-1 if not visited)
// ============================================================================
get_dist:
    load_addr x2, distances
    mov     x3, #MAX_GRID
    mul     x3, x0, x3
    add     x3, x3, x1
    lsl     x3, x3, #2
    ldr     w0, [x2, x3]
    ret

// ============================================================================
// set_dist: Set distance at grid position
// Input: x0 = row, x1 = col, x2 = distance
// ============================================================================
set_dist:
    load_addr x3, distances
    mov     x4, #MAX_GRID
    mul     x4, x0, x4
    add     x4, x4, x1
    lsl     x4, x4, #2
    str     w2, [x3, x4]
    ret

// ============================================================================
// pipe_connects: Check if pipe at position connects in given direction
// Input: x0 = row, x1 = col, x2 = dr, x3 = dc
// Output: x0 = 1 if connects, 0 otherwise
// ============================================================================
pipe_connects:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x2             // save dr
    mov     x20, x3             // save dc
    bl      get_char
    mov     w4, w0              // w4 = character

    // Check based on pipe type
    // | connects N/S (dr=-1,dc=0 or dr=1,dc=0)
    cmp     w4, #'|'
    b.ne    pipe_connects_check_dash
    cbnz    w20, pipe_connects_no         // dc must be 0
    b       pipe_connects_yes

pipe_connects_check_dash:
    // - connects E/W (dr=0,dc=-1 or dr=0,dc=1)
    cmp     w4, #'-'
    b.ne    pipe_connects_check_L
    cbnz    w19, pipe_connects_no         // dr must be 0
    b       pipe_connects_yes

pipe_connects_check_L:
    // L connects N/E (dr=-1,dc=0 or dr=0,dc=1)
    cmp     w4, #'L'
    b.ne    pipe_connects_check_J
    // N: dr=-1, dc=0
    cmn     w19, #1
    b.eq    pipe_connects_check_L_north
    // E: dr=0, dc=1
    cbnz    w19, pipe_connects_no
    cmp     w20, #1
    b.eq    pipe_connects_yes
    b       pipe_connects_no
pipe_connects_check_L_north:
    cbz     w20, pipe_connects_yes
    b       pipe_connects_no

pipe_connects_check_J:
    // J connects N/W (dr=-1,dc=0 or dr=0,dc=-1)
    cmp     w4, #'J'
    b.ne    pipe_connects_check_7
    // N: dr=-1, dc=0
    cmn     w19, #1
    b.eq    pipe_connects_check_J_north
    // W: dr=0, dc=-1
    cbnz    w19, pipe_connects_no
    cmn     w20, #1
    b.eq    pipe_connects_yes
    b       pipe_connects_no
pipe_connects_check_J_north:
    cbz     w20, pipe_connects_yes
    b       pipe_connects_no

pipe_connects_check_7:
    // 7 connects S/W (dr=1,dc=0 or dr=0,dc=-1)
    cmp     w4, #'7'
    b.ne    pipe_connects_check_F
    // S: dr=1, dc=0
    cmp     w19, #1
    b.eq    pipe_connects_check_7_south
    // W: dr=0, dc=-1
    cbnz    w19, pipe_connects_no
    cmn     w20, #1
    b.eq    pipe_connects_yes
    b       pipe_connects_no
pipe_connects_check_7_south:
    cbz     w20, pipe_connects_yes
    b       pipe_connects_no

pipe_connects_check_F:
    // F connects S/E (dr=1,dc=0 or dr=0,dc=1)
    cmp     w4, #'F'
    b.ne    pipe_connects_check_S
    // S: dr=1, dc=0
    cmp     w19, #1
    b.eq    pipe_connects_check_F_south
    // E: dr=0, dc=1
    cbnz    w19, pipe_connects_no
    cmp     w20, #1
    b.eq    pipe_connects_yes
    b       pipe_connects_no
pipe_connects_check_F_south:
    cbz     w20, pipe_connects_yes
    b       pipe_connects_no

pipe_connects_check_S:
    // S connects to any adjacent pipe that connects back
    cmp     w4, #'S'
    b.ne    pipe_connects_no
    b       pipe_connects_yes

pipe_connects_yes:
    mov     x0, #1
    b       pipe_connects_done

pipe_connects_no:
    mov     x0, #0

pipe_connects_done:
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// find_loop: BFS to find the pipe loop and calculate maximum distance
// Output: x0 = maximum distance from start
// ============================================================================
find_loop:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    // Initialize all distances to -1 (not visited)
    load_addr x0, distances
    mov     x1, #MAX_GRID
    mov     x2, #MAX_GRID
    mul     x3, x1, x2          // MAX_GRID * MAX_GRID
    mov     w4, #-1
find_loop_init_distances:
    cbz     x3, find_loop_init_done
    str     w4, [x0], #4
    sub     x3, x3, #1
    b       find_loop_init_distances
find_loop_init_done:

    // Get start position
    load_word w19, start_row, x19
    load_word w20, start_col, x20

    // Initialize BFS queue with start position
    load_addr x21, queue
    mov     x22, #0             // queue head
    mov     x23, #1             // queue tail (next insert pos)

    // Store start in queue
    str     w19, [x21]
    str     w20, [x21, #4]

    // Set distance[start] = 0
    mov     x0, x19
    mov     x1, x20
    mov     x2, #0
    bl      set_dist

    mov     x24, #0             // max_dist

    // Load grid dimensions
    load_word w25, rows, x25
    load_word w26, cols, x26

find_loop_bfs:
    cmp     x22, x23
    b.ge    find_loop_done

    // Pop from queue
    lsl     x0, x22, #3
    add     x0, x21, x0
    ldr     w27, [x0]           // current row
    ldr     w28, [x0, #4]       // current col
    add     x22, x22, #1

    // Get current distance
    mov     x0, x27
    mov     x1, x28
    bl      get_dist
    mov     w9, w0              // w9 = current distance

    // Update max_dist
    cmp     w9, w24
    csel    w24, w9, w24, gt

    // Try all 4 directions: N(-1,0), S(1,0), W(0,-1), E(0,1)

    // Direction North
    sub     w10, w27, #1        // nr = row - 1
    mov     w11, w28            // nc = col
    mov     w12, #-1            // dr = -1
    mov     w13, #0             // dc = 0
    bl      try_add_neighbor

    // Direction South
    add     w10, w27, #1        // nr = row + 1
    mov     w11, w28            // nc = col
    mov     w12, #1             // dr = 1
    mov     w13, #0             // dc = 0
    bl      try_add_neighbor

    // Direction West
    mov     w10, w27            // nr = row
    sub     w11, w28, #1        // nc = col - 1
    mov     w12, #0             // dr = 0
    mov     w13, #-1            // dc = -1
    bl      try_add_neighbor

    // Direction East
    mov     w10, w27            // nr = row
    add     w11, w28, #1        // nc = col + 1
    mov     w12, #0             // dr = 0
    mov     w13, #1             // dc = 1
    bl      try_add_neighbor

    b       find_loop_bfs

find_loop_done:
    mov     x0, x24

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// try_add_neighbor: Attempt to add a neighboring position to BFS queue
// Input: w10=nr, w11=nc, w12=dr, w13=dc
// Uses: x21=queue, x23=tail, x25=rows, x26=cols, w27=cur_row, w28=cur_col, w9=cur_dist
// ============================================================================
try_add_neighbor:
    stp     x29, x30, [sp, #-16]!
    stp     x9, x10, [sp, #-16]!
    stp     x11, x12, [sp, #-16]!
    stp     x13, x14, [sp, #-16]!

    // Check bounds
    cmp     w10, #0
    b.lt    try_add_neighbor_skip
    cmp     w10, w25
    b.ge    try_add_neighbor_skip
    cmp     w11, #0
    b.lt    try_add_neighbor_skip
    cmp     w11, w26
    b.ge    try_add_neighbor_skip

    // Check if already visited
    mov     x0, x10
    mov     x1, x11
    bl      get_dist
    cmn     w0, #1              // if dist != -1, already visited
    b.ne    try_add_neighbor_skip

    // Reload saved values
    ldp     x13, x14, [sp]
    ldp     x11, x12, [sp, #16]
    ldp     x9, x10, [sp, #32]

    // Check if current position connects in this direction
    mov     x0, x27             // current row
    mov     x1, x28             // current col
    mov     x2, x12             // dr
    mov     x3, x13             // dc
    bl      pipe_connects
    cbz     x0, try_add_neighbor_skip

    // Reload saved values
    ldp     x13, x14, [sp]
    ldp     x11, x12, [sp, #16]
    ldp     x9, x10, [sp, #32]

    // Check if neighbor connects back (in opposite direction)
    mov     x0, x10             // neighbor row
    mov     x1, x11             // neighbor col
    neg     w2, w12             // -dr
    neg     w3, w13             // -dc
    bl      pipe_connects
    cbz     x0, try_add_neighbor_skip

    // Reload saved values
    ldp     x13, x14, [sp]
    ldp     x11, x12, [sp, #16]
    ldp     x9, x10, [sp, #32]

    // Add neighbor to queue
    lsl     x0, x23, #3
    add     x0, x21, x0
    str     w10, [x0]
    str     w11, [x0, #4]
    add     x23, x23, #1

    // Set distance = current_dist + 1
    mov     x0, x10
    mov     x1, x11
    add     w2, w9, #1
    bl      set_dist

try_add_neighbor_skip:
    ldp     x13, x14, [sp], #16
    ldp     x11, x12, [sp], #16
    ldp     x9, x10, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// determine_start_pipe: Determine what pipe type 'S' actually represents
// Sets: start_pipe global variable
// ============================================================================
determine_start_pipe:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    // Get start position
    load_word w19, start_row, x19
    load_word w20, start_col, x20

    // Check which directions S connects to (based on loop membership)
    // Connection bitmap: bit0=N, bit1=S, bit2=W, bit3=E
    mov     w21, #0

    // Check North neighbor
    sub     w0, w19, #1
    mov     w1, w20
    bl      is_loop_member
    cbz     x0, determine_start_check_south
    // Check if north neighbor connects south
    sub     w0, w19, #1
    mov     w1, w20
    mov     w2, #1              // dr=1 (south)
    mov     w3, #0              // dc=0
    bl      pipe_connects
    cbz     x0, determine_start_check_south
    orr     w21, w21, #1        // North connected

determine_start_check_south:
    add     w0, w19, #1
    mov     w1, w20
    bl      is_loop_member
    cbz     x0, determine_start_check_west
    add     w0, w19, #1
    mov     w1, w20
    mov     w2, #-1             // dr=-1 (north)
    mov     w3, #0
    bl      pipe_connects
    cbz     x0, determine_start_check_west
    orr     w21, w21, #2        // South connected

determine_start_check_west:
    mov     w0, w19
    sub     w1, w20, #1
    bl      is_loop_member
    cbz     x0, determine_start_check_east
    mov     w0, w19
    sub     w1, w20, #1
    mov     w2, #0
    mov     w3, #1              // dc=1 (east)
    bl      pipe_connects
    cbz     x0, determine_start_check_east
    orr     w21, w21, #4        // West connected

determine_start_check_east:
    mov     w0, w19
    add     w1, w20, #1
    bl      is_loop_member
    cbz     x0, determine_start_resolve_type
    mov     w0, w19
    add     w1, w20, #1
    mov     w2, #0
    mov     w3, #-1             // dc=-1 (west)
    bl      pipe_connects
    cbz     x0, determine_start_resolve_type
    orr     w21, w21, #8        // East connected

determine_start_resolve_type:
    // Determine pipe type based on connection bitmap
    // N+S = | (bits 0,1 = 3)
    // W+E = - (bits 2,3 = 12)
    // N+E = L (bits 0,3 = 9)
    // N+W = J (bits 0,2 = 5)
    // S+W = 7 (bits 1,2 = 6)
    // S+E = F (bits 1,3 = 10)

    mov     w0, #'|'
    cmp     w21, #3
    b.eq    determine_start_store_result
    mov     w0, #'-'
    cmp     w21, #12
    b.eq    determine_start_store_result
    mov     w0, #'L'
    cmp     w21, #9
    b.eq    determine_start_store_result
    mov     w0, #'J'
    cmp     w21, #5
    b.eq    determine_start_store_result
    mov     w0, #'7'
    cmp     w21, #6
    b.eq    determine_start_store_result
    mov     w0, #'F'
    cmp     w21, #10
    b.eq    determine_start_store_result
    mov     w0, #'S'            // fallback (should not happen)

determine_start_store_result:
    store_byte w0, start_pipe, x1

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// is_loop_member: Check if a position is part of the pipe loop
// Input: w0 = row, w1 = col
// Output: x0 = 1 if in loop, 0 otherwise
// ============================================================================
is_loop_member:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     w19, w0
    mov     w20, w1

    // Check bounds first
    load_word w2, rows, x2
    load_word w3, cols, x3

    cmp     w19, #0
    b.lt    is_loop_member_no
    cmp     w19, w2
    b.ge    is_loop_member_no
    cmp     w20, #0
    b.lt    is_loop_member_no
    cmp     w20, w3
    b.ge    is_loop_member_no

    mov     x0, x19
    mov     x1, x20
    bl      get_dist

    cmn     w0, #1
    b.eq    is_loop_member_no

    mov     x0, #1
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

is_loop_member_no:
    mov     x0, #0
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// count_enclosed: Count tiles enclosed by the pipe loop using ray casting
// Output: x0 = count of enclosed tiles
// ============================================================================
count_enclosed:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    load_word w19, rows, x19
    load_word w20, cols, x20
    load_word w21, start_row, x21
    load_word w22, start_col, x22
    load_byte w23, start_pipe, x23

    mov     x24, #0             // enclosed count
    mov     x25, #0             // current row

count_enclosed_row_loop:
    cmp     w25, w19
    b.ge    count_enclosed_done

    mov     x26, #0             // current col
    mov     w27, #0             // inside = false

count_enclosed_col_loop:
    cmp     w26, w20
    b.ge    count_enclosed_next_row

    // Check if this position is in loop
    mov     w0, w25
    mov     w1, w26
    bl      is_loop_member
    cbz     x0, count_enclosed_not_loop

    // Get character at this position
    mov     x0, x25
    mov     x1, x26
    bl      get_char
    mov     w28, w0

    // If it's S, use the determined pipe type
    cmp     w28, #'S'
    b.ne    count_enclosed_check_north
    mov     w28, w23            // use start_pipe

count_enclosed_check_north:
    // Count pipes with north connection: |, L, J toggle inside/outside
    cmp     w28, #'|'
    b.eq    count_enclosed_toggle
    cmp     w28, #'L'
    b.eq    count_enclosed_toggle
    cmp     w28, #'J'
    b.eq    count_enclosed_toggle
    b       count_enclosed_next_col

count_enclosed_toggle:
    eor     w27, w27, #1        // toggle inside state
    b       count_enclosed_next_col

count_enclosed_not_loop:
    // Not part of loop - count if currently inside
    cbz     w27, count_enclosed_next_col
    add     x24, x24, #1

count_enclosed_next_col:
    add     x26, x26, #1
    b       count_enclosed_col_loop

count_enclosed_next_row:
    add     x25, x25, #1
    b       count_enclosed_row_loop

count_enclosed_done:
    mov     x0, x24

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print a string to stdout
// Input: x0 = string pointer, x1 = length
// ============================================================================
print_str:
    mov     x2, x1
    mov     x1, x0
    mov     x0, #1
    mov     x16, #SYS_WRITE
    svc     #0x80
    ret

// ============================================================================
// print_num: Print a decimal number to stdout
// Input: x0 = number to print
// ============================================================================
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0
    load_addr x20, num_buffer
    add     x20, x20, #30       // End of buffer
    mov     x1, #0              // Length

    // Handle 0 case
    cbnz    x19, print_num_loop
    mov     w2, #'0'
    sub     x20, x20, #1
    strb    w2, [x20]
    mov     x1, #1
    b       print_num_output

print_num_loop:
    cbz     x19, print_num_output
    mov     x2, #10
    udiv    x3, x19, x2
    msub    x4, x3, x2, x19     // remainder
    add     w4, w4, #'0'
    sub     x20, x20, #1
    strb    w4, [x20]
    add     x1, x1, #1
    mov     x19, x3
    b       print_num_loop

print_num_output:
    mov     x0, x20
    bl      print_str

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
