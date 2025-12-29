// Day 10: Pipe Maze - ARM64 Assembly for macOS
// Part 1: Find the maximum distance in the pipe loop (BFS)
// Part 2: Count tiles enclosed by the loop (ray casting)

.global _start
.align 4

// Constants
.equ BUFFER_SIZE, 32768
.equ MAX_GRID, 150
.equ MAX_QUEUE, 25000
.equ SYS_EXIT, 1
.equ SYS_READ, 3
.equ SYS_WRITE, 4
.equ SYS_OPEN, 5

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

// Grid dimensions
rows:           .word 0
cols:           .word 0
start_row:      .word 0
start_col:      .word 0
start_pipe:     .byte 0
.align 4

.text

_start:
    // Open file
    mov     x16, #5             // SYS_OPEN
    adrp    x0, filename@PAGE
    add     x0, x0, filename@PAGEOFF
    mov     x1, #0              // O_RDONLY
    mov     x2, #0
    svc     #0x80

    // Check for error
    cmp     x0, #0
    b.lt    exit_error

    mov     x19, x0             // Save fd

    // Read file
    mov     x16, #3             // SYS_READ
    mov     x0, x19
    adrp    x1, buffer@PAGE
    add     x1, x1, buffer@PAGEOFF
    mov     x2, #BUFFER_SIZE
    svc     #0x80

    mov     x20, x0             // Save bytes read

    // Close file
    mov     x16, #6             // SYS_CLOSE
    mov     x0, x19
    svc     #0x80

    // Parse grid and find dimensions
    bl      parse_grid

    // Find start position
    bl      find_start

    // Part 1: BFS to find loop and max distance
    bl      find_loop
    mov     x21, x0             // Save part 1 result

    // Part 2: Determine start pipe and count enclosed
    bl      determine_start_pipe
    bl      count_enclosed
    mov     x22, x0             // Save part 2 result

    // Print Part 1
    adrp    x0, part1_msg@PAGE
    add     x0, x0, part1_msg@PAGEOFF
    mov     x1, #8
    bl      print_str

    mov     x0, x21
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    mov     x1, #1
    bl      print_str

    // Print Part 2
    adrp    x0, part2_msg@PAGE
    add     x0, x0, part2_msg@PAGEOFF
    mov     x1, #8
    bl      print_str

    mov     x0, x22
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    mov     x1, #1
    bl      print_str

    // Exit success
    mov     x0, #0
    mov     x16, #1
    svc     #0x80

exit_error:
    mov     x0, #1
    mov     x16, #1
    svc     #0x80

// Parse grid from buffer into grid array
// Sets rows and cols
parse_grid:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    adrp    x19, buffer@PAGE
    add     x19, x19, buffer@PAGEOFF
    adrp    x21, grid@PAGE
    add     x21, x21, grid@PAGEOFF

    mov     x22, #0             // Current row
    mov     x23, #0             // Current col
    mov     x24, #0             // Max cols found
    mov     x25, x20            // Bytes to process

parse_loop:
    cbz     x25, parse_done
    ldrb    w0, [x19], #1
    sub     x25, x25, #1

    cmp     w0, #10             // newline
    b.eq    parse_newline

    cmp     w0, #13             // carriage return - skip
    b.eq    parse_loop

    // Store character using MAX_GRID stride
    mov     x1, x22             // row
    mov     x2, #MAX_GRID
    mul     x1, x1, x2
    add     x1, x1, x23         // + col
    strb    w0, [x21, x1]

    add     x23, x23, #1        // col++
    cmp     x23, x24
    csel    x24, x23, x24, gt   // max_cols = max(max_cols, col)

    b       parse_loop

parse_newline:
    cbz     x23, parse_loop     // Skip empty lines
    add     x22, x22, #1        // row++
    mov     x23, #0             // col = 0
    b       parse_loop

parse_done:
    // If last line didn't end with newline
    cbnz    x23, parse_add_row
    b       parse_store

parse_add_row:
    add     x22, x22, #1

parse_store:
    adrp    x0, rows@PAGE
    add     x0, x0, rows@PAGEOFF
    str     w22, [x0]

    adrp    x0, cols@PAGE
    add     x0, x0, cols@PAGEOFF
    str     w24, [x0]

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Find start position 'S'
find_start:
    stp     x29, x30, [sp, #-16]!

    adrp    x0, grid@PAGE
    add     x0, x0, grid@PAGEOFF
    adrp    x1, rows@PAGE
    add     x1, x1, rows@PAGEOFF
    ldr     w1, [x1]
    adrp    x2, cols@PAGE
    add     x2, x2, cols@PAGEOFF
    ldr     w2, [x2]

    mov     x3, #0              // row
find_start_row:
    cmp     w3, w1
    b.ge    find_start_done
    mov     x4, #0              // col

find_start_col:
    cmp     w4, w2
    b.ge    find_start_next_row

    mov     x5, x3
    mov     x6, #MAX_GRID
    mul     x5, x5, x6
    add     x5, x5, x4
    ldrb    w6, [x0, x5]

    cmp     w6, #'S'
    b.ne    find_start_next_col

    // Found start
    adrp    x7, start_row@PAGE
    add     x7, x7, start_row@PAGEOFF
    str     w3, [x7]
    adrp    x7, start_col@PAGE
    add     x7, x7, start_col@PAGEOFF
    str     w4, [x7]
    b       find_start_done

find_start_next_col:
    add     x4, x4, #1
    b       find_start_col

find_start_next_row:
    add     x3, x3, #1
    b       find_start_row

find_start_done:
    ldp     x29, x30, [sp], #16
    ret

// Get character at (row, col)
// x0 = row, x1 = col
// Returns character in x0
get_char:
    adrp    x2, grid@PAGE
    add     x2, x2, grid@PAGEOFF
    mov     x3, #MAX_GRID
    mul     x3, x0, x3
    add     x3, x3, x1
    ldrb    w0, [x2, x3]
    ret

// Get distance at (row, col)
// x0 = row, x1 = col
// Returns distance in x0 (-1 if not visited)
get_dist:
    adrp    x2, distances@PAGE
    add     x2, x2, distances@PAGEOFF
    mov     x3, #MAX_GRID
    mul     x3, x0, x3
    add     x3, x3, x1
    lsl     x3, x3, #2
    ldr     w0, [x2, x3]
    ret

// Set distance at (row, col)
// x0 = row, x1 = col, x2 = distance
set_dist:
    stp     x19, x20, [sp, #-16]!
    adrp    x3, distances@PAGE
    add     x3, x3, distances@PAGEOFF
    mov     x4, #MAX_GRID
    mul     x4, x0, x4
    add     x4, x4, x1
    lsl     x4, x4, #2
    str     w2, [x3, x4]
    ldp     x19, x20, [sp], #16
    ret

// Check if pipe at (row, col) connects in direction (dr, dc)
// x0 = row, x1 = col, x2 = dr, x3 = dc
// Returns 1 if connects, 0 otherwise
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
    b.ne    pipe_check_dash
    cbnz    w20, pipe_no         // dc must be 0
    b       pipe_yes

pipe_check_dash:
    // - connects E/W (dr=0,dc=-1 or dr=0,dc=1)
    cmp     w4, #'-'
    b.ne    pipe_check_L
    cbnz    w19, pipe_no         // dr must be 0
    b       pipe_yes

pipe_check_L:
    // L connects N/E (dr=-1,dc=0 or dr=0,dc=1)
    cmp     w4, #'L'
    b.ne    pipe_check_J
    // N: dr=-1, dc=0
    cmn     w19, #1
    b.eq    pipe_check_L_n
    // E: dr=0, dc=1
    cbnz    w19, pipe_no
    cmp     w20, #1
    b.eq    pipe_yes
    b       pipe_no
pipe_check_L_n:
    cbz     w20, pipe_yes
    b       pipe_no

pipe_check_J:
    // J connects N/W (dr=-1,dc=0 or dr=0,dc=-1)
    cmp     w4, #'J'
    b.ne    pipe_check_7
    // N: dr=-1, dc=0
    cmn     w19, #1
    b.eq    pipe_check_J_n
    // W: dr=0, dc=-1
    cbnz    w19, pipe_no
    cmn     w20, #1
    b.eq    pipe_yes
    b       pipe_no
pipe_check_J_n:
    cbz     w20, pipe_yes
    b       pipe_no

pipe_check_7:
    // 7 connects S/W (dr=1,dc=0 or dr=0,dc=-1)
    cmp     w4, #'7'
    b.ne    pipe_check_F
    // S: dr=1, dc=0
    cmp     w19, #1
    b.eq    pipe_check_7_s
    // W: dr=0, dc=-1
    cbnz    w19, pipe_no
    cmn     w20, #1
    b.eq    pipe_yes
    b       pipe_no
pipe_check_7_s:
    cbz     w20, pipe_yes
    b       pipe_no

pipe_check_F:
    // F connects S/E (dr=1,dc=0 or dr=0,dc=1)
    cmp     w4, #'F'
    b.ne    pipe_check_S
    // S: dr=1, dc=0
    cmp     w19, #1
    b.eq    pipe_check_F_s
    // E: dr=0, dc=1
    cbnz    w19, pipe_no
    cmp     w20, #1
    b.eq    pipe_yes
    b       pipe_no
pipe_check_F_s:
    cbz     w20, pipe_yes
    b       pipe_no

pipe_check_S:
    // S connects to any adjacent pipe that connects back
    cmp     w4, #'S'
    b.ne    pipe_no
    b       pipe_yes

pipe_yes:
    mov     x0, #1
    b       pipe_done

pipe_no:
    mov     x0, #0

pipe_done:
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// BFS to find loop
// Returns max distance in x0
find_loop:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    // Initialize all distances to -1 (using MAX_GRID x MAX_GRID)
    adrp    x0, distances@PAGE
    add     x0, x0, distances@PAGEOFF
    mov     x1, #MAX_GRID
    mov     x2, #MAX_GRID
    mul     x3, x1, x2          // MAX_GRID * MAX_GRID
    mov     w4, #-1
init_dist_loop:
    cbz     x3, init_dist_done
    str     w4, [x0], #4
    sub     x3, x3, #1
    b       init_dist_loop
init_dist_done:

    // Get start position
    adrp    x19, start_row@PAGE
    add     x19, x19, start_row@PAGEOFF
    ldr     w19, [x19]          // start_row
    adrp    x20, start_col@PAGE
    add     x20, x20, start_col@PAGEOFF
    ldr     w20, [x20]          // start_col

    // Initialize queue with start
    adrp    x21, queue@PAGE
    add     x21, x21, queue@PAGEOFF
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
    adrp    x25, rows@PAGE
    add     x25, x25, rows@PAGEOFF
    ldr     w25, [x25]
    adrp    x26, cols@PAGE
    add     x26, x26, cols@PAGEOFF
    ldr     w26, [x26]

bfs_loop:
    cmp     x22, x23
    b.ge    bfs_done

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

    // Direction N
    sub     w10, w27, #1        // nr = row - 1
    mov     w11, w28            // nc = col
    mov     w12, #-1            // dr = -1
    mov     w13, #0             // dc = 0
    bl      try_neighbor_inline

    // Direction S
    add     w10, w27, #1        // nr = row + 1
    mov     w11, w28            // nc = col
    mov     w12, #1             // dr = 1
    mov     w13, #0             // dc = 0
    bl      try_neighbor_inline

    // Direction W
    mov     w10, w27            // nr = row
    sub     w11, w28, #1        // nc = col - 1
    mov     w12, #0             // dr = 0
    mov     w13, #-1            // dc = -1
    bl      try_neighbor_inline

    // Direction E
    mov     w10, w27            // nr = row
    add     w11, w28, #1        // nc = col + 1
    mov     w12, #0             // dr = 0
    mov     w13, #1             // dc = 1
    bl      try_neighbor_inline

    b       bfs_loop

bfs_done:
    mov     x0, x24

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Try to add neighbor to queue
// w10=nr, w11=nc, w12=dr, w13=dc
// Uses: x21=queue, x23=tail, x25=rows, x26=cols, w27=cur_row, w28=cur_col, w9=cur_dist
try_neighbor_inline:
    stp     x29, x30, [sp, #-16]!
    stp     x9, x10, [sp, #-16]!
    stp     x11, x12, [sp, #-16]!
    stp     x13, x14, [sp, #-16]!

    // Check bounds
    cmp     w10, #0
    b.lt    try_neighbor_skip
    cmp     w10, w25
    b.ge    try_neighbor_skip
    cmp     w11, #0
    b.lt    try_neighbor_skip
    cmp     w11, w26
    b.ge    try_neighbor_skip

    // Check if already visited
    mov     x0, x10
    mov     x1, x11
    bl      get_dist
    cmn     w0, #1              // if dist != -1, already visited
    b.ne    try_neighbor_skip

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
    cbz     x0, try_neighbor_skip

    // Reload saved values
    ldp     x13, x14, [sp]
    ldp     x11, x12, [sp, #16]
    ldp     x9, x10, [sp, #32]

    // Check if neighbor connects back
    // Neighbor at (nr, nc) must connect in direction (-dr, -dc)
    mov     x0, x10             // neighbor row
    mov     x1, x11             // neighbor col
    neg     w2, w12             // -dr
    neg     w3, w13             // -dc
    bl      pipe_connects
    cbz     x0, try_neighbor_skip

    // Reload saved values
    ldp     x13, x14, [sp]
    ldp     x11, x12, [sp, #16]
    ldp     x9, x10, [sp, #32]

    // Add to queue
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

try_neighbor_skip:
    ldp     x13, x14, [sp], #16
    ldp     x11, x12, [sp], #16
    ldp     x9, x10, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Determine what pipe S actually is
determine_start_pipe:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    // Get start position
    adrp    x19, start_row@PAGE
    add     x19, x19, start_row@PAGEOFF
    ldr     w19, [x19]
    adrp    x20, start_col@PAGE
    add     x20, x20, start_col@PAGEOFF
    ldr     w20, [x20]

    // Check which directions S connects to (based on loop membership)
    mov     w21, #0             // connections bitmap: bit0=N, bit1=S, bit2=W, bit3=E

    // Check North
    sub     w0, w19, #1
    mov     w1, w20
    bl      is_loop_member
    cbz     x0, det_check_south
    // Check if north neighbor connects south
    sub     w0, w19, #1
    mov     w1, w20
    mov     w2, #1              // dr=1 (south)
    mov     w3, #0              // dc=0
    bl      pipe_connects
    cbz     x0, det_check_south
    orr     w21, w21, #1        // N connected

det_check_south:
    add     w0, w19, #1
    mov     w1, w20
    bl      is_loop_member
    cbz     x0, det_check_west
    add     w0, w19, #1
    mov     w1, w20
    mov     w2, #-1             // dr=-1 (north)
    mov     w3, #0
    bl      pipe_connects
    cbz     x0, det_check_west
    orr     w21, w21, #2        // S connected

det_check_west:
    mov     w0, w19
    sub     w1, w20, #1
    bl      is_loop_member
    cbz     x0, det_check_east
    mov     w0, w19
    sub     w1, w20, #1
    mov     w2, #0
    mov     w3, #1              // dc=1 (east)
    bl      pipe_connects
    cbz     x0, det_check_east
    orr     w21, w21, #4        // W connected

det_check_east:
    mov     w0, w19
    add     w1, w20, #1
    bl      is_loop_member
    cbz     x0, det_determine
    mov     w0, w19
    add     w1, w20, #1
    mov     w2, #0
    mov     w3, #-1             // dc=-1 (west)
    bl      pipe_connects
    cbz     x0, det_determine
    orr     w21, w21, #8        // E connected

det_determine:
    // Determine pipe type based on connections
    // N+S = | (bits 0,1 = 3)
    // W+E = - (bits 2,3 = 12)
    // N+E = L (bits 0,3 = 9)
    // N+W = J (bits 0,2 = 5)
    // S+W = 7 (bits 1,2 = 6)
    // S+E = F (bits 1,3 = 10)

    mov     w0, #'|'
    cmp     w21, #3
    b.eq    det_store
    mov     w0, #'-'
    cmp     w21, #12
    b.eq    det_store
    mov     w0, #'L'
    cmp     w21, #9
    b.eq    det_store
    mov     w0, #'J'
    cmp     w21, #5
    b.eq    det_store
    mov     w0, #'7'
    cmp     w21, #6
    b.eq    det_store
    mov     w0, #'F'
    cmp     w21, #10
    b.eq    det_store
    mov     w0, #'S'            // fallback

det_store:
    adrp    x1, start_pipe@PAGE
    add     x1, x1, start_pipe@PAGEOFF
    strb    w0, [x1]

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Check if position is in loop (distance != -1)
// w0 = row, w1 = col
// Returns 1 if in loop, 0 otherwise
is_loop_member:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     w19, w0
    mov     w20, w1

    // Check bounds first
    adrp    x2, rows@PAGE
    add     x2, x2, rows@PAGEOFF
    ldr     w2, [x2]
    adrp    x3, cols@PAGE
    add     x3, x3, cols@PAGEOFF
    ldr     w3, [x3]

    cmp     w19, #0
    b.lt    is_loop_no
    cmp     w19, w2
    b.ge    is_loop_no
    cmp     w20, #0
    b.lt    is_loop_no
    cmp     w20, w3
    b.ge    is_loop_no

    mov     x0, x19
    mov     x1, x20
    bl      get_dist

    cmn     w0, #1
    b.eq    is_loop_no

    mov     x0, #1
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

is_loop_no:
    mov     x0, #0
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Count enclosed tiles using ray casting
// Returns count in x0
count_enclosed:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    adrp    x19, rows@PAGE
    add     x19, x19, rows@PAGEOFF
    ldr     w19, [x19]
    adrp    x20, cols@PAGE
    add     x20, x20, cols@PAGEOFF
    ldr     w20, [x20]

    adrp    x21, start_row@PAGE
    add     x21, x21, start_row@PAGEOFF
    ldr     w21, [x21]
    adrp    x22, start_col@PAGE
    add     x22, x22, start_col@PAGEOFF
    ldr     w22, [x22]
    adrp    x23, start_pipe@PAGE
    add     x23, x23, start_pipe@PAGEOFF
    ldrb    w23, [x23]

    mov     x24, #0             // enclosed count
    mov     x25, #0             // current row

count_row_loop:
    cmp     w25, w19
    b.ge    count_done

    mov     x26, #0             // current col
    mov     w27, #0             // inside = false

count_col_loop:
    cmp     w26, w20
    b.ge    count_next_row

    // Check if this position is in loop
    mov     w0, w25
    mov     w1, w26
    bl      is_loop_member
    cbz     x0, count_not_loop

    // Get character at this position
    mov     x0, x25
    mov     x1, x26
    bl      get_char
    mov     w28, w0

    // If it's S, use the determined pipe type
    cmp     w28, #'S'
    b.ne    count_check_north
    mov     w28, w23            // use start_pipe

count_check_north:
    // Count pipes with north connection: |, L, J
    cmp     w28, #'|'
    b.eq    count_toggle
    cmp     w28, #'L'
    b.eq    count_toggle
    cmp     w28, #'J'
    b.eq    count_toggle
    b       count_next_col

count_toggle:
    eor     w27, w27, #1        // toggle inside
    b       count_next_col

count_not_loop:
    // Not part of loop - count if inside
    cbz     w27, count_next_col
    add     x24, x24, #1

count_next_col:
    add     x26, x26, #1
    b       count_col_loop

count_next_row:
    add     x25, x25, #1
    b       count_row_loop

count_done:
    mov     x0, x24

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Print string
// x0 = string pointer, x1 = length
print_str:
    mov     x2, x1
    mov     x1, x0
    mov     x0, #1
    mov     x16, #4
    svc     #0x80
    ret

// Print number
// x0 = number to print
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0
    adrp    x20, num_buffer@PAGE
    add     x20, x20, num_buffer@PAGEOFF
    add     x20, x20, #30       // End of buffer
    mov     x1, #0              // Length

    // Handle 0 case
    cbnz    x19, print_num_loop
    mov     w2, #'0'
    sub     x20, x20, #1
    strb    w2, [x20]
    mov     x1, #1
    b       print_num_out

print_num_loop:
    cbz     x19, print_num_out
    mov     x2, #10
    udiv    x3, x19, x2
    msub    x4, x3, x2, x19     // remainder
    add     w4, w4, #'0'
    sub     x20, x20, #1
    strb    w4, [x20]
    add     x1, x1, #1
    mov     x19, x3
    b       print_num_loop

print_num_out:
    mov     x0, x20
    bl      print_str

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
