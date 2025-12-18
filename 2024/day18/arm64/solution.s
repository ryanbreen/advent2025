// Day 18: RAM Run - ARM64 Assembly (macOS)
//
// Part 1: BFS to find shortest path on 71x71 grid after 1024 bytes corrupted
// Part 2: Binary search + BFS to find first byte that blocks all paths

.global _start
.align 4

// Constants
.equ MAX_INPUT_SIZE, 65536
.equ GRID_SIZE, 71
.equ MAX_POSITIONS, 4096
.equ MAX_QUEUE, 8192
.equ NUM_BYTES_P1, 1024

// Macro for loading addresses from data section
.macro LOAD_ADDR reg, label
    adrp    \reg, \label@PAGE
    add     \reg, \reg, \label@PAGEOFF
.endm

// ============================================================================
// Data Section
// ============================================================================
.data

// String constants
input_path:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
comma:          .asciz ","
newline:        .asciz "\n"
error_msg:      .asciz "Error reading file\n"

.align 3
// File I/O buffer
file_buffer:    .space MAX_INPUT_SIZE

// Parsed positions (x, y pairs as 32-bit integers)
positions:      .space MAX_POSITIONS * 8
num_positions:  .quad 0

// Grid storage (0 = free, 1 = corrupted)
grid:           .space GRID_SIZE * GRID_SIZE

// BFS queue (stores cell indices)
queue:          .space MAX_QUEUE * 4
queue_head:     .quad 0
queue_tail:     .quad 0

// Visited array for BFS
visited:        .space GRID_SIZE * GRID_SIZE

// Distance array for BFS
distances:      .space GRID_SIZE * GRID_SIZE * 4

// Directions: up, right, down, left (dy, dx pairs)
directions:     .word -1, 0     // up
                .word 0, 1      // right
                .word 1, 0      // down
                .word 0, -1     // left

// Results for Part 2
blocking_x:     .quad 0
blocking_y:     .quad 0

// ============================================================================
// Code Section
// ============================================================================
.text

// ============================================================================
// Main entry point
// ============================================================================
_start:
    // Open input file
    LOAD_ADDR x0, input_path
    mov     x1, #0                          // O_RDONLY
    mov     x2, #0
    mov     x16, #5                         // open() syscall
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit

    mov     x19, x0                         // Save fd

    // Read file
    mov     x0, x19
    LOAD_ADDR x1, file_buffer
    mov     x2, #MAX_INPUT_SIZE
    mov     x16, #3                         // read() syscall
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit

    mov     x20, x0                         // Save bytes read

    // Close file
    mov     x0, x19
    mov     x16, #6                         // close() syscall
    svc     #0x80

    // Parse input
    bl      parse_input

    // Solve Part 1
    bl      solve_part1
    mov     x21, x0                         // Save part1 result

    // Solve Part 2
    bl      solve_part2

    // Print Part 1 result
    LOAD_ADDR x0, part1_msg
    bl      print_str
    mov     x0, x21
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Print Part 2 result
    LOAD_ADDR x0, part2_msg
    bl      print_str
    LOAD_ADDR x0, blocking_x
    ldr     x0, [x0]
    bl      print_num
    LOAD_ADDR x0, comma
    bl      print_str
    LOAD_ADDR x0, blocking_y
    ldr     x0, [x0]
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Exit successfully
    mov     x0, #0
    mov     x16, #1                         // exit() syscall
    svc     #0x80

error_exit:
    LOAD_ADDR x0, error_msg
    bl      print_str
    mov     x0, #1
    mov     x16, #1
    svc     #0x80

// ============================================================================
// parse_input: Parse comma-separated x,y positions
// ============================================================================
parse_input:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x19, file_buffer              // Input pointer
    LOAD_ADDR x20, positions                // Output pointer
    mov     x21, #0                         // Position count

parse_loop:
    // Skip to start of number or end
    ldrb    w0, [x19]
    cbz     w0, parse_done

    // Skip non-digit characters (except we need digits)
    cmp     w0, #'0'
    b.lt    skip_non_digit
    cmp     w0, #'9'
    b.le    parse_x
skip_non_digit:
    add     x19, x19, #1
    b       parse_loop

parse_x:
    // Parse x coordinate
    mov     x22, #0                         // x value
parse_x_loop:
    ldrb    w0, [x19]
    cmp     w0, #'0'
    b.lt    parse_x_done
    cmp     w0, #'9'
    b.gt    parse_x_done
    sub     w0, w0, #'0'
    mov     x1, #10
    mul     x22, x22, x1
    add     x22, x22, x0
    add     x19, x19, #1
    b       parse_x_loop

parse_x_done:
    // Store x
    str     w22, [x20], #4

    // Skip comma
    ldrb    w0, [x19]
    cmp     w0, #','
    b.ne    parse_y                         // Skip if not comma
    add     x19, x19, #1

parse_y:
    // Parse y coordinate
    mov     x22, #0                         // y value
parse_y_loop:
    ldrb    w0, [x19]
    cmp     w0, #'0'
    b.lt    parse_y_done
    cmp     w0, #'9'
    b.gt    parse_y_done
    sub     w0, w0, #'0'
    mov     x1, #10
    mul     x22, x22, x1
    add     x22, x22, x0
    add     x19, x19, #1
    b       parse_y_loop

parse_y_done:
    // Store y
    str     w22, [x20], #4
    add     x21, x21, #1

    // Continue to next line
    b       parse_loop

parse_done:
    // Save count
    LOAD_ADDR x0, num_positions
    str     x21, [x0]

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// setup_grid: Mark first n_bytes positions as corrupted
// Input: x0 = number of bytes to mark
// ============================================================================
setup_grid:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0                         // n_bytes

    // Clear grid
    LOAD_ADDR x0, grid
    mov     x1, #GRID_SIZE * GRID_SIZE
clear_grid:
    cbz     x1, clear_done
    strb    wzr, [x0], #1
    sub     x1, x1, #1
    b       clear_grid

clear_done:
    // Mark corrupted positions
    LOAD_ADDR x20, positions
    mov     x21, #0                         // index

mark_loop:
    cmp     x21, x19
    b.ge    mark_done

    // Load x, y
    ldr     w22, [x20], #4                  // x
    ldr     w0, [x20], #4                   // y

    // Calculate grid index: y * GRID_SIZE + x
    mov     w1, #GRID_SIZE
    mul     w1, w0, w1
    add     w1, w1, w22

    // Mark as corrupted
    LOAD_ADDR x2, grid
    mov     w3, #1
    strb    w3, [x2, x1]

    add     x21, x21, #1
    b       mark_loop

mark_done:
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// bfs: Find shortest path from (0,0) to (70,70)
// Returns: x0 = distance, or -1 if no path
// ============================================================================
bfs:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    // Check if start or goal is corrupted
    LOAD_ADDR x0, grid
    ldrb    w1, [x0]                        // (0,0)
    cbnz    w1, no_path

    mov     w2, #GRID_SIZE * GRID_SIZE - 1
    ldrb    w1, [x0, x2]                    // (70,70)
    cbnz    w1, no_path

    // Clear visited
    LOAD_ADDR x0, visited
    mov     x1, #GRID_SIZE * GRID_SIZE
clear_visited:
    cbz     x1, init_queue
    strb    wzr, [x0], #1
    sub     x1, x1, #1
    b       clear_visited

init_queue:
    // Initialize queue with (0,0) at distance 0
    LOAD_ADDR x19, queue
    str     wzr, [x19]                      // queue[0] = 0 (index of 0,0)
    LOAD_ADDR x0, queue_head
    str     xzr, [x0]
    LOAD_ADDR x0, queue_tail
    mov     x1, #1
    str     x1, [x0]

    // Mark start as visited
    LOAD_ADDR x0, visited
    mov     w1, #1
    strb    w1, [x0]

    // Initialize distances
    LOAD_ADDR x0, distances
    str     wzr, [x0]                       // distance[0] = 0

    // Goal index
    mov     w20, #GRID_SIZE * GRID_SIZE - 1

bfs_loop:
    // Check if queue is empty
    LOAD_ADDR x0, queue_head
    ldr     x21, [x0]
    LOAD_ADDR x0, queue_tail
    ldr     x22, [x0]
    cmp     x21, x22
    b.ge    no_path

    // Dequeue
    ldr     w23, [x19, x21, lsl #2]         // current cell index
    add     x21, x21, #1
    LOAD_ADDR x0, queue_head
    str     x21, [x0]

    // Check if at goal
    cmp     w23, w20
    b.eq    found_path

    // Get current distance
    LOAD_ADDR x0, distances
    ldr     w24, [x0, x23, lsl #2]          // current distance

    // Convert index to (y, x)
    mov     w0, #GRID_SIZE
    udiv    w25, w23, w0                    // y = index / GRID_SIZE
    msub    w26, w25, w0, w23               // x = index % GRID_SIZE

    // Try all 4 directions
    LOAD_ADDR x27, directions
    mov     w28, #0                         // direction index

dir_loop:
    cmp     w28, #4
    b.ge    bfs_loop

    // Get dy, dx (each direction is 8 bytes: dy at +0, dx at +4)
    lsl     w0, w28, #3
    ldrsw   x1, [x27, x0]                   // dy
    add     w0, w0, #4
    ldrsw   x2, [x27, x0]                   // dx

    // Calculate new position
    add     w3, w25, w1                     // ny = y + dy
    add     w4, w26, w2                     // nx = x + dx

    // Check bounds
    cmp     w3, #0
    b.lt    next_dir
    cmp     w3, #GRID_SIZE
    b.ge    next_dir
    cmp     w4, #0
    b.lt    next_dir
    cmp     w4, #GRID_SIZE
    b.ge    next_dir

    // Calculate new index
    mov     w0, #GRID_SIZE
    mul     w5, w3, w0
    add     w5, w5, w4                      // new_index = ny * GRID_SIZE + nx

    // Check if visited
    LOAD_ADDR x0, visited
    ldrb    w6, [x0, x5]
    cbnz    w6, next_dir

    // Check if corrupted
    LOAD_ADDR x0, grid
    ldrb    w6, [x0, x5]
    cbnz    w6, next_dir

    // Mark visited
    LOAD_ADDR x0, visited
    mov     w6, #1
    strb    w6, [x0, x5]

    // Set distance
    LOAD_ADDR x0, distances
    add     w7, w24, #1
    str     w7, [x0, x5, lsl #2]

    // Enqueue
    LOAD_ADDR x0, queue_tail
    ldr     x6, [x0]
    str     w5, [x19, x6, lsl #2]
    add     x6, x6, #1
    str     x6, [x0]

next_dir:
    add     w28, w28, #1
    b       dir_loop

found_path:
    // Return distance to goal
    LOAD_ADDR x0, distances
    ldr     w0, [x0, x20, lsl #2]
    b       bfs_return

no_path:
    mov     x0, #-1

bfs_return:
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_part1: Find shortest path after 1024 bytes
// ============================================================================
solve_part1:
    stp     x29, x30, [sp, #-16]!

    mov     x0, #NUM_BYTES_P1
    bl      setup_grid
    bl      bfs

    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_part2: Binary search for first blocking byte
// ============================================================================
solve_part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    // Get total positions
    LOAD_ADDR x0, num_positions
    ldr     x22, [x0]

    // Binary search: left = 0, right = num_positions
    mov     x19, #0                         // left
    mov     x20, x22                        // right

binary_loop:
    cmp     x19, x20
    b.ge    binary_done

    // mid = (left + right) / 2
    add     x21, x19, x20
    lsr     x21, x21, #1

    // Setup grid with mid+1 bytes and run BFS
    add     x0, x21, #1
    bl      setup_grid
    bl      bfs

    // If no path (bfs returns -1), search left half
    cmp     x0, #0
    b.lt    search_left
    // Otherwise search right half
    add     x19, x21, #1
    b       binary_loop

search_left:
    mov     x20, x21
    b       binary_loop

binary_done:
    // x19 is the index of the first blocking byte
    // Get its coordinates
    LOAD_ADDR x0, positions
    lsl     x1, x19, #3                     // index * 8 (each position is 8 bytes)
    ldr     w2, [x0, x1]                    // x
    add     x1, x1, #4
    ldr     w3, [x0, x1]                    // y

    LOAD_ADDR x0, blocking_x
    str     x2, [x0]
    LOAD_ADDR x0, blocking_y
    str     x3, [x0]

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print a null-terminated string
// Input: x0 = pointer to string
// ============================================================================
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0

    // Find length
    mov     x20, #0
1:  ldrb    w1, [x19, x20]
    cbz     w1, 2f
    add     x20, x20, #1
    b       1b

2:  // Write to stdout
    mov     x0, #1
    mov     x1, x19
    mov     x2, x20
    mov     x16, #4                         // write() syscall
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_num: Print a number
// Input: x0 = number
// ============================================================================
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    sub     sp, sp, #32

    mov     x19, x0
    add     x20, sp, #31
    strb    wzr, [x20]

    // Handle zero case
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
