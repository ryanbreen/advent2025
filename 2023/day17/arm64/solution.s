// Day 17: Clumsy Crucible - ARM64 Assembly (macOS)
//
// Dijkstra's shortest path with movement constraints:
// - State: (row, col, direction, consecutive_steps)
// - Part 1: max 3 consecutive blocks in same direction
// - Part 2: min 4, max 10 consecutive blocks (ultra crucible)
//
// Priority queue implemented as a binary min-heap.
// Directions: 0=right, 1=down, 2=left, 3=up

.global _start
.align 4

// Constants
.equ MAX_INPUT_SIZE, 65536
.equ MAX_GRID_SIZE, 150
.equ MAX_HEAP, 500000
.equ MAX_CONSEC, 11          // 0-10 for part 2

// Heap entry structure (16 bytes each for alignment):
// offset 0: heat_loss (4 bytes)
// offset 4: row (2 bytes)
// offset 6: col (2 bytes)
// offset 8: direction (1 byte)
// offset 9: consecutive (1 byte)
// offset 10-15: padding (6 bytes)

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
newline:        .asciz "\n"
error_msg:      .asciz "Error reading file\n"

.align 3
// Grid dimensions
rows:           .quad 0
cols:           .quad 0

// Dijkstra parameters
min_straight:   .quad 0
max_straight:   .quad 0

// File I/O buffer
file_buffer:    .space MAX_INPUT_SIZE

// Grid storage (char per cell, stored as heat values 1-9)
grid:           .space MAX_GRID_SIZE * MAX_GRID_SIZE

// Distance array: dist[row][col][dir][consec]
// Size: 150 * 150 * 4 * 11 = 990,000 entries, 4 bytes each
.align 3
dist:           .space MAX_GRID_SIZE * MAX_GRID_SIZE * 4 * MAX_CONSEC * 4

// Binary min-heap for priority queue
// Each entry: 16 bytes (heat, row, col, dir, consec, padding)
.align 4
heap:           .space MAX_HEAP * 16
heap_size:      .quad 0

// Direction deltas: dr, dc for each direction (right, down, left, up)
dr:             .word 0, 1, 0, -1
dc:             .word 1, 0, -1, 0

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

    // Parse input grid
    bl      parse_grid

    // Solve Part 1: min_straight=0, max_straight=3
    LOAD_ADDR x0, min_straight
    str     xzr, [x0]
    LOAD_ADDR x0, max_straight
    mov     x1, #3
    str     x1, [x0]
    bl      dijkstra
    mov     x21, x0                         // Save part1 result

    // Solve Part 2: min_straight=4, max_straight=10
    LOAD_ADDR x0, min_straight
    mov     x1, #4
    str     x1, [x0]
    LOAD_ADDR x0, max_straight
    mov     x1, #10
    str     x1, [x0]
    bl      dijkstra
    mov     x22, x0                         // Save part2 result

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
    mov     x0, x22
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
// parse_grid: Parse the input into grid array
// Sets rows and cols globals
// ============================================================================
parse_grid:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x19, file_buffer              // Input pointer
    LOAD_ADDR x20, grid                     // Output pointer
    mov     x21, #0                         // row count
    mov     x22, #0                         // col count (width)

    mov     x23, #0                         // current col in row

parse_loop:
    ldrb    w0, [x19], #1
    cbz     w0, parse_done

    cmp     w0, #'\n'
    b.eq    end_of_line

    // Convert '0'-'9' to numeric value and store
    sub     w0, w0, #'0'
    strb    w0, [x20], #1
    add     x23, x23, #1
    b       parse_loop

end_of_line:
    // First line sets column count
    cbz     x22, set_cols
    b       next_row
set_cols:
    mov     x22, x23

next_row:
    add     x21, x21, #1
    mov     x23, #0
    b       parse_loop

parse_done:
    // Handle last row if no trailing newline
    cbnz    x23, count_last_row
    b       save_dims

count_last_row:
    add     x21, x21, #1

save_dims:
    // Save dimensions
    LOAD_ADDR x0, rows
    str     x21, [x0]
    LOAD_ADDR x0, cols
    str     x22, [x0]

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// dijkstra: Find minimum heat loss path
// Uses globals: min_straight, max_straight, rows, cols, grid
// Returns: x0 = minimum heat loss to reach (rows-1, cols-1)
// ============================================================================
dijkstra:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    // Load parameters into registers
    LOAD_ADDR x0, rows
    ldr     x19, [x0]                       // x19 = rows
    LOAD_ADDR x0, cols
    ldr     x20, [x0]                       // x20 = cols
    LOAD_ADDR x0, min_straight
    ldr     x21, [x0]                       // x21 = min_straight
    LOAD_ADDR x0, max_straight
    ldr     x22, [x0]                       // x22 = max_straight

    // Initialize distance array to infinity (0x7FFFFFFF)
    LOAD_ADDR x0, dist
    mul     x1, x19, x20
    mov     x2, #4
    mul     x1, x1, x2                      // * 4 directions
    mov     x2, #MAX_CONSEC
    mul     x1, x1, x2                      // * 11 consec values
    mov     w3, #0x7F
    lsl     w3, w3, #24                     // 0x7F000000
    orr     w3, w3, #0xFFFFFF               // 0x7FFFFFFF
init_dist_loop:
    cbz     x1, init_heap
    str     w3, [x0], #4
    sub     x1, x1, #1
    b       init_dist_loop

init_heap:
    // Initialize heap with starting position (0,0) with direction -1 (special)
    // We'll push both (0,0,right,0) and (0,0,down,0) with heat=0
    LOAD_ADDR x0, heap_size
    str     xzr, [x0]

    // Push (heat=0, row=0, col=0, dir=0, consec=0)
    mov     x0, #0                          // heat
    mov     x1, #0                          // row
    mov     x2, #0                          // col
    mov     x3, #-1                         // dir = -1 (no direction yet)
    mov     x4, #0                          // consec
    bl      heap_push

dijkstra_loop:
    // Check if heap is empty
    LOAD_ADDR x0, heap_size
    ldr     x0, [x0]
    cbz     x0, no_path_found

    // Pop minimum from heap
    bl      heap_pop
    // Returns: x0=heat, x1=row, x2=col, x3=dir, x4=consec

    mov     x23, x0                         // current heat
    mov     x24, x1                         // current row
    mov     x25, x2                         // current col
    sxtw    x26, w3                         // current dir (sign-extend to 64-bit)
    mov     x27, x4                         // current consec

    // Check if we reached the goal
    sub     x5, x19, #1                     // rows - 1
    sub     x6, x20, #1                     // cols - 1
    cmp     x24, x5
    b.ne    not_at_goal
    cmp     x25, x6
    b.ne    not_at_goal

    // At goal - check min_straight constraint
    cbz     x21, found_goal                 // if min_straight == 0, done
    cmp     x27, x21
    b.ge    found_goal                      // if consec >= min_straight, done
    b       not_at_goal                     // need more consecutive moves

found_goal:
    mov     x0, x23
    b       dijkstra_done

not_at_goal:
    // Try all four directions
    mov     x28, #0                         // nd = 0

try_direction:
    cmp     x28, #4
    b.ge    dijkstra_loop

    // Can't reverse direction (if current dir is valid)
    cmp     x26, #-1
    b.eq    skip_reverse_check

    // Check if nd == (d + 2) % 4
    add     x5, x26, #2
    and     x5, x5, #3
    cmp     x28, x5
    b.eq    next_direction

skip_reverse_check:
    // Calculate next position
    LOAD_ADDR x0, dr
    ldrsw   x5, [x0, x28, lsl #2]           // dr[nd]
    LOAD_ADDR x0, dc
    ldrsw   x6, [x0, x28, lsl #2]           // dc[nd]

    add     x7, x24, x5                     // nr = row + dr[nd]
    add     x8, x25, x6                     // nc = col + dc[nd]

    // Bounds check
    cmp     x7, #0
    b.lt    next_direction
    cmp     x7, x19
    b.ge    next_direction
    cmp     x8, #0
    b.lt    next_direction
    cmp     x8, x20
    b.ge    next_direction

    // Calculate new_consec
    cmp     x28, x26
    b.eq    same_direction

    // Turning - check min_straight constraint
    cmp     x26, #-1
    b.eq    turn_ok                         // first move, no constraint
    cmp     x27, x21
    b.lt    next_direction                  // consec < min_straight, can't turn

turn_ok:
    mov     x9, #1                          // new_consec = 1
    b       check_max_straight

same_direction:
    add     x9, x27, #1                     // new_consec = consec + 1

check_max_straight:
    // Check max_straight constraint
    cmp     x9, x22
    b.gt    next_direction                  // new_consec > max_straight

    // Calculate new heat = current heat + grid[nr][nc]
    mul     x10, x7, x20                    // nr * cols
    add     x10, x10, x8                    // + nc
    LOAD_ADDR x0, grid
    ldrb    w11, [x0, x10]                  // grid[nr][nc]
    add     x12, x23, x11                   // new_heat

    // Calculate dist index: (nr * cols * 4 * 11) + (nc * 4 * 11) + (nd * 11) + new_consec
    mul     x10, x7, x20                    // nr * cols
    add     x10, x10, x8                    // + nc
    mov     x13, #4
    mul     x10, x10, x13                   // * 4
    mov     x13, #MAX_CONSEC
    mul     x10, x10, x13                   // * 11
    mov     x13, #MAX_CONSEC
    mul     x14, x28, x13                   // nd * 11
    add     x10, x10, x14                   // + nd * 11
    add     x10, x10, x9                    // + new_consec

    // Check if new_heat < dist[state]
    LOAD_ADDR x0, dist
    ldr     w13, [x0, x10, lsl #2]          // dist[state]
    cmp     w12, w13
    b.ge    next_direction                  // not better

    // Update distance
    str     w12, [x0, x10, lsl #2]

    // Push to heap
    mov     x0, x12                         // heat
    mov     x1, x7                          // row
    mov     x2, x8                          // col
    mov     x3, x28                         // dir
    mov     x4, x9                          // consec
    bl      heap_push

next_direction:
    add     x28, x28, #1
    b       try_direction

no_path_found:
    mov     x0, #-1

dijkstra_done:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// heap_push: Add element to min-heap
// Input: x0=heat, x1=row, x2=col, x3=dir, x4=consec
// ============================================================================
heap_push:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    // Save parameters
    mov     w19, w0                         // heat (32-bit)
    mov     w20, w1                         // row
    mov     w21, w2                         // col
    mov     w22, w3                         // dir
    mov     w23, w4                         // consec

    // Get current size and increment
    LOAD_ADDR x0, heap_size
    ldr     x24, [x0]
    add     x25, x24, #1
    str     x25, [x0]

    // Calculate position in heap array
    LOAD_ADDR x26, heap
    mov     x5, #16
    mul     x27, x24, x5                    // offset = size * 16
    add     x27, x26, x27                   // heap + offset

    // Store entry at position
    str     w19, [x27, #0]                  // heat
    strh    w20, [x27, #4]                  // row
    strh    w21, [x27, #6]                  // col
    strb    w22, [x27, #8]                  // dir
    strb    w23, [x27, #9]                  // consec

    // Bubble up
    mov     x0, x24                         // current index
bubble_up:
    cbz     x0, heap_push_done              // at root

    // Calculate parent index: (i - 1) / 2
    sub     x1, x0, #1
    lsr     x1, x1, #1                      // parent index

    // Compare with parent
    mov     x2, #16
    mul     x3, x0, x2
    add     x3, x26, x3                     // current ptr
    mul     x4, x1, x2
    add     x4, x26, x4                     // parent ptr

    ldr     w5, [x3, #0]                    // current heat
    ldr     w6, [x4, #0]                    // parent heat

    cmp     w5, w6
    b.ge    heap_push_done                  // current >= parent, done

    // Swap current with parent
    // Load current entry (16 bytes)
    ldp     x7, x8, [x3]
    // Load parent entry
    ldp     x9, x10, [x4]
    // Store swapped
    stp     x9, x10, [x3]
    stp     x7, x8, [x4]

    // Move up
    mov     x0, x1
    b       bubble_up

heap_push_done:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// heap_pop: Remove and return minimum element from heap
// Returns: x0=heat, x1=row, x2=col, x3=dir, x4=consec
// ============================================================================
heap_pop:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    LOAD_ADDR x19, heap
    LOAD_ADDR x20, heap_size
    ldr     x21, [x20]                      // size

    // Save root values to return
    ldr     w22, [x19, #0]                  // heat
    ldrh    w23, [x19, #4]                  // row
    ldrh    w24, [x19, #6]                  // col
    ldrsb   w25, [x19, #8]                  // dir (signed byte, so -1 stays -1)
    ldrb    w26, [x19, #9]                  // consec

    // Decrease size
    sub     x21, x21, #1
    str     x21, [x20]

    // If heap is now empty, done
    cbz     x21, heap_pop_return

    // Move last element to root
    mov     x0, #16
    mul     x1, x21, x0                     // last offset
    add     x1, x19, x1                     // last ptr
    ldp     x2, x3, [x1]
    stp     x2, x3, [x19]

    // Bubble down
    mov     x0, #0                          // current index
bubble_down:
    // Calculate children indices
    lsl     x1, x0, #1
    add     x1, x1, #1                      // left = 2*i + 1
    add     x2, x1, #1                      // right = 2*i + 2

    mov     x3, x0                          // smallest = current

    // Check left child
    cmp     x1, x21
    b.ge    check_swap

    mov     x4, #16
    mul     x5, x3, x4
    add     x5, x19, x5                     // smallest ptr
    mul     x6, x1, x4
    add     x6, x19, x6                     // left ptr

    ldr     w7, [x5, #0]                    // smallest heat
    ldr     w8, [x6, #0]                    // left heat

    cmp     w8, w7
    b.ge    check_right
    mov     x3, x1                          // smallest = left

check_right:
    cmp     x2, x21
    b.ge    check_swap

    mov     x4, #16
    mul     x5, x3, x4
    add     x5, x19, x5                     // smallest ptr
    mul     x6, x2, x4
    add     x6, x19, x6                     // right ptr

    ldr     w7, [x5, #0]                    // smallest heat
    ldr     w8, [x6, #0]                    // right heat

    cmp     w8, w7
    b.ge    check_swap
    mov     x3, x2                          // smallest = right

check_swap:
    cmp     x3, x0
    b.eq    heap_pop_return                 // no swap needed

    // Swap current with smallest
    mov     x4, #16
    mul     x5, x0, x4
    add     x5, x19, x5                     // current ptr
    mul     x6, x3, x4
    add     x6, x19, x6                     // smallest ptr

    ldp     x7, x8, [x5]
    ldp     x9, x10, [x6]
    stp     x9, x10, [x5]
    stp     x7, x8, [x6]

    mov     x0, x3
    b       bubble_down

heap_pop_return:
    mov     w0, w22                         // heat
    mov     w1, w23                         // row
    mov     w2, w24                         // col
    mov     w3, w25                         // dir
    mov     w4, w26                         // consec

    ldp     x23, x24, [sp], #16
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
