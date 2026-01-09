// Day 8: Treetop Tree House - ARM64 Assembly (macOS)
//
// Algorithm:
//   Part 1: Count trees visible from outside the grid
//           A tree is visible if all trees between it and any edge are shorter
//   Part 2: Find max scenic score (product of viewing distances in 4 directions)
//
// Grid is stored as raw digits (0-9) in a flat array
// Grid dimensions: rows x cols (typically 99x99)

.global _start
.align 4

// Constants
.equ MAX_GRID_SIZE, 100         // Max grid dimension (99x99 for real input)
.equ BUFFER_SIZE, 16384         // Input file buffer size

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
// Grid metadata
grid_rows:      .quad 0                  // Number of rows in grid
grid_cols:      .quad 0                  // Number of columns in grid

// Grid data: store tree heights as bytes (0-9)
.align 4
grid_data:      .space MAX_GRID_SIZE * MAX_GRID_SIZE

// File I/O buffer
.align 4
file_buffer:    .space BUFFER_SIZE

// ============================================================================
// Code Section
// ============================================================================
.text

// ============================================================================
// Main entry point
// ============================================================================
_start:
    // Open and read input file
    LOAD_ADDR x0, input_path
    mov     x1, #0                          // O_RDONLY
    mov     x2, #0                          // mode (not used for O_RDONLY)
    mov     x16, #5                         // open() syscall
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit

    mov     x19, x0                         // Save fd in x19

    // Read file
    mov     x0, x19
    LOAD_ADDR x1, file_buffer
    mov     x2, #BUFFER_SIZE
    mov     x16, #3                         // read() syscall
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit

    mov     x20, x0                         // Save bytes read in x20

    // Close file
    mov     x0, x19
    mov     x16, #6                         // close() syscall
    svc     #0x80

    // Parse the grid
    bl      parse_grid

    // Solve Part 1
    bl      solve_part1
    mov     x21, x0                         // Save part1 result

    // Solve Part 2
    bl      solve_part2
    mov     x22, x0                         // Save part2 result

    // Print results
    LOAD_ADDR x0, part1_msg
    bl      print_str
    mov     x0, x21
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    LOAD_ADDR x0, part2_msg
    bl      print_str
    mov     x0, x22
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Exit
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
// parse_grid: Parse input and build the grid
// Input: file_buffer contains the grid data
// Sets: grid_rows, grid_cols, grid_data
// ============================================================================
parse_grid:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x19, file_buffer              // Source pointer
    LOAD_ADDR x20, grid_data                // Destination pointer
    mov     x21, #0                         // Row counter
    mov     x22, #0                         // Col counter (for first row)

parse_row:
    mov     x22, #0                         // Reset col for this row

parse_char:
    ldrb    w0, [x19], #1                   // Load character
    cbz     w0, parse_done                  // End of buffer

    cmp     w0, #'\n'
    b.eq    parse_newline

    // It's a digit - convert ASCII to number (subtract '0')
    sub     w0, w0, #'0'
    strb    w0, [x20], #1                   // Store in grid
    add     x22, x22, #1                    // col++
    b       parse_char

parse_newline:
    // If this is the first row, set grid_cols
    cbnz    x21, next_row
    LOAD_ADDR x0, grid_cols
    str     x22, [x0]

next_row:
    add     x21, x21, #1                    // row++
    b       parse_row

parse_done:
    // Set grid_rows
    LOAD_ADDR x0, grid_rows
    str     x21, [x0]

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_part1: Count trees visible from outside the grid
// Returns: count in x0
// ============================================================================
solve_part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    // Load grid dimensions
    LOAD_ADDR x0, grid_rows
    ldr     x19, [x0]                       // x19 = rows
    LOAD_ADDR x0, grid_cols
    ldr     x20, [x0]                       // x20 = cols

    LOAD_ADDR x21, grid_data                // x21 = grid base

    mov     x22, #0                         // Visible count
    mov     x23, #0                         // Current row

p1_row_loop:
    cmp     x23, x19
    b.ge    p1_done

    mov     x24, #0                         // Current col

p1_col_loop:
    cmp     x24, x20
    b.ge    p1_next_row

    // Check if tree at (x23, x24) is visible
    // Calculate grid index: row * cols + col
    mul     x0, x23, x20
    add     x0, x0, x24
    ldrb    w25, [x21, x0]                  // w25 = tree height

    // Check visibility from each direction
    // A tree is visible if ANY direction has all shorter trees

    // Check left (col - 1 down to 0)
    mov     x26, x24                        // Start at current col
    cbz     x26, p1_visible                 // Edge tree is always visible
    sub     x26, x26, #1                    // Start checking from col - 1

p1_check_left:
    mul     x0, x23, x20
    add     x0, x0, x26
    ldrb    w1, [x21, x0]
    cmp     w1, w25
    b.ge    p1_check_right                  // Blocked, try next direction
    cbz     x26, p1_visible                 // Reached edge, visible!
    sub     x26, x26, #1
    b       p1_check_left

p1_check_right:
    // Check right (col + 1 to cols - 1)
    add     x26, x24, #1
    cmp     x26, x20
    b.ge    p1_visible                      // Edge tree is always visible

p1_check_right_loop:
    cmp     x26, x20
    b.ge    p1_visible                      // Reached edge, visible!
    mul     x0, x23, x20
    add     x0, x0, x26
    ldrb    w1, [x21, x0]
    cmp     w1, w25
    b.ge    p1_check_top                    // Blocked, try next direction
    add     x26, x26, #1
    b       p1_check_right_loop

p1_check_top:
    // Check top (row - 1 down to 0)
    mov     x26, x23
    cbz     x26, p1_visible                 // Edge tree is always visible
    sub     x26, x26, #1

p1_check_top_loop:
    mul     x0, x26, x20
    add     x0, x0, x24
    ldrb    w1, [x21, x0]
    cmp     w1, w25
    b.ge    p1_check_bottom                 // Blocked, try next direction
    cbz     x26, p1_visible                 // Reached edge, visible!
    sub     x26, x26, #1
    b       p1_check_top_loop

p1_check_bottom:
    // Check bottom (row + 1 to rows - 1)
    add     x26, x23, #1
    cmp     x26, x19
    b.ge    p1_visible                      // Edge tree is always visible

p1_check_bottom_loop:
    cmp     x26, x19
    b.ge    p1_visible                      // Reached edge, visible!
    mul     x0, x26, x20
    add     x0, x0, x24
    ldrb    w1, [x21, x0]
    cmp     w1, w25
    b.ge    p1_not_visible                  // Blocked from all directions
    add     x26, x26, #1
    b       p1_check_bottom_loop

p1_visible:
    add     x22, x22, #1                    // Increment visible count

p1_not_visible:
    add     x24, x24, #1                    // Next col
    b       p1_col_loop

p1_next_row:
    add     x23, x23, #1                    // Next row
    b       p1_row_loop

p1_done:
    mov     x0, x22                         // Return count

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_part2: Find maximum scenic score
// Returns: max score in x0
// ============================================================================
solve_part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    // Load grid dimensions
    LOAD_ADDR x0, grid_rows
    ldr     x19, [x0]                       // x19 = rows
    LOAD_ADDR x0, grid_cols
    ldr     x20, [x0]                       // x20 = cols

    LOAD_ADDR x21, grid_data                // x21 = grid base

    mov     x22, #0                         // Max scenic score
    mov     x23, #0                         // Current row

p2_row_loop:
    cmp     x23, x19
    b.ge    p2_done

    mov     x24, #0                         // Current col

p2_col_loop:
    cmp     x24, x20
    b.ge    p2_next_row

    // Calculate scenic score for tree at (x23, x24)
    // Calculate grid index: row * cols + col
    mul     x0, x23, x20
    add     x0, x0, x24
    ldrb    w25, [x21, x0]                  // w25 = tree height

    // Count viewing distance in each direction
    // x27 will accumulate the product

    // Left viewing distance
    mov     x26, #0                         // Distance count
    cbz     x24, p2_left_done               // At edge, distance = 0
    mov     x0, x24
    sub     x0, x0, #1                      // Start from col - 1

p2_left_loop:
    add     x26, x26, #1                    // Count this tree
    mul     x1, x23, x20
    add     x1, x1, x0
    ldrb    w2, [x21, x1]
    cmp     w2, w25
    b.ge    p2_left_done                    // Hit a blocking tree
    cbz     x0, p2_left_done                // Reached edge
    sub     x0, x0, #1
    b       p2_left_loop

p2_left_done:
    mov     x27, x26                        // Start product with left distance

    // Right viewing distance
    mov     x26, #0                         // Distance count
    add     x0, x24, #1                     // Start from col + 1
    cmp     x0, x20
    b.ge    p2_right_done                   // At edge, distance = 0

p2_right_loop:
    cmp     x0, x20
    b.ge    p2_right_done                   // Reached edge
    add     x26, x26, #1                    // Count this tree
    mul     x1, x23, x20
    add     x1, x1, x0
    ldrb    w2, [x21, x1]
    cmp     w2, w25
    b.ge    p2_right_done                   // Hit a blocking tree
    add     x0, x0, #1
    b       p2_right_loop

p2_right_done:
    mul     x27, x27, x26                   // Multiply by right distance

    // Up viewing distance
    mov     x26, #0                         // Distance count
    cbz     x23, p2_up_done                 // At edge, distance = 0
    mov     x0, x23
    sub     x0, x0, #1                      // Start from row - 1

p2_up_loop:
    add     x26, x26, #1                    // Count this tree
    mul     x1, x0, x20
    add     x1, x1, x24
    ldrb    w2, [x21, x1]
    cmp     w2, w25
    b.ge    p2_up_done                      // Hit a blocking tree
    cbz     x0, p2_up_done                  // Reached edge
    sub     x0, x0, #1
    b       p2_up_loop

p2_up_done:
    mul     x27, x27, x26                   // Multiply by up distance

    // Down viewing distance
    mov     x26, #0                         // Distance count
    add     x0, x23, #1                     // Start from row + 1
    cmp     x0, x19
    b.ge    p2_down_done                    // At edge, distance = 0

p2_down_loop:
    cmp     x0, x19
    b.ge    p2_down_done                    // Reached edge
    add     x26, x26, #1                    // Count this tree
    mul     x1, x0, x20
    add     x1, x1, x24
    ldrb    w2, [x21, x1]
    cmp     w2, w25
    b.ge    p2_down_done                    // Hit a blocking tree
    add     x0, x0, #1
    b       p2_down_loop

p2_down_done:
    mul     x27, x27, x26                   // Multiply by down distance

    // Update max if this score is higher
    cmp     x27, x22
    csel    x22, x27, x22, gt               // x22 = max(x22, x27)

    add     x24, x24, #1                    // Next col
    b       p2_col_loop

p2_next_row:
    add     x23, x23, #1                    // Next row
    b       p2_row_loop

p2_done:
    mov     x0, x22                         // Return max score

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print a null-terminated string
// Input: x0 = address of string
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
    mov     x16, #4
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_num: Print a number
// Input: x0 = number to print
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
