//==============================================================================
// Advent of Code 2024 Day 4: Ceres Search
// ARM64 Assembly for macOS (Apple Silicon)
//
// Part 1: Find all occurrences of "XMAS" in a word search grid in all 8
//         directions (horizontal, vertical, diagonal, forward/backward)
//
// Part 2: Find X-MAS patterns where two "MAS" strings form an X with "A"
//         in the center (each diagonal can be MAS or SAM)
//
// Compile: as -o solution.o solution.s && \
//          ld -o solution solution.o -lSystem \
//          -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64
//
// Run: ./solution (reads ../input.txt)
//==============================================================================

.global _start
.align 4

// macOS system calls
.equ SYS_READ, 0x2000003
.equ SYS_WRITE, 0x2000004
.equ SYS_OPEN, 0x2000005
.equ SYS_CLOSE, 0x2000006
.equ SYS_EXIT, 0x2000001

// File flags
.equ O_RDONLY, 0x0000

// File descriptor constants
.equ STDOUT, 1

.data
input_path:     .asciz "../input.txt"
part1_label:    .asciz "Part 1: "
part2_label:    .asciz "\nPart 2: "
newline:        .asciz "\n"

.bss
.align 4
buffer:         .skip 65536          // 64KB buffer for input file
grid_rows:      .skip 8              // Number of rows
grid_cols:      .skip 8              // Number of columns
grid_lines:     .skip 8192           // Array of pointers to line starts (max 1024 lines)
output_buf:     .skip 32             // Buffer for number output

.text

// Main entry point
_start:
    // Open input file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0005
    adrp x0, input_path@PAGE
    add x0, x0, input_path@PAGEOFF
    mov x1, #O_RDONLY
    mov x2, #0
    svc #0x80

    cmp x0, #0
    b.lt exit_error
    mov x19, x0                      // Save file descriptor in x19

    // Read entire file into buffer
    movz x16, #0x2000, lsl #16
    movk x16, #0x0003
    mov x0, x19                      // File descriptor
    adrp x1, buffer@PAGE
    add x1, x1, buffer@PAGEOFF
    mov x2, #65536                   // Max bytes to read
    svc #0x80

    cmp x0, #0
    b.lt exit_error
    mov x20, x0                      // Save file size in x20

    // Close file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0006
    mov x0, x19
    svc #0x80

    // Parse grid into lines
    adrp x0, buffer@PAGE
    add x0, x0, buffer@PAGEOFF
    mov x1, x20                      // Buffer size
    bl parse_grid

    // ===== PART 1 =====
    adrp x0, part1_label@PAGE
    add x0, x0, part1_label@PAGEOFF
    bl print_string

    bl part1_solve
    mov x21, x0                      // Save part1 result

    bl print_number

    // ===== PART 2 =====
    adrp x0, part2_label@PAGE
    add x0, x0, part2_label@PAGEOFF
    bl print_string

    bl part2_solve
    mov x22, x0                      // Save part2 result

    bl print_number

    // Print final newline
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_string

    // Exit success
    movz x16, #0x2000, lsl #16
    movk x16, #0x0001
    mov x0, #0
    svc #0x80

exit_error:
    movz x16, #0x2000, lsl #16
    movk x16, #0x0001
    mov x0, #1
    svc #0x80

//==============================================================================
// Parse grid into array of line pointers
// Input: x0 = buffer pointer, x1 = buffer size
//==============================================================================
parse_grid:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0                      // Buffer pointer
    mov x20, x1                      // Buffer size
    add x20, x19, x20                // Buffer end

    adrp x21, grid_lines@PAGE
    add x21, x21, grid_lines@PAGEOFF
    mov x22, #0                      // Row count
    mov x23, #0                      // Column count (from first row)

    // Store first line
    str x19, [x21], #8
    mov x24, x19                     // Current line start

.parse_grid_loop:
    cmp x19, x20
    b.ge .parse_grid_done

    ldrb w0, [x19]

    cmp w0, #'\n'
    b.eq .parse_grid_newline

    cmp w0, #0
    b.eq .parse_grid_done

    add x19, x19, #1
    b .parse_grid_loop

.parse_grid_newline:
    // Calculate column count from first row
    cbz x22, .parse_grid_calc_cols
    b .parse_grid_next_line

.parse_grid_calc_cols:
    sub x23, x19, x24                // cols = current - line_start

.parse_grid_next_line:
    add x22, x22, #1                 // Increment row count
    add x19, x19, #1                 // Skip newline

    cmp x19, x20
    b.ge .parse_grid_done

    // Store next line pointer
    str x19, [x21], #8
    mov x24, x19
    b .parse_grid_loop

.parse_grid_done:
    // Save grid dimensions
    adrp x0, grid_rows@PAGE
    add x0, x0, grid_rows@PAGEOFF
    str x22, [x0]

    adrp x0, grid_cols@PAGE
    add x0, x0, grid_cols@PAGEOFF
    str x23, [x0]

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

//==============================================================================
// Get character at grid[row][col]
// Input: x0 = row, x1 = col
// Output: x0 = character (0 if out of bounds)
//==============================================================================
get_char:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    // Check bounds
    adrp x19, grid_rows@PAGE
    add x19, x19, grid_rows@PAGEOFF
    ldr x19, [x19]
    cmp x0, #0
    b.lt .get_char_oob
    cmp x0, x19
    b.ge .get_char_oob

    adrp x19, grid_cols@PAGE
    add x19, x19, grid_cols@PAGEOFF
    ldr x19, [x19]
    cmp x1, #0
    b.lt .get_char_oob
    cmp x1, x19
    b.ge .get_char_oob

    // Get line pointer
    adrp x19, grid_lines@PAGE
    add x19, x19, grid_lines@PAGEOFF
    ldr x19, [x19, x0, lsl #3]

    // Get character
    ldrb w0, [x19, x1]

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.get_char_oob:
    mov x0, #0
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

//==============================================================================
// Part 1: Find all occurrences of "XMAS" in all 8 directions
// Output: x0 = count
//==============================================================================
part1_solve:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x25, #0                      // Total count

    adrp x19, grid_rows@PAGE
    add x19, x19, grid_rows@PAGEOFF
    ldr x19, [x19]                   // rows

    adrp x20, grid_cols@PAGE
    add x20, x20, grid_cols@PAGEOFF
    ldr x20, [x20]                   // cols

    mov x21, #0                      // Current row

.part1_row_loop:
    cmp x21, x19
    b.ge .part1_done

    mov x22, #0                      // Current col

.part1_col_loop:
    cmp x22, x20
    b.ge .part1_next_row

    // Try all 8 directions from this position
    mov x23, #0                      // Direction index

.part1_dir_loop:
    cmp x23, #8
    b.ge .part1_next_col

    // Check if "XMAS" exists in direction x23 from (x21, x22)
    mov x0, x21                      // row
    mov x1, x22                      // col
    mov x2, x23                      // direction
    bl check_xmas

    add x25, x25, x0                 // Add result to count
    add x23, x23, #1
    b .part1_dir_loop

.part1_next_col:
    add x22, x22, #1
    b .part1_col_loop

.part1_next_row:
    add x21, x21, #1
    b .part1_row_loop

.part1_done:
    mov x0, x25                      // Return count

    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

//==============================================================================
// Check if "XMAS" exists starting at (row, col) in given direction
// Input: x0 = row, x1 = col, x2 = direction (0-7)
// Output: x0 = 1 if found, 0 otherwise
// Directions: 0=right, 1=left, 2=down, 3=up, 4=down-right, 5=down-left, 6=up-right, 7=up-left
//==============================================================================
check_xmas:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0                      // row
    mov x20, x1                      // col
    mov x21, x2                      // direction

    // Get direction deltas
    adr x22, .direction_table
    add x22, x22, x21, lsl #4        // Each entry is 16 bytes (2 x 8-byte values)
    ldp x23, x24, [x22]              // Load dr, dc

    // Check each character in "XMAS"
    mov x0, x19
    mov x1, x20
    bl get_char
    cmp w0, #'X'
    b.ne .check_xmas_fail

    add x19, x19, x23                // row + dr
    add x20, x20, x24                // col + dc
    mov x0, x19
    mov x1, x20
    bl get_char
    cmp w0, #'M'
    b.ne .check_xmas_fail

    add x19, x19, x23
    add x20, x20, x24
    mov x0, x19
    mov x1, x20
    bl get_char
    cmp w0, #'A'
    b.ne .check_xmas_fail

    add x19, x19, x23
    add x20, x20, x24
    mov x0, x19
    mov x1, x20
    bl get_char
    cmp w0, #'S'
    b.ne .check_xmas_fail

    mov x0, #1                       // Found!
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.check_xmas_fail:
    mov x0, #0
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.align 4
.direction_table:
    .quad 0, 1                       // 0: right (dr=0, dc=1)
    .quad 0, -1                      // 1: left (dr=0, dc=-1)
    .quad 1, 0                       // 2: down (dr=1, dc=0)
    .quad -1, 0                      // 3: up (dr=-1, dc=0)
    .quad 1, 1                       // 4: down-right (dr=1, dc=1)
    .quad 1, -1                      // 5: down-left (dr=1, dc=-1)
    .quad -1, 1                      // 6: up-right (dr=-1, dc=1)
    .quad -1, -1                     // 7: up-left (dr=-1, dc=-1)

//==============================================================================
// Part 2: Find X-MAS patterns (two MAS forming an X)
// Output: x0 = count
//==============================================================================
part2_solve:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x25, #0                      // Total count

    adrp x19, grid_rows@PAGE
    add x19, x19, grid_rows@PAGEOFF
    ldr x19, [x19]                   // rows

    adrp x20, grid_cols@PAGE
    add x20, x20, grid_cols@PAGEOFF
    ldr x20, [x20]                   // cols

    mov x21, #1                      // Start at row 1 (need borders)

.part2_row_loop:
    sub x0, x19, #1
    cmp x21, x0
    b.ge .part2_done

    mov x22, #1                      // Start at col 1

.part2_col_loop:
    sub x0, x20, #1
    cmp x22, x0
    b.ge .part2_next_row

    // Check if center is 'A'
    mov x0, x21                      // row
    mov x1, x22                      // col
    bl get_char
    cmp w0, #'A'
    b.ne .part2_next_col

    // Get four corners
    sub x0, x21, #1                  // row - 1
    sub x1, x22, #1                  // col - 1
    bl get_char
    mov x23, x0                      // top_left

    sub x0, x21, #1                  // row - 1
    add x1, x22, #1                  // col + 1
    bl get_char
    mov x24, x0                      // top_right

    add x0, x21, #1                  // row + 1
    sub x1, x22, #1                  // col - 1
    bl get_char
    mov x26, x0                      // bottom_left

    add x0, x21, #1                  // row + 1
    add x1, x22, #1                  // col + 1
    bl get_char
    mov x27, x0                      // bottom_right

    // Check diagonal 1 (top-left to bottom-right): MAS or SAM
    cmp w23, #'M'
    b.ne .part2_check_diag1_sam
    cmp w27, #'S'
    b.eq .part2_diag1_ok
    b .part2_next_col

.part2_check_diag1_sam:
    cmp w23, #'S'
    b.ne .part2_next_col
    cmp w27, #'M'
    b.ne .part2_next_col

.part2_diag1_ok:
    // Check diagonal 2 (top-right to bottom-left): MAS or SAM
    cmp w24, #'M'
    b.ne .part2_check_diag2_sam
    cmp w26, #'S'
    b.eq .part2_found
    b .part2_next_col

.part2_check_diag2_sam:
    cmp w24, #'S'
    b.ne .part2_next_col
    cmp w26, #'M'
    b.ne .part2_next_col

.part2_found:
    add x25, x25, #1                 // Found X-MAS pattern

.part2_next_col:
    add x22, x22, #1
    b .part2_col_loop

.part2_next_row:
    add x21, x21, #1
    b .part2_row_loop

.part2_done:
    mov x0, x25                      // Return count

    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

//==============================================================================
// Print a null-terminated string
// Input: x0 = string pointer
//==============================================================================
print_string:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    mov x19, x0                      // Save string pointer

    // Calculate string length
    mov x20, #0
.strlen_loop:
    ldrb w1, [x19, x20]
    cbz w1, .strlen_done
    add x20, x20, #1
    b .strlen_loop

.strlen_done:
    // Write to stdout
    movz x16, #0x2000, lsl #16
    movk x16, #0x0004
    mov x0, #STDOUT
    mov x1, x19
    mov x2, x20
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

//==============================================================================
// Print a number in decimal
// Input: x0 = number to print
//==============================================================================
print_number:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0                      // Number to print
    adrp x20, output_buf@PAGE
    add x20, x20, output_buf@PAGEOFF
    add x20, x20, #31                // Point to end of buffer
    mov w21, #0
    strb w21, [x20]                  // Null terminator

    mov x22, #10                     // Divisor

    // Handle zero special case
    cbnz x19, .print_convert_loop
    sub x20, x20, #1
    mov w21, #'0'
    strb w21, [x20]
    b .print_output

.print_convert_loop:
    cbz x19, .print_output

    udiv x1, x19, x22                // Divide by 10
    msub x2, x1, x22, x19            // Get remainder (digit)

    add w2, w2, #'0'                 // Convert to ASCII
    sub x20, x20, #1
    strb w2, [x20]

    mov x19, x1                      // Continue with quotient
    b .print_convert_loop

.print_output:
    mov x0, x20
    bl print_string

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
