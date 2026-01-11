// ARM64 Assembly solution for AoC 2021 Day 5 - Hydrothermal Venture
// macOS ARM64 syscalls

.global _start
.align 2

.equ STDOUT, 1
.equ GRID_SIZE, 1000             // 1000x1000 grid
.equ MAX_LINES, 512              // Max line segments
.equ BUFFER_SIZE, 32768

.data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

.align 3
file_buffer: .skip BUFFER_SIZE
// Store lines as x1,y1,x2,y2 (4 x 4 bytes = 16 bytes per line)
lines: .skip MAX_LINES * 16
line_count: .skip 8
// Grid uses 1 byte per cell, need 1000*1000 = 1,000,000 bytes
grid: .skip GRID_SIZE * GRID_SIZE
output_buffer: .skip 32

.text
_start:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    // Open file
    mov x16, #5             // open syscall
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #0              // O_RDONLY
    mov x2, #0
    svc #0x80
    cmp x0, #0
    b.lt exit_error
    mov x19, x0             // Save fd

    // Read file
    mov x16, #3             // read syscall
    mov x0, x19
    adrp x1, file_buffer@PAGE
    add x1, x1, file_buffer@PAGEOFF
    mov x2, #BUFFER_SIZE
    svc #0x80
    mov x20, x0             // Save bytes read

    // Close file
    mov x16, #6             // close syscall
    mov x0, x19
    svc #0x80

    // Parse input
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20
    bl parse_input

    // Part 1: Only horizontal and vertical lines
    mov x0, #0              // include_diagonals = false
    bl count_overlaps
    mov x21, x0             // Save Part 1 result

    // Print Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_str
    mov x0, x21
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Part 2: Include diagonals
    mov x0, #1              // include_diagonals = true
    bl count_overlaps
    mov x22, x0             // Save Part 2 result

    // Print Part 2
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_str
    mov x0, x22
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Exit success
    mov x0, #0
    mov x16, #1
    svc #0x80

exit_error:
    mov x0, #1
    mov x16, #1
    svc #0x80

// Parse input: lines in format "x1,y1 -> x2,y2"
// x0 = buffer, x1 = length
parse_input:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, x0             // Buffer pointer
    add x20, x19, x1        // Buffer end

    adrp x21, lines@PAGE
    add x21, x21, lines@PAGEOFF
    mov x22, #0             // Line count

parse_line_loop:
    cmp x19, x20
    b.ge parse_done

    // Skip any leading whitespace/newlines
    ldrb w23, [x19]
    cmp w23, #'\n'
    b.eq skip_char
    cmp w23, #'\r'
    b.eq skip_char
    cmp w23, #' '
    b.eq skip_char

    // Check if it's a digit (start of line)
    cmp w23, #'0'
    b.lt parse_done
    cmp w23, #'9'
    b.gt parse_done

    // Parse x1
    bl parse_number
    mov x23, x0             // x1

    // Skip comma
    add x19, x19, #1

    // Parse y1
    bl parse_number
    mov x24, x0             // y1

    // Skip " -> " (space, minus, greater, space)
    add x19, x19, #4

    // Parse x2
    bl parse_number
    mov x25, x0             // x2

    // Skip comma
    add x19, x19, #1

    // Parse y2
    bl parse_number
    mov x26, x0             // y2

    // Store line (x1, y1, x2, y2)
    mov x0, #16
    mul x0, x22, x0         // Offset = line_count * 16
    str w23, [x21, x0]      // x1
    add x0, x0, #4
    str w24, [x21, x0]      // y1
    add x0, x0, #4
    str w25, [x21, x0]      // x2
    add x0, x0, #4
    str w26, [x21, x0]      // y2

    add x22, x22, #1
    b parse_line_loop

skip_char:
    add x19, x19, #1
    b parse_line_loop

parse_done:
    adrp x0, line_count@PAGE
    add x0, x0, line_count@PAGEOFF
    str x22, [x0]

    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Parse a number from buffer at x19
// Updates x19 to point after the number
// Returns number in x0
parse_number:
    mov x0, #0              // Result
parse_num_loop:
    cmp x19, x20
    b.ge parse_num_done
    ldrb w1, [x19]
    cmp w1, #'0'
    b.lt parse_num_done
    cmp w1, #'9'
    b.gt parse_num_done

    mov x2, #10
    mul x0, x0, x2
    sub w1, w1, #'0'
    add x0, x0, x1
    add x19, x19, #1
    b parse_num_loop

parse_num_done:
    ret

// Clear the grid
clear_grid:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    adrp x19, grid@PAGE
    add x19, x19, grid@PAGEOFF
    mov x0, #GRID_SIZE
    mov x1, #GRID_SIZE
    mul x20, x0, x1         // Total cells
    mov x0, #0

clear_loop:
    cmp x0, x20
    b.ge clear_done
    strb wzr, [x19, x0]
    add x0, x0, #1
    b clear_loop

clear_done:
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Calculate sign of x0: returns -1, 0, or 1
// x0 = input, returns result in x0
sign:
    cmp x0, #0
    b.eq sign_zero
    b.gt sign_positive
    // Negative
    mov x0, #-1
    ret
sign_zero:
    mov x0, #0
    ret
sign_positive:
    mov x0, #1
    ret

// Draw a line on the grid
// x0 = x1, x1 = y1, x2 = x2, x3 = y2
draw_line:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, x0             // x1 (current x)
    mov x20, x1             // y1 (current y)
    mov x21, x2             // x2 (target x)
    mov x22, x3             // y2 (target y)

    // Calculate dx = sign(x2 - x1)
    sub x0, x21, x19
    bl sign
    mov x23, x0             // dx

    // Calculate dy = sign(y2 - y1)
    sub x0, x22, x20
    bl sign
    mov x24, x0             // dy

    adrp x25, grid@PAGE
    add x25, x25, grid@PAGEOFF

draw_loop:
    // Mark current point: grid[y * GRID_SIZE + x]++
    mov x0, #GRID_SIZE
    mul x0, x20, x0         // y * GRID_SIZE
    add x0, x0, x19         // + x
    ldrb w1, [x25, x0]
    add w1, w1, #1
    strb w1, [x25, x0]

    // Check if we've reached the target
    cmp x19, x21
    b.ne draw_next
    cmp x20, x22
    b.eq draw_done

draw_next:
    // Move to next point
    add x19, x19, x23       // x += dx
    add x20, x20, x24       // y += dy
    b draw_loop

draw_done:
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Count overlaps on grid (cells with value >= 2)
// x0 = include_diagonals (0 or 1)
// Returns count in x0
count_overlaps:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    mov x27, x0             // Save include_diagonals flag

    // Clear grid
    bl clear_grid

    adrp x19, lines@PAGE
    add x19, x19, lines@PAGEOFF
    adrp x20, line_count@PAGE
    add x20, x20, line_count@PAGEOFF
    ldr x20, [x20]          // Number of lines
    mov x21, #0             // Line index

process_lines:
    cmp x21, x20
    b.ge count_cells

    // Load line: x1, y1, x2, y2
    mov x0, #16
    mul x0, x21, x0
    ldr w22, [x19, x0]      // x1
    add x0, x0, #4
    ldr w23, [x19, x0]      // y1
    add x0, x0, #4
    ldr w24, [x19, x0]      // x2
    add x0, x0, #4
    ldr w25, [x19, x0]      // y2

    // Check if diagonal (dx != 0 && dy != 0)
    sub x0, x24, x22        // dx = x2 - x1
    sub x1, x25, x23        // dy = y2 - y1

    // If include_diagonals is false, skip diagonals
    cbz x27, check_diagonal
    b draw_this_line

check_diagonal:
    // Skip if both dx and dy are non-zero (diagonal)
    cbz x0, draw_this_line  // dx == 0, vertical line
    cbz x1, draw_this_line  // dy == 0, horizontal line
    b skip_line             // diagonal, skip

draw_this_line:
    mov x0, x22             // x1
    mov x1, x23             // y1
    mov x2, x24             // x2
    mov x3, x25             // y2
    bl draw_line

skip_line:
    add x21, x21, #1
    b process_lines

count_cells:
    // Count cells with value >= 2
    adrp x19, grid@PAGE
    add x19, x19, grid@PAGEOFF
    mov x0, #GRID_SIZE
    mov x1, #GRID_SIZE
    mul x20, x0, x1         // Total cells
    mov x21, #0             // Index
    mov x22, #0             // Count

count_loop:
    cmp x21, x20
    b.ge count_done

    ldrb w0, [x19, x21]
    cmp w0, #2
    b.lt count_next
    add x22, x22, #1

count_next:
    add x21, x21, #1
    b count_loop

count_done:
    mov x0, x22             // Return count

    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print null-terminated string
// x0 = string pointer
print_str:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    mov x19, x0
    mov x20, #0
ps_len:
    ldrb w1, [x19, x20]
    cbz w1, ps_write
    add x20, x20, #1
    b ps_len
ps_write:
    mov x16, #4             // write syscall
    mov x0, #STDOUT
    mov x1, x19
    mov x2, x20
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print decimal number
// x0 = number to print
print_number:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    adrp x19, output_buffer@PAGE
    add x19, x19, output_buffer@PAGEOFF
    add x19, x19, #31       // Start at end of buffer
    mov w1, #0
    strb w1, [x19]          // Null terminator
    mov x20, x0
    mov x2, #10

pn_loop:
    udiv x3, x20, x2
    msub x4, x3, x2, x20    // remainder = x20 - (x3 * 10)
    add w4, w4, #'0'
    sub x19, x19, #1
    strb w4, [x19]
    mov x20, x3
    cbnz x20, pn_loop

    mov x0, x19
    bl print_str

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
