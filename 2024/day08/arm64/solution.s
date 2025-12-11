// Day 8: Resonant Collinearity - ARM64 Assembly (macOS)
//
// Finds antinodes created by antenna pairs with the same frequency.
// Part 1: Antinodes occur at 2:1 distance ratio from antenna pairs
// Part 2: Antinodes occur at all collinear points along antenna pairs
//
// Algorithm:
//   1. Parse grid and group antennas by frequency
//   2. For each frequency with 2+ antennas:
//      - Part 1: For each pair, calculate 2 antinode positions (2:1 ratio)
//      - Part 2: For each pair, extend line in both directions
//   3. Use sets to track unique antinode positions

.global _start
.align 4

// Constants
.equ MAX_ANTENNAS, 4096        // Max total antennas
.equ MAX_ANTINODES, 8192       // Max antinode positions
.equ MAX_GRID_SIZE, 64         // Max grid dimension

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
// grid_data:      .space 4096           // Unused - grid stored in file_buffer only
grid_rows:      .quad 0                  // Number of rows in grid
grid_cols:      .quad 0                  // Number of columns in grid

// Antenna storage: Group antennas by frequency character (0-255 ASCII)
// For each frequency: count (32-bit) + array of positions (packed 32-bit)
// Position encoding: (row << 16 | col) fits in 32 bits for grids up to 64x64
antenna_counts:     .space 256 * 4       // 256 frequencies × 4 bytes per count
                                         // Total: 1,024 bytes
antenna_positions:  .space 256 * 64 * 4  // 256 frequencies × 64 max positions × 4 bytes
                                         // Total: 65,536 bytes

// Antinode sets: Store unique antinode positions for each part
// Uses same packed position format: (row << 16 | col)
antinodes_p1:       .space MAX_ANTINODES * 4  // 8192 positions × 4 bytes = 32,768 bytes
antinodes_p2:       .space MAX_ANTINODES * 4  // 8192 positions × 4 bytes = 32,768 bytes
antinode_count_p1:  .quad 0
antinode_count_p2:  .quad 0

// File I/O buffer
.align 4
file_buffer:    .space 8192              // Input file read buffer (8,192 bytes)

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
    mov     x2, #8192
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
// parse_grid: Parse input and extract antenna positions
// Input: file_buffer contains the grid data
// Sets: grid_rows, grid_cols, antenna_counts, antenna_positions
// ============================================================================
parse_grid:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    // Clear antenna counts
    LOAD_ADDR x0, antenna_counts
    mov     x1, #0
    mov     x2, #256
1:  str     w1, [x0], #4
    subs    x2, x2, #1
    b.ne    1b

    LOAD_ADDR x19, file_buffer                // Current position in buffer
    mov     x20, #0                         // Row counter
    mov     x21, #0                         // Col counter (for first row)
    mov     x24, #0                         // Flag: have we set cols yet?

parse_row:
    mov     x21, #0                         // Reset col for this row

parse_char:
    ldrb    w22, [x19], #1                  // Load character
    cbz     w22, parse_done                 // End of buffer

    cmp     w22, #'\n'
    b.eq    parse_newline

    // Check if this is an antenna (not '.')
    cmp     w22, #'.'
    b.eq    parse_next_col

    // It's an antenna - store its position
    // Get the count for this frequency
    LOAD_ADDR x0, antenna_counts
    add     x0, x0, x22, lsl #2             // offset = freq * 4
    ldr     w23, [x0]                       // Load current count

    // Store position: pack (row << 16 | col) into 32 bits
    lsl     w1, w20, #16                    // row << 16
    orr     w1, w1, w21                     // | col

    // Store in antenna_positions[freq][count]
    LOAD_ADDR x2, antenna_positions
    mov     x3, #256                        // 256 bytes per frequency (64 positions * 4 bytes)
    mul     x3, x22, x3                     // freq * 256
    add     x2, x2, x3
    add     x2, x2, x23, lsl #2             // + count * 4
    str     w1, [x2]

    // Increment count
    add     w23, w23, #1
    str     w23, [x0]

parse_next_col:
    add     x21, x21, #1                    // col++
    b       parse_char

parse_newline:
    // If this is the first row, set grid_cols
    cbz     x24, set_cols
    b       next_row

set_cols:
    LOAD_ADDR x0, grid_cols
    str     x21, [x0]
    mov     x24, #1                         // Mark that we've set cols

next_row:
    add     x20, x20, #1                    // row++
    b       parse_row

parse_done:
    // Set grid_rows
    LOAD_ADDR x0, grid_rows
    str     x20, [x0]

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_part1: Calculate antinodes for part 1
// For each pair of same-frequency antennas, add 2 antinodes
// Returns: count in x0
// ============================================================================
solve_part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    // Clear antinode count
    LOAD_ADDR x0, antinode_count_p1
    str     xzr, [x0]

    LOAD_ADDR x0, grid_rows
    ldr     x25, [x0]                       // x25 = rows
    LOAD_ADDR x0, grid_cols
    ldr     x26, [x0]                       // x26 = cols

    mov     x19, #0                         // Frequency iterator

p1_freq_loop:
    cmp     x19, #256
    b.ge    p1_done

    // Get count for this frequency
    LOAD_ADDR x0, antenna_counts
    add     x0, x0, x19, lsl #2
    ldr     w20, [x0]                       // w20 = count

    // Skip if count < 2
    cmp     w20, #2
    b.lt    p1_next_freq

    // Get base address of positions for this frequency
    LOAD_ADDR x21, antenna_positions
    mov     x0, #256
    mul     x0, x19, x0
    add     x21, x21, x0                    // x21 = positions array base

    // Iterate through all pairs (i, j) where i < j
    mov     x22, #0                         // i
p1_pair_i:
    cmp     x22, x20
    b.ge    p1_next_freq

    add     x23, x22, #1                    // j = i + 1
p1_pair_j:
    cmp     x23, x20
    b.ge    p1_next_i

    // Load antenna 1 position (i)
    ldr     w0, [x21, x22, lsl #2]
    lsr     w1, w0, #16                     // r1 = row
    and     w2, w0, #0xFFFF                 // c1 = col

    // Load antenna 2 position (j)
    ldr     w3, [x21, x23, lsl #2]
    lsr     w4, w3, #16                     // r2 = row
    and     w5, w3, #0xFFFF                 // c2 = col

    // CRITICAL: Save antenna coordinates in callee-saved registers
    // x0-x18 are caller-saved and WILL be clobbered by add_to_set calls
    // x19-x28 are callee-saved and guaranteed to be preserved
    mov     w27, w1                         // save r1
    mov     w28, w2                         // save c1
    mov     w24, w4                         // save r2 (x24 already saved on stack)
    // Note: c2 in w5 will be recalculated after first add_to_set call

    // Calculate antinode 1: (2*r1 - r2, 2*c1 - c2)
    lsl     w6, w1, #1                      // 2*r1
    sub     w6, w6, w4                      // 2*r1 - r2
    lsl     w7, w2, #1                      // 2*c1
    sub     w7, w7, w5                      // 2*c1 - c2

    // Sign extend for comparison
    sxtw    x6, w6
    sxtw    x7, w7

    // Check if in bounds
    cmp     x6, #0
    b.lt    p1_check_antinode2
    cmp     x6, x25
    b.ge    p1_check_antinode2
    cmp     x7, #0
    b.lt    p1_check_antinode2
    cmp     x7, x26
    b.ge    p1_check_antinode2

    // Add to set - pack position (use w6, w7 which have correct values)
    and     w8, w6, #0xFFFF              // Mask to 16 bits
    lsl     w8, w8, #16
    and     w9, w7, #0xFFFF              // Mask to 16 bits
    orr     w8, w8, w9
    mov     x0, x8
    LOAD_ADDR x1, antinodes_p1
    LOAD_ADDR x2, antinode_count_p1
    bl      add_to_set

p1_check_antinode2:
    // Reload antenna 2 col (c2) - w5 was clobbered by add_to_set
    // (w3 contains caller-saved register that may have changed)
    ldr     w3, [x21, x23, lsl #2]
    and     w5, w3, #0xFFFF                 // c2 = col

    // Calculate antinode 2: (2*r2 - r1, 2*c2 - c1) using saved values
    lsl     w6, w24, #1                     // 2*r2 (from saved)
    sub     w6, w6, w27                     // 2*r2 - r1 (r1 from saved)
    lsl     w7, w5, #1                      // 2*c2
    sub     w7, w7, w28                     // 2*c2 - c1 (c1 from saved)

    // Sign extend for comparison
    sxtw    x6, w6
    sxtw    x7, w7

    // Check if in bounds
    cmp     x6, #0
    b.lt    p1_next_j
    cmp     x6, x25
    b.ge    p1_next_j
    cmp     x7, #0
    b.lt    p1_next_j
    cmp     x7, x26
    b.ge    p1_next_j

    // Add to set - pack position
    and     w8, w6, #0xFFFF              // Mask to 16 bits
    lsl     w8, w8, #16
    and     w9, w7, #0xFFFF              // Mask to 16 bits
    orr     w8, w8, w9
    mov     x0, x8
    LOAD_ADDR x1, antinodes_p1
    LOAD_ADDR x2, antinode_count_p1
    bl      add_to_set

p1_next_j:
    add     x23, x23, #1
    b       p1_pair_j

p1_next_i:
    add     x22, x22, #1
    b       p1_pair_i

p1_next_freq:
    add     x19, x19, #1
    b       p1_freq_loop

p1_done:
    LOAD_ADDR x0, antinode_count_p1
    ldr     x0, [x0]

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_part2: Calculate antinodes for part 2
// For each pair of same-frequency antennas, extend line in both directions
// Returns: count in x0
// ============================================================================
solve_part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    // Clear antinode count
    LOAD_ADDR x0, antinode_count_p2
    str     xzr, [x0]

    LOAD_ADDR x0, grid_rows
    ldr     x25, [x0]                       // x25 = rows
    LOAD_ADDR x0, grid_cols
    ldr     x26, [x0]                       // x26 = cols

    mov     x19, #0                         // Frequency iterator

p2_freq_loop:
    cmp     x19, #256
    b.ge    p2_done

    // Get count for this frequency
    LOAD_ADDR x0, antenna_counts
    add     x0, x0, x19, lsl #2
    ldr     w20, [x0]                       // w20 = count

    // Skip if count < 2
    cmp     w20, #2
    b.lt    p2_next_freq

    // Get base address of positions for this frequency
    LOAD_ADDR x21, antenna_positions
    mov     x0, #256
    mul     x0, x19, x0
    add     x21, x21, x0                    // x21 = positions array base

    // Iterate through all pairs (i, j) where i < j
    mov     x22, #0                         // i
p2_pair_i:
    cmp     x22, x20
    b.ge    p2_next_freq

    add     x23, x22, #1                    // j = i + 1
p2_pair_j:
    cmp     x23, x20
    b.ge    p2_next_i

    // Load antenna 1 position (i)
    ldr     w0, [x21, x22, lsl #2]
    lsr     w1, w0, #16                     // r1 = row
    and     w2, w0, #0xFFFF                 // c1 = col

    // Load antenna 2 position (j)
    ldr     w3, [x21, x23, lsl #2]
    lsr     w4, w3, #16                     // r2 = row
    and     w5, w3, #0xFFFF                 // c2 = col

    // Calculate delta vector: dr = r2 - r1, dc = c2 - c1
    // This gives us the step to move from antenna 1 to antenna 2
    sub     w27, w4, w1                     // dr (save in callee-saved x27)
    sub     w28, w5, w2                     // dc (save in callee-saved x28)

    // Save r1, c1 in x24 for later use in direction 2
    // Pack as (r1 << 32 | c1) to preserve both values across loop
    lsl     x24, x1, #32
    orr     x24, x24, x2

    // Direction 1: Start at (r1,c1) and extend towards and beyond (r2,c2)
    // Keep adding (dr,dc) until we go out of bounds
    mov     w6, w1                          // r = r1
    mov     w7, w2                          // c = c1
p2_dir1_loop:
    // Check bounds: coordinates can become negative during subtraction
    // Sign-extend w6/w7 to x8/x9 so negative values are properly detected
    // (without sign extension, negative values would appear as large positive)
    sxtw    x8, w6                          // Sign extend r for comparison
    sxtw    x9, w7                          // Sign extend c for comparison
    cmp     x8, #0
    b.lt    p2_dir2
    cmp     x8, x25
    b.ge    p2_dir2
    cmp     x9, #0
    b.lt    p2_dir2
    cmp     x9, x26
    b.ge    p2_dir2

    // Save current position to stack before function call
    stp     x6, x7, [sp, #-16]!

    // Add to set
    and     w8, w6, #0xFFFF
    lsl     w8, w8, #16
    and     w9, w7, #0xFFFF
    orr     w8, w8, w9
    mov     x0, x8
    LOAD_ADDR x1, antinodes_p2
    LOAD_ADDR x2, antinode_count_p2
    bl      add_to_set

    // Restore current position after function call
    ldp     x6, x7, [sp], #16

    // Move in direction
    add     w6, w6, w27                     // r += dr
    add     w7, w7, w28                     // c += dc
    b       p2_dir1_loop

p2_dir2:
    // Direction 2: Start at (r1,c1) and extend away from (r2,c2)
    // Keep subtracting (dr,dc) until we go out of bounds
    // Recover r1, c1 from x24 (packed earlier)
    lsr     x1, x24, #32                    // r1 = upper 32 bits
    and     x2, x24, #0xFFFFFFFF            // c1 = lower 32 bits
    sub     w6, w1, w27                     // r = r1 - dr (start one step back)
    sub     w7, w2, w28                     // c = c1 - dc
p2_dir2_loop:
    // Check bounds: same sign-extension logic as direction 1
    sxtw    x8, w6                          // Sign extend r for comparison
    sxtw    x9, w7                          // Sign extend c for comparison
    cmp     x8, #0
    b.lt    p2_next_j
    cmp     x8, x25
    b.ge    p2_next_j
    cmp     x9, #0
    b.lt    p2_next_j
    cmp     x9, x26
    b.ge    p2_next_j

    // Save current position to stack before function call
    stp     x6, x7, [sp, #-16]!

    // Add to set
    and     w8, w6, #0xFFFF
    lsl     w8, w8, #16
    and     w9, w7, #0xFFFF
    orr     w8, w8, w9
    mov     x0, x8
    LOAD_ADDR x1, antinodes_p2
    LOAD_ADDR x2, antinode_count_p2
    bl      add_to_set

    // Restore current position after function call
    ldp     x6, x7, [sp], #16

    // Move in opposite direction
    sub     w6, w6, w27                     // r -= dr
    sub     w7, w7, w28                     // c -= dc
    b       p2_dir2_loop

p2_next_j:
    add     x23, x23, #1
    b       p2_pair_j

p2_next_i:
    add     x22, x22, #1
    b       p2_pair_i

p2_next_freq:
    add     x19, x19, #1
    b       p2_freq_loop

p2_done:
    LOAD_ADDR x0, antinode_count_p2
    ldr     x0, [x0]

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// add_to_set: Add a value to a set (if not already present)
// Simple linear search through existing elements to check for duplicates.
// For small sets (< 2000 elements), this is faster than a hash table.
//
// Input:  x0 = value (32-bit packed position: row << 16 | col)
//         x1 = set array address (array of 32-bit packed positions)
//         x2 = count address (pointer to 64-bit count)
// Output: Set updated with value if not already present
// Clobbers: x0-x4 (caller-saved registers)
// ============================================================================
add_to_set:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x1                         // Set array
    mov     x20, x2                         // Count address

    // Check if value already in set
    ldr     x2, [x20]                       // Current count
    mov     x3, #0                          // Index

check_loop:
    cmp     x3, x2
    b.ge    add_new

    ldr     w4, [x19, x3, lsl #2]
    cmp     w4, w0
    b.eq    add_done                        // Already in set

    add     x3, x3, #1
    b       check_loop

add_new:
    // Add to end of set
    str     w0, [x19, x2, lsl #2]
    add     x2, x2, #1
    str     x2, [x20]

add_done:
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
