// =============================================================================
// Advent of Code 2023 - Day 14: Parabolic Reflector Dish
// ARM64 Assembly Solution for macOS
//
// Problem: Tilting platform with rolling rocks ('O'=79) and fixed rocks ('#'=35)
// Part 1: Tilt north, calculate load (sum of (rows - row_index) for each O)
// Part 2: Run 1,000,000,000 spin cycles (N->W->S->E), use cycle detection
//
// Algorithm: For each tilt direction, scan and move O's to nearest empty space
// For Part 2: Hash grid states, detect cycle, skip to final state
// =============================================================================

.global _start
.align 4

// -----------------------------------------------------------------------------
// macOS ARM64 BSD syscall numbers (0x2000000 + syscall_number)
// -----------------------------------------------------------------------------
.equ SYSCALL_BASE_HI, 0x2000
.equ SYS_EXIT_LO,     0x0001
.equ SYS_READ_LO,     0x0003
.equ SYS_WRITE_LO,    0x0004
.equ SYS_OPEN_LO,     0x0005
.equ SYS_CLOSE_LO,    0x0006

.equ O_RDONLY, 0x0000

// Character codes
.equ CHAR_O, 79       // 'O' - round rock
.equ CHAR_HASH, 35    // '#' - cube rock
.equ CHAR_DOT, 46     // '.' - empty space
.equ CHAR_NEWLINE, 10 // '\n'

// Buffer sizes
.equ MAX_FILE_SIZE, 65536
.equ MAX_GRID_SIZE, 200   // max 200x200 grid
.equ MAX_STATES, 500      // max states for cycle detection

// =============================================================================
// DATA SECTION
// =============================================================================
.data
input_path: .asciz "../input.txt"
part1_msg:  .asciz "Part 1: "
part2_msg:  .asciz "Part 2: "
newline:    .asciz "\n"

// =============================================================================
// BSS SECTION
// =============================================================================
.bss
.align 4
file_buffer:    .skip MAX_FILE_SIZE
grid:           .skip MAX_GRID_SIZE * MAX_GRID_SIZE   // Working grid
grid_rows:      .skip 8
grid_cols:      .skip 8
output_buffer:  .skip 32
file_size:      .skip 8

// Cycle detection: store hashes and cycle numbers
state_hashes:   .skip MAX_STATES * 8      // hash values
state_cycles:   .skip MAX_STATES * 8      // cycle numbers when state was seen
state_count:    .skip 8

// =============================================================================
// TEXT SECTION
// =============================================================================
.text

// -----------------------------------------------------------------------------
// _start: Program entry point
// -----------------------------------------------------------------------------
_start:
    // Open input file
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_OPEN_LO
    adrp    x0, input_path@PAGE
    add     x0, x0, input_path@PAGEOFF
    mov     x1, #O_RDONLY
    mov     x2, #0
    svc     #0
    mov     x19, x0                     // x19 = fd

    // Read file
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_READ_LO
    mov     x0, x19
    adrp    x1, file_buffer@PAGE
    add     x1, x1, file_buffer@PAGEOFF
    mov     x2, #MAX_FILE_SIZE
    svc     #0

    adrp    x1, file_size@PAGE
    add     x1, x1, file_size@PAGEOFF
    str     x0, [x1]

    // Close file
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_CLOSE_LO
    mov     x0, x19
    svc     #0

    // Parse input into grid
    bl      parse_input

    // -------------------------------------------------------------------------
    // Part 1: Tilt north and calculate load
    // -------------------------------------------------------------------------
    adrp    x0, part1_msg@PAGE
    add     x0, x0, part1_msg@PAGEOFF
    bl      print_str

    // Copy file buffer to grid for Part 1
    bl      reset_grid
    bl      tilt_north
    bl      calculate_load
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    // -------------------------------------------------------------------------
    // Part 2: 1 billion spin cycles with cycle detection
    // -------------------------------------------------------------------------
    adrp    x0, part2_msg@PAGE
    add     x0, x0, part2_msg@PAGEOFF
    bl      print_str

    bl      reset_grid
    bl      part2_solve
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    // Exit
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_EXIT_LO
    mov     x0, #0
    svc     #0

// =============================================================================
// parse_input: Determine grid dimensions from file buffer
// Sets grid_rows and grid_cols
// =============================================================================
parse_input:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    adrp    x19, file_buffer@PAGE
    add     x19, x19, file_buffer@PAGEOFF
    adrp    x20, file_size@PAGE
    add     x20, x20, file_size@PAGEOFF
    ldr     x20, [x20]

    // Find first newline to get width
    mov     x0, #0
.L_find_width:
    ldrb    w1, [x19, x0]
    cmp     w1, #CHAR_NEWLINE
    b.eq    .L_found_width
    add     x0, x0, #1
    b       .L_find_width
.L_found_width:
    // x0 = width (cols)
    adrp    x1, grid_cols@PAGE
    add     x1, x1, grid_cols@PAGEOFF
    str     x0, [x1]

    // Calculate rows = file_size / (cols + 1)
    add     x2, x0, #1                  // cols + 1 (including newline)
    udiv    x3, x20, x2
    adrp    x1, grid_rows@PAGE
    add     x1, x1, grid_rows@PAGEOFF
    str     x3, [x1]

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// reset_grid: Copy file buffer to working grid (stripping newlines)
// =============================================================================
reset_grid:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    adrp    x19, file_buffer@PAGE
    add     x19, x19, file_buffer@PAGEOFF
    adrp    x20, grid@PAGE
    add     x20, x20, grid@PAGEOFF
    adrp    x21, file_size@PAGE
    add     x21, x21, file_size@PAGEOFF
    ldr     x21, [x21]

    mov     x22, #0                     // dest index
    mov     x0, #0                      // src index
.L_copy_grid:
    cmp     x0, x21
    b.ge    .L_copy_done
    ldrb    w1, [x19, x0]
    cmp     w1, #CHAR_NEWLINE
    b.eq    .L_skip_newline
    strb    w1, [x20, x22]
    add     x22, x22, #1
.L_skip_newline:
    add     x0, x0, #1
    b       .L_copy_grid
.L_copy_done:

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// tilt_north: Move all 'O' rocks northward
// For each column, scan top to bottom, track write_pos
// =============================================================================
tilt_north:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    adrp    x19, grid@PAGE
    add     x19, x19, grid@PAGEOFF
    adrp    x20, grid_rows@PAGE
    add     x20, x20, grid_rows@PAGEOFF
    ldr     x20, [x20]                  // rows
    adrp    x21, grid_cols@PAGE
    add     x21, x21, grid_cols@PAGEOFF
    ldr     x21, [x21]                  // cols

    mov     x22, #0                     // col = 0
.L_north_col:
    cmp     x22, x21
    b.ge    .L_north_done

    mov     x23, #0                     // write_pos = 0
    mov     x24, #0                     // row = 0
.L_north_row:
    cmp     x24, x20
    b.ge    .L_north_next_col

    // Calculate grid[row][col] offset = row * cols + col
    mul     x0, x24, x21
    add     x0, x0, x22
    ldrb    w25, [x19, x0]

    cmp     w25, #CHAR_HASH
    b.ne    .L_north_check_o
    add     x23, x24, #1                // write_pos = row + 1
    b       .L_north_next_row

.L_north_check_o:
    cmp     w25, #CHAR_O
    b.ne    .L_north_next_row

    // Move O from row to write_pos
    // grid[row][col] = '.'
    mov     w26, #CHAR_DOT
    strb    w26, [x19, x0]
    // grid[write_pos][col] = 'O'
    mul     x1, x23, x21
    add     x1, x1, x22
    mov     w26, #CHAR_O
    strb    w26, [x19, x1]
    add     x23, x23, #1                // write_pos++

.L_north_next_row:
    add     x24, x24, #1
    b       .L_north_row

.L_north_next_col:
    add     x22, x22, #1
    b       .L_north_col

.L_north_done:
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// tilt_south: Move all 'O' rocks southward
// =============================================================================
tilt_south:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    adrp    x19, grid@PAGE
    add     x19, x19, grid@PAGEOFF
    adrp    x20, grid_rows@PAGE
    add     x20, x20, grid_rows@PAGEOFF
    ldr     x20, [x20]                  // rows
    adrp    x21, grid_cols@PAGE
    add     x21, x21, grid_cols@PAGEOFF
    ldr     x21, [x21]                  // cols

    mov     x22, #0                     // col = 0
.L_south_col:
    cmp     x22, x21
    b.ge    .L_south_done

    sub     x23, x20, #1                // write_pos = rows - 1
    sub     x24, x20, #1                // row = rows - 1
.L_south_row:
    cmp     x24, #0
    b.lt    .L_south_next_col

    mul     x0, x24, x21
    add     x0, x0, x22
    ldrb    w25, [x19, x0]

    cmp     w25, #CHAR_HASH
    b.ne    .L_south_check_o
    sub     x23, x24, #1                // write_pos = row - 1
    b       .L_south_next_row

.L_south_check_o:
    cmp     w25, #CHAR_O
    b.ne    .L_south_next_row

    // Move O from row to write_pos
    mov     w26, #CHAR_DOT
    strb    w26, [x19, x0]
    mul     x1, x23, x21
    add     x1, x1, x22
    mov     w26, #CHAR_O
    strb    w26, [x19, x1]
    sub     x23, x23, #1                // write_pos--

.L_south_next_row:
    sub     x24, x24, #1
    b       .L_south_row

.L_south_next_col:
    add     x22, x22, #1
    b       .L_south_col

.L_south_done:
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// tilt_west: Move all 'O' rocks westward
// =============================================================================
tilt_west:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    adrp    x19, grid@PAGE
    add     x19, x19, grid@PAGEOFF
    adrp    x20, grid_rows@PAGE
    add     x20, x20, grid_rows@PAGEOFF
    ldr     x20, [x20]                  // rows
    adrp    x21, grid_cols@PAGE
    add     x21, x21, grid_cols@PAGEOFF
    ldr     x21, [x21]                  // cols

    mov     x22, #0                     // row = 0
.L_west_row:
    cmp     x22, x20
    b.ge    .L_west_done

    mov     x23, #0                     // write_pos = 0
    mov     x24, #0                     // col = 0
.L_west_col:
    cmp     x24, x21
    b.ge    .L_west_next_row

    mul     x0, x22, x21
    add     x0, x0, x24
    ldrb    w25, [x19, x0]

    cmp     w25, #CHAR_HASH
    b.ne    .L_west_check_o
    add     x23, x24, #1                // write_pos = col + 1
    b       .L_west_next_col

.L_west_check_o:
    cmp     w25, #CHAR_O
    b.ne    .L_west_next_col

    // Move O from col to write_pos
    mov     w26, #CHAR_DOT
    strb    w26, [x19, x0]
    mul     x1, x22, x21
    add     x1, x1, x23
    mov     w26, #CHAR_O
    strb    w26, [x19, x1]
    add     x23, x23, #1                // write_pos++

.L_west_next_col:
    add     x24, x24, #1
    b       .L_west_col

.L_west_next_row:
    add     x22, x22, #1
    b       .L_west_row

.L_west_done:
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// tilt_east: Move all 'O' rocks eastward
// =============================================================================
tilt_east:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    adrp    x19, grid@PAGE
    add     x19, x19, grid@PAGEOFF
    adrp    x20, grid_rows@PAGE
    add     x20, x20, grid_rows@PAGEOFF
    ldr     x20, [x20]                  // rows
    adrp    x21, grid_cols@PAGE
    add     x21, x21, grid_cols@PAGEOFF
    ldr     x21, [x21]                  // cols

    mov     x22, #0                     // row = 0
.L_east_row:
    cmp     x22, x20
    b.ge    .L_east_done

    sub     x23, x21, #1                // write_pos = cols - 1
    sub     x24, x21, #1                // col = cols - 1
.L_east_col:
    cmp     x24, #0
    b.lt    .L_east_next_row

    mul     x0, x22, x21
    add     x0, x0, x24
    ldrb    w25, [x19, x0]

    cmp     w25, #CHAR_HASH
    b.ne    .L_east_check_o
    sub     x23, x24, #1                // write_pos = col - 1
    b       .L_east_next_col

.L_east_check_o:
    cmp     w25, #CHAR_O
    b.ne    .L_east_next_col

    // Move O from col to write_pos
    mov     w26, #CHAR_DOT
    strb    w26, [x19, x0]
    mul     x1, x22, x21
    add     x1, x1, x23
    mov     w26, #CHAR_O
    strb    w26, [x19, x1]
    sub     x23, x23, #1                // write_pos--

.L_east_next_col:
    sub     x24, x24, #1
    b       .L_east_col

.L_east_next_row:
    add     x22, x22, #1
    b       .L_east_row

.L_east_done:
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// spin_cycle: Perform N->W->S->E tilt sequence
// =============================================================================
spin_cycle:
    stp     x29, x30, [sp, #-16]!

    bl      tilt_north
    bl      tilt_west
    bl      tilt_south
    bl      tilt_east

    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// calculate_load: Sum of (rows - row_index) for each 'O'
// Returns: x0 = total load
// =============================================================================
calculate_load:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    adrp    x19, grid@PAGE
    add     x19, x19, grid@PAGEOFF
    adrp    x20, grid_rows@PAGE
    add     x20, x20, grid_rows@PAGEOFF
    ldr     x20, [x20]                  // rows
    adrp    x21, grid_cols@PAGE
    add     x21, x21, grid_cols@PAGEOFF
    ldr     x21, [x21]                  // cols

    mov     x0, #0                      // total = 0
    mov     x22, #0                     // index = 0

.L_load_loop:
    mul     x1, x20, x21                // rows * cols
    cmp     x22, x1
    b.ge    .L_load_done

    ldrb    w2, [x19, x22]
    cmp     w2, #CHAR_O
    b.ne    .L_load_next

    // row = index / cols
    udiv    x3, x22, x21
    // load = rows - row
    sub     x4, x20, x3
    add     x0, x0, x4

.L_load_next:
    add     x22, x22, #1
    b       .L_load_loop

.L_load_done:
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// hash_grid: Compute a simple hash of the grid
// Returns: x0 = hash value
// Uses FNV-1a style hashing
// =============================================================================
hash_grid:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    adrp    x19, grid@PAGE
    add     x19, x19, grid@PAGEOFF
    adrp    x20, grid_rows@PAGE
    add     x20, x20, grid_rows@PAGEOFF
    ldr     x20, [x20]
    adrp    x21, grid_cols@PAGE
    add     x21, x21, grid_cols@PAGEOFF
    ldr     x21, [x21]
    mul     x20, x20, x21               // total size

    // FNV-1a offset basis
    movz    x0, #0x4B35, lsl #48
    movk    x0, #0xB502, lsl #32
    movk    x0, #0x406F, lsl #16
    movk    x0, #0x7CF5

    // FNV prime
    movz    x22, #0x0001, lsl #48
    movk    x22, #0x00B3, lsl #32
    movk    x22, #0x0000, lsl #16
    movk    x22, #0x0193

    mov     x1, #0
.L_hash_loop:
    cmp     x1, x20
    b.ge    .L_hash_done

    ldrb    w2, [x19, x1]
    eor     x0, x0, x2
    mul     x0, x0, x22

    add     x1, x1, #1
    b       .L_hash_loop

.L_hash_done:
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// part2_solve: Run 1 billion cycles with cycle detection
// Returns: x0 = final load
// =============================================================================
part2_solve:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    // Initialize state storage
    adrp    x19, state_count@PAGE
    add     x19, x19, state_count@PAGEOFF
    str     xzr, [x19]

    // Target = 1,000,000,000
    movz    x20, #0x3B9A, lsl #16
    movk    x20, #0xCA00                // 0x3B9ACA00 = 1,000,000,000

    mov     x21, #0                     // cycle_num = 0

.L_p2_loop:
    cmp     x21, x20
    b.ge    .L_p2_done

    // Hash current grid state
    bl      hash_grid
    mov     x22, x0                     // current hash

    // Search for this hash in seen states
    adrp    x23, state_hashes@PAGE
    add     x23, x23, state_hashes@PAGEOFF
    adrp    x24, state_cycles@PAGE
    add     x24, x24, state_cycles@PAGEOFF
    adrp    x25, state_count@PAGE
    add     x25, x25, state_count@PAGEOFF
    ldr     x25, [x25]                  // num_states

    mov     x26, #0                     // i = 0
.L_search_state:
    cmp     x26, x25
    b.ge    .L_state_not_found

    ldr     x0, [x23, x26, lsl #3]      // state_hashes[i]
    cmp     x0, x22
    b.eq    .L_state_found

    add     x26, x26, #1
    b       .L_search_state

.L_state_found:
    // Cycle detected!
    // cycle_start = state_cycles[i]
    ldr     x0, [x24, x26, lsl #3]
    // cycle_length = cycle_num - cycle_start
    sub     x1, x21, x0
    // remaining = (target - cycle_num) % cycle_length
    sub     x2, x20, x21
    udiv    x3, x2, x1
    msub    x4, x3, x1, x2              // remaining = x2 - x3 * x1

    // Run remaining cycles
.L_run_remaining:
    cbz     x4, .L_p2_done
    bl      spin_cycle
    sub     x4, x4, #1
    b       .L_run_remaining

.L_state_not_found:
    // Store current state
    adrp    x25, state_count@PAGE
    add     x25, x25, state_count@PAGEOFF
    ldr     x26, [x25]

    str     x22, [x23, x26, lsl #3]     // state_hashes[count] = hash
    str     x21, [x24, x26, lsl #3]     // state_cycles[count] = cycle_num

    add     x26, x26, #1
    str     x26, [x25]                  // state_count++

    // Perform one spin cycle
    bl      spin_cycle
    add     x21, x21, #1
    b       .L_p2_loop

.L_p2_done:
    bl      calculate_load

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// UTILITY FUNCTIONS
// =============================================================================

// -----------------------------------------------------------------------------
// print_str: Write null-terminated string to stdout
// Input: x0 = string address
// -----------------------------------------------------------------------------
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0

    // Calculate length
    mov     x20, #0
.L_strlen:
    ldrb    w1, [x19, x20]
    cbz     w1, .L_strlen_done
    add     x20, x20, #1
    b       .L_strlen
.L_strlen_done:

    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_WRITE_LO
    mov     x0, #1
    mov     x1, x19
    mov     x2, x20
    svc     #0

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// -----------------------------------------------------------------------------
// print_num: Write decimal number to stdout
// Input: x0 = number to print
// -----------------------------------------------------------------------------
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0
    adrp    x20, output_buffer@PAGE
    add     x20, x20, output_buffer@PAGEOFF
    add     x20, x20, #31
    mov     x21, #0

    // Handle zero
    cbnz    x19, .L_num_loop
    mov     w22, #'0'
    strb    w22, [x20, #-1]!
    mov     x21, #1
    b       .L_num_print

.L_num_loop:
    cbz     x19, .L_num_print

    mov     x1, #10
    udiv    x2, x19, x1
    msub    x3, x2, x1, x19

    add     w3, w3, #'0'
    strb    w3, [x20, #-1]!
    add     x21, x21, #1

    mov     x19, x2
    b       .L_num_loop

.L_num_print:
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_WRITE_LO
    mov     x0, #1
    mov     x1, x20
    mov     x2, x21
    svc     #0

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
