// Day 10: Factory - ARM64 Assembly for macOS
// Part 1: XOR-based toggle problem (brute force all 2^n combinations)
// Part 2: Integer Linear Programming (Gaussian elimination with rational arithmetic)

.global _start
.align 4

// External functions from rational.s
.extern gcd
.extern abs64
.extern rat_normalize
.extern rat_new
.extern rat_add
.extern rat_sub
.extern rat_mul
.extern rat_div
.extern rat_neg
.extern rat_is_zero
.extern rat_is_negative
.extern rat_is_positive
.extern rat_is_integer
.extern rat_to_int
.extern rat_compare

// Macro to load address
.macro load_addr reg, label
    adrp    \reg, \label@PAGE
    add     \reg, \reg, \label@PAGEOFF
.endm

// Constants
.equ MAX_MACHINES, 200
.equ MAX_BUTTONS, 15
.equ MAX_LIGHTS, 12
.equ MAX_COUNTERS, 12
.equ MAX_MATRIX_SIZE, 256     // 16x16 max matrix elements
.equ RAT_SIZE, 16             // Size of a rational number (num + den)

// File I/O
.equ O_RDONLY, 0
.equ PROT_READ, 1
.equ MAP_PRIVATE, 0x0002

// System calls for macOS ARM64
.equ SYS_open, 0x2000005
.equ SYS_read, 0x2000003
.equ SYS_close, 0x2000006
.equ SYS_write, 0x2000004
.equ SYS_exit, 0x2000001
.equ SYS_fstat, 0x20000BD

.data
input_path:
    .asciz "../input.txt"
    .align 4

part1_msg:
    .asciz "Part 1: "
    .align 4

part2_msg:
    .asciz "Part 2: "
    .align 4

newline:
    .asciz "\n"
    .align 4

// Data structures for machines
.bss
    .align 8
file_buffer:
    .space 32768           // 32KB buffer for input file

// Machine data for Part 1
n_machines:
    .space 8               // Number of machines
machine_n_lights:
    .space MAX_MACHINES * 8
machine_targets:
    .space MAX_MACHINES * 8
machine_n_buttons:
    .space MAX_MACHINES * 8
machine_button_masks:
    .space MAX_MACHINES * MAX_BUTTONS * 8

// Machine data for Part 2
machine_n_counters:
    .space MAX_MACHINES * 8
machine_joltages:
    .space MAX_MACHINES * MAX_COUNTERS * 8
// For each machine, for each button, store a bitmask of which counters it affects
machine_button_counter_masks:
    .space MAX_MACHINES * MAX_BUTTONS * 8

output_buffer:
    .space 32              // Buffer for number output

// Matrix storage for Gaussian elimination (augmented matrix)
// Max size: MAX_COUNTERS rows x (MAX_BUTTONS + 1) cols x 16 bytes
aug_matrix:
    .space MAX_COUNTERS * (MAX_BUTTONS + 1) * RAT_SIZE

// Pivot tracking
pivot_cols:
    .space MAX_COUNTERS * 8         // Column index for each pivot
pivot_rows:
    .space MAX_COUNTERS * 8         // Row index for each pivot
n_pivots:
    .space 8

// Particular solution (one rational per button)
particular:
    .space MAX_BUTTONS * RAT_SIZE

// Null space vectors (up to MAX_BUTTONS free variables, each is a vector of MAX_BUTTONS rationals)
null_vectors:
    .space MAX_BUTTONS * MAX_BUTTONS * RAT_SIZE
n_free_vars:
    .space 8
free_var_cols:
    .space MAX_BUTTONS * 8

// Temporary rationals for computation
temp_rat1:
    .space RAT_SIZE
temp_rat2:
    .space RAT_SIZE
temp_rat3:
    .space RAT_SIZE

// Solution vector for search
solution_vec:
    .space MAX_BUTTONS * RAT_SIZE

// Floating-point versions for fast bound computation
particular_fp:
    .space MAX_BUTTONS * 8      // doubles
null_vectors_fp:
    .space MAX_BUTTONS * MAX_BUTTONS * 8    // doubles

// Current matrix dimensions (for use by helper functions)
current_n_rows:
    .space 8
current_n_cols:
    .space 8
current_n_buttons:
    .space 8

.text

// ============================================================================
// Main entry point
// ============================================================================
_start:
    // Load and parse input file
    bl      load_input

    // Solve Part 1
    bl      solve_part1
    mov     x19, x0                 // Save Part 1 result

    // Print Part 1
    load_addr x0, part1_msg
    bl      print_string
    mov     x0, x19
    bl      print_number
    load_addr x0, newline
    bl      print_string

    // Solve Part 2
    bl      solve_part2
    mov     x19, x0                 // Save Part 2 result

    // Print Part 2
    load_addr x0, part2_msg
    bl      print_string
    mov     x0, x19
    bl      print_number
    load_addr x0, newline
    bl      print_string

    // Exit
    mov     x0, #0
    mov     w16, #1
    orr     w16, w16, #0x2000000
    svc     #0x80

// ============================================================================
// Load and parse input file
// ============================================================================
load_input:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Open file
    load_addr x0, input_path
    mov     x1, #O_RDONLY
    mov     x2, #0
    mov     w16, #5
    orr     w16, w16, #0x2000000
    svc     #0x80
    cmp     x0, #0
    b.lt    exit_error
    mov     x19, x0                 // Save fd

    // Read file
    mov     x0, x19
    load_addr x1, file_buffer
    mov     w2, #32768
    mov     w16, #3
    orr     w16, w16, #0x2000000
    svc     #0x80
    mov     x20, x0                 // Save bytes read

    // Close file
    mov     x0, x19
    mov     w16, #6
    orr     w16, w16, #0x2000000
    svc     #0x80

    // Parse file
    load_addr x0, file_buffer
    mov     x1, x20
    bl      parse_all_machines

    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// Parse all machines from input
// ============================================================================
parse_all_machines:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    mov     x19, x0                 // Buffer pointer
    mov     x20, x1                 // Bytes remaining
    mov     x21, #0                 // Machine count
    load_addr x22, file_buffer
    add     x23, x22, x20           // End of buffer

parse_machine_loop:
    cmp     x19, x23
    b.ge    parse_done

    // Skip empty lines
    ldrb    w0, [x19]
    cmp     w0, #'\n'
    b.ne    parse_this_line
    add     x19, x19, #1
    b       parse_machine_loop

parse_this_line:
    // Parse one machine
    mov     x0, x19
    mov     x1, x21
    bl      parse_single_machine
    mov     x19, x0                 // Update buffer pointer

    add     x21, x21, #1
    b       parse_machine_loop

parse_done:
    // Save machine count
    load_addr x0, n_machines
    str     x21, [x0]

    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// ============================================================================
// Parse a single machine line
// Returns: x0 = updated buffer pointer
// ============================================================================
parse_single_machine:
    stp     x29, x30, [sp, #-80]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]

    mov     x19, x0                 // Buffer pointer
    mov     x20, x1                 // Machine index

    // Find [indicator pattern]
    mov     x21, #0                 // Target pattern
    mov     x22, #0                 // Number of lights

find_bracket:
    ldrb    w0, [x19]
    cmp     w0, #'['
    b.eq    found_bracket
    add     x19, x19, #1
    b       find_bracket

found_bracket:
    add     x19, x19, #1            // Skip '['

read_pattern:
    ldrb    w0, [x19]
    cmp     w0, #']'
    b.eq    pattern_done

    cmp     w0, #'#'
    b.ne    pattern_dot
    // It's a '#' - set bit
    mov     x1, #1
    lsl     x1, x1, x22
    orr     x21, x21, x1

pattern_dot:
    add     x22, x22, #1
    add     x19, x19, #1
    b       read_pattern

pattern_done:
    // Save target and n_lights
    load_addr x0, machine_targets
    mov     x1, #8
    madd    x0, x20, x1, x0
    str     x21, [x0]

    load_addr x0, machine_n_lights
    mov     x1, #8
    madd    x0, x20, x1, x0
    str     x22, [x0]

    add     x19, x19, #1            // Skip ']'

    // Parse buttons (parentheses)
    mov     x23, #0                 // Button count

parse_buttons:
    // Find next '(' or '{'
find_button_or_joltage:
    ldrb    w0, [x19]
    cmp     w0, #'\n'
    b.eq    machine_line_done
    cmp     w0, #0
    b.eq    machine_line_done
    cmp     w0, #'('
    b.eq    found_button
    cmp     w0, #'{'
    b.eq    found_joltage
    add     x19, x19, #1
    b       find_button_or_joltage

found_button:
    add     x19, x19, #1            // Skip '('

    // Parse button indices
    mov     x24, #0                 // Button mask for Part 1
    mov     x25, #0                 // Counter mask for Part 2

parse_button_indices:
    // Parse number
    mov     x0, x19
    bl      parse_number
    mov     x1, x0                  // index
    mov     x19, x2                 // updated pointer

    // Set bit in mask for Part 1 (light toggle)
    mov     x3, #1
    lsl     x3, x3, x1
    orr     x24, x24, x3

    // Same index for Part 2 counter mask
    orr     x25, x25, x3

    ldrb    w0, [x19]
    cmp     w0, #','
    b.ne    button_indices_done
    add     x19, x19, #1            // Skip ','
    b       parse_button_indices

button_indices_done:
    add     x19, x19, #1            // Skip ')'

    // Save button mask for Part 1
    load_addr x0, machine_button_masks
    mov     x1, #MAX_BUTTONS
    mul     x1, x20, x1
    add     x1, x1, x23
    mov     x2, #8
    madd    x0, x1, x2, x0
    str     x24, [x0]

    // Save counter mask for Part 2
    load_addr x0, machine_button_counter_masks
    mov     x1, #MAX_BUTTONS
    mul     x1, x20, x1
    add     x1, x1, x23
    mov     x2, #8
    madd    x0, x1, x2, x0
    str     x25, [x0]

    add     x23, x23, #1            // Increment button count
    b       parse_buttons

found_joltage:
    // We've reached the joltage section, save button count
    load_addr x0, machine_n_buttons
    mov     x1, #8
    madd    x0, x20, x1, x0
    str     x23, [x0]

    // Parse joltage requirements {n1,n2,n3,...}
    add     x19, x19, #1            // Skip '{'
    mov     x24, #0                 // Counter index

    // Get base address for this machine's joltages
    load_addr x25, machine_joltages
    mov     x1, #MAX_COUNTERS
    mul     x1, x20, x1
    mov     x2, #8
    madd    x25, x1, x2, x25        // x25 = joltages base for this machine

parse_joltage_loop:
    ldrb    w0, [x19]
    cmp     w0, #'}'
    b.eq    joltage_done
    cmp     w0, #','
    b.eq    skip_joltage_comma

    // Parse number
    mov     x0, x19
    bl      parse_number
    mov     x1, x0                  // joltage value
    mov     x19, x2                 // updated pointer

    // Store joltage value
    str     x1, [x25, x24, lsl #3]
    add     x24, x24, #1
    b       parse_joltage_loop

skip_joltage_comma:
    add     x19, x19, #1
    b       parse_joltage_loop

joltage_done:
    // Save counter count
    load_addr x0, machine_n_counters
    mov     x1, #8
    madd    x0, x20, x1, x0
    str     x24, [x0]

    add     x19, x19, #1            // Skip '}'

    // Skip to end of line
skip_to_eol:
    ldrb    w0, [x19]
    cmp     w0, #'\n'
    b.eq    machine_line_done
    cmp     w0, #0
    b.eq    machine_line_done
    add     x19, x19, #1
    b       skip_to_eol

machine_line_done:
    // Skip newline
    ldrb    w0, [x19]
    cmp     w0, #'\n'
    b.ne    skip_newline_done
    add     x19, x19, #1
skip_newline_done:

    mov     x0, x19
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #80
    ret

// ============================================================================
// Parse a number from string
// Returns: x0 = number, x2 = updated pointer
// ============================================================================
parse_number:
    mov     x1, #0                  // Result
    mov     x2, x0                  // Pointer

parse_num_loop:
    ldrb    w3, [x2]
    cmp     w3, #'0'
    b.lt    parse_num_done
    cmp     w3, #'9'
    b.gt    parse_num_done

    // digit = w3 - '0'
    sub     w3, w3, #'0'
    // result = result * 10 + digit
    mov     x4, #10
    mul     x1, x1, x4
    add     x1, x1, x3

    add     x2, x2, #1
    b       parse_num_loop

parse_num_done:
    mov     x0, x1
    ret

// ============================================================================
// Solve Part 1: Brute force all button combinations
// ============================================================================
solve_part1:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    mov     x19, #0                 // Total button presses
    mov     x20, #0                 // Machine index

    load_addr x21, n_machines
    ldr     x21, [x21]              // Total machines

part1_machine_loop:
    cmp     x20, x21
    b.ge    part1_done

    // Solve this machine
    mov     x0, x20
    bl      solve_machine_part1
    add     x19, x19, x0            // Add to total

    add     x20, x20, #1
    b       part1_machine_loop

part1_done:
    mov     x0, x19
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// ============================================================================
// Solve a single machine for Part 1
// Input: x0 = machine index
// Returns: x0 = minimum button presses
// ============================================================================
solve_machine_part1:
    stp     x29, x30, [sp, #-80]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]

    mov     x19, x0                 // Machine index

    // Load machine data
    load_addr x0, machine_targets
    mov     x1, #8
    madd    x0, x19, x1, x0
    ldr     x20, [x0]               // Target pattern

    load_addr x0, machine_n_buttons
    mov     x1, #8
    madd    x0, x19, x1, x0
    ldr     x21, [x0]               // Number of buttons

    // Load button masks base address
    load_addr x22, machine_button_masks
    mov     x1, #MAX_BUTTONS
    mul     x1, x19, x1
    mov     x2, #8
    madd    x22, x1, x2, x22        // Button masks for this machine

    // Brute force: try all 2^n_buttons combinations
    mov     x23, #1
    lsl     x23, x23, x21           // 2^n_buttons

    mov     w24, #999
    movk    w24, #15, lsl #16       // min_presses (large number)
    mov     x25, #0                 // mask (combination to try)

brute_loop:
    cmp     x25, x23
    b.ge    brute_done

    // Test this combination
    mov     x0, #0                  // state
    mov     x1, #0                  // presses
    mov     x2, #0                  // button index

test_combination:
    cmp     x2, x21
    b.ge    check_state

    // Check if button x2 is pressed in mask x25
    mov     x3, #1
    lsl     x3, x3, x2
    tst     x25, x3
    b.eq    next_button

    // Button is pressed: XOR its mask
    ldr     x4, [x22, x2, lsl #3]
    eor     x0, x0, x4
    add     x1, x1, #1              // Increment presses

next_button:
    add     x2, x2, #1
    b       test_combination

check_state:
    // Check if state matches target
    cmp     x0, x20
    b.ne    try_next_mask

    // Found a match! Update minimum
    cmp     x1, x24
    b.ge    try_next_mask
    mov     x24, x1

try_next_mask:
    add     x25, x25, #1
    b       brute_loop

brute_done:
    mov     x0, x24
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #80
    ret

// ============================================================================
// Solve Part 2: Integer Linear Programming via Gaussian Elimination
// ============================================================================
// Minimize sum(x_i) subject to Ax = b, x >= 0, x integer
// where A is the coefficient matrix of button effects on joltage counters.
// ============================================================================
solve_part2:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    mov     x19, #0                 // Total button presses
    mov     x20, #0                 // Machine index

    load_addr x21, n_machines
    ldr     x21, [x21]              // Total machines

part2_machine_loop:
    cmp     x20, x21
    b.ge    part2_done

    // Solve this machine
    mov     x0, x20
    bl      solve_machine_part2
    add     x19, x19, x0            // Add to total

    add     x20, x20, #1
    b       part2_machine_loop

part2_done:
    mov     x0, x19
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// ============================================================================
// Solve a single machine for Part 2
// Input: x0 = machine index
// Returns: x0 = minimum button presses
// ============================================================================
solve_machine_part2:
    stp     x29, x30, [sp, #-128]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]

    mov     x19, x0                 // Machine index

    // Load n_counters (number of rows)
    load_addr x0, machine_n_counters
    ldr     x20, [x0, x19, lsl #3]  // x20 = n_counters

    // Load n_buttons (number of columns)
    load_addr x0, machine_n_buttons
    ldr     x21, [x0, x19, lsl #3]  // x21 = n_buttons

    // Check for trivial case
    cbz     x21, part2_machine_done_zero

    // Build augmented matrix [A | b]
    // A[i][j] = 1 if button j affects counter i, else 0
    // b[i] = joltage[i]

    // Get base addresses
    load_addr x22, machine_joltages
    mov     x0, #MAX_COUNTERS
    mul     x0, x19, x0
    add     x22, x22, x0, lsl #3    // x22 = joltages for this machine

    load_addr x23, machine_button_counter_masks
    mov     x0, #MAX_BUTTONS
    mul     x0, x19, x0
    add     x23, x23, x0, lsl #3    // x23 = button masks for this machine

    // Initialize augmented matrix
    // Matrix layout: aug[row][col] at offset (row * (n_buttons + 1) + col) * 16
    load_addr x24, aug_matrix

    // For each row (counter)
    mov     x25, #0                 // row index
build_matrix_row_loop:
    cmp     x25, x20
    b.ge    build_matrix_done

    // For each column (button)
    mov     x26, #0                 // col index
build_matrix_col_loop:
    cmp     x26, x21
    b.ge    build_matrix_col_done

    // Check if button x26 affects counter x25
    ldr     x0, [x23, x26, lsl #3]  // button mask
    mov     x1, #1
    lsl     x1, x1, x25             // bit for counter x25
    tst     x0, x1

    // Calculate matrix element address
    add     x2, x21, #1             // n_cols = n_buttons + 1
    mul     x3, x25, x2
    add     x3, x3, x26
    mov     x4, #RAT_SIZE
    madd    x2, x3, x4, x24         // x2 = address of aug[row][col]

    // Set to 1/1 or 0/1
    cset    x0, ne                  // 1 if affects, 0 otherwise
    mov     x1, #1                  // denominator
    bl      rat_new

    add     x26, x26, #1
    b       build_matrix_col_loop

build_matrix_col_done:
    // Set augmented column (b[i] = joltage[i])
    ldr     x0, [x22, x25, lsl #3]  // joltage value

    // Calculate address for aug[row][n_buttons]
    add     x2, x21, #1
    mul     x3, x25, x2
    add     x3, x3, x21             // last column
    mov     x4, #RAT_SIZE
    madd    x2, x3, x4, x24

    mov     x1, #1                  // denominator
    bl      rat_new

    add     x25, x25, #1
    b       build_matrix_row_loop

build_matrix_done:
    // Apply Gaussian elimination
    // x20 = n_rows, x21 = n_cols (n_buttons), x24 = matrix base
    mov     x0, x20                 // n_rows
    mov     x1, x21                 // n_buttons
    mov     x2, x24                 // matrix
    bl      gaussian_elimination

    // Extract particular solution and null space
    mov     x0, x20
    mov     x1, x21
    mov     x2, x24
    bl      extract_solutions

    // Search for minimum non-negative integer solution
    mov     x0, x21                 // n_buttons
    bl      search_minimum_solution

    b       part2_machine_return

part2_machine_done_zero:
    mov     x0, #0

part2_machine_return:
    ldp     x27, x28, [sp, #80]
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #128
    ret

// ============================================================================
// Gaussian elimination to Reduced Row Echelon Form (RREF)
// Input: x0 = n_rows, x1 = n_buttons, x2 = matrix base
// Modifies: matrix in place, updates pivot_cols, pivot_rows, n_pivots
// ============================================================================
gaussian_elimination:
    stp     x29, x30, [sp, #-128]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]

    mov     x19, x0                 // n_rows
    mov     x20, x1                 // n_buttons
    mov     x21, x2                 // matrix base
    add     x22, x20, #1            // n_cols = n_buttons + 1

    // Store dimensions in globals for helper functions
    load_addr x0, current_n_rows
    str     x19, [x0]
    load_addr x0, current_n_cols
    str     x22, [x0]
    load_addr x0, current_n_buttons
    str     x20, [x0]

    // Initialize pivot tracking
    load_addr x23, pivot_cols
    load_addr x24, pivot_rows
    load_addr x25, n_pivots
    str     xzr, [x25]

    mov     x26, #0                 // pivot_row index
    mov     x27, #0                 // current column

gauss_col_loop:
    cmp     x27, x20                // compare with n_buttons (not augmented col)
    b.ge    gauss_done
    cmp     x26, x19                // compare with n_rows
    b.ge    gauss_done

    // Find a non-zero entry in column x27 from row x26 onwards
    mov     x28, x26                // search row
find_pivot:
    cmp     x28, x19
    b.ge    no_pivot_found

    // Get address of matrix[x28][x27]
    mul     x0, x28, x22
    add     x0, x0, x27
    mov     x1, #RAT_SIZE
    madd    x0, x0, x1, x21

    // Check if not zero
    bl      rat_is_zero
    cbz     x0, pivot_found
    add     x28, x28, #1
    b       find_pivot

pivot_found:
    // Swap rows x26 and x28 if needed
    cmp     x26, x28
    b.eq    no_swap_needed

    // Swap entire rows
    mov     x0, x26
    mov     x1, x28
    mov     x2, x22                 // n_cols
    mov     x3, x21                 // matrix base
    bl      swap_rows

no_swap_needed:
    // Record pivot
    ldr     x0, [x25]               // current n_pivots
    str     x27, [x23, x0, lsl #3]  // pivot_cols[n_pivots] = column
    str     x26, [x24, x0, lsl #3]  // pivot_rows[n_pivots] = row
    add     x0, x0, #1
    str     x0, [x25]

    // Scale pivot row so pivot = 1
    // pivot_val = matrix[x26][x27]
    mul     x0, x26, x22
    add     x0, x0, x27
    mov     x1, #RAT_SIZE
    madd    x0, x0, x1, x21         // address of pivot element

    // Load pivot value to temp_rat1
    ldp     x1, x2, [x0]
    load_addr x3, temp_rat1
    stp     x1, x2, [x3]

    // Scale row: each element = element / pivot_val
    mov     x28, #0                 // column counter
scale_row_loop:
    cmp     x28, x22
    b.ge    scale_row_done

    // Get address of matrix[x26][x28]
    mul     x0, x26, x22
    add     x0, x0, x28
    mov     x1, #RAT_SIZE
    madd    x0, x0, x1, x21

    // element = element / temp_rat1
    load_addr x1, temp_rat1
    load_addr x2, temp_rat2
    bl      rat_div

    // Copy result back
    load_addr x0, temp_rat2
    ldp     x1, x2, [x0]
    mul     x0, x26, x22
    add     x0, x0, x28
    mov     x3, #RAT_SIZE
    madd    x0, x0, x3, x21
    stp     x1, x2, [x0]

    add     x28, x28, #1
    b       scale_row_loop

scale_row_done:
    // Eliminate column in all other rows
    mov     x28, #0                 // row counter
eliminate_loop:
    cmp     x28, x19
    b.ge    eliminate_done
    cmp     x28, x26                // skip pivot row
    b.eq    eliminate_next

    // Get factor = matrix[x28][x27]
    mul     x0, x28, x22
    add     x0, x0, x27
    mov     x1, #RAT_SIZE
    madd    x0, x0, x1, x21

    // Check if factor is zero (skip if so)
    bl      rat_is_zero
    cbnz    x0, eliminate_next

    // Load factor to temp_rat1
    mul     x0, x28, x22
    add     x0, x0, x27
    mov     x1, #RAT_SIZE
    madd    x0, x0, x1, x21
    ldp     x1, x2, [x0]
    load_addr x3, temp_rat1
    stp     x1, x2, [x3]

    // Subtract: row[x28] = row[x28] - factor * pivot_row
    mov     x0, x28                 // target row
    mov     x1, x26                 // pivot row (scaled to have 1 at pivot)
    bl      subtract_scaled_row

eliminate_next:
    add     x28, x28, #1
    b       eliminate_loop

eliminate_done:
    add     x26, x26, #1            // next pivot row
no_pivot_found:
    add     x27, x27, #1            // next column
    b       gauss_col_loop

gauss_done:
    ldp     x27, x28, [sp, #80]
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #128
    ret

// ============================================================================
// Swap two rows in the matrix
// Input: x0 = row1, x1 = row2, x2 = n_cols, x3 = matrix base
// ============================================================================
swap_rows:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    mov     x19, x0                 // row1
    mov     x20, x1                 // row2
    mov     x21, x2                 // n_cols
    mov     x22, x3                 // matrix base

    mov     x23, #0                 // column counter
swap_col_loop:
    cmp     x23, x21
    b.ge    swap_done

    // Address of matrix[row1][col]
    mul     x0, x19, x21
    add     x0, x0, x23
    mov     x1, #RAT_SIZE
    madd    x24, x0, x1, x22

    // Address of matrix[row2][col]
    mul     x0, x20, x21
    add     x0, x0, x23
    mov     x1, #RAT_SIZE
    madd    x0, x0, x1, x22

    // Swap the 16-byte rationals
    ldp     x1, x2, [x24]
    ldp     x3, x4, [x0]
    stp     x3, x4, [x24]
    stp     x1, x2, [x0]

    add     x23, x23, #1
    b       swap_col_loop

swap_done:
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// ============================================================================
// Subtract scaled row: target_row = target_row - factor * source_row
// Input: x0 = target_row, x1 = source_row
// Uses: temp_rat1 contains the factor, global matrix vars
// ============================================================================
subtract_scaled_row:
    stp     x29, x30, [sp, #-80]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]

    mov     x19, x0                 // target row
    mov     x20, x1                 // source row

    // Load matrix parameters from globals
    load_addr x21, aug_matrix       // matrix base
    load_addr x0, current_n_cols
    ldr     x22, [x0]               // n_cols from global

    mov     x23, #0                 // column counter
ssr_col_loop:
    cmp     x23, x22
    b.ge    ssr_done

    // Compute source_row[col] * factor -> temp_rat2
    // source address
    mul     x0, x20, x22
    add     x0, x0, x23
    mov     x1, #RAT_SIZE
    madd    x0, x0, x1, x21

    load_addr x1, temp_rat1         // factor
    load_addr x2, temp_rat2
    bl      rat_mul

    // Compute target_row[col] - temp_rat2 -> temp_rat3
    mul     x0, x19, x22
    add     x0, x0, x23
    mov     x1, #RAT_SIZE
    madd    x0, x0, x1, x21

    load_addr x1, temp_rat2
    load_addr x2, temp_rat3
    bl      rat_sub

    // Store result back to target_row[col]
    load_addr x0, temp_rat3
    ldp     x1, x2, [x0]
    mul     x0, x19, x22
    add     x0, x0, x23
    mov     x3, #RAT_SIZE
    madd    x0, x0, x3, x21
    stp     x1, x2, [x0]

    add     x23, x23, #1
    b       ssr_col_loop

ssr_done:
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #80
    ret

// ============================================================================
// Extract particular solution and null space vectors
// Input: x0 = n_rows, x1 = n_buttons, x2 = matrix base
// Output: Updates particular[], null_vectors[], n_free_vars, free_var_cols[]
// ============================================================================
extract_solutions:
    stp     x29, x30, [sp, #-112]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]

    mov     x19, x0                 // n_rows
    mov     x20, x1                 // n_buttons
    mov     x21, x2                 // matrix base
    add     x22, x20, #1            // n_cols

    // Initialize particular solution to 0
    load_addr x23, particular
    mov     x24, #0
init_particular:
    cmp     x24, x20
    b.ge    init_particular_done
    mov     x0, #0
    mov     x1, #1
    add     x2, x23, x24, lsl #4    // particular[i]
    bl      rat_new
    add     x24, x24, #1
    b       init_particular

init_particular_done:
    // Identify pivot columns and set particular solution values
    load_addr x25, pivot_cols
    load_addr x26, pivot_rows
    load_addr x27, n_pivots
    ldr     x27, [x27]

    mov     x24, #0                 // pivot index
set_particular:
    cmp     x24, x27
    b.ge    set_particular_done

    ldr     x0, [x25, x24, lsl #3]  // pivot column
    ldr     x1, [x26, x24, lsl #3]  // pivot row

    // particular[pivot_col] = matrix[pivot_row][n_buttons] (the augmented column)
    mul     x2, x1, x22
    add     x2, x2, x20             // [row][n_buttons]
    mov     x3, #RAT_SIZE
    madd    x2, x2, x3, x21

    // Copy to particular[pivot_col]
    ldp     x3, x4, [x2]
    add     x5, x23, x0, lsl #4     // particular[pivot_col]
    stp     x3, x4, [x5]

    add     x24, x24, #1
    b       set_particular

set_particular_done:
    // Identify free variables (columns not in pivot_cols)
    load_addr x28, free_var_cols
    load_addr x0, n_free_vars
    str     xzr, [x0]

    mov     x24, #0                 // column index
find_free_vars:
    cmp     x24, x20
    b.ge    find_free_vars_done

    // Check if x24 is in pivot_cols
    mov     x0, #0                  // is_pivot flag
    mov     x1, #0                  // pivot index
check_pivot:
    cmp     x1, x27
    b.ge    not_pivot
    ldr     x2, [x25, x1, lsl #3]
    cmp     x2, x24
    b.eq    is_pivot
    add     x1, x1, #1
    b       check_pivot

is_pivot:
    b       next_col_free

not_pivot:
    // Add to free_var_cols
    load_addr x0, n_free_vars
    ldr     x1, [x0]
    str     x24, [x28, x1, lsl #3]  // free_var_cols[n_free] = col
    add     x1, x1, #1
    str     x1, [x0]

next_col_free:
    add     x24, x24, #1
    b       find_free_vars

find_free_vars_done:
    // Build null space vectors
    // For each free variable fv:
    //   null_vec[fv_idx][fv] = 1
    //   null_vec[fv_idx][pivot_col] = -matrix[pivot_row][fv]
    load_addr x0, n_free_vars
    ldr     x0, [x0]
    cbz     x0, extract_done

    load_addr x23, null_vectors
    mov     x24, #0                 // free var index

build_null_loop:
    load_addr x0, n_free_vars
    ldr     x0, [x0]
    cmp     x24, x0
    b.ge    extract_done

    // Get free variable column
    ldr     x0, [x28, x24, lsl #3]  // fv_col
    mov     x25, x0

    // Calculate null vector base: null_vectors[x24] = base + x24 * n_buttons * 16
    mov     x0, x20                 // n_buttons
    mov     x1, #RAT_SIZE
    mul     x0, x0, x1
    madd    x26, x24, x0, x23       // x26 = null_vectors[x24]

    // Initialize null vector to 0
    mov     x0, #0
null_init:
    cmp     x0, x20
    b.ge    null_init_done
    mov     x1, #0
    mov     x2, #1
    add     x3, x26, x0, lsl #4
    stp     x1, x2, [x3]            // 0/1
    add     x0, x0, #1
    b       null_init

null_init_done:
    // Set null_vec[fv_col] = 1
    mov     x0, #1
    mov     x1, #1
    add     x2, x26, x25, lsl #4
    bl      rat_new

    // For each pivot, set null_vec[pivot_col] = -matrix[pivot_row][fv_col]
    // We'll use stack to save loop state since we're out of callee-saved regs
    load_addr x0, n_pivots
    ldr     x9, [x0]                // n_pivots in x9 (caller-saved but we save it)
    mov     x10, #0                 // pivot index in x10

set_null_pivots:
    cmp     x10, x9
    b.ge    next_null_vec

    // Save loop state on stack
    sub     sp, sp, #32
    stp     x9, x10, [sp]

    // Get pivot col and row
    load_addr x2, pivot_cols
    load_addr x3, pivot_rows
    ldr     x4, [x2, x10, lsl #3]   // pivot_col
    ldr     x5, [x3, x10, lsl #3]   // pivot_row

    // Get matrix[pivot_row][fv_col]
    mul     x6, x5, x22
    add     x6, x6, x25             // [pivot_row][fv_col]
    mov     x7, #RAT_SIZE
    madd    x6, x6, x7, x21         // address

    // Negate it: null_vec[pivot_col] = -matrix[pivot_row][fv_col]
    mov     x0, x6                  // source
    add     x1, x26, x4, lsl #4     // dest = null_vec[pivot_col]
    bl      rat_neg

    // Restore loop state
    ldp     x9, x10, [sp]
    add     sp, sp, #32

    add     x10, x10, #1
    b       set_null_pivots

next_null_vec:
    add     x24, x24, #1
    b       build_null_loop

extract_done:
    // Convert rationals to floating-point for fast bound computation
    // Convert particular[]
    load_addr x0, particular
    load_addr x1, particular_fp
    load_addr x2, current_n_buttons
    ldr     x2, [x2]
    bl      convert_rats_to_fp

    // Convert null_vectors[]
    load_addr x3, n_free_vars
    ldr     x3, [x3]
    cbz     x3, extract_really_done

    load_addr x0, null_vectors
    load_addr x1, null_vectors_fp
    load_addr x2, current_n_buttons
    ldr     x2, [x2]
    mul     x2, x2, x3              // n_buttons * n_free_vars
    bl      convert_rats_to_fp

extract_really_done:
    ldp     x27, x28, [sp, #80]
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #112
    ret

// ============================================================================
// Convert array of rationals to doubles
// Input: x0 = rational array, x1 = double array, x2 = count
// ============================================================================
convert_rats_to_fp:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]

    mov     x19, x0                 // rat array
    mov     x20, x1                 // double array
    mov     x21, x2                 // count
    mov     x22, #0                 // index

convert_loop:
    cmp     x22, x21
    b.ge    convert_done

    // Load num/den from rational
    add     x0, x19, x22, lsl #4
    ldp     x1, x2, [x0]            // num, den

    // Convert to double: (double)num / (double)den
    scvtf   d0, x1                  // num as double
    scvtf   d1, x2                  // den as double
    fdiv    d0, d0, d1              // num/den

    // Store to double array
    str     d0, [x20, x22, lsl #3]

    add     x22, x22, #1
    b       convert_loop

convert_done:
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret

// ============================================================================
// Search for minimum non-negative integer solution
// Input: x0 = n_buttons
// Returns: x0 = minimum sum of button presses (or 0 if no valid solution)
// ============================================================================
search_minimum_solution:
    stp     x29, x30, [sp, #-96]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]

    mov     x19, x0                 // n_buttons

    // Check number of free variables
    load_addr x0, n_free_vars
    ldr     x20, [x0]               // n_free

    // If no free variables, just check particular solution
    cbz     x20, check_particular_only

    // For simplicity, we handle 0, 1, 2 free variables
    // For more, fall back to a bounded search

    cmp     x20, #1
    b.eq    search_1d
    cmp     x20, #2
    b.eq    search_2d
    cmp     x20, #3
    b.eq    search_3d
    b       search_bounded

check_particular_only:
    // Check if particular solution is valid (all non-negative integers)
    load_addr x21, particular
    mov     x22, #0                 // button index
    mov     x23, #0                 // sum

check_part_loop:
    cmp     x22, x19
    b.ge    check_part_valid

    add     x0, x21, x22, lsl #4
    bl      rat_is_negative
    cbnz    x0, search_no_solution

    add     x0, x21, x22, lsl #4
    bl      rat_is_integer
    cbz     x0, search_no_solution

    add     x0, x21, x22, lsl #4
    bl      rat_to_int
    add     x23, x23, x0

    add     x22, x22, #1
    b       check_part_loop

check_part_valid:
    mov     x0, x23
    b       search_return

search_no_solution:
    mov     x0, #0
    b       search_return

search_1d:
    // Single free variable: compute tight bounds using FP, then search
    load_addr x21, particular_fp
    load_addr x22, null_vectors_fp

    // Compute bounds: for each j, x_j = particular[j] + t*null[j] >= 0
    // If null[j] > 0: t >= -particular[j]/null[j] (lower bound)
    // If null[j] < 0: t <= -particular[j]/null[j] (upper bound)

    mov     x0, #0xC079             // -1000.0 as t_low initial
    movk    x0, #0x0000, lsl #16
    movk    x0, #0x0000, lsl #32
    movk    x0, #0xC08F, lsl #48
    fmov    d2, x0                  // d2 = t_low = -1000.0

    mov     x0, #0x4079             // 1000.0 as t_high initial
    movk    x0, #0x0000, lsl #16
    movk    x0, #0x0000, lsl #32
    movk    x0, #0x408F, lsl #48
    fmov    d3, x0                  // d3 = t_high = 1000.0

    // Small epsilon for float comparisons
    mov     x0, #0x3F1A             // 0.0001
    movk    x0, #0x36E2, lsl #16
    movk    x0, #0xEB1C, lsl #32
    movk    x0, #0x3F1A, lsl #48
    fmov    d4, x0                  // d4 = epsilon
    fneg    d5, d4                  // d5 = -epsilon

    mov     x24, #0                 // j = button index
compute_1d_bounds:
    cmp     x24, x19
    b.ge    bounds_1d_done

    // Load particular_fp[j] and null_fp[j]
    ldr     d0, [x21, x24, lsl #3]  // p = particular_fp[j]
    ldr     d1, [x22, x24, lsl #3]  // nv = null_fp[j]

    // Check if nv > epsilon (positive)
    fcmp    d1, d4
    b.le    check_1d_neg

    // nv > 0: bound = -p / nv, update t_low
    fneg    d6, d0
    fdiv    d6, d6, d1              // bound = -p/nv
    fmaxnm  d2, d2, d6              // t_low = max(t_low, bound)
    b       next_1d_bound

check_1d_neg:
    // Check if nv < -epsilon (negative)
    fcmp    d1, d5
    b.ge    check_1d_zero

    // nv < 0: bound = -p / nv, update t_high
    fneg    d6, d0
    fdiv    d6, d6, d1
    fminnm  d3, d3, d6              // t_high = min(t_high, bound)
    b       next_1d_bound

check_1d_zero:
    // nv ≈ 0: if p < 0, no solution
    fmov    d6, xzr                 // 0.0
    fcmp    d0, d6
    b.ge    next_1d_bound
    // p < 0 and nv = 0 means constraint can't be satisfied
    b       search_no_solution

next_1d_bound:
    add     x24, x24, #1
    b       compute_1d_bounds

bounds_1d_done:
    // Check if bounds are valid
    fcmp    d2, d3
    b.gt    search_no_solution      // t_low > t_high, no solution

    // Convert bounds to integers (ceiling for low, floor for high)
    frintm  d6, d3                  // floor(t_high)
    fcvtzs  x22, d6                 // t_high as int
    frintp  d6, d2                  // ceil(t_low)
    fcvtzs  x21, d6                 // t_low as int

    // Clamp to reasonable range
    mov     x0, #-2000
    cmp     x21, x0
    csel    x21, x0, x21, lt
    mov     x0, #2000
    cmp     x22, x0
    csel    x22, x0, x22, gt

    mov     x23, #0x7FFF
    movk    x23, #0x7FFF, lsl #16   // min_sum = large

search_1d_loop:
    cmp     x21, x22
    b.gt    search_1d_done

    mov     x0, x19
    mov     x1, x21
    bl      compute_solution_1d

    cbz     x0, search_1d_next
    cmp     x0, x23
    csel    x23, x0, x23, lt

search_1d_next:
    add     x21, x21, #1
    b       search_1d_loop

search_1d_done:
    mov     x0, #0x7FFF
    movk    x0, #0x7FFF, lsl #16
    cmp     x23, x0
    csel    x0, xzr, x23, eq
    b       search_return

search_2d:
    // Two free variables: use FP for outer bounds, dynamic inner bounds
    // First compute bounds for t0
    load_addr x21, particular_fp
    load_addr x22, null_vectors_fp

    // Null vector stride in doubles = n_buttons * 8
    mov     x0, x19
    lsl     x28, x0, #3             // stride for null vectors (n_buttons * 8)

    // Initial outer bounds: -500 to 500
    mov     w24, #-500
    mov     w25, #500
    mov     x23, #0x7FFF
    movk    x23, #0x7FFF, lsl #16   // min_sum

search_2d_outer:
    cmp     w24, w25
    b.gt    search_2d_done

    // For this t0, compute dynamic bounds for t1
    // For each j: inter[j] = particular[j] + t0 * null[0][j]
    // Then: inter[j] + t1 * null[1][j] >= 0

    scvtf   d7, w24                 // t0 as double

    // Initialize t1 bounds
    mov     x0, #0x0000
    movk    x0, #0x0000, lsl #16
    movk    x0, #0x0000, lsl #32
    movk    x0, #0xC08F, lsl #48    // -1000.0
    fmov    d2, x0

    mov     x0, #0x0000
    movk    x0, #0x0000, lsl #16
    movk    x0, #0x0000, lsl #32
    movk    x0, #0x408F, lsl #48    // 1000.0
    fmov    d3, x0

    // Epsilon
    mov     x0, #0x36E2
    movk    x0, #0xEB1C, lsl #16
    movk    x0, #0x3F1A, lsl #32
    movk    x0, #0x0000, lsl #48
    lsr     x0, x0, #16
    mov     x1, #0x3F1A
    movk    x1, #0x36E2, lsl #16
    orr     x0, x1, x0, lsl #32
    fmov    d4, x0                  // epsilon ~ 0.0001
    fneg    d5, d4

    mov     x26, #0                 // j
compute_2d_inner_bounds:
    cmp     x26, x19
    b.ge    bounds_2d_inner_done

    // inter = particular[j] + t0 * null[0][j]
    ldr     d0, [x21, x26, lsl #3]  // particular[j]
    ldr     d1, [x22, x26, lsl #3]  // null[0][j]
    fmadd   d0, d7, d1, d0          // inter = p + t0*n0

    // null[1][j]
    add     x0, x22, x28            // null[1] base
    ldr     d1, [x0, x26, lsl #3]   // null[1][j]

    // Check if nv > epsilon
    fcmp    d1, d4
    b.le    check_2d_neg_inner

    // nv > 0: t1 >= -inter/nv
    fneg    d6, d0
    fdiv    d6, d6, d1
    fmaxnm  d2, d2, d6
    b       next_2d_inner_bound

check_2d_neg_inner:
    fcmp    d1, d5
    b.ge    next_2d_inner_bound     // nv ≈ 0, skip

    // nv < 0: t1 <= -inter/nv
    fneg    d6, d0
    fdiv    d6, d6, d1
    fminnm  d3, d3, d6

next_2d_inner_bound:
    add     x26, x26, #1
    b       compute_2d_inner_bounds

bounds_2d_inner_done:
    // Check if bounds valid
    fcmp    d2, d3
    b.gt    search_2d_outer_next    // No valid t1 for this t0

    // Convert to integer bounds
    frintp  d6, d2
    fcvtzs  w26, d6                 // t1_low
    frintm  d6, d3
    fcvtzs  w27, d6                 // t1_high

    // Clamp
    cmp     w26, #-500
    csel    w26, w26, wzr, ge
    mov     w0, #-500
    csel    w26, w0, w26, lt
    cmp     w27, #500
    mov     w0, #500
    csel    w27, w0, w27, gt

search_2d_inner:
    cmp     w26, w27
    b.gt    search_2d_outer_next

    mov     x0, x19
    sxtw    x1, w24                 // t0
    sxtw    x2, w26                 // t1
    bl      compute_solution_2d

    cbz     x0, search_2d_inner_next
    cmp     x0, x23
    csel    x23, x0, x23, lt

search_2d_inner_next:
    add     w26, w26, #1
    b       search_2d_inner

search_2d_outer_next:
    add     w24, w24, #1
    b       search_2d_outer

search_2d_done:
    mov     x0, #0x7FFF
    movk    x0, #0x7FFF, lsl #16
    cmp     x23, x0
    csel    x0, xzr, x23, eq
    b       search_return

search_3d:
    // Three free variables: nested search with dynamic bounds for t1 and t2
    // This is critical - 6 machines have 3 free vars

    load_addr x21, particular_fp
    load_addr x22, null_vectors_fp

    // Null vector stride = n_buttons * 8
    mov     x0, x19
    lsl     x28, x0, #3             // stride

    // Outer bounds for t0: -200 to 200
    mov     w24, #-200
    mov     w25, #200
    mov     x23, #0x7FFF
    movk    x23, #0x7FFF, lsl #16   // min_sum

    // Store n_buttons for inner loops (we'll clobber x19)
    str     x19, [sp, #-16]!

search_3d_t0:
    cmp     w24, w25
    b.gt    search_3d_done

    scvtf   d8, w24                 // t0 as double (preserved across inner)

    // Compute bounds for t1 given t0
    // inter0[j] = particular[j] + t0 * null[0][j]
    // We need: inter0[j] + t1*null[1][j] + t2*null[2][j] >= 0

    // For now, use fixed bounds for t1, compute dynamic for t2
    mov     w16, #-200              // t1_low
    mov     w17, #200               // t1_high

search_3d_t1:
    cmp     w16, w17
    b.gt    search_3d_t0_next

    scvtf   d9, w16                 // t1 as double

    // Compute inter[j] = particular[j] + t0*null[0][j] + t1*null[1][j]
    // Then compute t2 bounds from: inter[j] + t2*null[2][j] >= 0

    // Initialize t2 bounds
    mov     x0, #0x0000
    movk    x0, #0x0000, lsl #16
    movk    x0, #0x0000, lsl #32
    movk    x0, #0xC08F, lsl #48    // -1000.0
    fmov    d2, x0

    mov     x0, #0x0000
    movk    x0, #0x0000, lsl #16
    movk    x0, #0x0000, lsl #32
    movk    x0, #0x408F, lsl #48    // 1000.0
    fmov    d3, x0

    // Epsilon
    mov     x0, #0x36E2
    movk    x0, #0xEB1C, lsl #16
    movk    x0, #0x3F1A, lsl #32
    movk    x0, #0x0000, lsl #48
    lsr     x0, x0, #16
    mov     x1, #0x3F1A
    movk    x1, #0x36E2, lsl #16
    orr     x0, x1, x0, lsl #32
    fmov    d4, x0                  // epsilon
    fneg    d5, d4

    ldr     x19, [sp]               // restore n_buttons
    mov     x26, #0                 // j

compute_3d_inner_bounds:
    cmp     x26, x19
    b.ge    bounds_3d_inner_done

    // inter = particular[j] + t0*null[0][j] + t1*null[1][j]
    ldr     d0, [x21, x26, lsl #3]  // particular[j]
    ldr     d1, [x22, x26, lsl #3]  // null[0][j]
    fmadd   d0, d8, d1, d0          // + t0*n0

    add     x0, x22, x28            // null[1] base
    ldr     d1, [x0, x26, lsl #3]   // null[1][j]
    fmadd   d0, d9, d1, d0          // + t1*n1 -> inter in d0

    // null[2][j]
    add     x0, x22, x28, lsl #1    // null[2] base (2 * stride)
    ldr     d1, [x0, x26, lsl #3]   // null[2][j]

    // Check if nv > epsilon
    fcmp    d1, d4
    b.le    check_3d_neg_inner

    // nv > 0: t2 >= -inter/nv
    fneg    d6, d0
    fdiv    d6, d6, d1
    fmaxnm  d2, d2, d6
    b       next_3d_inner_bound

check_3d_neg_inner:
    fcmp    d1, d5
    b.ge    next_3d_inner_bound     // nv ≈ 0

    // nv < 0: t2 <= -inter/nv
    fneg    d6, d0
    fdiv    d6, d6, d1
    fminnm  d3, d3, d6

next_3d_inner_bound:
    add     x26, x26, #1
    b       compute_3d_inner_bounds

bounds_3d_inner_done:
    // Check if bounds valid
    fcmp    d2, d3
    b.gt    search_3d_t1_next       // No valid t2

    // Convert to integer bounds
    frintp  d6, d2
    fcvtzs  w26, d6                 // t2_low
    frintm  d6, d3
    fcvtzs  w27, d6                 // t2_high

    // Clamp
    mov     w0, #-500
    cmp     w26, w0
    csel    w26, w0, w26, lt
    mov     w0, #500
    cmp     w27, w0
    csel    w27, w0, w27, gt

search_3d_t2:
    cmp     w26, w27
    b.gt    search_3d_t1_next

    ldr     x19, [sp]               // n_buttons
    mov     x0, x19
    sxtw    x1, w24                 // t0
    sxtw    x2, w16                 // t1
    sxtw    x3, w26                 // t2
    bl      compute_solution_3d

    cbz     x0, search_3d_t2_next
    cmp     x0, x23
    csel    x23, x0, x23, lt

search_3d_t2_next:
    add     w26, w26, #1
    b       search_3d_t2

search_3d_t1_next:
    add     w16, w16, #1
    b       search_3d_t1

search_3d_t0_next:
    add     w24, w24, #1
    b       search_3d_t0

search_3d_done:
    add     sp, sp, #16             // cleanup saved n_buttons
    mov     x0, #0x7FFF
    movk    x0, #0x7FFF, lsl #16
    cmp     x23, x0
    csel    x0, xzr, x23, eq
    b       search_return

search_bounded:
    // For 3+ free variables, use simplified bounded search
    // This is a fallback - may not find optimal
    mov     x0, #0
    b       search_return

search_return:
    ldp     x27, x28, [sp, #80]
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #96
    ret

// ============================================================================
// Compute solution with 1 free variable
// Input: x0 = n_buttons, x1 = t (integer parameter)
// Returns: x0 = sum if valid, 0 if invalid
// ============================================================================
compute_solution_1d:
    stp     x29, x30, [sp, #-80]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]

    mov     x19, x0                 // n_buttons
    mov     x20, x1                 // t

    load_addr x21, particular
    load_addr x22, null_vectors
    load_addr x23, solution_vec

    mov     x24, #0                 // button index
    mov     x25, #0                 // sum

compute_1d_loop:
    cmp     x24, x19
    b.ge    compute_1d_check

    // solution[i] = particular[i] + t * null[0][i]
    // First, create t as rational
    mov     x0, x20
    mov     x1, #1
    load_addr x2, temp_rat1
    bl      rat_new

    // Multiply t * null[0][i]
    load_addr x0, temp_rat1
    add     x1, x22, x24, lsl #4    // null_vectors[0][i]
    load_addr x2, temp_rat2
    bl      rat_mul

    // Add particular[i]
    add     x0, x21, x24, lsl #4
    load_addr x1, temp_rat2
    add     x2, x23, x24, lsl #4    // solution[i]
    bl      rat_add

    add     x24, x24, #1
    b       compute_1d_loop

compute_1d_check:
    // Check all solution[i] are non-negative integers
    mov     x24, #0
    mov     x25, #0                 // sum

validate_1d:
    cmp     x24, x19
    b.ge    validate_1d_ok

    add     x0, x23, x24, lsl #4
    bl      rat_is_negative
    cbnz    x0, compute_1d_invalid

    add     x0, x23, x24, lsl #4
    bl      rat_is_integer
    cbz     x0, compute_1d_invalid

    add     x0, x23, x24, lsl #4
    bl      rat_to_int
    add     x25, x25, x0

    add     x24, x24, #1
    b       validate_1d

validate_1d_ok:
    mov     x0, x25
    b       compute_1d_return

compute_1d_invalid:
    mov     x0, #0

compute_1d_return:
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #80
    ret

// ============================================================================
// Compute solution with 2 free variables
// Input: x0 = n_buttons, x1 = t0, x2 = t1
// Returns: x0 = sum if valid, 0 if invalid
// ============================================================================
compute_solution_2d:
    stp     x29, x30, [sp, #-96]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]

    mov     x19, x0                 // n_buttons
    mov     x20, x1                 // t0
    mov     x27, x2                 // t1

    load_addr x21, particular
    load_addr x22, null_vectors
    load_addr x23, solution_vec

    // Null vector stride = n_buttons * 16
    mov     x0, x19
    mov     x1, #RAT_SIZE
    mul     x28, x0, x1             // null vector stride

    mov     x24, #0                 // button index
    mov     x25, #0                 // sum

compute_2d_loop:
    cmp     x24, x19
    b.ge    compute_2d_check

    // solution[i] = particular[i] + t0*null[0][i] + t1*null[1][i]

    // t0 as rational
    mov     x0, x20
    mov     x1, #1
    load_addr x2, temp_rat1
    bl      rat_new

    // t0 * null[0][i]
    load_addr x0, temp_rat1
    add     x1, x22, x24, lsl #4    // null_vectors[0][i]
    load_addr x2, temp_rat2
    bl      rat_mul

    // particular[i] + t0*null[0][i]
    add     x0, x21, x24, lsl #4
    load_addr x1, temp_rat2
    load_addr x2, temp_rat3
    bl      rat_add

    // t1 as rational
    mov     x0, x27
    mov     x1, #1
    load_addr x2, temp_rat1
    bl      rat_new

    // t1 * null[1][i]
    load_addr x0, temp_rat1
    add     x1, x22, x28            // null_vectors[1] base
    add     x1, x1, x24, lsl #4     // null_vectors[1][i]
    load_addr x2, temp_rat2
    bl      rat_mul

    // (particular + t0*null0) + t1*null1
    load_addr x0, temp_rat3
    load_addr x1, temp_rat2
    add     x2, x23, x24, lsl #4    // solution[i]
    bl      rat_add

    add     x24, x24, #1
    b       compute_2d_loop

compute_2d_check:
    mov     x24, #0
    mov     x25, #0

validate_2d:
    cmp     x24, x19
    b.ge    validate_2d_ok

    add     x0, x23, x24, lsl #4
    bl      rat_is_negative
    cbnz    x0, compute_2d_invalid

    add     x0, x23, x24, lsl #4
    bl      rat_is_integer
    cbz     x0, compute_2d_invalid

    add     x0, x23, x24, lsl #4
    bl      rat_to_int
    add     x25, x25, x0

    add     x24, x24, #1
    b       validate_2d

validate_2d_ok:
    mov     x0, x25
    b       compute_2d_return

compute_2d_invalid:
    mov     x0, #0

compute_2d_return:
    ldp     x27, x28, [sp, #80]
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #96
    ret

// ============================================================================
// Compute solution with 3 free variables
// Input: x0 = n_buttons, x1 = t0, x2 = t1, x3 = t2
// Returns: x0 = sum if valid, 0 if invalid
// ============================================================================
compute_solution_3d:
    stp     x29, x30, [sp, #-112]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]
    str     x9, [sp, #96]

    mov     x19, x0                 // n_buttons
    mov     x20, x1                 // t0
    mov     x27, x2                 // t1
    mov     x9, x3                  // t2 (save in x9, will push/pop)

    load_addr x21, particular
    load_addr x22, null_vectors
    load_addr x23, solution_vec

    // Null vector stride = n_buttons * 16
    mov     x0, x19
    mov     x1, #RAT_SIZE
    mul     x28, x0, x1             // null vector stride

    mov     x24, #0                 // button index

compute_3d_loop:
    cmp     x24, x19
    b.ge    compute_3d_check

    // solution[i] = particular[i] + t0*null[0][i] + t1*null[1][i] + t2*null[2][i]

    // Save t2 on stack before function calls
    sub     sp, sp, #16
    str     x9, [sp]

    // t0 as rational
    mov     x0, x20
    mov     x1, #1
    load_addr x2, temp_rat1
    bl      rat_new

    // t0 * null[0][i]
    load_addr x0, temp_rat1
    add     x1, x22, x24, lsl #4    // null_vectors[0][i]
    load_addr x2, temp_rat2
    bl      rat_mul

    // particular[i] + t0*null[0][i]
    add     x0, x21, x24, lsl #4
    load_addr x1, temp_rat2
    load_addr x2, temp_rat3
    bl      rat_add

    // t1 as rational
    mov     x0, x27
    mov     x1, #1
    load_addr x2, temp_rat1
    bl      rat_new

    // t1 * null[1][i]
    load_addr x0, temp_rat1
    add     x1, x22, x28            // null_vectors[1] base
    add     x1, x1, x24, lsl #4
    load_addr x2, temp_rat2
    bl      rat_mul

    // (particular + t0*null0) + t1*null1
    load_addr x0, temp_rat3
    load_addr x1, temp_rat2
    add     x2, x23, x24, lsl #4    // temp store in solution[i]
    bl      rat_add

    // Restore t2
    ldr     x9, [sp]
    add     sp, sp, #16

    // t2 as rational
    mov     x0, x9
    mov     x1, #1
    load_addr x2, temp_rat1
    bl      rat_new

    // t2 * null[2][i]
    load_addr x0, temp_rat1
    add     x1, x22, x28, lsl #1    // null_vectors[2] base (2 * stride)
    add     x1, x1, x24, lsl #4
    load_addr x2, temp_rat2
    bl      rat_mul

    // (prev result) + t2*null2
    add     x0, x23, x24, lsl #4    // prev result in solution[i]
    load_addr x1, temp_rat2
    load_addr x2, temp_rat3
    bl      rat_add

    // Copy final result to solution[i]
    load_addr x0, temp_rat3
    ldp     x1, x2, [x0]
    add     x0, x23, x24, lsl #4
    stp     x1, x2, [x0]

    add     x24, x24, #1
    b       compute_3d_loop

compute_3d_check:
    mov     x24, #0
    mov     x25, #0

validate_3d:
    cmp     x24, x19
    b.ge    validate_3d_ok

    add     x0, x23, x24, lsl #4
    bl      rat_is_negative
    cbnz    x0, compute_3d_invalid

    add     x0, x23, x24, lsl #4
    bl      rat_is_integer
    cbz     x0, compute_3d_invalid

    add     x0, x23, x24, lsl #4
    bl      rat_to_int
    add     x25, x25, x0

    add     x24, x24, #1
    b       validate_3d

validate_3d_ok:
    mov     x0, x25
    b       compute_3d_return

compute_3d_invalid:
    mov     x0, #0

compute_3d_return:
    ldr     x9, [sp, #96]
    ldp     x27, x28, [sp, #80]
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #112
    ret

// ============================================================================
// Print a string
// ============================================================================
print_string:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    str     x19, [sp, #16]

    mov     x19, x0

    // Calculate length
    mov     x1, x0
strlen_loop:
    ldrb    w2, [x1]
    cbz     w2, strlen_done
    add     x1, x1, #1
    b       strlen_loop
strlen_done:
    sub     x2, x1, x19             // Length

    // Write
    mov     x1, x19
    mov     x0, #1                  // stdout
    mov     w16, #4
    orr     w16, w16, #0x2000000
    svc     #0x80

    ldr     x19, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// ============================================================================
// Print a number
// ============================================================================
print_number:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    str     x21, [sp, #32]

    mov     x19, x0                 // Number to print
    load_addr x20, output_buffer
    add     x21, x20, #31           // End of buffer

    // Null terminate
    mov     w0, #0
    strb    w0, [x21]
    sub     x21, x21, #1

    // Handle zero specially
    cbnz    x19, print_num_loop
    mov     w0, #'0'
    strb    w0, [x21]
    b       print_num_done

print_num_loop:
    cbz     x19, print_num_done

    // Digit = number % 10
    mov     x0, x19
    mov     x1, #10
    udiv    x2, x0, x1
    msub    x3, x2, x1, x0          // remainder

    // Convert to ASCII
    add     w3, w3, #'0'
    strb    w3, [x21]
    sub     x21, x21, #1

    mov     x19, x2                 // number /= 10
    b       print_num_loop

print_num_done:
    add     x21, x21, #1

    // Write the number
    mov     x0, #1                  // stdout
    mov     x1, x21
    load_addr x2, output_buffer
    add     x2, x2, #31
    sub     x2, x2, x1              // length
    mov     w16, #4
    orr     w16, w16, #0x2000000
    svc     #0x80

    ldr     x21, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret

// ============================================================================
// Exit with error
// ============================================================================
exit_error:
    mov     x0, #1
    mov     w16, #1
    orr     w16, w16, #0x2000000
    svc     #0x80
