// ARM64 Assembly Solution for Advent of Code 2025 Day 3
// macOS ARM64 (Apple Silicon)

.global _main
.align 4

// System call numbers for macOS ARM64
.equ SYS_EXIT, 1
.equ SYS_READ, 3
.equ SYS_WRITE, 4
.equ SYS_OPEN, 5
.equ SYS_CLOSE, 6

// File flags
.equ O_RDONLY, 0

// Constants
.equ MAX_LINE_LEN, 128
.equ MAX_LINES, 256
.equ BUFFER_SIZE, 32768

.data
input_file:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"

.bss
.align 4
file_buffer:    .skip BUFFER_SIZE
lines:          .skip MAX_LINES * 8      // Array of pointers to line starts
line_lengths:   .skip MAX_LINES * 4      // Array of line lengths
num_lines:      .skip 4                  // Number of lines
num_buffer:     .skip 32                 // Buffer for number to string conversion

.text

// Main entry point
_main:
    // Save frame pointer and link register
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    // Open input file
    adrp    x0, input_file@PAGE
    add     x0, x0, input_file@PAGEOFF
    mov     x1, #O_RDONLY
    mov     x16, #SYS_OPEN
    svc     #0x80

    // Check if file opened successfully
    cmp     x0, #0
    b.lt    exit_error

    // Save file descriptor
    mov     x19, x0

    // Read file into buffer
    mov     x0, x19              // fd
    adrp    x1, file_buffer@PAGE
    add     x1, x1, file_buffer@PAGEOFF
    mov     x2, #BUFFER_SIZE
    mov     x16, #SYS_READ
    svc     #0x80

    // Save bytes read
    mov     x20, x0

    // Close file
    mov     x0, x19
    mov     x16, #SYS_CLOSE
    svc     #0x80

    // Parse lines from buffer
    mov     x0, x20              // bytes_read
    bl      parse_lines

    // Part 1
    bl      solve_part1
    mov     x21, x0              // Save part1 result

    // Print "Part 1: "
    adrp    x0, part1_msg@PAGE
    add     x0, x0, part1_msg@PAGEOFF
    bl      print_string

    // Print result
    mov     x0, x21
    bl      print_number

    // Print newline
    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    // Part 2
    bl      solve_part2
    mov     x21, x0              // Save part2 result

    // Print "Part 2: "
    adrp    x0, part2_msg@PAGE
    add     x0, x0, part2_msg@PAGEOFF
    bl      print_string

    // Print result
    mov     x0, x21
    bl      print_number

    // Print newline
    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    // Exit success
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    mov     x0, #0
    mov     x16, #SYS_EXIT
    svc     #0x80

exit_error:
    mov     x0, #1
    mov     x16, #SYS_EXIT
    svc     #0x80

// Parse lines from file_buffer
// Input: x0 = bytes_read
parse_lines:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    mov     x19, x0                         // bytes_read
    adrp    x20, file_buffer@PAGE
    add     x20, x20, file_buffer@PAGEOFF  // Buffer start
    adrp    x21, lines@PAGE
    add     x21, x21, lines@PAGEOFF        // Lines array
    adrp    x22, line_lengths@PAGE
    add     x22, x22, line_lengths@PAGEOFF // Line lengths array

    mov     x23, #0                         // Line counter
    mov     x24, x20                        // Line start pointer
    mov     x25, #0                         // Current line length
    mov     x26, #0                         // Current offset in buffer

parse_loop:
    cmp     x26, x19
    b.ge    parse_done

    ldrb    w27, [x20, x26]

    // Check for newline
    cmp     w27, #10                        // '\n'
    b.eq    store_line

    // Not newline, increment line length
    add     x25, x25, #1
    add     x26, x26, #1
    b       parse_loop

store_line:
    // Store line if it has content
    cbz     x25, skip_line

    // Store line pointer
    str     x24, [x21, x23, lsl #3]

    // Store line length
    str     w25, [x22, x23, lsl #2]

    // Increment line counter
    add     x23, x23, #1

skip_line:
    // Move to next line
    add     x26, x26, #1                    // Skip newline
    add     x24, x20, x26                   // New line start
    mov     x25, #0                         // Reset line length
    b       parse_loop

parse_done:
    // Store final line if exists
    cbz     x25, no_final_line
    str     x24, [x21, x23, lsl #3]
    str     w25, [x22, x23, lsl #2]
    add     x23, x23, #1

no_final_line:
    // Save number of lines
    adrp    x0, num_lines@PAGE
    add     x0, x0, num_lines@PAGEOFF
    str     w23, [x0]

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Solve Part 1
// Returns: x0 = total
solve_part1:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    // Load num_lines
    adrp    x0, num_lines@PAGE
    add     x0, x0, num_lines@PAGEOFF
    ldr     w19, [x0]                // num_lines

    adrp    x20, lines@PAGE
    add     x20, x20, lines@PAGEOFF
    adrp    x21, line_lengths@PAGE
    add     x21, x21, line_lengths@PAGEOFF

    mov     x22, #0                  // Total sum
    mov     x23, #0                  // Line index

part1_line_loop:
    cmp     x23, x19
    b.ge    part1_done

    // Get line pointer and length
    ldr     x24, [x20, x23, lsl #3]  // Line pointer
    ldr     w25, [x21, x23, lsl #2]  // Line length

    // Find max 2-digit number for this line
    mov     w26, #0                  // max_joltage
    mov     x27, #0                  // Position i

part1_inner_loop:
    // Check if we can pick 2 batteries (need at least i+1 < length)
    add     x28, x27, #1
    cmp     x28, x25
    b.ge    part1_add_to_total

    // Get first digit
    ldrb    w0, [x24, x27]
    sub     w0, w0, #'0'

    // Find max digit from position i+1 to end
    mov     x1, x28                  // Start from i+1
    mov     w2, #0                   // max_second_digit

part1_find_max:
    cmp     x1, x25
    b.ge    part1_compute_joltage

    ldrb    w3, [x24, x1]
    sub     w3, w3, #'0'
    cmp     w3, w2
    csel    w2, w3, w2, gt

    add     x1, x1, #1
    b       part1_find_max

part1_compute_joltage:
    // Compute joltage = first_digit * 10 + max_second_digit
    mov     w3, #10
    mul     w4, w0, w3
    add     w4, w4, w2

    // Update max_joltage
    cmp     w4, w26
    csel    w26, w4, w26, gt

    // Next position
    add     x27, x27, #1
    b       part1_inner_loop

part1_add_to_total:
    // Add max_joltage to total
    add     x22, x22, x26
    add     x23, x23, #1
    b       part1_line_loop

part1_done:
    mov     x0, x22

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Solve Part 2
// Returns: x0 = total
solve_part2:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    // Load num_lines
    adrp    x0, num_lines@PAGE
    add     x0, x0, num_lines@PAGEOFF
    ldr     w19, [x0]                // num_lines

    adrp    x20, lines@PAGE
    add     x20, x20, lines@PAGEOFF
    adrp    x21, line_lengths@PAGE
    add     x21, x21, line_lengths@PAGEOFF

    mov     x22, #0                  // Total sum
    mov     x23, #0                  // Line index

part2_line_loop:
    cmp     x23, x19
    b.ge    part2_done

    // Get line pointer and length
    ldr     x24, [x20, x23, lsl #3]  // Line pointer
    ldr     w25, [x21, x23, lsl #2]  // Line length

    // Process this line using greedy algorithm
    mov     x0, x24                  // Line pointer
    mov     x1, x25                  // Line length
    bl      greedy_select_12

    // Add result to total
    add     x22, x22, x0

    add     x23, x23, #1
    b       part2_line_loop

part2_done:
    mov     x0, x22

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Greedy algorithm to select 12 digits forming maximum number
// Input: x0 = line pointer, x1 = line length
// Returns: x0 = resulting number
greedy_select_12:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0                 // Line pointer
    mov     x20, x1                 // Line length
    mov     x21, #0                 // current_pos
    mov     x22, #0                 // Result accumulator
    mov     x23, #0                 // Digit counter (0 to 11)

greedy_digit_loop:
    cmp     x23, #12
    b.ge    greedy_done

    // Calculate remaining_needed = 12 - digit_count - 1
    mov     x24, #12
    sub     x24, x24, x23
    sub     x24, x24, #1

    // Calculate search_end = line_length - remaining_needed
    sub     x25, x20, x24

    // Find max digit in range [current_pos, search_end)
    mov     w26, #-1                // max_digit
    mov     x27, x21                // max_pos
    mov     x28, x21                // j (search position)

greedy_search_loop:
    cmp     x28, x25
    b.ge    greedy_found_max

    ldrb    w0, [x19, x28]
    sub     w0, w0, #'0'

    cmp     w0, w26
    b.le    greedy_next_pos

    mov     w26, w0                 // New max_digit
    mov     x27, x28                // New max_pos

greedy_next_pos:
    add     x28, x28, #1
    b       greedy_search_loop

greedy_found_max:
    // Accumulate result: result = result * 10 + max_digit
    mov     x0, #10
    mul     x22, x22, x0
    add     x22, x22, x26

    // Update current_pos = max_pos + 1
    add     x21, x27, #1

    // Next digit
    add     x23, x23, #1
    b       greedy_digit_loop

greedy_done:
    mov     x0, x22

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Print a null-terminated string
// Input: x0 = pointer to string
print_string:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0

    // Find string length
    mov     x20, #0
strlen_loop:
    ldrb    w1, [x19, x20]
    cbz     w1, strlen_done
    add     x20, x20, #1
    b       strlen_loop

strlen_done:
    // Write to stdout
    mov     x0, #1              // stdout
    mov     x1, x19             // string
    mov     x2, x20             // length
    mov     x16, #SYS_WRITE
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Print a number
// Input: x0 = number to print
print_number:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0             // Number to convert

    adrp    x20, num_buffer@PAGE
    add     x20, x20, num_buffer@PAGEOFF
    add     x21, x20, #31       // Point to end of buffer
    mov     w22, #0
    strb    w22, [x21]          // Null terminator

    // Handle zero specially
    cbnz    x19, num_convert
    sub     x21, x21, #1
    mov     w22, #'0'
    strb    w22, [x21]
    b       num_print

num_convert:
    cbz     x19, num_print

    // Divide by 10
    mov     x22, #10
    udiv    x23, x19, x22
    msub    x24, x23, x22, x19  // remainder

    // Convert digit to ASCII
    add     w24, w24, #'0'
    sub     x21, x21, #1
    strb    w24, [x21]

    mov     x19, x23
    b       num_convert

num_print:
    mov     x0, x21
    bl      print_string

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
