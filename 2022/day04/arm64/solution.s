// ARM64 Assembly solution for AoC 2022 Day 4 - Camp Cleanup
// macOS syscalls
// Range overlap detection

.global _start
.align 2

.equ STDOUT, 1

.data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

.align 3
file_buffer: .skip 16384
output_buffer: .skip 32

.text
_start:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    // Open file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0005
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #0              // O_RDONLY
    mov x2, #0
    svc #0x80
    cmp x0, #0
    b.lt exit_error
    mov x19, x0             // Save fd

    // Read file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0003
    mov x0, x19
    adrp x1, file_buffer@PAGE
    add x1, x1, file_buffer@PAGEOFF
    mov x2, #16384
    svc #0x80
    mov x20, x0             // Save bytes read

    // Close file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0006
    mov x0, x19
    svc #0x80

    // Solve Part 1
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20
    bl part1
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

    // Solve Part 2
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20
    bl part2
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

    mov x0, #0
    ldp x29, x30, [sp], #16
    movz x16, #0x2000, lsl #16
    movk x16, #0x0001
    svc #0x80

exit_error:
    mov x0, #1
    ldp x29, x30, [sp], #16
    movz x16, #0x2000, lsl #16
    movk x16, #0x0001
    svc #0x80

// Parse a number from string
// x0 = pointer, returns x0 = new pointer, x1 = number
parse_number:
    mov x1, #0              // Result
    mov x2, #10             // Base
parse_num_loop:
    ldrb w3, [x0]
    cmp w3, #'0'
    b.lt parse_num_done
    cmp w3, #'9'
    b.gt parse_num_done
    sub w3, w3, #'0'
    mul x1, x1, x2
    add x1, x1, x3
    add x0, x0, #1
    b parse_num_loop
parse_num_done:
    ret

// Parse a line: "a1-b1,a2-b2\n"
// x0 = pointer to line start
// Returns: x0 = pointer after line, x1 = a1, x2 = b1, x3 = a2, x4 = b2
// If line is empty/invalid, returns x1 = 0, x2 = 0, x3 = 0, x4 = 0
parse_line:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0             // Save pointer

    // Check for newline or end of input at start
    ldrb w20, [x19]
    cmp w20, #'\n'
    b.eq parse_line_empty
    cmp w20, #0
    b.eq parse_line_empty

    // Parse a1
    mov x0, x19
    bl parse_number
    mov x19, x0
    mov x21, x1             // a1

    // Skip '-'
    add x19, x19, #1

    // Parse b1
    mov x0, x19
    bl parse_number
    mov x19, x0
    mov x22, x1             // b1

    // Skip ','
    add x19, x19, #1

    // Parse a2
    mov x0, x19
    bl parse_number
    mov x19, x0
    mov x23, x1             // a2

    // Skip '-'
    add x19, x19, #1

    // Parse b2
    mov x0, x19
    bl parse_number
    mov x19, x0
    mov x24, x1             // b2

    // Skip to end of line
skip_to_eol:
    ldrb w20, [x19]
    cmp w20, #'\n'
    b.eq found_eol
    cmp w20, #0
    b.eq at_end
    add x19, x19, #1
    b skip_to_eol

found_eol:
    add x19, x19, #1        // Skip newline

at_end:
    mov x0, x19
    mov x1, x21
    mov x2, x22
    mov x3, x23
    mov x4, x24

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

parse_line_empty:
    // Skip newline if present
    cmp w20, #'\n'
    b.ne no_skip_nl
    add x19, x19, #1
no_skip_nl:
    mov x0, x19
    mov x1, #0
    mov x2, #0
    mov x3, #0
    mov x4, #0

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Check if one range fully contains the other
// x1 = a1, x2 = b1, x3 = a2, x4 = b2
// Returns: x0 = 1 if fully contains, 0 otherwise
// (a1 <= a2 && b1 >= b2) || (a2 <= a1 && b2 >= b1)
fully_contains:
    // Check first condition: a1 <= a2 && b1 >= b2
    cmp x1, x3              // a1 <= a2?
    b.gt check_second_cond
    cmp x2, x4              // b1 >= b2?
    b.lt check_second_cond
    mov x0, #1
    ret

check_second_cond:
    // Check second condition: a2 <= a1 && b2 >= b1
    cmp x3, x1              // a2 <= a1?
    b.gt no_contain
    cmp x4, x2              // b2 >= b1?
    b.lt no_contain
    mov x0, #1
    ret

no_contain:
    mov x0, #0
    ret

// Check if ranges overlap at all
// x1 = a1, x2 = b1, x3 = a2, x4 = b2
// Returns: x0 = 1 if overlap, 0 otherwise
// Ranges overlap if: a1 <= b2 && a2 <= b1
overlaps:
    cmp x1, x4              // a1 <= b2?
    b.gt no_overlap
    cmp x3, x2              // a2 <= b1?
    b.gt no_overlap
    mov x0, #1
    ret

no_overlap:
    mov x0, #0
    ret

// Part 1: Count pairs where one range fully contains the other
// x0 = buffer, x1 = length
// Returns: x0 = count
part1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, x0             // Buffer pointer
    add x20, x19, x1        // Buffer end
    mov x21, #0             // Count

part1_loop:
    cmp x19, x20
    b.ge part1_done

    mov x0, x19
    bl parse_line
    mov x19, x0             // Update pointer

    // Check if valid line (a1 != 0 or b1 != 0 or a2 != 0 or b2 != 0)
    orr x5, x1, x2
    orr x5, x5, x3
    orr x5, x5, x4
    cbz x5, part1_loop      // Skip empty lines

    // Save parsed values
    mov x22, x1             // a1
    mov x23, x2             // b1
    mov x24, x3             // a2
    mov x25, x4             // b2

    // Check fully contains
    mov x1, x22
    mov x2, x23
    mov x3, x24
    mov x4, x25
    bl fully_contains
    add x21, x21, x0        // Add result (0 or 1) to count

    b part1_loop

part1_done:
    mov x0, x21

    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 2: Count pairs where ranges overlap at all
// x0 = buffer, x1 = length
// Returns: x0 = count
part2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, x0             // Buffer pointer
    add x20, x19, x1        // Buffer end
    mov x21, #0             // Count

part2_loop:
    cmp x19, x20
    b.ge part2_done

    mov x0, x19
    bl parse_line
    mov x19, x0             // Update pointer

    // Check if valid line
    orr x5, x1, x2
    orr x5, x5, x3
    orr x5, x5, x4
    cbz x5, part2_loop      // Skip empty lines

    // Save parsed values
    mov x22, x1             // a1
    mov x23, x2             // b1
    mov x24, x3             // a2
    mov x25, x4             // b2

    // Check overlap
    mov x1, x22
    mov x2, x23
    mov x3, x24
    mov x4, x25
    bl overlaps
    add x21, x21, x0        // Add result (0 or 1) to count

    b part2_loop

part2_done:
    mov x0, x21

    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print string
// x0 = string pointer
print_str:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    mov x19, x0
    mov x20, #0
ps_len_loop:
    ldrb w1, [x19, x20]
    cbz w1, ps_write
    add x20, x20, #1
    b ps_len_loop
ps_write:
    movz x16, #0x2000, lsl #16
    movk x16, #0x0004
    mov x0, #STDOUT
    mov x1, x19
    mov x2, x20
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print number
// x0 = number to print
print_number:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    adrp x19, output_buffer@PAGE
    add x19, x19, output_buffer@PAGEOFF
    add x19, x19, #31
    mov w1, #0
    strb w1, [x19]
    mov x20, x0
    mov x2, #10

pn_loop:
    udiv x3, x20, x2
    msub x4, x3, x2, x20
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
