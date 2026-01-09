// ARM64 Assembly solution for AoC 2022 Day 3 - Rucksack Reorganization
// macOS syscalls
// Uses 64-bit bitmasks for efficient set operations

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

// Convert char to priority (bit position)
// Input: w0 = char
// Output: x0 = priority (1-52), or 0 if invalid
// a-z = 1-26, A-Z = 27-52
char_to_priority:
    // Check lowercase a-z
    cmp w0, #'a'
    b.lt check_upper
    cmp w0, #'z'
    b.gt invalid_char
    sub w0, w0, #'a'
    add w0, w0, #1          // a=1, z=26
    ret

check_upper:
    cmp w0, #'A'
    b.lt invalid_char
    cmp w0, #'Z'
    b.gt invalid_char
    sub w0, w0, #'A'
    add w0, w0, #27         // A=27, Z=52
    ret

invalid_char:
    mov x0, #0
    ret

// Build bitmask from string portion
// Input: x0 = string start, x1 = length
// Output: x0 = bitmask
build_bitmask:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0             // String pointer
    mov x20, x1             // Length
    mov x21, #0             // Bitmask result

build_mask_loop:
    cbz x20, build_mask_done

    ldrb w0, [x19], #1
    sub x20, x20, #1

    bl char_to_priority
    cbz x0, build_mask_loop // Skip invalid chars

    // Set bit at position (priority - 1)
    sub x0, x0, #1
    mov x22, #1
    lsl x22, x22, x0
    orr x21, x21, x22

    b build_mask_loop

build_mask_done:
    mov x0, x21

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Find lowest set bit position + 1 (the priority)
// Input: x0 = bitmask with at least one bit set
// Output: x0 = priority (1-52)
find_lowest_bit:
    clz x1, x0              // Count leading zeros
    rbit x0, x0             // Reverse bits
    clz x0, x0              // Count leading zeros of reversed = trailing zeros
    add x0, x0, #1          // Convert to 1-based priority
    ret

// Part 1: Find common char between two halves of each line
// Input: x0 = buffer, x1 = length
// Output: x0 = sum of priorities
part1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, x0             // Buffer pointer
    add x20, x19, x1        // Buffer end
    mov x21, #0             // Total priority sum

part1_line_loop:
    cmp x19, x20
    b.ge part1_done

    // Find line length (until newline or end)
    mov x22, x19            // Line start
    mov x23, #0             // Line length

find_line_end:
    add x24, x22, x23
    cmp x24, x20
    b.ge line_found
    ldrb w25, [x24]
    cmp w25, #'\n'
    b.eq line_found
    add x23, x23, #1
    b find_line_end

line_found:
    // Skip empty lines
    cbz x23, skip_empty_line

    // Split line in half
    lsr x24, x23, #1        // Half length

    // Build bitmask for first half
    mov x0, x22
    mov x1, x24
    bl build_bitmask
    mov x25, x0             // First half bitmask

    // Build bitmask for second half
    add x0, x22, x24
    sub x1, x23, x24
    bl build_bitmask
    mov x26, x0             // Second half bitmask

    // Find common (AND the bitmasks)
    and x0, x25, x26

    // Find the priority of common item
    cbz x0, skip_empty_line
    bl find_lowest_bit
    add x21, x21, x0        // Add to total

skip_empty_line:
    // Move to next line
    add x19, x22, x23
    add x19, x19, #1        // Skip newline
    b part1_line_loop

part1_done:
    mov x0, x21

    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Build bitmask for entire line
// Input: x0 = line start, x1 = buffer end
// Output: x0 = bitmask, x1 = pointer to next line start
build_line_bitmask:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0             // Current position
    mov x20, x1             // Buffer end
    mov x21, #0             // Bitmask

line_mask_loop:
    cmp x19, x20
    b.ge line_mask_done

    ldrb w0, [x19]
    cmp w0, #'\n'
    b.eq line_mask_done

    // Get priority and set bit
    bl char_to_priority
    cbz x0, line_mask_next

    sub x0, x0, #1
    mov x22, #1
    lsl x22, x22, x0
    orr x21, x21, x22

line_mask_next:
    add x19, x19, #1
    b line_mask_loop

line_mask_done:
    mov x0, x21
    // Skip past newline if present
    cmp x19, x20
    b.ge no_newline_skip
    ldrb w1, [x19]
    cmp w1, #'\n'
    b.ne no_newline_skip
    add x19, x19, #1
no_newline_skip:
    mov x1, x19             // Return next line start

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 2: Find common char among groups of 3 lines
// Input: x0 = buffer, x1 = length
// Output: x0 = sum of priorities
part2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, x0             // Current position
    add x20, x19, x1        // Buffer end
    mov x21, #0             // Total priority sum

part2_group_loop:
    cmp x19, x20
    b.ge part2_done

    // Process group of 3 lines
    // Line 1
    mov x0, x19
    mov x1, x20
    bl build_line_bitmask
    mov x22, x0             // Line 1 bitmask
    mov x19, x1             // Update position

    cmp x19, x20
    b.ge part2_done

    // Line 2
    mov x0, x19
    mov x1, x20
    bl build_line_bitmask
    mov x23, x0             // Line 2 bitmask
    mov x19, x1             // Update position

    cmp x19, x20
    b.ge part2_done

    // Line 3
    mov x0, x19
    mov x1, x20
    bl build_line_bitmask
    mov x24, x0             // Line 3 bitmask
    mov x19, x1             // Update position

    // Find common (AND all three bitmasks)
    and x0, x22, x23
    and x0, x0, x24

    // Find the priority of common item
    cbz x0, part2_group_loop
    bl find_lowest_bit
    add x21, x21, x0        // Add to total

    b part2_group_loop

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
