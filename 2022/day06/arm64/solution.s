// ARM64 Assembly solution for AoC 2022 Day 6 - Tuning Trouble
// macOS syscalls
// Find first position where last N characters are all unique
// Part 1: N=4, Part 2: N=14

.global _start
.align 2

.equ STDOUT, 1

.data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

.align 3
file_buffer: .skip 8192
buffer_len: .quad 0

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
    mov x2, #8192
    svc #0x80

    adrp x1, buffer_len@PAGE
    add x1, x1, buffer_len@PAGEOFF
    str x0, [x1]            // Save bytes read

    // Close file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0006
    mov x0, x19
    svc #0x80

    // Part 1: Find marker with window size 4
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    adrp x1, buffer_len@PAGE
    add x1, x1, buffer_len@PAGEOFF
    ldr x1, [x1]
    mov x2, #4              // Window size = 4
    bl find_marker
    mov x19, x0             // Save Part 1 result

    // Print Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_str
    mov x0, x19
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Part 2: Find marker with window size 14
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    adrp x1, buffer_len@PAGE
    add x1, x1, buffer_len@PAGEOFF
    ldr x1, [x1]
    mov x2, #14             // Window size = 14
    bl find_marker
    mov x19, x0             // Save Part 2 result

    // Print Part 2
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_str
    mov x0, x19
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

// Find first position where last N characters are all unique
// x0 = buffer pointer
// x1 = buffer length
// x2 = window size (N)
// Returns x0 = position (1-indexed)
find_marker:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, x0             // Buffer pointer
    mov x20, x1             // Buffer length
    mov x21, x2             // Window size

    // Start checking from position window_size (0-indexed: window_size-1 is last char of first window)
    mov x22, x21            // Current position (1-indexed, position after window)

find_marker_loop:
    cmp x22, x20
    b.gt not_found

    // Check if characters at [x22 - window_size, x22) are all unique
    // We use a 32-bit bitmask for lowercase letters (a-z)
    mov x23, #0             // Bitmask for seen characters
    mov x24, #0             // Index within window
    mov x25, #0             // Duplicate found flag

check_window:
    cmp x24, x21
    b.ge window_done

    // Get character at position (x22 - x21 + x24)
    sub x26, x22, x21       // Start of window (0-indexed)
    add x26, x26, x24       // Current position in window
    ldrb w3, [x19, x26]

    // Skip non-lowercase letters (newline, etc.)
    cmp w3, #'a'
    b.lt next_char
    cmp w3, #'z'
    b.gt next_char

    // Convert to bit position (a=0, b=1, ..., z=25)
    sub w3, w3, #'a'

    // Check if already seen (bit set in mask)
    mov x4, #1
    lsl x4, x4, x3
    tst x23, x4
    b.eq not_duplicate

    // Duplicate found
    mov x25, #1
    b window_done

not_duplicate:
    // Mark as seen
    orr x23, x23, x4

next_char:
    add x24, x24, #1
    b check_window

window_done:
    // If no duplicates found, we found the marker
    cbz x25, found_marker

    // Move to next position
    add x22, x22, #1
    b find_marker_loop

found_marker:
    mov x0, x22             // Return position (1-indexed)
    b find_marker_end

not_found:
    mov x0, #-1             // Return -1 if not found

find_marker_end:
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

    mov x19, x0             // Number
    adrp x20, output_buffer@PAGE
    add x20, x20, output_buffer@PAGEOFF

    // Handle zero case
    cbnz x19, pn_convert
    mov w1, #'0'
    strb w1, [x20]
    mov w1, #0
    strb w1, [x20, #1]
    mov x0, x20
    bl print_str
    b pn_done

pn_convert:
    // Convert number to string (reverse)
    add x20, x20, #20       // Start from end of buffer
    mov x1, #0
    strb w1, [x20]          // Null terminator
    mov x2, #10             // Base

pn_loop:
    cbz x19, pn_print
    udiv x3, x19, x2        // x3 = x19 / 10
    msub x4, x3, x2, x19    // x4 = x19 - (x3 * 10) = remainder
    add w4, w4, #'0'
    sub x20, x20, #1
    strb w4, [x20]
    mov x19, x3
    b pn_loop

pn_print:
    mov x0, x20
    bl print_str

pn_done:
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
