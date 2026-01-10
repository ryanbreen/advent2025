// ARM64 Assembly solution for AoC 2021 Day 1 - Sonar Sweep
// macOS syscalls

.global _main
.align 2

.equ STDOUT, 1
.equ MAX_DEPTHS, 2048

.data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

.align 3
file_buffer: .skip 32768
depths: .skip MAX_DEPTHS * 4    // Array of depth values (32-bit integers)
depth_count: .skip 8            // Number of depth readings
output_buffer: .skip 32

.text
_main:
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
    mov x2, #32768
    svc #0x80
    mov x20, x0             // Save bytes read

    // Close file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0006
    mov x0, x19
    svc #0x80

    // Parse input into depths array
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20
    bl parse_input

    // Part 1: Count increases from previous depth
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

    // Part 2: Count sliding window sum increases
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

// Parse input into array of depths
// x0 = buffer, x1 = length
// Each line contains a single integer depth value
parse_input:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0             // Buffer pointer
    add x20, x19, x1        // Buffer end
    adrp x21, depths@PAGE
    add x21, x21, depths@PAGEOFF
    mov x22, #0             // Depth count

parse_loop:
    cmp x19, x20
    b.ge parse_done

    // Skip any whitespace/newlines
    ldrb w23, [x19]
    cmp w23, #'\n'
    b.eq skip_whitespace
    cmp w23, #' '
    b.eq skip_whitespace
    cmp w23, #'\r'
    b.eq skip_whitespace

    // Check for end of buffer
    cmp w23, #0
    b.eq parse_done

    // Check if it's a digit
    cmp w23, #'0'
    b.lt skip_whitespace
    cmp w23, #'9'
    b.gt skip_whitespace

    // Parse a number
    mov x0, x19
    bl parse_number
    mov x19, x0             // Update pointer

    // Store depth as 32-bit integer
    str w1, [x21, x22, lsl #2]
    add x22, x22, #1
    b parse_loop

skip_whitespace:
    add x19, x19, #1
    b parse_loop

parse_done:
    adrp x0, depth_count@PAGE
    add x0, x0, depth_count@PAGEOFF
    str x22, [x0]

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

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

// Part 1: Count how many times depths[i] > depths[i-1]
// Returns count in x0
part1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    adrp x1, depths@PAGE
    add x1, x1, depths@PAGEOFF
    adrp x2, depth_count@PAGE
    add x2, x2, depth_count@PAGEOFF
    ldr x2, [x2]            // Total count

    mov x0, #0              // Increase count
    cmp x2, #2
    b.lt part1_done         // Need at least 2 elements

    // Load first element
    ldr w3, [x1], #4        // Previous value
    sub x2, x2, #1          // Remaining elements to check

part1_loop:
    cbz x2, part1_done
    ldr w4, [x1], #4        // Current value
    cmp w4, w3
    b.le part1_not_inc
    add x0, x0, #1          // Increment count
part1_not_inc:
    mov w3, w4              // Previous = current
    sub x2, x2, #1
    b part1_loop

part1_done:
    ldp x29, x30, [sp], #16
    ret

// Part 2: Count sliding window increases
// Comparing depths[i+3] > depths[i] (middle terms cancel)
// Returns count in x0
part2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    adrp x1, depths@PAGE
    add x1, x1, depths@PAGEOFF
    adrp x2, depth_count@PAGE
    add x2, x2, depth_count@PAGEOFF
    ldr x2, [x2]            // Total count

    mov x0, #0              // Increase count
    cmp x2, #4
    b.lt part2_done         // Need at least 4 elements for comparison

    sub x2, x2, #3          // Number of comparisons to make (count - 3)

part2_loop:
    cbz x2, part2_done
    ldr w3, [x1]            // depths[i]
    ldr w4, [x1, #12]       // depths[i+3] (offset 3*4=12 bytes)
    cmp w4, w3
    b.le part2_not_inc
    add x0, x0, #1          // Increment count
part2_not_inc:
    add x1, x1, #4          // Move to next position
    sub x2, x2, #1
    b part2_loop

part2_done:
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
