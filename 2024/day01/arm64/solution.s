// ARM64 Assembly solution for AoC 2024 Day 1
// macOS syscalls

.global _main
.align 2

.equ STDOUT, 1
.equ MAX_NUMBERS, 1000

.data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

.align 3
file_buffer: .skip 32768
left_list: .skip MAX_NUMBERS * 8
right_list: .skip MAX_NUMBERS * 8
num_count: .skip 8
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
    mov x1, #0  // O_RDONLY
    mov x2, #0
    svc #0x80
    cmp x0, #0
    b.lt exit_error
    mov x19, x0  // Save fd

    // Read file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0003
    mov x0, x19
    adrp x1, file_buffer@PAGE
    add x1, x1, file_buffer@PAGEOFF
    mov x2, #32768
    svc #0x80
    mov x20, x0  // Save bytes read

    // Close file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0006
    mov x0, x19
    svc #0x80

    // Parse input into two lists
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20
    bl parse_input

    // Part 1: Sort and calculate distance
    bl part1

    // Print Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_str
    mov x0, x21  // Part 1 result
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Part 2: Calculate similarity score
    bl part2

    // Print Part 2
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_str
    mov x0, x22  // Part 2 result
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

// Parse input into two arrays
// x0 = buffer, x1 = length
parse_input:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0  // Buffer pointer
    mov x20, x1  // Buffer end
    add x20, x19, x20
    adrp x21, left_list@PAGE
    add x21, x21, left_list@PAGEOFF
    adrp x22, right_list@PAGE
    add x22, x22, right_list@PAGEOFF
    mov x23, #0  // Count

parse_loop:
    cmp x19, x20
    b.ge parse_done

    // Parse first number
    mov x0, x19
    bl parse_number
    str x1, [x21, x23, lsl #3]  // Store in left_list
    mov x19, x0

    // Skip whitespace
    bl skip_whitespace
    mov x19, x0

    // Parse second number
    mov x0, x19
    bl parse_number
    str x1, [x22, x23, lsl #3]  // Store in right_list
    mov x19, x0
    add x23, x23, #1

    // Skip to next line
    bl skip_to_newline
    mov x19, x0
    b parse_loop

parse_done:
    adrp x0, num_count@PAGE
    add x0, x0, num_count@PAGEOFF
    str x23, [x0]

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Parse a number from string
// x0 = pointer, returns x0 = new pointer, x1 = number
parse_number:
    mov x1, #0  // Result
    mov x2, #10 // Base
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

// Skip whitespace
// x0 = pointer, returns x0 = new pointer
skip_whitespace:
skip_ws_loop:
    ldrb w1, [x0]
    cmp w1, #' '
    b.eq skip_ws_next
    cmp w1, #'\t'
    b.eq skip_ws_next
    ret
skip_ws_next:
    add x0, x0, #1
    b skip_ws_loop

// Skip to newline
// x0 = pointer, returns x0 = new pointer
skip_to_newline:
skip_nl_loop:
    ldrb w1, [x0]
    cmp w1, #'\n'
    b.eq skip_nl_found
    cmp w1, #0
    b.eq skip_nl_done
    add x0, x0, #1
    b skip_nl_loop
skip_nl_found:
    add x0, x0, #1
skip_nl_done:
    ret

// Part 1: Sort both lists and calculate total distance
part1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    // Sort left list
    adrp x0, left_list@PAGE
    add x0, x0, left_list@PAGEOFF
    adrp x1, num_count@PAGE
    add x1, x1, num_count@PAGEOFF
    ldr x1, [x1]
    bl bubble_sort

    // Sort right list
    adrp x0, right_list@PAGE
    add x0, x0, right_list@PAGEOFF
    adrp x1, num_count@PAGE
    add x1, x1, num_count@PAGEOFF
    ldr x1, [x1]
    bl bubble_sort

    // Calculate total distance
    adrp x19, left_list@PAGE
    add x19, x19, left_list@PAGEOFF
    adrp x20, right_list@PAGE
    add x20, x20, right_list@PAGEOFF
    adrp x0, num_count@PAGE
    add x0, x0, num_count@PAGEOFF
    ldr x2, [x0]
    mov x21, #0  // Total distance

part1_loop:
    cbz x2, part1_done
    ldr x3, [x19], #8
    ldr x4, [x20], #8
    subs x5, x3, x4
    cneg x5, x5, mi  // Absolute value
    add x21, x21, x5
    sub x2, x2, #1
    b part1_loop

part1_done:
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Bubble sort
// x0 = array, x1 = length
bubble_sort:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0  // Array
    mov x20, x1  // Length
    cbz x20, bs_done
    cmp x20, #1
    b.le bs_done

bs_outer:
    mov x21, #0  // i
    sub x22, x20, #1  // n-1

bs_inner:
    cmp x21, x22
    b.ge bs_outer_check

    // Compare array[i] and array[i+1]
    ldr x3, [x19, x21, lsl #3]
    add x4, x21, #1
    ldr x5, [x19, x4, lsl #3]

    cmp x3, x5
    b.le bs_no_swap

    // Swap
    str x5, [x19, x21, lsl #3]
    str x3, [x19, x4, lsl #3]

bs_no_swap:
    add x21, x21, #1
    b bs_inner

bs_outer_check:
    subs x22, x22, #1
    b.gt bs_outer

bs_done:
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 2: Calculate similarity score
part2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    adrp x19, left_list@PAGE
    add x19, x19, left_list@PAGEOFF
    adrp x20, right_list@PAGE
    add x20, x20, right_list@PAGEOFF
    adrp x0, num_count@PAGE
    add x0, x0, num_count@PAGEOFF
    ldr x21, [x0]
    mov x22, #0  // Similarity score

part2_outer:
    cbz x21, part2_done
    ldr x3, [x19], #8  // Current left number

    // Count occurrences in right list
    adrp x4, right_list@PAGE
    add x4, x4, right_list@PAGEOFF
    adrp x0, num_count@PAGE
    add x0, x0, num_count@PAGEOFF
    ldr x5, [x0]
    mov x6, #0  // Count

part2_inner:
    cbz x5, part2_calc
    ldr x7, [x4], #8
    cmp x7, x3
    b.ne part2_next
    add x6, x6, #1
part2_next:
    sub x5, x5, #1
    b part2_inner

part2_calc:
    mul x7, x3, x6
    add x22, x22, x7
    sub x21, x21, #1
    b part2_outer

part2_done:
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
