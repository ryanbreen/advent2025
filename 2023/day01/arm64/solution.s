// Advent of Code 2023 Day 1 - ARM64 Assembly for macOS
// Part 1: Extract first and last digit from each line
// Part 2: Also recognize spelled-out digit words (one, two, ..., nine)

.global _main
.align 4

// Data section
.data
    filename: .asciz "../input.txt"
    msg_part1: .asciz "Part 1: "
    msg_part2: .asciz "Part 2: "
    newline: .asciz "\n"

    // Word lookup table: word pointer, length, value
    .align 3
word_table:
    .quad word_one, 3, 1
    .quad word_two, 3, 2
    .quad word_three, 5, 3
    .quad word_four, 4, 4
    .quad word_five, 4, 5
    .quad word_six, 3, 6
    .quad word_seven, 5, 7
    .quad word_eight, 5, 8
    .quad word_nine, 4, 9
    .quad 0, 0, 0           // Sentinel

    // Spelled-out digit words for Part 2
    word_one:   .asciz "one"
    word_two:   .asciz "two"
    word_three: .asciz "three"
    word_four:  .asciz "four"
    word_five:  .asciz "five"
    word_six:   .asciz "six"
    word_seven: .asciz "seven"
    word_eight: .asciz "eight"
    word_nine:  .asciz "nine"

.bss
    .align 4
    buffer: .skip 65536        // Buffer for file contents
    num_buffer: .skip 32       // Buffer for number conversion

.text

// Main entry point
_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    // Open and read file
    bl read_file
    cbz x0, exit_error

    // x0 now has file size, save it
    adrp x9, buffer@PAGE
    add x9, x9, buffer@PAGEOFF

    // Part 1
    adrp x0, msg_part1@PAGE
    add x0, x0, msg_part1@PAGEOFF
    bl print_str

    adrp x0, buffer@PAGE
    add x0, x0, buffer@PAGEOFF
    bl part1
    bl print_num

    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Part 2
    adrp x0, msg_part2@PAGE
    add x0, x0, msg_part2@PAGEOFF
    bl print_str

    adrp x0, buffer@PAGE
    add x0, x0, buffer@PAGEOFF
    bl part2
    bl print_num

    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Exit
    mov x0, #0
    ldp x29, x30, [sp], #16
    mov x16, #1
    svc #0x80

exit_error:
    mov x0, #1
    ldp x29, x30, [sp], #16
    mov x16, #1
    svc #0x80

// Read file into buffer, returns size in x0
read_file:
    stp x29, x30, [sp, #-32]!
    mov x29, sp
    str x19, [sp, #16]

    // open(filename, O_RDONLY, 0)
    mov x16, #5
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #0
    mov x2, #0
    svc #0x80
    cmp x0, #0
    blt rf_error
    mov x19, x0              // x19 = fd

    // read(fd, buffer, 65536)
    mov x16, #3
    mov x0, x19
    adrp x1, buffer@PAGE
    add x1, x1, buffer@PAGEOFF
    mov x2, #65536
    svc #0x80
    cmp x0, #0
    blt rf_error

    // Store size for validation
    mov x2, x0

    // close(fd)
    mov x16, #6
    mov x0, x19
    svc #0x80

    mov x0, x2
    ldr x19, [sp, #16]
    ldp x29, x30, [sp], #32
    ret

rf_error:
    mov x0, #0
    ldr x19, [sp, #16]
    ldp x29, x30, [sp], #32
    ret

// Part 1: sum of calibration values (numeric digits only)
// x0 = buffer pointer
// Returns sum in x0
part1:
    stp x29, x30, [sp, #-48]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]

    mov x19, x0              // x19 = buffer ptr
    mov x20, #0              // x20 = total sum
    mov x21, #-1             // x21 = first digit
    mov x22, #-1             // x22 = last digit

p1_loop:
    ldrb w0, [x19]
    cbz w0, p1_end           // null = end

    cmp w0, #10              // newline?
    beq p1_newline

    // Check if digit '0'-'9' (48-57)
    cmp w0, #48
    blt p1_next
    cmp w0, #57
    bgt p1_next

    // Found digit
    sub w0, w0, #48
    cmp x21, #-1
    bne p1_not_first
    mov x21, x0              // first digit
p1_not_first:
    mov x22, x0              // last digit

p1_next:
    add x19, x19, #1
    b p1_loop

p1_newline:
    // End of line - calculate value
    cmp x21, #-1
    beq p1_skip_line

    // value = first*10 + last
    // Optimized multiply: x*10 = x*8 + x*2 = (x<<3) + (x<<1)
    add x0, x22, x21, lsl #3  // x0 = last + (first << 3)
    add x0, x0, x21, lsl #1   // x0 = x0 + (first << 1)
    add x20, x20, x0

p1_skip_line:
    mov x21, #-1
    mov x22, #-1
    add x19, x19, #1
    b p1_loop

p1_end:
    // Handle last line if no trailing newline
    cmp x21, #-1
    beq p1_done
    add x0, x22, x21, lsl #3
    add x0, x0, x21, lsl #1
    add x20, x20, x0

p1_done:
    mov x0, x20
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #48
    ret

// Part 2: sum of calibration values (including spelled words)
// x0 = buffer pointer
// Returns sum in x0
part2:
    stp x29, x30, [sp, #-64]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]

    mov x19, x0              // x19 = buffer ptr
    mov x20, #0              // x20 = total sum
    mov x21, #-1             // x21 = first digit
    mov x22, #-1             // x22 = last digit

p2_loop:
    ldrb w23, [x19]
    cbz w23, p2_end          // null = end

    cmp w23, #10             // newline?
    beq p2_newline

    // Check if numeric digit '0'-'9' (48-57)
    cmp w23, #48
    blt p2_check_word
    cmp w23, #57
    bgt p2_check_word

    // Found numeric digit
    sub w24, w23, #48
    b p2_found_digit

p2_check_word:
    // First-character dispatch optimization
    // Branch based on first character: o, t, f, s, e, n
    cmp w23, #'o'            // 'o' = one
    beq p2_check_one
    cmp w23, #'t'            // 't' = two, three
    beq p2_check_t_words
    cmp w23, #'f'            // 'f' = four, five
    beq p2_check_f_words
    cmp w23, #'s'            // 's' = six, seven
    beq p2_check_s_words
    cmp w23, #'e'            // 'e' = eight
    beq p2_check_eight
    cmp w23, #'n'            // 'n' = nine
    beq p2_check_nine
    b p2_next

p2_check_one:
    mov x0, x19
    adrp x1, word_one@PAGE
    add x1, x1, word_one@PAGEOFF
    mov x2, #3
    bl check_prefix
    cbnz x0, p2_found_one
    b p2_next

p2_check_t_words:
    // Check two first
    mov x0, x19
    adrp x1, word_two@PAGE
    add x1, x1, word_two@PAGEOFF
    mov x2, #3
    bl check_prefix
    cbnz x0, p2_found_two
    // Check three
    mov x0, x19
    adrp x1, word_three@PAGE
    add x1, x1, word_three@PAGEOFF
    mov x2, #5
    bl check_prefix
    cbnz x0, p2_found_three
    b p2_next

p2_check_f_words:
    // Check four first
    mov x0, x19
    adrp x1, word_four@PAGE
    add x1, x1, word_four@PAGEOFF
    mov x2, #4
    bl check_prefix
    cbnz x0, p2_found_four
    // Check five
    mov x0, x19
    adrp x1, word_five@PAGE
    add x1, x1, word_five@PAGEOFF
    mov x2, #4
    bl check_prefix
    cbnz x0, p2_found_five
    b p2_next

p2_check_s_words:
    // Check six first
    mov x0, x19
    adrp x1, word_six@PAGE
    add x1, x1, word_six@PAGEOFF
    mov x2, #3
    bl check_prefix
    cbnz x0, p2_found_six
    // Check seven
    mov x0, x19
    adrp x1, word_seven@PAGE
    add x1, x1, word_seven@PAGEOFF
    mov x2, #5
    bl check_prefix
    cbnz x0, p2_found_seven
    b p2_next

p2_check_eight:
    mov x0, x19
    adrp x1, word_eight@PAGE
    add x1, x1, word_eight@PAGEOFF
    mov x2, #5
    bl check_prefix
    cbnz x0, p2_found_eight
    b p2_next

p2_check_nine:
    mov x0, x19
    adrp x1, word_nine@PAGE
    add x1, x1, word_nine@PAGEOFF
    mov x2, #4
    bl check_prefix
    cbnz x0, p2_found_nine
    b p2_next

p2_found_one:
    mov w24, #1
    b p2_found_digit
p2_found_two:
    mov w24, #2
    b p2_found_digit
p2_found_three:
    mov w24, #3
    b p2_found_digit
p2_found_four:
    mov w24, #4
    b p2_found_digit
p2_found_five:
    mov w24, #5
    b p2_found_digit
p2_found_six:
    mov w24, #6
    b p2_found_digit
p2_found_seven:
    mov w24, #7
    b p2_found_digit
p2_found_eight:
    mov w24, #8
    b p2_found_digit
p2_found_nine:
    mov w24, #9
    b p2_found_digit

p2_found_digit:
    cmp x21, #-1
    bne p2_not_first
    mov x21, x24             // first digit
p2_not_first:
    mov x22, x24             // last digit

p2_next:
    add x19, x19, #1
    b p2_loop

p2_newline:
    // End of line - calculate value
    cmp x21, #-1
    beq p2_skip_line

    // value = first*10 + last
    // Optimized multiply: x*10 = x*8 + x*2 = (x<<3) + (x<<1)
    add x0, x22, x21, lsl #3  // x0 = last + (first << 3)
    add x0, x0, x21, lsl #1   // x0 = x0 + (first << 1)
    add x20, x20, x0

p2_skip_line:
    mov x21, #-1
    mov x22, #-1
    add x19, x19, #1
    b p2_loop

p2_end:
    // Handle last line if no trailing newline
    cmp x21, #-1
    beq p2_done
    add x0, x22, x21, lsl #3
    add x0, x0, x21, lsl #1
    add x20, x20, x0

p2_done:
    mov x0, x20
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

// Check if x0 starts with string at x1 of length x2
// Returns 1 if match, 0 otherwise
// Registers: x0=buffer, x1=pattern, x2=length
// Uses: x3, x4, x5, w6, w7
check_prefix:
    mov x3, x0               // x3 = buffer ptr
    mov x4, x1               // x4 = pattern ptr
    mov x5, x2               // x5 = remaining length

cp_loop:
    cbz x5, cp_match
    ldrb w6, [x3], #1
    ldrb w7, [x4], #1
    cmp w6, w7
    bne cp_no_match
    sub x5, x5, #1
    b cp_loop

cp_match:
    mov x0, #1
    ret

cp_no_match:
    mov x0, #0
    ret

// Print null-terminated string at x0
// Registers: x0=string ptr
// Uses: x1, x2, x3, x16, x19
print_str:
    stp x29, x30, [sp, #-32]!
    mov x29, sp
    str x19, [sp, #16]

    mov x19, x0

    // Find length
    mov x1, x0
    mov x2, #0
ps_len:
    ldrb w3, [x1], #1
    cbz w3, ps_write
    add x2, x2, #1
    b ps_len

ps_write:
    // write(1, string, length)
    mov x16, #4
    mov x0, #1
    mov x1, x19
    svc #0x80

    ldr x19, [sp, #16]
    ldp x29, x30, [sp], #32
    ret

// Print number in x0
// Registers: x0=number to print
// Uses: x1, x2, x3, w1, x16, x19, x20
print_num:
    stp x29, x30, [sp, #-32]!
    mov x29, sp
    stp x19, x20, [sp, #16]

    mov x19, x0              // x19 = number to print

    // Use num_buffer for digit conversion
    adrp x20, num_buffer@PAGE
    add x20, x20, num_buffer@PAGEOFF
    add x20, x20, #30        // x20 = end of buffer
    mov w1, #0
    strb w1, [x20]           // null terminator

    // Handle zero
    cbnz x19, pn_convert
    sub x20, x20, #1
    mov w1, #48
    strb w1, [x20]
    b pn_print

pn_convert:
    mov x1, #10
pn_loop:
    cbz x19, pn_print
    sub x20, x20, #1
    udiv x2, x19, x1         // x2 = quotient
    msub x3, x2, x1, x19     // x3 = remainder
    add w3, w3, #48          // convert to ASCII
    strb w3, [x20]
    mov x19, x2
    b pn_loop

pn_print:
    mov x0, x20
    bl print_str

    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #32
    ret
