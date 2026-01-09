// ARM64 Assembly solution for Advent of Code 2022 Day 25
// SNAFU number system (balanced base-5) conversion
// macOS ARM64 conventions

.global _start
.align 4

// Constants
.equ SYS_EXIT, 1
.equ SYS_READ, 3
.equ SYS_WRITE, 4
.equ SYS_OPEN, 5
.equ SYS_CLOSE, 6

.equ O_RDONLY, 0
.equ STDIN, 0
.equ STDOUT, 1
.equ STDERR, 2

.equ BUFFER_SIZE, 8192
.equ SNAFU_MAX, 64

.section __DATA,__data

filename:
    .asciz "../input.txt"

part1_prefix:
    .asciz "Part 1: "

part2_msg:
    .asciz "\nPart 2: No Part 2 on Day 25!\n"

.section __DATA,__bss
.align 4
file_buffer:
    .space BUFFER_SIZE
snafu_result:
    .space SNAFU_MAX
temp_digits:
    .space SNAFU_MAX

.section __TEXT,__text

// _start - entry point
_start:
    // Open file
    adrp    x0, filename@PAGE
    add     x0, x0, filename@PAGEOFF
    mov     x1, #O_RDONLY
    mov     x2, #0
    mov     x16, #SYS_OPEN
    svc     #0x80

    // Check for error
    cmp     x0, #0
    b.lt    exit_error

    mov     x19, x0             // Save file descriptor in x19

    // Read file
    mov     x0, x19
    adrp    x1, file_buffer@PAGE
    add     x1, x1, file_buffer@PAGEOFF
    mov     x2, #BUFFER_SIZE
    mov     x16, #SYS_READ
    svc     #0x80

    cmp     x0, #0
    b.lt    exit_error

    mov     x20, x0             // Save bytes read in x20

    // Close file
    mov     x0, x19
    mov     x16, #SYS_CLOSE
    svc     #0x80

    // Process all SNAFU numbers and sum them
    mov     x21, #0             // x21 = total sum
    adrp    x22, file_buffer@PAGE
    add     x22, x22, file_buffer@PAGEOFF  // x22 = current position in buffer
    mov     x23, x20            // x23 = remaining bytes

process_loop:
    cmp     x23, #0
    b.le    done_processing

    // Skip empty lines and find start of SNAFU number
    ldrb    w0, [x22]
    cmp     w0, #'\n'
    b.eq    skip_newline
    cmp     w0, #'\r'
    b.eq    skip_newline
    cmp     w0, #0
    b.eq    done_processing

    // Convert current SNAFU line to decimal
    mov     x0, x22             // x0 = start of SNAFU string
    bl      snafu_to_decimal
    add     x21, x21, x0        // Add to total

    // Skip to end of line
skip_to_eol:
    ldrb    w0, [x22]
    add     x22, x22, #1
    sub     x23, x23, #1
    cmp     x23, #0
    b.le    done_processing
    cmp     w0, #'\n'
    b.ne    skip_to_eol
    b       process_loop

skip_newline:
    add     x22, x22, #1
    sub     x23, x23, #1
    b       process_loop

done_processing:
    // Convert total to SNAFU
    mov     x0, x21             // x0 = decimal number
    adrp    x1, snafu_result@PAGE
    add     x1, x1, snafu_result@PAGEOFF  // x1 = result buffer
    bl      decimal_to_snafu
    mov     x24, x0             // x24 = length of SNAFU string

    // Print "Part 1: "
    mov     x0, #STDOUT
    adrp    x1, part1_prefix@PAGE
    add     x1, x1, part1_prefix@PAGEOFF
    mov     x2, #8              // "Part 1: " is 8 chars
    mov     x16, #SYS_WRITE
    svc     #0x80

    // Print SNAFU result
    mov     x0, #STDOUT
    adrp    x1, snafu_result@PAGE
    add     x1, x1, snafu_result@PAGEOFF
    mov     x2, x24
    mov     x16, #SYS_WRITE
    svc     #0x80

    // Print Part 2 message
    mov     x0, #STDOUT
    adrp    x1, part2_msg@PAGE
    add     x1, x1, part2_msg@PAGEOFF
    mov     x2, #31             // length of part2_msg
    mov     x16, #SYS_WRITE
    svc     #0x80

    // Exit success
    mov     x0, #0
    mov     x16, #SYS_EXIT
    svc     #0x80

exit_error:
    mov     x0, #1
    mov     x16, #SYS_EXIT
    svc     #0x80

// snafu_to_decimal: Convert SNAFU string to decimal
// Input: x0 = pointer to SNAFU string (null or newline terminated)
// Output: x0 = decimal value
// Clobbers: x1-x5
snafu_to_decimal:
    mov     x1, x0              // x1 = string pointer
    mov     x0, #0              // x0 = result
    mov     x2, #5              // x2 = base 5

snafu_loop:
    ldrb    w3, [x1], #1        // Load char, increment pointer

    // Check for end of string
    cmp     w3, #0
    b.eq    snafu_done
    cmp     w3, #'\n'
    b.eq    snafu_done
    cmp     w3, #'\r'
    b.eq    snafu_done

    // Multiply result by 5
    mul     x0, x0, x2

    // Convert digit
    cmp     w3, #'2'
    b.eq    digit_2
    cmp     w3, #'1'
    b.eq    digit_1
    cmp     w3, #'0'
    b.eq    digit_0
    cmp     w3, #'-'
    b.eq    digit_minus
    cmp     w3, #'='
    b.eq    digit_dminus
    b       snafu_loop          // Unknown char, skip

digit_2:
    add     x0, x0, #2
    b       snafu_loop
digit_1:
    add     x0, x0, #1
    b       snafu_loop
digit_0:
    b       snafu_loop
digit_minus:
    sub     x0, x0, #1
    b       snafu_loop
digit_dminus:
    sub     x0, x0, #2
    b       snafu_loop

snafu_done:
    ret

// decimal_to_snafu: Convert decimal to SNAFU string
// Input: x0 = decimal value, x1 = output buffer
// Output: x0 = length of result string
// Clobbers: x2-x10
decimal_to_snafu:
    // Save callee-saved registers
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    mov     x2, x0              // x2 = n (working copy)
    mov     x3, x1              // x3 = output buffer
    adrp    x4, temp_digits@PAGE
    add     x4, x4, temp_digits@PAGEOFF  // x4 = temporary digit buffer
    mov     x5, #0              // x5 = digit index
    mov     x6, #5              // x6 = divisor (5)

    // Handle zero case
    cmp     x2, #0
    b.ne    convert_loop

    mov     w0, #'0'
    strb    w0, [x3]
    mov     w0, #0
    strb    w0, [x3, #1]
    mov     x0, #1
    ldp     x29, x30, [sp], #16
    ret

convert_loop:
    cmp     x2, #0
    b.le    reverse_digits

    // remainder = n % 5
    udiv    x7, x2, x6          // x7 = n / 5
    msub    x8, x7, x6, x2      // x8 = n - (n/5)*5 = n % 5

    // Check remainder value
    cmp     x8, #2
    b.gt    handle_3_or_4

    // remainder <= 2: digit = '0' + remainder
    add     w9, w8, #'0'
    strb    w9, [x4, x5]
    add     x5, x5, #1
    mov     x2, x7              // n = n / 5
    b       convert_loop

handle_3_or_4:
    cmp     x8, #3
    b.ne    handle_4

    // remainder == 3: digit = '=', n = n/5 + 1
    mov     w9, #'='
    strb    w9, [x4, x5]
    add     x5, x5, #1
    add     x2, x7, #1          // n = n/5 + 1
    b       convert_loop

handle_4:
    // remainder == 4: digit = '-', n = n/5 + 1
    mov     w9, #'-'
    strb    w9, [x4, x5]
    add     x5, x5, #1
    add     x2, x7, #1          // n = n/5 + 1
    b       convert_loop

reverse_digits:
    // Reverse temp_digits into output buffer
    mov     x7, #0              // x7 = output index
    sub     x8, x5, #1          // x8 = source index (last digit)

reverse_loop:
    cmp     x8, #0
    b.lt    reverse_done

    ldrb    w9, [x4, x8]
    strb    w9, [x3, x7]
    add     x7, x7, #1
    sub     x8, x8, #1
    b       reverse_loop

reverse_done:
    // Null terminate
    mov     w9, #0
    strb    w9, [x3, x7]

    mov     x0, x5              // Return length
    ldp     x29, x30, [sp], #16
    ret
