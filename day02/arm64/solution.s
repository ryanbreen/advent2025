// Day 2: Gift Shop - ARM64 Assembly for macOS
// On ARM64 macOS, printf varargs are passed on the stack, not in registers!
// Assemble: as -o solution.o solution.s && ld -o solution solution.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _main
// Run: ./solution

.global _main
.align 4

// Constants
.equ O_RDONLY, 0
.equ BUFFER_SIZE, 262144

.section __DATA,__data
.align 3
input_path: .asciz "../input.txt"
part1_fmt: .asciz "Part 1: %lld\n"
part2_fmt: .asciz "Part 2: %lld\n"

.section __DATA,__bss
.align 3
buffer: .space BUFFER_SIZE
num_str: .space 32              // Buffer for number to string conversion

.section __TEXT,__text

// Main function
_main:
    // Prologue - allocate stack frame (16-byte aligned)
    sub sp, sp, #128
    stp x29, x30, [sp, #112]
    add x29, sp, #112
    stp x19, x20, [sp, #96]
    stp x21, x22, [sp, #80]
    stp x23, x24, [sp, #64]
    stp x25, x26, [sp, #48]
    stp x27, x28, [sp, #32]

    // Open file
    adrp x0, input_path@PAGE
    add x0, x0, input_path@PAGEOFF
    mov w1, #O_RDONLY
    bl _open
    sxtw x19, w0            // x19 = fd

    cmp x19, #0
    b.lt exit_error

    // Get buffer address into x20
    adrp x20, buffer@PAGE
    add x20, x20, buffer@PAGEOFF

    // Read file
    mov x0, x19
    mov x1, x20
    mov x2, #BUFFER_SIZE
    bl _read
    mov x21, x0             // x21 = bytes read

    // Close file
    mov x0, x19
    bl _close

    cmp x21, #0
    b.le exit_error

    // Null-terminate
    strb wzr, [x20, x21]

    // ===== Part 1 =====
    mov x22, #0             // x22 = total sum (part 1)
    mov x23, x20            // x23 = input pointer

part1_range_loop:
    // Parse start number
    bl parse_number         // Returns number in x0, updates x23
    mov x24, x0             // x24 = start

    // Skip '-'
    ldrb w0, [x23]
    cmp w0, #'-'
    b.ne part1_done
    add x23, x23, #1

    // Parse end number
    bl parse_number
    mov x25, x0             // x25 = end

    // Iterate through range [start, end]
part1_num_loop:
    cmp x24, x25
    b.gt part1_next_range

    // Check if x24 is invalid (part 1 - exactly twice)
    mov x0, x24
    bl is_invalid_part1
    cbz x0, part1_num_next
    add x22, x22, x24       // Add to sum

part1_num_next:
    add x24, x24, #1
    b part1_num_loop

part1_next_range:
    // Skip comma or end
    ldrb w0, [x23]
    cbz w0, part1_done
    cmp w0, #','
    b.ne part1_skip_char
    add x23, x23, #1
    b part1_range_loop

part1_skip_char:
    cmp w0, #'\n'
    b.eq part1_done
    cmp w0, #'\r'
    b.eq part1_done
    add x23, x23, #1
    b part1_next_range

part1_done:
    // Print Part 1
    str x22, [sp]
    adrp x0, part1_fmt@PAGE
    add x0, x0, part1_fmt@PAGEOFF
    bl _printf

    // ===== Part 2 =====
    mov x22, #0             // x22 = total sum (part 2)
    mov x23, x20            // x23 = input pointer

part2_range_loop:
    // Parse start number
    bl parse_number
    mov x24, x0             // x24 = start

    // Skip '-'
    ldrb w0, [x23]
    cmp w0, #'-'
    b.ne part2_done
    add x23, x23, #1

    // Parse end number
    bl parse_number
    mov x25, x0             // x25 = end

    // Iterate through range
part2_num_loop:
    cmp x24, x25
    b.gt part2_next_range

    // Check if x24 is invalid (part 2 - at least twice)
    mov x0, x24
    bl is_invalid_part2
    cbz x0, part2_num_next
    add x22, x22, x24

part2_num_next:
    add x24, x24, #1
    b part2_num_loop

part2_next_range:
    ldrb w0, [x23]
    cbz w0, part2_done
    cmp w0, #','
    b.ne part2_skip_char
    add x23, x23, #1
    b part2_range_loop

part2_skip_char:
    cmp w0, #'\n'
    b.eq part2_done
    cmp w0, #'\r'
    b.eq part2_done
    add x23, x23, #1
    b part2_next_range

part2_done:
    // Print Part 2
    str x22, [sp]
    adrp x0, part2_fmt@PAGE
    add x0, x0, part2_fmt@PAGEOFF
    bl _printf

    mov x0, #0
    b exit_program

exit_error:
    mov x0, #1

exit_program:
    ldp x27, x28, [sp, #32]
    ldp x25, x26, [sp, #48]
    ldp x23, x24, [sp, #64]
    ldp x21, x22, [sp, #80]
    ldp x19, x20, [sp, #96]
    ldp x29, x30, [sp, #112]
    add sp, sp, #128
    bl _exit

// Parse a number from x23, returns value in x0, advances x23
parse_number:
    mov x0, #0
1:  ldrb w1, [x23]
    sub w2, w1, #'0'
    cmp w2, #9
    b.hi 2f
    mov x3, #10
    mul x0, x0, x3
    and x2, x2, #0xFF
    add x0, x0, x2
    add x23, x23, #1
    b 1b
2:  ret

// Check if number in x0 is invalid for Part 1 (pattern repeated exactly twice)
// Returns 1 if invalid, 0 if valid
is_invalid_part1:
    stp x29, x30, [sp, #-64]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]

    mov x19, x0             // x19 = number

    // Convert number to string, get length
    bl num_to_str           // Returns length in x0, string in num_str
    mov x20, x0             // x20 = length

    // Must have even length
    tst x20, #1
    b.ne invalid1_false

    // Get string pointer
    adrp x21, num_str@PAGE
    add x21, x21, num_str@PAGEOFF

    // Compare first half with second half
    lsr x22, x20, #1        // x22 = mid = length / 2
    mov x23, #0             // x23 = index

invalid1_compare:
    cmp x23, x22
    b.ge invalid1_true      // All chars matched

    ldrb w0, [x21, x23]             // first_half[i]
    add x24, x23, x22
    ldrb w1, [x21, x24]             // second_half[i]
    cmp w0, w1
    b.ne invalid1_false

    add x23, x23, #1
    b invalid1_compare

invalid1_true:
    mov x0, #1
    b invalid1_done

invalid1_false:
    mov x0, #0

invalid1_done:
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

// Check if number in x0 is invalid for Part 2 (pattern repeated at least twice)
// Returns 1 if invalid, 0 if valid
is_invalid_part2:
    stp x29, x30, [sp, #-80]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]
    stp x25, x26, [sp, #64]

    mov x19, x0             // x19 = number

    // Convert number to string
    bl num_to_str
    mov x20, x0             // x20 = length

    // Get string pointer
    adrp x21, num_str@PAGE
    add x21, x21, num_str@PAGEOFF

    // Try all pattern lengths from 1 to length/2
    mov x22, #1             // x22 = pattern_length

invalid2_try_pattern:
    lsr x0, x20, #1         // length / 2
    cmp x22, x0
    b.gt invalid2_false

    // Check if length is divisible by pattern_length
    udiv x23, x20, x22
    msub x24, x23, x22, x20
    cbnz x24, invalid2_next_pattern

    // Must repeat at least twice
    cmp x23, #2
    b.lt invalid2_next_pattern

    // Check if repeating the pattern gives original string
    // x22 = pattern_length, x23 = repeat_count
    mov x24, #0             // x24 = current position
    mov x25, #1             // x25 = match (assume true)

invalid2_check_repeat:
    cmp x24, x20
    b.ge invalid2_check_done

    // Compare str[pos] with pattern[pos % pattern_length]
    udiv x26, x24, x22
    msub x26, x26, x22, x24  // x26 = pos % pattern_length

    ldrb w0, [x21, x24]      // str[pos]
    ldrb w1, [x21, x26]      // pattern[pos % pattern_length]
    cmp w0, w1
    b.eq invalid2_next_char

    mov x25, #0              // mismatch
    b invalid2_check_done

invalid2_next_char:
    add x24, x24, #1
    b invalid2_check_repeat

invalid2_check_done:
    cbnz x25, invalid2_true

invalid2_next_pattern:
    add x22, x22, #1
    b invalid2_try_pattern

invalid2_true:
    mov x0, #1
    b invalid2_done

invalid2_false:
    mov x0, #0

invalid2_done:
    ldp x25, x26, [sp, #64]
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #80
    ret

// Convert number in x0 to string in num_str, returns length in x0
num_to_str:
    stp x29, x30, [sp, #-48]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]

    mov x19, x0             // x19 = number
    adrp x20, num_str@PAGE
    add x20, x20, num_str@PAGEOFF

    // Handle zero specially
    cbnz x19, num_to_str_nonzero
    mov w0, #'0'
    strb w0, [x20]
    strb wzr, [x20, #1]
    mov x0, #1
    b num_to_str_done

num_to_str_nonzero:
    // Find end position by counting digits
    mov x21, x19
    mov x22, #0             // digit count
1:  cbz x21, 2f
    mov x0, #10
    udiv x21, x21, x0
    add x22, x22, #1
    b 1b
2:
    // x22 = number of digits
    // Write digits from end to start
    mov x21, x22            // save length
    strb wzr, [x20, x22]    // null terminate
3:  cbz x19, 4f
    sub x22, x22, #1
    mov x0, #10
    udiv x23, x19, x0       // x23 = x19 / 10
    msub x0, x23, x0, x19   // x0 = x19 % 10
    add w0, w0, #'0'
    strb w0, [x20, x22]
    mov x19, x23
    b 3b
4:
    mov x0, x21             // return length

num_to_str_done:
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #48
    ret
