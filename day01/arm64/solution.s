// Day 1: Secret Entrance - ARM64 Assembly for macOS
// On ARM64 macOS, printf varargs are passed on the stack, not in registers!
// Assemble: as -o solution.o solution.s && ld -o solution solution.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _main
// Run: ./solution

.global _main
.align 4

// Constants
.equ DIAL_SIZE, 100
.equ START_POS, 50
.equ O_RDONLY, 0
.equ BUFFER_SIZE, 131072

.section __DATA,__data
.align 3
input_path: .asciz "../input.txt"
part1_fmt: .asciz "Part 1: %lld\n"
part2_fmt: .asciz "Part 2: %lld\n"

.section __DATA,__bss
.align 3
buffer: .space BUFFER_SIZE

.section __TEXT,__text

// Main function
_main:
    // Prologue - allocate stack frame (needs to be 16-byte aligned)
    sub sp, sp, #96
    stp x29, x30, [sp, #80]
    add x29, sp, #80
    stp x19, x20, [sp, #64]
    stp x21, x22, [sp, #48]
    stp x23, x24, [sp, #32]
    stp x25, x26, [sp, #16]

    // Open file
    adrp x0, input_path@PAGE
    add x0, x0, input_path@PAGEOFF
    mov w1, #O_RDONLY
    bl _open
    sxtw x19, w0            // x19 = fd (sign-extended)

    // Check for error
    cmp x19, #0
    b.lt exit_error

    // Get buffer address into x20
    adrp x20, buffer@PAGE
    add x20, x20, buffer@PAGEOFF

    // Read file into buffer
    mov x0, x19             // fd
    mov x1, x20             // buffer
    mov x2, #BUFFER_SIZE
    bl _read
    mov x21, x0             // x21 = bytes read

    // Close file
    mov x0, x19
    bl _close

    // Check if read failed
    cmp x21, #0
    b.le exit_error

    // Null-terminate the buffer
    strb wzr, [x20, x21]

    // ===== Part 1 =====
    mov x22, #START_POS     // x22 = position
    mov x23, #0             // x23 = zero_count
    mov x24, x20            // x24 = buffer pointer

part1_loop:
    ldrb w0, [x24]
    cbz w0, part1_done

    // Skip whitespace
    cmp w0, #' '
    b.eq part1_skip
    cmp w0, #'\n'
    b.eq part1_skip
    cmp w0, #'\r'
    b.eq part1_skip

    cmp w0, #'L'
    b.eq part1_left
    cmp w0, #'R'
    b.eq part1_right

part1_skip:
    add x24, x24, #1
    b part1_loop

part1_left:
    add x24, x24, #1
    // Parse number inline
    mov x0, #0              // result
1:  ldrb w1, [x24]
    sub w2, w1, #'0'
    cmp w2, #9
    b.hi 2f
    mov x3, #10
    mul x0, x0, x3
    and x2, x2, #0xFF
    add x0, x0, x2
    add x24, x24, #1
    b 1b
2:
    // position = (position - distance) % 100
    sub x22, x22, x0
    mov x1, #DIAL_SIZE
    sdiv x2, x22, x1
    msub x22, x2, x1, x22
    tbz x22, #63, 3f
    add x22, x22, x1
3:
    cbnz x22, part1_loop
    add x23, x23, #1
    b part1_loop

part1_right:
    add x24, x24, #1
    // Parse number inline
    mov x0, #0
1:  ldrb w1, [x24]
    sub w2, w1, #'0'
    cmp w2, #9
    b.hi 2f
    mov x3, #10
    mul x0, x0, x3
    and x2, x2, #0xFF
    add x0, x0, x2
    add x24, x24, #1
    b 1b
2:
    // position = (position + distance) % 100
    add x22, x22, x0
    mov x1, #DIAL_SIZE
    udiv x2, x22, x1
    msub x22, x2, x1, x22
    cbnz x22, part1_loop
    add x23, x23, #1
    b part1_loop

part1_done:
    // Print Part 1 - varargs on stack for ARM64 macOS
    str x23, [sp]           // Put vararg on stack
    adrp x0, part1_fmt@PAGE
    add x0, x0, part1_fmt@PAGEOFF
    bl _printf

    // ===== Part 2 =====
    mov x22, #START_POS     // position
    mov x23, #0             // zero_count
    mov x24, x20            // buffer pointer

part2_loop:
    ldrb w0, [x24]
    cbz w0, part2_done

    cmp w0, #' '
    b.eq part2_skip
    cmp w0, #'\n'
    b.eq part2_skip
    cmp w0, #'\r'
    b.eq part2_skip

    cmp w0, #'L'
    b.eq part2_left
    cmp w0, #'R'
    b.eq part2_right

part2_skip:
    add x24, x24, #1
    b part2_loop

part2_left:
    add x24, x24, #1
    // Parse number inline into x25
    mov x25, #0
1:  ldrb w1, [x24]
    sub w2, w1, #'0'
    cmp w2, #9
    b.hi 2f
    mov x3, #10
    mul x25, x25, x3
    and x2, x2, #0xFF
    add x25, x25, x2
    add x24, x24, #1
    b 1b
2:
    // Count zeros: if pos > 0 && dist >= pos: 1 + (dist - pos) / 100
    cbz x22, part2_left_zero_pos
    cmp x25, x22
    b.lo part2_left_update
    sub x0, x25, x22
    mov x1, #DIAL_SIZE
    udiv x0, x0, x1
    add x0, x0, #1
    add x23, x23, x0
    b part2_left_update

part2_left_zero_pos:
    cmp x25, #DIAL_SIZE
    b.lo part2_left_update
    mov x0, x25
    mov x1, #DIAL_SIZE
    udiv x0, x0, x1
    add x23, x23, x0

part2_left_update:
    sub x22, x22, x25
    mov x1, #DIAL_SIZE
    sdiv x2, x22, x1
    msub x22, x2, x1, x22
    tbz x22, #63, part2_loop
    add x22, x22, x1
    b part2_loop

part2_right:
    add x24, x24, #1
    // Parse number inline into x25
    mov x25, #0
1:  ldrb w1, [x24]
    sub w2, w1, #'0'
    cmp w2, #9
    b.hi 2f
    mov x3, #10
    mul x25, x25, x3
    and x2, x2, #0xFF
    add x25, x25, x2
    add x24, x24, #1
    b 1b
2:
    // Count zeros
    cbz x22, part2_right_zero_pos
    mov x0, #DIAL_SIZE
    sub x0, x0, x22         // steps_to_zero = 100 - position
    cmp x25, x0
    b.lo part2_right_update
    sub x1, x25, x0
    mov x0, #DIAL_SIZE
    udiv x1, x1, x0
    add x1, x1, #1
    add x23, x23, x1
    b part2_right_update

part2_right_zero_pos:
    cmp x25, #DIAL_SIZE
    b.lo part2_right_update
    mov x0, x25
    mov x1, #DIAL_SIZE
    udiv x0, x0, x1
    add x23, x23, x0

part2_right_update:
    add x22, x22, x25
    mov x1, #DIAL_SIZE
    udiv x2, x22, x1
    msub x22, x2, x1, x22
    b part2_loop

part2_done:
    // Print Part 2 - varargs on stack
    str x23, [sp]
    adrp x0, part2_fmt@PAGE
    add x0, x0, part2_fmt@PAGEOFF
    bl _printf

    mov x0, #0
    b exit_program

exit_error:
    mov x0, #1

exit_program:
    // Epilogue - restore callee-saved registers
    ldp x25, x26, [sp, #16]
    ldp x23, x24, [sp, #32]
    ldp x21, x22, [sp, #48]
    ldp x19, x20, [sp, #64]
    ldp x29, x30, [sp, #80]
    add sp, sp, #96
    bl _exit
