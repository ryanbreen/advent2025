// ============================================================================
// Advent of Code 2023 - Day 6: Wait For It
// ARM64 Assembly Solution for macOS
// ============================================================================
//
// ALGORITHM:
// Use the quadratic formula to find the range of valid hold times.
// t * (T - t) > D  =>  -t^2 + T*t - D > 0
// Roots: t = (T +/- sqrt(T^2 - 4D)) / 2
// Count integers strictly between the roots.
//
// ============================================================================

.global _start
.align 4

// ============================================================================
// DATA SECTION
// ============================================================================
.data
    .align 4
    filename:   .asciz "../input.txt"
    msg_part1:  .asciz "Part 1: "
    msg_part2:  .asciz "Part 2: "
    newline:    .asciz "\n"

    .align 4
    buffer:     .space 4096
    num_buffer: .space 32

    .align 4
    times:      .space 64               // Up to 8 times (8 bytes each)
    distances:  .space 64               // Up to 8 distances (8 bytes each)
    race_count: .quad 0

    concat_time:     .quad 0
    concat_distance: .quad 0

// ============================================================================
// TEXT SECTION
// ============================================================================
.text

_start:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    bl      read_file
    cbz     x0, exit_error

    bl      parse_input

    adrp    x0, msg_part1@PAGE
    add     x0, x0, msg_part1@PAGEOFF
    bl      print_str

    bl      solve_part1
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    adrp    x0, msg_part2@PAGE
    add     x0, x0, msg_part2@PAGEOFF
    bl      print_str

    bl      solve_part2
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    mov     x0, #0
    ldp     x29, x30, [sp], #16
    mov     x16, #1
    svc     #0x80

exit_error:
    mov     x0, #1
    ldp     x29, x30, [sp], #16
    mov     x16, #1
    svc     #0x80

// ============================================================================
// FILE I/O
// ============================================================================
read_file:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    adrp    x0, filename@PAGE
    add     x0, x0, filename@PAGEOFF
    mov     x1, #0
    mov     x16, #5
    svc     #0x80
    cmp     x0, #0
    b.lt    read_error
    mov     x19, x0

    mov     x0, x19
    adrp    x1, buffer@PAGE
    add     x1, x1, buffer@PAGEOFF
    mov     x2, #4096
    mov     x16, #3
    svc     #0x80
    mov     x20, x0

    mov     x0, x19
    mov     x16, #6
    svc     #0x80

    mov     x0, x20
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

read_error:
    mov     x0, #0
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// PARSING
// ============================================================================
// parse_number: parse integer from string
// Input: x0 = string pointer
// Output: x0 = parsed number, x1 = updated pointer, x2 = digit count
parse_number:
    mov     x3, x0              // save start pointer
    mov     x0, #0              // result
    mov     x2, #0              // digit count
    // Skip leading spaces only (not newlines)
1:  ldrb    w4, [x3]
    cmp     w4, #' '
    b.ne    2f
    add     x3, x3, #1
    b       1b
2:  ldrb    w4, [x3]
    cmp     w4, #'0'
    b.lt    3f
    cmp     w4, #'9'
    b.gt    3f
    mov     x5, #10
    mul     x0, x0, x5
    sub     w4, w4, #'0'
    add     x0, x0, x4
    add     x3, x3, #1
    add     x2, x2, #1          // count digit
    b       2b
3:  mov     x1, x3
    ret

skip_to_char:
1:  ldrb    w2, [x0]
    cbz     w2, 2f
    cmp     w2, w1
    b.eq    2f
    add     x0, x0, #1
    b       1b
2:  add     x0, x0, #1
    ret

// Skip spaces only (NOT newlines or tabs)
skip_spaces:
1:  ldrb    w1, [x0]
    cmp     w1, #' '
    b.ne    2f
    add     x0, x0, #1
    b       1b
2:  ret

// Compute 10^n for n in x0, result in x0
power_of_10:
    mov     x1, x0              // n
    mov     x0, #1              // result
    mov     x2, #10
1:  cbz     x1, 2f
    mul     x0, x0, x2
    sub     x1, x1, #1
    b       1b
2:  ret

parse_input:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    adrp    x19, buffer@PAGE
    add     x19, x19, buffer@PAGEOFF

    adrp    x20, times@PAGE
    add     x20, x20, times@PAGEOFF

    adrp    x21, distances@PAGE
    add     x21, x21, distances@PAGEOFF

    mov     x22, #0                     // Race count
    mov     x23, #0                     // Concat time
    mov     x24, #0                     // Concat distance

    // Skip "Time:" header
    mov     x0, x19
    mov     w1, #':'
    bl      skip_to_char
    mov     x19, x0

parse_times_loop:
    // Skip spaces only (preserve newlines for line boundary detection)
    mov     x0, x19
    bl      skip_spaces
    mov     x19, x0

    ldrb    w0, [x19]
    cmp     w0, #'\n'
    b.eq    times_done
    cbz     w0, times_done
    cmp     w0, #'0'
    b.lt    times_done
    cmp     w0, #'9'
    b.gt    times_done

    mov     x0, x19
    bl      parse_number
    mov     x25, x0                     // parsed number
    mov     x19, x1                     // updated pointer
    mov     x26, x2                     // digit count

    str     x25, [x20, x22, lsl #3]

    // Concatenate: concat_time = concat_time * 10^digits + value
    mov     x0, x26
    bl      power_of_10
    mul     x23, x23, x0
    add     x23, x23, x25

    add     x22, x22, #1
    b       parse_times_loop

times_done:
    adrp    x0, race_count@PAGE
    add     x0, x0, race_count@PAGEOFF
    str     x22, [x0]

    adrp    x0, concat_time@PAGE
    add     x0, x0, concat_time@PAGEOFF
    str     x23, [x0]

    // Now x19 points at newline - skip to next line's ':'
    mov     x0, x19
    mov     w1, #':'
    bl      skip_to_char
    mov     x19, x0

    mov     x22, #0

parse_distances_loop:
    mov     x0, x19
    bl      skip_spaces
    mov     x19, x0

    ldrb    w0, [x19]
    cmp     w0, #'\n'
    b.eq    distances_done
    cbz     w0, distances_done
    cmp     w0, #'0'
    b.lt    distances_done
    cmp     w0, #'9'
    b.gt    distances_done

    mov     x0, x19
    bl      parse_number
    mov     x25, x0
    mov     x19, x1
    mov     x26, x2

    str     x25, [x21, x22, lsl #3]

    mov     x0, x26
    bl      power_of_10
    mul     x24, x24, x0
    add     x24, x24, x25

    add     x22, x22, #1
    b       parse_distances_loop

distances_done:
    adrp    x0, concat_distance@PAGE
    add     x0, x0, concat_distance@PAGEOFF
    str     x24, [x0]

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// QUADRATIC FORMULA
// ============================================================================
count_ways_to_win:
    stp     x29, x30, [sp, #-16]!
    stp     d8, d9, [sp, #-16]!

    ucvtf   d0, x0                      // T as double
    ucvtf   d1, x1                      // D as double

    // discriminant = T^2 - 4*D
    fmul    d2, d0, d0                  // T^2
    mov     x2, #4
    ucvtf   d3, x2
    fmul    d3, d3, d1                  // 4*D
    fsub    d2, d2, d3                  // discriminant

    fcmp    d2, #0.0
    b.le    no_ways

    fsqrt   d3, d2                      // sqrt_d

    // t_low = (T - sqrt_d) / 2
    fsub    d4, d0, d3
    mov     x2, #2
    ucvtf   d5, x2
    fdiv    d4, d4, d5

    // t_high = (T + sqrt_d) / 2
    fadd    d6, d0, d3
    fdiv    d6, d6, d5

    // first = floor(t_low) + 1
    frintm  d7, d4                      // floor(t_low)
    fcvtzs  x3, d7
    add     x3, x3, #1

    // last = ceil(t_high) - 1
    frintp  d8, d6                      // ceil(t_high)
    fcvtzs  x4, d8
    sub     x4, x4, #1

    cmp     x4, x3
    b.lt    no_ways

    sub     x0, x4, x3
    add     x0, x0, #1

    ldp     d8, d9, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

no_ways:
    mov     x0, #0
    ldp     d8, d9, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// SOLVE PARTS
// ============================================================================
solve_part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    adrp    x19, times@PAGE
    add     x19, x19, times@PAGEOFF

    adrp    x20, distances@PAGE
    add     x20, x20, distances@PAGEOFF

    adrp    x0, race_count@PAGE
    add     x0, x0, race_count@PAGEOFF
    ldr     x21, [x0]

    mov     x22, #0
    mov     x23, #1

part1_loop:
    cmp     x22, x21
    b.ge    part1_done

    ldr     x0, [x19, x22, lsl #3]
    ldr     x1, [x20, x22, lsl #3]

    bl      count_ways_to_win

    mul     x23, x23, x0

    add     x22, x22, #1
    b       part1_loop

part1_done:
    mov     x0, x23

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

solve_part2:
    stp     x29, x30, [sp, #-16]!

    adrp    x0, concat_time@PAGE
    add     x0, x0, concat_time@PAGEOFF
    ldr     x0, [x0]

    adrp    x1, concat_distance@PAGE
    add     x1, x1, concat_distance@PAGEOFF
    ldr     x1, [x1]

    bl      count_ways_to_win

    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// OUTPUT
// ============================================================================
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    mov     x19, x0

    mov     x1, #0
1:  ldrb    w2, [x19, x1]
    cbz     w2, 2f
    add     x1, x1, #1
    b       1b

2:  mov     x2, x1
    mov     x1, x19
    mov     x0, #1
    mov     x16, #4
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0
    adrp    x20, num_buffer@PAGE
    add     x20, x20, num_buffer@PAGEOFF
    add     x20, x20, #30
    strb    wzr, [x20]

    cbz     x19, print_zero

    mov     x2, #10
1:  sub     x20, x20, #1
    udiv    x3, x19, x2
    msub    x4, x3, x2, x19
    add     w4, w4, #'0'
    strb    w4, [x20]
    mov     x19, x3
    cbnz    x19, 1b
    b       print_done

print_zero:
    sub     x20, x20, #1
    mov     w4, #'0'
    strb    w4, [x20]

print_done:
    mov     x0, x20
    bl      print_str

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
