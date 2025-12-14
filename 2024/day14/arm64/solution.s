.global _main
.align 4

.equ WIDTH, 101
.equ HEIGHT, 103
.equ MAX_ROBOTS, 512

.data
filename:   .asciz "../input.txt"
read_mode:  .asciz "r"
format_p1:  .asciz "Part 1: %lld\n"
format_p2:  .asciz "Part 2: %lld\n"

.bss
.align 8
file_buffer:    .skip 65536
robots:         .skip MAX_ROBOTS * 16
positions:      .skip MAX_ROBOTS * 8
grid:           .skip WIDTH * HEIGHT
num_robots:     .skip 8

.text

_main:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    bl      read_file
    bl      parse_robots
    bl      part1
    bl      part2

    mov     w0, #0
    ldp     x29, x30, [sp], #16
    ret

// Read file into buffer
read_file:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]

    adrp    x0, filename@PAGE
    add     x0, x0, filename@PAGEOFF
    adrp    x1, read_mode@PAGE
    add     x1, x1, read_mode@PAGEOFF
    bl      _fopen
    mov     x19, x0

    adrp    x0, file_buffer@PAGE
    add     x0, x0, file_buffer@PAGEOFF
    mov     x20, x0
    mov     x1, #1
    mov     x2, #65536
    mov     x3, x19
    bl      _fread

    add     x1, x20, x0
    strb    wzr, [x1]

    mov     x0, x19
    bl      _fclose

    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// Parse robots from file buffer
parse_robots:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    adrp    x19, file_buffer@PAGE
    add     x19, x19, file_buffer@PAGEOFF
    adrp    x20, robots@PAGE
    add     x20, x20, robots@PAGEOFF

    mov     x21, #0

parse_loop:
    cmp     x21, #MAX_ROBOTS
    b.ge    parse_done

    ldrb    w0, [x19]
    cbz     w0, parse_done

    mov     x0, x19
    bl      skip_to_number
    mov     x19, x0

    // Check if skip_to_number hit null (end of input)
    ldrb    w0, [x19]
    cbz     w0, parse_done

    mov     x0, x19
    bl      parse_signed_int
    mov     x22, x1
    mov     x19, x0

    add     x19, x19, #1

    mov     x0, x19
    bl      parse_signed_int
    mov     x23, x1
    mov     x19, x0

    mov     x0, x19
    bl      skip_to_number
    mov     x19, x0

    mov     x0, x19
    bl      parse_signed_int
    mov     x24, x1
    mov     x19, x0

    add     x19, x19, #1

    mov     x0, x19
    bl      parse_signed_int
    str     w22, [x20]
    str     w23, [x20, #4]
    str     w24, [x20, #8]
    str     w1, [x20, #12]
    mov     x19, x0

    add     x20, x20, #16
    add     x21, x21, #1
    b       parse_loop

parse_done:
    adrp    x0, num_robots@PAGE
    add     x0, x0, num_robots@PAGEOFF
    str     x21, [x0]

    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

skip_to_number:
    ldrb    w1, [x0]
    cbz     w1, 1f
    cmp     w1, #'-'
    b.eq    1f
    cmp     w1, #'0'
    b.lt    2f
    cmp     w1, #'9'
    b.le    1f
2:  add     x0, x0, #1
    b       skip_to_number
1:  ret

parse_signed_int:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    str     x19, [sp, #16]

    mov     x19, #0
    mov     w2, #1

    ldrb    w1, [x0]
    cmp     w1, #'-'
    b.ne    1f
    mov     w2, #-1
    add     x0, x0, #1

1:  ldrb    w1, [x0]
    cmp     w1, #'0'
    b.lt    2f
    cmp     w1, #'9'
    b.gt    2f

    sub     w1, w1, #'0'
    mov     x3, #10
    mul     x19, x19, x3
    add     x19, x19, x1
    add     x0, x0, #1
    b       1b

2:  sxtw    x2, w2
    mul     x1, x19, x2
    ldr     x19, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// Part 1: Safety factor after 100 seconds
part1:
    sub     sp, sp, #96
    stp     x29, x30, [sp, #80]
    add     x29, sp, #80
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]

    mov     x0, #100
    bl      simulate

    mov     x19, #0
    mov     x20, #0
    mov     x21, #0
    mov     x22, #0

    mov     w23, #WIDTH / 2
    mov     w24, #HEIGHT / 2

    adrp    x0, positions@PAGE
    add     x0, x0, positions@PAGEOFF
    adrp    x1, num_robots@PAGE
    add     x1, x1, num_robots@PAGEOFF
    ldr     x1, [x1]
    mov     x2, #0

count_loop:
    cmp     x2, x1
    b.ge    count_done

    ldr     w3, [x0]
    ldr     w4, [x0, #4]

    cmp     w3, w23
    b.eq    count_next
    cmp     w4, w24
    b.eq    count_next

    cmp     w3, w23
    b.ge    check_right

    cmp     w4, w24
    b.lt    inc_q1
    add     x21, x21, #1
    b       count_next
inc_q1:
    add     x19, x19, #1
    b       count_next

check_right:
    cmp     w4, w24
    b.lt    inc_q2
    add     x22, x22, #1
    b       count_next
inc_q2:
    add     x20, x20, #1

count_next:
    add     x0, x0, #8
    add     x2, x2, #1
    b       count_loop

count_done:
    mul     x25, x19, x20
    mul     x26, x21, x22
    mul     x25, x25, x26

    // Print result - variadic arg goes on stack at [sp]
    str     x25, [sp]
    adrp    x0, format_p1@PAGE
    add     x0, x0, format_p1@PAGEOFF
    bl      _printf

    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp, #80]
    add     sp, sp, #96
    ret

// Part 2: Find Christmas tree pattern
part2:
    sub     sp, sp, #80
    stp     x29, x30, [sp, #64]
    add     x29, sp, #64
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    mov     x19, #1
    mov     w20, #WIDTH * HEIGHT

time_loop:
    cmp     x19, x20
    b.gt    not_found

    mov     x0, x19
    bl      simulate

    bl      build_grid

    mov     w21, #0

check_y_loop:
    cmp     w21, #HEIGHT
    b.ge    time_next

    mov     w22, #0
    mov     w23, #0
    mov     w24, #0

check_x_loop:
    cmp     w22, #WIDTH
    b.ge    check_y_next

    mov     w0, w22
    mov     w1, w21
    bl      has_robot
    cbnz    w0, has_robot_here

    mov     w24, #0
    b       check_x_next

has_robot_here:
    add     w24, w24, #1
    cmp     w24, w23
    csel    w23, w24, w23, gt

check_x_next:
    add     w22, w22, #1
    b       check_x_loop

check_y_next:
    cmp     w23, #20
    b.ge    found_tree
    add     w21, w21, #1
    b       check_y_loop

time_next:
    add     x19, x19, #1
    b       time_loop

found_tree:
    str     x19, [sp]
    adrp    x0, format_p2@PAGE
    add     x0, x0, format_p2@PAGEOFF
    bl      _printf

    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp, #64]
    add     sp, sp, #80
    ret

not_found:
    mov     x8, #-1
    str     x8, [sp]
    adrp    x0, format_p2@PAGE
    add     x0, x0, format_p2@PAGEOFF
    bl      _printf

    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp, #64]
    add     sp, sp, #80
    ret

// Simulate robots for given seconds
simulate:
    sub     sp, sp, #96
    stp     x29, x30, [sp, #80]
    add     x29, sp, #80
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]

    mov     x19, x0

    adrp    x20, robots@PAGE
    add     x20, x20, robots@PAGEOFF
    adrp    x21, positions@PAGE
    add     x21, x21, positions@PAGEOFF
    adrp    x0, num_robots@PAGE
    add     x0, x0, num_robots@PAGEOFF
    ldr     x22, [x0]
    mov     x23, #0

sim_loop:
    cmp     x23, x22
    b.ge    sim_done

    ldrsw   x24, [x20]
    ldrsw   x25, [x20, #4]
    ldrsw   x26, [x20, #8]

    mul     x0, x26, x19
    add     x0, x24, x0
    bl      mod_width
    str     w0, [x21]

    ldrsw   x0, [x20, #12]
    mul     x0, x0, x19
    add     x0, x25, x0
    bl      mod_height
    str     w0, [x21, #4]

    add     x20, x20, #16
    add     x21, x21, #8
    add     x23, x23, #1
    b       sim_loop

sim_done:
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp, #80]
    add     sp, sp, #96
    ret

mod_width:
    mov     x1, #WIDTH
    sdiv    x2, x0, x1
    msub    x0, x2, x1, x0
    cmp     x0, #0
    b.ge    1f
    add     x0, x0, x1
1:  ret

mod_height:
    mov     x1, #HEIGHT
    sdiv    x2, x0, x1
    msub    x0, x2, x1, x0
    cmp     x0, #0
    b.ge    1f
    add     x0, x0, x1
1:  ret

build_grid:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    str     x21, [sp, #32]

    adrp    x0, grid@PAGE
    add     x0, x0, grid@PAGEOFF
    mov     w1, #WIDTH * HEIGHT
    mov     x2, #0

clear_loop:
    cbz     w1, clear_done
    strb    wzr, [x0], #1
    sub     w1, w1, #1
    b       clear_loop

clear_done:
    adrp    x19, positions@PAGE
    add     x19, x19, positions@PAGEOFF
    adrp    x0, num_robots@PAGE
    add     x0, x0, num_robots@PAGEOFF
    ldr     x20, [x0]
    mov     x21, #0

mark_loop:
    cmp     x21, x20
    b.ge    mark_done

    ldr     w0, [x19]
    ldr     w1, [x19, #4]

    mov     w2, #WIDTH
    mul     w2, w1, w2
    add     w2, w2, w0

    adrp    x3, grid@PAGE
    add     x3, x3, grid@PAGEOFF
    sxtw    x2, w2
    add     x3, x3, x2
    mov     w4, #1
    strb    w4, [x3]

    add     x19, x19, #8
    add     x21, x21, #1
    b       mark_loop

mark_done:
    ldr     x21, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret

has_robot:
    mov     w2, #WIDTH
    mul     w2, w1, w2
    add     w2, w2, w0

    adrp    x3, grid@PAGE
    add     x3, x3, grid@PAGEOFF
    sxtw    x2, w2
    add     x3, x3, x2
    ldrb    w0, [x3]
    ret
