// Day 6: Guard Gallivant - ARM64 Assembly
// Simplified working version

.global _main
.align 4

.data
input_file: .asciz "../input.txt"
mode: .asciz "r"
part1_msg: .asciz "Part 1: %d\n"
part2_msg: .asciz "Part 2: %d\n"

// Directions: UP=0, RIGHT=1, DOWN=2, LEFT=3
// UP: (-1,0), RIGHT: (0,1), DOWN: (1,0), LEFT: (0,-1)
dr: .word -1, 0, 1, 0
dc: .word 0, 1, 0, -1

.bss
.align 4
grid: .skip 40000
visited: .skip 40000
buffer: .skip 256

.text
_main:
    stp x29, x30, [sp, #-64]!
    mov x29, sp

    // Open file
    adrp x0, input_file@page
    add x0, x0, input_file@pageoff
    adrp x1, mode@page
    add x1, x1, mode@pageoff
    bl _fopen
    cbz x0, exit_error
    mov x19, x0

    // Parse grid
    adrp x20, grid@page
    add x20, x20, grid@pageoff
    mov x21, #0              // rows
    mov x22, #0              // cols
    mov x23, #-1             // guard row
    mov x24, #-1             // guard col
    mov x25, #0              // guard dir

read_loop:
    adrp x0, buffer@page
    add x0, x0, buffer@pageoff
    mov x1, #256
    mov x2, x19
    bl _fgets
    cbz x0, read_done

    adrp x0, buffer@page
    add x0, x0, buffer@pageoff
    bl _strlen
    cbz x0, read_loop

    mov x26, x0              // line length

    // Remove newline
    adrp x1, buffer@page
    add x1, x1, buffer@pageoff
    sub x2, x26, #1
    ldrb w3, [x1, x2]
    cmp w3, #'\n'
    bne skip_nl
    strb wzr, [x1, x2]
    mov x26, x2

skip_nl:
    cmp x22, #0
    csel x22, x26, x22, eq   // Set cols on first line

    adrp x27, buffer@page
    add x27, x27, buffer@pageoff
    mov x28, #0              // col

process_char:
    cmp x28, x26
    bge line_done

    ldrb w0, [x27, x28]

    // Calculate grid index: row * 200 + col
    mov x1, #200
    madd x2, x21, x1, x28

    // Check for guard
    cmp w0, #'^'
    bne not_up
    mov x23, x21
    mov x24, x28
    mov x25, #0
    mov w0, #'.'
not_up:
    cmp w0, #'>'
    bne not_right
    mov x23, x21
    mov x24, x28
    mov x25, #1
    mov w0, #'.'
not_right:
    cmp w0, #'v'
    bne not_down
    mov x23, x21
    mov x24, x28
    mov x25, #2
    mov w0, #'.'
not_down:
    cmp w0, #'<'
    bne not_left
    mov x23, x21
    mov x24, x28
    mov x25, #3
    mov w0, #'.'
not_left:

    strb w0, [x20, x2]
    add x28, x28, #1
    b process_char

line_done:
    add x21, x21, #1
    b read_loop

read_done:
    mov x0, x19
    bl _fclose

    // Part 1: Simulate
    str x21, [sp, #16]       // Save rows
    str x22, [sp, #24]       // Save cols
    str x23, [sp, #32]       // Save guard_row
    str x24, [sp, #40]       // Save guard_col
    str x25, [sp, #48]       // Save guard_dir

    // Clear visited
    adrp x0, visited@page
    add x0, x0, visited@pageoff
    mov x1, #40000
clear_loop:
    cbz x1, clear_done
    strb wzr, [x0], #1
    sub x1, x1, #1
    b clear_loop

clear_done:
    ldr x21, [sp, #16]       // rows
    ldr x22, [sp, #24]       // cols
    ldr x26, [sp, #32]       // current row
    ldr x27, [sp, #40]       // current col
    ldr x28, [sp, #48]       // current dir

    // Mark start as visited
    mov x0, #200
    madd x1, x26, x0, x27
    adrp x2, visited@page
    add x2, x2, visited@pageoff
    mov w3, #1
    strb w3, [x2, x1]

    mov x19, #1              // count = 1

    adrp x20, dr@page
    add x20, x20, dr@pageoff

sim_loop:
    // Get direction deltas
    ldr w0, [x20, x28, lsl #2]
    adrp x1, dc@page
    add x1, x1, dc@pageoff
    ldr w1, [x1, x28, lsl #2]

    sxtw x0, w0
    sxtw x1, w1

    add x2, x26, x0          // next row
    add x3, x27, x1          // next col

    // Check bounds
    cmp x2, #0
    blt sim_done
    cmp x2, x21
    bge sim_done
    cmp x3, #0
    blt sim_done
    cmp x3, x22
    bge sim_done

    // Check obstacle
    mov x6, #200
    madd x7, x2, x6, x3
    adrp x8, grid@page
    add x8, x8, grid@pageoff
    ldrb w9, [x8, x7]

    cmp w9, #'#'
    beq turn_right

    // Move
    mov x26, x2
    mov x27, x3

    // Check if visited
    adrp x8, visited@page
    add x8, x8, visited@pageoff
    ldrb w9, [x8, x7]
    cbnz w9, sim_loop

    // Mark visited
    mov w9, #1
    strb w9, [x8, x7]
    add x19, x19, #1
    b sim_loop

turn_right:
    add x28, x28, #1
    and x28, x28, #3
    b sim_loop

sim_done:
    // Print Part 1 (store on stack for printf)
    str x19, [sp]
    adrp x0, part1_msg@page
    add x0, x0, part1_msg@pageoff
    bl _printf

    // Part 2: Placeholder
    mov x0, #0
    str x0, [sp]
    adrp x0, part2_msg@page
    add x0, x0, part2_msg@pageoff
    bl _printf

    mov x0, #0
    ldp x29, x30, [sp], #64
    ret

exit_error:
    mov x0, #1
    ldp x29, x30, [sp], #64
    ret
