// Day 24: Crossed Wires - ARM64 Assembly for macOS
// Circuit simulation with wire swapping detection

.global _main
.align 4

// macOS BSD syscall base and numbers
.equ BSD_BASE,   0x2000000
.equ SYS_EXIT,   1
.equ SYS_READ,   3
.equ SYS_WRITE,  4
.equ SYS_OPEN,   5
.equ SYS_CLOSE,  6

.equ O_RDONLY,   0x0000
.equ MAX_WIRES,  512
.equ MAX_GATES,  400
.equ WIRE_SIZE,  16      // name[4] + value[4] + padding[8]
.equ GATE_SIZE,  20      // in1[4] + op[4] + in2[4] + out[4] + flag[4]

.data
.align 4
input_path:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"

.bss
.align 8
file_buffer:    .space 40000
wires:          .space MAX_WIRES * WIRE_SIZE
gates:          .space MAX_GATES * GATE_SIZE
wire_count:     .space 8
gate_count:     .space 8
max_z_bit:      .space 8
swapped:        .space 64          // 8 wire names * 4 bytes + padding
swap_count:     .space 8
output_buffer:  .space 128
num_buffer:     .space 32

.text

_main:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]

    // Open input file
    adrp    x0, input_path@PAGE
    add     x0, x0, input_path@PAGEOFF
    mov     x1, #O_RDONLY
    mov     x16, #SYS_OPEN
    movk    x16, #0x200, lsl #16
    svc     #0x80
    cmp     x0, #0
    b.lt    Lexit_error
    mov     x19, x0             // save fd

    // Read file
    mov     x0, x19
    adrp    x1, file_buffer@PAGE
    add     x1, x1, file_buffer@PAGEOFF
    mov     x2, #40000
    mov     x16, #SYS_READ
    movk    x16, #0x200, lsl #16
    svc     #0x80
    mov     x20, x0             // save bytes read

    // Close file
    mov     x0, x19
    mov     x16, #SYS_CLOSE
    movk    x16, #0x200, lsl #16
    svc     #0x80

    // Null terminate buffer
    adrp    x0, file_buffer@PAGE
    add     x0, x0, file_buffer@PAGEOFF
    strb    wzr, [x0, x20]

    // Initialize counts
    adrp    x0, wire_count@PAGE
    add     x0, x0, wire_count@PAGEOFF
    str     xzr, [x0]
    adrp    x0, gate_count@PAGE
    add     x0, x0, gate_count@PAGEOFF
    str     xzr, [x0]
    adrp    x0, max_z_bit@PAGE
    add     x0, x0, max_z_bit@PAGEOFF
    str     xzr, [x0]
    adrp    x0, swap_count@PAGE
    add     x0, x0, swap_count@PAGEOFF
    str     xzr, [x0]

    // Parse input
    bl      parse_input

    // Simulate circuit
    bl      simulate
    mov     x21, x0             // save Part 1 result

    // Print Part 1
    adrp    x1, part1_msg@PAGE
    add     x1, x1, part1_msg@PAGEOFF
    mov     x2, #8
    bl      print_string
    mov     x0, x21
    bl      print_number
    adrp    x1, newline@PAGE
    add     x1, x1, newline@PAGEOFF
    mov     x2, #1
    bl      print_string

    // Part 2
    bl      find_swapped
    bl      sort_swapped
    bl      format_output

    // Print Part 2
    adrp    x1, part2_msg@PAGE
    add     x1, x1, part2_msg@PAGEOFF
    mov     x2, #8
    bl      print_string
    adrp    x1, output_buffer@PAGE
    add     x1, x1, output_buffer@PAGEOFF
    bl      print_cstring
    adrp    x1, newline@PAGE
    add     x1, x1, newline@PAGEOFF
    mov     x2, #1
    bl      print_string

    // Exit success
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    mov     x0, #0
    mov     x16, #SYS_EXIT
    movk    x16, #0x200, lsl #16
    svc     #0x80

Lexit_error:
    mov     x0, #1
    mov     x16, #SYS_EXIT
    movk    x16, #0x200, lsl #16
    svc     #0x80

// Print x0 as decimal number
print_number:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    str     x19, [sp, #16]

    mov     x19, x0
    adrp    x1, num_buffer@PAGE
    add     x1, x1, num_buffer@PAGEOFF
    add     x1, x1, #30
    strb    wzr, [x1]
    sub     x1, x1, #1
    mov     x2, #10

Lnum_loop:
    udiv    x3, x19, x2
    msub    x4, x3, x2, x19
    add     w4, w4, #'0'
    strb    w4, [x1]
    sub     x1, x1, #1
    mov     x19, x3
    cbnz    x19, Lnum_loop

    add     x1, x1, #1
    adrp    x2, num_buffer@PAGE
    add     x2, x2, num_buffer@PAGEOFF
    add     x2, x2, #30
    sub     x2, x2, x1
    bl      print_string

    ldr     x19, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// Print string at x1 with length x2
print_string:
    mov     x0, #1
    mov     x16, #SYS_WRITE
    movk    x16, #0x200, lsl #16
    svc     #0x80
    ret

// Print C string at x1
print_cstring:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    mov     x19, x1
    mov     x2, #0
Lcstr_loop:
    ldrb    w0, [x19, x2]
    cbz     w0, Lcstr_done
    add     x2, x2, #1
    b       Lcstr_loop
Lcstr_done:
    mov     x1, x19
    bl      print_string
    ldp     x29, x30, [sp], #16
    ret

// Find or create wire by name (x0 = pointer to 3-char name)
// Returns wire index in x0
find_wire:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]

    mov     x19, x0                     // save name ptr
    adrp    x20, wire_count@PAGE
    add     x20, x20, wire_count@PAGEOFF
    ldr     x21, [x20]
    adrp    x22, wires@PAGE
    add     x22, x22, wires@PAGEOFF
    mov     x0, #0

Lfw_loop:
    cmp     x0, x21
    b.ge    Lfw_create
    mov     x1, #WIRE_SIZE
    mul     x1, x0, x1
    add     x1, x22, x1

    ldrb    w2, [x19]
    ldrb    w3, [x1]
    cmp     w2, w3
    b.ne    Lfw_next
    ldrb    w2, [x19, #1]
    ldrb    w3, [x1, #1]
    cmp     w2, w3
    b.ne    Lfw_next
    ldrb    w2, [x19, #2]
    ldrb    w3, [x1, #2]
    cmp     w2, w3
    b.eq    Lfw_found

Lfw_next:
    add     x0, x0, #1
    b       Lfw_loop

Lfw_found:
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret

Lfw_create:
    mov     x0, #WIRE_SIZE
    mul     x0, x21, x0
    add     x0, x22, x0

    ldrb    w1, [x19]
    strb    w1, [x0]
    ldrb    w1, [x19, #1]
    strb    w1, [x0, #1]
    ldrb    w1, [x19, #2]
    strb    w1, [x0, #2]
    strb    wzr, [x0, #3]
    mov     w1, #-1
    str     w1, [x0, #4]

    mov     x0, x21
    add     x21, x21, #1
    str     x21, [x20]

    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret

// Parse input file
parse_input:
    stp     x29, x30, [sp, #-80]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]

    adrp    x19, file_buffer@PAGE
    add     x19, x19, file_buffer@PAGEOFF

    // Parse initial wire values
    // Input format: lines of "xyz: V" followed by blank line, then gates
Lpi_init:
    ldrb    w0, [x19]
    cbz     w0, Lpi_done
    cmp     w0, #'\n'
    b.eq    Lpi_gates           // blank line = separator, go to gates
    b       Lpi_init_line       // otherwise parse wire line

Lpi_init_line:
    mov     x0, x19
    bl      find_wire
    mov     x20, x0
    add     x19, x19, #5
    ldrb    w0, [x19]
    sub     w0, w0, #'0'

    adrp    x1, wires@PAGE
    add     x1, x1, wires@PAGEOFF
    mov     x2, #WIRE_SIZE
    mul     x2, x20, x2
    add     x1, x1, x2
    str     w0, [x1, #4]

Lpi_skip1:
    ldrb    w0, [x19]
    add     x19, x19, #1
    cmp     w0, #'\n'
    b.ne    Lpi_skip1
    b       Lpi_init

Lpi_gates:
    add     x19, x19, #1
    adrp    x21, gate_count@PAGE
    add     x21, x21, gate_count@PAGEOFF
    adrp    x22, gates@PAGE
    add     x22, x22, gates@PAGEOFF
    adrp    x24, max_z_bit@PAGE
    add     x24, x24, max_z_bit@PAGEOFF
    mov     x23, #0

Lpi_gate:
    ldrb    w0, [x19]
    cbz     w0, Lpi_gates_done
    cmp     w0, #'\n'
    b.eq    Lpi_skipnl

    // Parse: in1 OP in2 -> out
    mov     x0, x19
    bl      find_wire
    mov     x20, x0             // in1
    add     x19, x19, #4

    ldrb    w0, [x19]
    cmp     w0, #'A'
    b.eq    Lpi_and
    cmp     w0, #'O'
    b.eq    Lpi_or
    mov     w1, #2              // XOR
    add     x19, x19, #4
    b       Lpi_in2
Lpi_and:
    mov     w1, #0
    add     x19, x19, #4
    b       Lpi_in2
Lpi_or:
    mov     w1, #1
    add     x19, x19, #3

Lpi_in2:
    // Save op in a callee-saved register temporarily
    mov     w25, w1             // op in w25
    mov     x0, x19
    bl      find_wire
    mov     x26, x0             // in2 index in x26
    add     x19, x19, #7        // "def -> " -> skip to output name

    mov     x0, x19
    bl      find_wire
    mov     x3, x0              // out index
    mov     x2, x26             // in2 index
    mov     w1, w25             // op

    // Check for z wire
    ldrb    w4, [x19]
    cmp     w4, #'z'
    b.ne    Lpi_store
    ldrb    w4, [x19, #1]
    sub     w4, w4, #'0'
    mov     w5, #10
    mul     w4, w4, w5
    ldrb    w5, [x19, #2]
    sub     w5, w5, #'0'
    add     w4, w4, w5
    ldr     x5, [x24]
    cmp     x4, x5
    b.le    Lpi_store
    str     x4, [x24]

Lpi_store:
    mov     x4, #GATE_SIZE
    mul     x4, x23, x4
    add     x4, x22, x4
    str     w20, [x4]           // in1
    str     w1, [x4, #4]        // op
    str     w2, [x4, #8]        // in2
    str     w3, [x4, #12]       // out
    add     x23, x23, #1
    add     x19, x19, #3

Lpi_skip2:
    ldrb    w0, [x19]
    cbz     w0, Lpi_gates_done
    add     x19, x19, #1
    cmp     w0, #'\n'
    b.ne    Lpi_skip2
    b       Lpi_gate

Lpi_skipnl:
    add     x19, x19, #1
    b       Lpi_gate

Lpi_gates_done:
    str     x23, [x21]

Lpi_done:
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #80
    ret

// Simulate circuit, return z value in x0
simulate:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    adrp    x19, gates@PAGE
    add     x19, x19, gates@PAGEOFF
    adrp    x20, gate_count@PAGE
    add     x20, x20, gate_count@PAGEOFF
    ldr     x20, [x20]
    adrp    x21, wires@PAGE
    add     x21, x21, wires@PAGEOFF

Lsim_loop:
    mov     x22, #0             // progress
    mov     x23, #0             // gate index

Lsim_gate:
    cmp     x23, x20
    b.ge    Lsim_check

    mov     x0, #GATE_SIZE
    mul     x0, x23, x0
    add     x24, x19, x0

    ldr     w0, [x24]           // in1 idx
    ldr     w1, [x24, #4]       // op
    ldr     w2, [x24, #8]       // in2 idx
    ldr     w3, [x24, #12]      // out idx

    mov     x4, #WIRE_SIZE
    mul     x4, x0, x4
    add     x4, x21, x4
    ldr     w4, [x4, #4]        // in1 val
    cmp     w4, #0
    b.lt    Lsim_next

    mov     x5, #WIRE_SIZE
    mul     x5, x2, x5
    add     x5, x21, x5
    ldr     w5, [x5, #4]        // in2 val
    cmp     w5, #0
    b.lt    Lsim_next

    mov     x6, #WIRE_SIZE
    mul     x6, x3, x6
    add     x6, x21, x6
    ldr     w7, [x6, #4]        // out val
    cmp     w7, #0
    b.ge    Lsim_next

    cmp     w1, #0
    b.eq    Lsim_and
    cmp     w1, #1
    b.eq    Lsim_or
    eor     w7, w4, w5
    b       Lsim_store
Lsim_and:
    and     w7, w4, w5
    b       Lsim_store
Lsim_or:
    orr     w7, w4, w5
Lsim_store:
    str     w7, [x6, #4]
    mov     x22, #1

Lsim_next:
    add     x23, x23, #1
    b       Lsim_gate

Lsim_check:
    cbnz    x22, Lsim_loop

    // Collect z bits
    adrp    x0, wire_count@PAGE
    add     x0, x0, wire_count@PAGEOFF
    ldr     x0, [x0]
    mov     x1, #0              // result
    mov     x2, #0              // idx

Lcollect:
    cmp     x2, x0
    b.ge    Lsim_done

    mov     x3, #WIRE_SIZE
    mul     x3, x2, x3
    add     x3, x21, x3

    ldrb    w4, [x3]
    cmp     w4, #'z'
    b.ne    Lcollect_next

    ldrb    w4, [x3, #1]
    sub     w4, w4, #'0'
    mov     w5, #10
    mul     w4, w4, w5
    ldrb    w5, [x3, #2]
    sub     w5, w5, #'0'
    add     w4, w4, w5

    ldr     w5, [x3, #4]
    cmp     w5, #0
    b.le    Lcollect_next

    mov     x6, #1
    lsl     x6, x6, x4
    orr     x1, x1, x6

Lcollect_next:
    add     x2, x2, #1
    b       Lcollect

Lsim_done:
    mov     x0, x1
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// Check if wire starts with char: x0 = wire idx, w1 = char
// Returns 1 if match, 0 otherwise in w0
wire_starts_with:
    adrp    x2, wires@PAGE
    add     x2, x2, wires@PAGEOFF
    mov     x3, #WIRE_SIZE
    mul     x3, x0, x3
    add     x2, x2, x3
    ldrb    w0, [x2]
    cmp     w0, w1
    cset    w0, eq
    ret

// Check if wire is x00 or y00
is_bit_zero:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    adrp    x1, wires@PAGE
    add     x1, x1, wires@PAGEOFF
    mov     x2, #WIRE_SIZE
    mul     x2, x0, x2
    add     x1, x1, x2

    ldrb    w2, [x1]
    cmp     w2, #'x'
    b.eq    Libz_check
    cmp     w2, #'y'
    b.ne    Libz_no

Libz_check:
    ldrb    w2, [x1, #1]
    cmp     w2, #'0'
    b.ne    Libz_no
    ldrb    w2, [x1, #2]
    cmp     w2, #'0'
    b.ne    Libz_no
    mov     w0, #1
    ldp     x29, x30, [sp], #16
    ret

Libz_no:
    mov     w0, #0
    ldp     x29, x30, [sp], #16
    ret

// Add wire to swapped list
add_swapped:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]

    mov     x19, x0

    adrp    x20, wires@PAGE
    add     x20, x20, wires@PAGEOFF
    mov     x1, #WIRE_SIZE
    mul     x1, x19, x1
    add     x20, x20, x1

    adrp    x21, swap_count@PAGE
    add     x21, x21, swap_count@PAGEOFF
    ldr     x22, [x21]
    adrp    x0, swapped@PAGE
    add     x0, x0, swapped@PAGEOFF
    mov     x1, #0

Las_check:
    cmp     x1, x22
    b.ge    Las_add

    lsl     x2, x1, #2
    add     x2, x0, x2
    ldrb    w3, [x2]
    ldrb    w4, [x20]
    cmp     w3, w4
    b.ne    Las_next
    ldrb    w3, [x2, #1]
    ldrb    w4, [x20, #1]
    cmp     w3, w4
    b.ne    Las_next
    ldrb    w3, [x2, #2]
    ldrb    w4, [x20, #2]
    cmp     w3, w4
    b.eq    Las_done

Las_next:
    add     x1, x1, #1
    b       Las_check

Las_add:
    adrp    x0, swapped@PAGE
    add     x0, x0, swapped@PAGEOFF
    lsl     x1, x22, #2
    add     x0, x0, x1

    ldrb    w1, [x20]
    strb    w1, [x0]
    ldrb    w1, [x20, #1]
    strb    w1, [x0, #1]
    ldrb    w1, [x20, #2]
    strb    w1, [x0, #2]
    strb    wzr, [x0, #3]

    add     x22, x22, #1
    str     x22, [x21]

Las_done:
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret

// Find swapped wires using adder circuit rules
find_swapped:
    stp     x29, x30, [sp, #-96]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]

    adrp    x19, gates@PAGE
    add     x19, x19, gates@PAGEOFF
    adrp    x20, gate_count@PAGE
    add     x20, x20, gate_count@PAGEOFF
    ldr     x20, [x20]
    adrp    x21, wires@PAGE
    add     x21, x21, wires@PAGEOFF
    adrp    x26, max_z_bit@PAGE
    add     x26, x26, max_z_bit@PAGEOFF
    ldr     x26, [x26]

    mov     x22, #0

Lfs_loop:
    cmp     x22, x20
    b.ge    Lfs_done

    mov     x0, #GATE_SIZE
    mul     x0, x22, x0
    add     x23, x19, x0

    ldr     w24, [x23]          // in1
    ldr     w25, [x23, #4]      // op
    ldr     w27, [x23, #8]      // in2
    ldr     w28, [x23, #12]     // out

    // Debug: print first gate's wire indices
    cbnz    x22, 1f              // only for first iteration
    mov     x0, x24
    bl      print_number
    adrp    x1, newline@PAGE
    add     x1, x1, newline@PAGEOFF
    mov     x2, #1
    bl      print_string
1:

    // Rule 1: XOR not x,y should output z
    cmp     w25, #2
    b.ne    Lfs_next            // DEBUG: skip to next for non-XOR

    mov     x0, x24
    mov     w1, #'x'
    bl      wire_starts_with
    cbz     w0, Lfs_not_xy_xor

    mov     x0, x27
    mov     w1, #'x'
    bl      wire_starts_with
    cbnz    w0, Lfs_xy_xor
    mov     x0, x27
    mov     w1, #'y'
    bl      wire_starts_with
    cbnz    w0, Lfs_xy_xor
    b       Lfs_not_xy_xor

Lfs_xy_xor:
    mov     x0, x24
    mov     w1, #'y'
    bl      wire_starts_with
    cbnz    w0, Lfs_rule2
    b       Lfs_not_xy_xor

Lfs_not_xy_xor:
    mov     x0, x28
    mov     w1, #'z'
    bl      wire_starts_with
    cbnz    w0, Lfs_rule2
    mov     x0, x28
    bl      add_swapped

Lfs_rule2:
    // z outputs (not max) should come from XOR
    mov     x0, x28
    mov     w1, #'z'
    bl      wire_starts_with
    cbz     w0, Lfs_rule3

    mov     x0, #WIRE_SIZE
    mul     x0, x28, x0
    add     x0, x21, x0
    ldrb    w1, [x0, #1]
    sub     w1, w1, #'0'
    mov     w2, #10
    mul     w1, w1, w2
    ldrb    w2, [x0, #2]
    sub     w2, w2, #'0'
    add     w1, w1, w2

    cmp     x1, x26
    b.eq    Lfs_rule3
    cmp     w25, #2
    b.eq    Lfs_rule3
    mov     x0, x28
    bl      add_swapped

Lfs_rule3:
    // AND (not bit 0) should feed OR
    cmp     w25, #0
    b.ne    Lfs_rule4

    mov     x0, x24
    bl      is_bit_zero
    cbz     w0, Lfs_check_or
    mov     x0, x27
    bl      is_bit_zero
    cbnz    w0, Lfs_rule4

Lfs_check_or:
    mov     x0, #0
    mov     x1, #0

Lfs_or_loop:
    cmp     x0, x20
    b.ge    Lfs_or_done

    mov     x2, #GATE_SIZE
    mul     x2, x0, x2
    add     x2, x19, x2

    ldr     w3, [x2, #4]
    cmp     w3, #1
    b.ne    Lfs_or_next

    ldr     w3, [x2]
    cmp     w3, w28
    b.eq    Lfs_or_found
    ldr     w3, [x2, #8]
    cmp     w3, w28
    b.eq    Lfs_or_found
    b       Lfs_or_next

Lfs_or_found:
    mov     x1, #1
    b       Lfs_or_done

Lfs_or_next:
    add     x0, x0, #1
    b       Lfs_or_loop

Lfs_or_done:
    cbnz    x1, Lfs_rule4
    mov     x0, x28
    bl      add_swapped

Lfs_rule4:
    // XOR of x,y (not bit 0) should feed XOR and AND
    cmp     w25, #2
    b.ne    Lfs_next

    mov     x0, x24
    mov     w1, #'x'
    bl      wire_starts_with
    mov     x3, x0
    mov     x0, x24
    mov     w1, #'y'
    bl      wire_starts_with
    orr     w3, w3, w0
    cbz     w3, Lfs_next

    mov     x0, x27
    mov     w1, #'x'
    bl      wire_starts_with
    mov     x3, x0
    mov     x0, x27
    mov     w1, #'y'
    bl      wire_starts_with
    orr     w3, w3, w0
    cbz     w3, Lfs_next

    mov     x0, x24
    bl      is_bit_zero
    cbz     w0, Lfs_check_usage
    mov     x0, x27
    bl      is_bit_zero
    cbnz    w0, Lfs_next

Lfs_check_usage:
    mov     x0, #0
    mov     x3, #0
    mov     x4, #0

Lfs_usage_loop:
    cmp     x0, x20
    b.ge    Lfs_usage_done

    mov     x2, #GATE_SIZE
    mul     x2, x0, x2
    add     x2, x19, x2

    ldr     w5, [x2]
    ldr     w6, [x2, #8]
    cmp     w5, w28
    b.eq    Lfs_check_op
    cmp     w6, w28
    b.ne    Lfs_usage_next

Lfs_check_op:
    ldr     w5, [x2, #4]
    cmp     w5, #2
    b.ne    1f
    mov     x3, #1
1:  cmp     w5, #0
    b.ne    Lfs_usage_next
    mov     x4, #1

Lfs_usage_next:
    add     x0, x0, #1
    b       Lfs_usage_loop

Lfs_usage_done:
    and     x0, x3, x4
    cbnz    x0, Lfs_next
    mov     x0, x28
    bl      add_swapped

Lfs_next:
    add     x22, x22, #1
    b       Lfs_loop

Lfs_done:
    ldp     x27, x28, [sp, #80]
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #96
    ret

// Bubble sort swapped names
sort_swapped:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]

    adrp    x19, swap_count@PAGE
    add     x19, x19, swap_count@PAGEOFF
    ldr     x19, [x19]
    cmp     x19, #1
    b.le    Lsort_done

    adrp    x20, swapped@PAGE
    add     x20, x20, swapped@PAGEOFF

Lsort_outer:
    mov     x0, #0
    mov     x1, #0

Lsort_inner:
    add     x2, x0, #1
    cmp     x2, x19
    b.ge    Lsort_check

    lsl     x3, x0, #2
    add     x3, x20, x3
    lsl     x4, x2, #2
    add     x4, x20, x4

    ldrb    w5, [x3]
    ldrb    w6, [x4]
    cmp     w5, w6
    b.hi    Lsort_swap
    b.lo    Lsort_cont
    ldrb    w5, [x3, #1]
    ldrb    w6, [x4, #1]
    cmp     w5, w6
    b.hi    Lsort_swap
    b.lo    Lsort_cont
    ldrb    w5, [x3, #2]
    ldrb    w6, [x4, #2]
    cmp     w5, w6
    b.ls    Lsort_cont

Lsort_swap:
    ldr     w5, [x3]
    ldr     w6, [x4]
    str     w6, [x3]
    str     w5, [x4]
    mov     x1, #1

Lsort_cont:
    add     x0, x0, #1
    b       Lsort_inner

Lsort_check:
    cbnz    x1, Lsort_outer

Lsort_done:
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// Format output to comma-separated string
format_output:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]

    adrp    x19, output_buffer@PAGE
    add     x19, x19, output_buffer@PAGEOFF
    adrp    x20, swap_count@PAGE
    add     x20, x20, swap_count@PAGEOFF
    ldr     x20, [x20]
    adrp    x0, swapped@PAGE
    add     x0, x0, swapped@PAGEOFF

    mov     x1, #0
    mov     x2, x19

Lfmt_loop:
    cmp     x1, x20
    b.ge    Lfmt_done

    cbz     x1, Lfmt_name
    mov     w3, #','
    strb    w3, [x2]
    add     x2, x2, #1

Lfmt_name:
    lsl     x3, x1, #2
    add     x3, x0, x3
    ldrb    w4, [x3]
    strb    w4, [x2]
    ldrb    w4, [x3, #1]
    strb    w4, [x2, #1]
    ldrb    w4, [x3, #2]
    strb    w4, [x2, #2]
    add     x2, x2, #3
    add     x1, x1, #1
    b       Lfmt_loop

Lfmt_done:
    strb    wzr, [x2]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret
