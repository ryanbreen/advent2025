.global _start
.align 4

.data
filename: .asciz "../input.txt"
msg_part1: .asciz "Part 1: "
msg_part2: .asciz "Part 2: "
newline: .asciz "\n"
comma: .asciz ","

.bss
.align 3
buffer:     .space 40000
wires:      .space 8192      // 512 wires * 16 bytes (name[4] + value[4] + padding[8])
gates:      .space 16384     // 400 gates * 40 bytes
output:     .space 512
wire_count: .quad 0
gate_count: .quad 0
swapped:    .space 512       // swapped wire names
swap_count: .quad 0
max_bit:    .quad 0

.text

// x0 = string pointer to 3-char wire name
// Returns: index into wires array, or creates new wire
find_wire:
    stp x29, x30, [sp, #-48]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]

    mov x19, x0                     // save name pointer
    adrp x20, wire_count@PAGE
    add x20, x20, wire_count@PAGEOFF
    ldr x21, [x20]                  // current count
    adrp x22, wires@PAGE
    add x22, x22, wires@PAGEOFF
    mov x0, #0

Lfind_loop:
    cmp x0, x21
    b.ge Lcreate_wire
    mov x1, #16
    mul x1, x0, x1
    add x1, x22, x1                 // pointer to wire entry

    // Compare 3 chars
    ldrb w2, [x19]
    ldrb w3, [x1]
    cmp w2, w3
    b.ne Lfind_next
    ldrb w2, [x19, #1]
    ldrb w3, [x1, #1]
    cmp w2, w3
    b.ne Lfind_next
    ldrb w2, [x19, #2]
    ldrb w3, [x1, #2]
    cmp w2, w3
    b.eq Lfound

Lfind_next:
    add x0, x0, #1
    b Lfind_loop

Lfound:
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #48
    ret

Lcreate_wire:
    mov x0, #16
    mul x0, x21, x0
    add x0, x22, x0                 // pointer to new entry

    // Copy 3-char name
    ldrb w1, [x19]
    strb w1, [x0]
    ldrb w1, [x19, #1]
    strb w1, [x0, #1]
    ldrb w1, [x19, #2]
    strb w1, [x0, #2]
    strb wzr, [x0, #3]              // null terminate

    // Set value to -1 (unknown)
    mov w1, #-1
    str w1, [x0, #4]

    mov x0, x21                     // return index
    add x21, x21, #1
    str x21, [x20]                  // update count

    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #48
    ret

// Parse input from buffer
parse:
    stp x29, x30, [sp, #-80]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]
    stp x25, x26, [sp, #64]

    adrp x19, buffer@PAGE
    add x19, x19, buffer@PAGEOFF    // current position
    adrp x26, max_bit@PAGE
    add x26, x26, max_bit@PAGEOFF
    str xzr, [x26]

    // Parse initial wire values
Lparse_init:
    ldrb w0, [x19]
    cbz w0, Lparse_gates_done
    cmp w0, #'\n'
    b.ne Lparse_init_line
    add x19, x19, #1
    ldrb w0, [x19]
    cmp w0, #'\n'
    b.eq Lparse_gates_start
    b Lparse_init

Lparse_init_line:
    mov x0, x19                     // wire name at current pos
    bl find_wire
    mov x20, x0                     // wire index

    // Skip past ": " and get value
    add x19, x19, #5                // name(3) + ": "(2)
    ldrb w0, [x19]
    sub w0, w0, #'0'                // convert to 0 or 1

    // Store value
    adrp x1, wires@PAGE
    add x1, x1, wires@PAGEOFF
    mov x2, #16
    mul x2, x20, x2
    add x1, x1, x2
    str w0, [x1, #4]

    // Skip to next line
Lskip_line1:
    ldrb w0, [x19]
    add x19, x19, #1
    cmp w0, #'\n'
    b.ne Lskip_line1
    b Lparse_init

Lparse_gates_start:
    add x19, x19, #1                // skip second newline
    adrp x21, gate_count@PAGE
    add x21, x21, gate_count@PAGEOFF
    adrp x22, gates@PAGE
    add x22, x22, gates@PAGEOFF
    mov x23, #0                     // gate counter

Lparse_gate:
    ldrb w0, [x19]
    cbz w0, Lparse_gates_done
    cmp w0, #'\n'
    b.eq Lskip_nl

    // Parse: in1 OP in2 -> out
    // in1
    mov x0, x19
    bl find_wire
    mov x24, x0                     // in1 index
    add x19, x19, #4                // name(3) + space(1)

    // OP
    ldrb w0, [x19]
    cmp w0, #'A'
    b.eq Lop_and
    cmp w0, #'O'
    b.eq Lop_or
    mov w25, #2                     // XOR
    add x19, x19, #4                // "XOR "
    b Lparse_in2
Lop_and:
    mov w25, #0                     // AND
    add x19, x19, #4                // "AND "
    b Lparse_in2
Lop_or:
    mov w25, #1                     // OR
    add x19, x19, #3                // "OR "

Lparse_in2:
    mov x0, x19
    bl find_wire
    mov x20, x0                     // in2 index
    add x19, x19, #7                // name(3) + " -> "(4)

    // out
    mov x0, x19
    bl find_wire

    // Check if output is z wire and track max bit
    ldrb w1, [x19]
    cmp w1, #'z'
    b.ne Lstore_gate

    ldrb w1, [x19, #1]
    sub w1, w1, #'0'
    mov w2, #10
    mul w1, w1, w2
    ldrb w2, [x19, #2]
    sub w2, w2, #'0'
    add w1, w1, w2
    ldr x2, [x26]
    cmp x1, x2
    b.le Lstore_gate
    str x1, [x26]

Lstore_gate:
    // Store gate: in1, op, in2, out (4 words = 16 bytes per gate)
    // Plus space for flags (40 bytes total per gate)
    mov x1, #40
    mul x1, x23, x1
    add x1, x22, x1
    str w24, [x1]                   // in1
    str w25, [x1, #4]               // op
    str w20, [x1, #8]               // in2
    str w0, [x1, #12]               // out

    add x23, x23, #1
    add x19, x19, #3                // skip output name

Lskip_to_nl:
    ldrb w0, [x19]
    cbz w0, Lparse_gates_done
    add x19, x19, #1
    cmp w0, #'\n'
    b.ne Lskip_to_nl
    b Lparse_gate

Lskip_nl:
    add x19, x19, #1
    b Lparse_gate

Lparse_gates_done:
    str x23, [x21]                  // store gate count
    ldp x25, x26, [sp, #64]
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #80
    ret

// Simulate the circuit
// Returns: z-value as 64-bit number
simulate:
    stp x29, x30, [sp, #-64]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]

    adrp x19, gates@PAGE
    add x19, x19, gates@PAGEOFF
    adrp x20, gate_count@PAGE
    add x20, x20, gate_count@PAGEOFF
    ldr x20, [x20]
    adrp x21, wires@PAGE
    add x21, x21, wires@PAGEOFF

Lsim_loop:
    mov x22, #0                     // progress flag
    mov x23, #0                     // gate index

Lsim_gate:
    cmp x23, x20
    b.ge Lsim_check

    mov x0, #40
    mul x0, x23, x0
    add x24, x19, x0                // gate pointer

    ldr w0, [x24]                   // in1 index
    ldr w1, [x24, #4]               // op
    ldr w2, [x24, #8]               // in2 index
    ldr w3, [x24, #12]              // out index

    // Get in1 value
    mov x4, #16
    mul x4, x0, x4
    add x4, x21, x4
    ldr w4, [x4, #4]                // in1 value
    cmp w4, #0
    b.lt Lsim_next                 // not ready

    // Get in2 value
    mov x5, #16
    mul x5, x2, x5
    add x5, x21, x5
    ldr w5, [x5, #4]                // in2 value
    cmp w5, #0
    b.lt Lsim_next                 // not ready

    // Get out slot
    mov x6, #16
    mul x6, x3, x6
    add x6, x21, x6
    ldr w7, [x6, #4]
    cmp w7, #0
    b.ge Lsim_next                 // already computed

    // Compute result
    cmp w1, #0
    b.eq Lsim_and
    cmp w1, #1
    b.eq Lsim_or
    eor w7, w4, w5
    b Lsim_store
Lsim_and:
    and w7, w4, w5
    b Lsim_store
Lsim_or:
    orr w7, w4, w5
Lsim_store:
    str w7, [x6, #4]
    mov x22, #1                     // made progress

Lsim_next:
    add x23, x23, #1
    b Lsim_gate

Lsim_check:
    cbnz x22, Lsim_loop

    // Collect z-bits
    adrp x0, wire_count@PAGE
    add x0, x0, wire_count@PAGEOFF
    ldr x0, [x0]
    mov x1, #0                      // result
    mov x2, #0                      // wire index

Lcollect:
    cmp x2, x0
    b.ge Lsim_done

    mov x3, #16
    mul x3, x2, x3
    add x3, x21, x3                 // wire pointer

    ldrb w4, [x3]
    cmp w4, #'z'
    b.ne Lcollect_next

    // Parse bit number
    ldrb w4, [x3, #1]
    sub w4, w4, #'0'
    mov w5, #10
    mul w4, w4, w5
    ldrb w5, [x3, #2]
    sub w5, w5, #'0'
    add w4, w4, w5                  // bit number

    ldr w5, [x3, #4]                // value
    cmp w5, #0
    b.le Lcollect_next

    mov x6, #1
    lsl x6, x6, x4
    orr x1, x1, x6

Lcollect_next:
    add x2, x2, #1
    b Lcollect

Lsim_done:
    mov x0, x1
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

// Add a wire name to swapped list if not already present
// x0 = wire index
add_swapped:
    stp x29, x30, [sp, #-48]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]

    mov x19, x0                     // wire index

    // Get wire name
    adrp x20, wires@PAGE
    add x20, x20, wires@PAGEOFF
    mov x1, #16
    mul x1, x19, x1
    add x20, x20, x1                // pointer to wire name

    // Check if already in swapped
    adrp x21, swap_count@PAGE
    add x21, x21, swap_count@PAGEOFF
    ldr x22, [x21]
    adrp x0, swapped@PAGE
    add x0, x0, swapped@PAGEOFF
    mov x1, #0

Lcheck_swap:
    cmp x1, x22
    b.ge Ladd_swap

    // Compare names
    ldrb w2, [x0]
    ldrb w3, [x20]
    cmp w2, w3
    b.ne Lcheck_next
    ldrb w2, [x0, #1]
    ldrb w3, [x20, #1]
    cmp w2, w3
    b.ne Lcheck_next
    ldrb w2, [x0, #2]
    ldrb w3, [x20, #2]
    cmp w2, w3
    b.eq Ladd_done                 // already present

Lcheck_next:
    add x0, x0, #4
    add x1, x1, #1
    b Lcheck_swap

Ladd_swap:
    // Add to list
    adrp x0, swapped@PAGE
    add x0, x0, swapped@PAGEOFF
    mov x1, #4
    mul x1, x22, x1
    add x0, x0, x1

    ldrb w1, [x20]
    strb w1, [x0]
    ldrb w1, [x20, #1]
    strb w1, [x0, #1]
    ldrb w1, [x20, #2]
    strb w1, [x0, #2]
    strb wzr, [x0, #3]

    add x22, x22, #1
    str x22, [x21]

Ladd_done:
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #48
    ret

// Check if wire at index starts with given char
// x0 = wire index, w1 = char to check
// Returns: 1 if matches, 0 otherwise
wire_starts_with:
    adrp x2, wires@PAGE
    add x2, x2, wires@PAGEOFF
    mov x3, #16
    mul x3, x0, x3
    add x2, x2, x3
    ldrb w0, [x2]
    cmp w0, w1
    cset w0, eq
    ret

// Check if wire is x00 or y00
// x0 = wire index
// Returns: 1 if x00 or y00, 0 otherwise
is_bit_zero:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    adrp x1, wires@PAGE
    add x1, x1, wires@PAGEOFF
    mov x2, #16
    mul x2, x0, x2
    add x1, x1, x2

    ldrb w2, [x1]
    cmp w2, #'x'
    b.eq Lcheck_00
    cmp w2, #'y'
    b.ne Lnot_b0

Lcheck_00:
    ldrb w2, [x1, #1]
    cmp w2, #'0'
    b.ne Lnot_b0
    ldrb w2, [x1, #2]
    cmp w2, #'0'
    b.ne Lnot_b0
    mov w0, #1
    ldp x29, x30, [sp], #16
    ret

Lnot_b0:
    mov w0, #0
    ldp x29, x30, [sp], #16
    ret

// Part 2: Find swapped wires
part2:
    stp x29, x30, [sp, #-96]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]
    stp x25, x26, [sp, #64]
    stp x27, x28, [sp, #80]

    adrp x19, gates@PAGE
    add x19, x19, gates@PAGEOFF
    adrp x20, gate_count@PAGE
    add x20, x20, gate_count@PAGEOFF
    ldr x20, [x20]
    adrp x21, wires@PAGE
    add x21, x21, wires@PAGEOFF
    adrp x26, max_bit@PAGE
    add x26, x26, max_bit@PAGEOFF
    ldr x26, [x26]

    // Clear swap count
    adrp x0, swap_count@PAGE
    add x0, x0, swap_count@PAGEOFF
    str xzr, [x0]

    mov x22, #0                     // gate index

Lp2_loop:
    cmp x22, x20
    b.ge Lp2_done

    mov x0, #40
    mul x0, x22, x0
    add x23, x19, x0                // gate pointer

    ldr w24, [x23]                  // in1
    ldr w25, [x23, #4]              // op
    ldr w27, [x23, #8]              // in2
    ldr w28, [x23, #12]             // out

    // Rule 1: XOR gates not taking x,y should output to z
    cmp w25, #2                     // XOR?
    b.ne Lcheck_rule2

    // Check if in1 is x or y
    mov x0, x24
    mov w1, #'x'
    bl wire_starts_with
    cbz w0, Lnot_xy_xor

    mov x0, x27
    mov w1, #'x'
    bl wire_starts_with
    cbnz w0, Lxy_xor

    mov x0, x27
    mov w1, #'y'
    bl wire_starts_with
    cbnz w0, Lxy_xor
    b Lnot_xy_xor

Lxy_xor:
    mov x0, x24
    mov w1, #'y'
    bl wire_starts_with
    cbnz w0, Lcheck_rule2          // Both x,y - skip this rule
    b Lnot_xy_xor

Lnot_xy_xor:
    // Second-level XOR should output to z
    mov x0, x28
    mov w1, #'z'
    bl wire_starts_with
    cbnz w0, Lcheck_rule2

    mov x0, x28
    bl add_swapped

Lcheck_rule2:
    // Rule 2: z outputs (except highest bit) should come from XOR
    mov x0, x28
    mov w1, #'z'
    bl wire_starts_with
    cbz w0, Lcheck_rule3

    // Get bit number
    mov x0, #16
    mul x0, x28, x0
    add x0, x21, x0
    ldrb w1, [x0, #1]
    sub w1, w1, #'0'
    mov w2, #10
    mul w1, w1, w2
    ldrb w2, [x0, #2]
    sub w2, w2, #'0'
    add w1, w1, w2

    cmp x1, x26                     // is highest bit?
    b.eq Lcheck_rule3

    cmp w25, #2                     // is XOR?
    b.eq Lcheck_rule3

    mov x0, x28
    bl add_swapped

Lcheck_rule3:
    // Rule 3: AND gates (except x00&y00) should feed into OR
    cmp w25, #0                     // AND?
    b.ne Lcheck_rule4

    mov x0, x24
    bl is_bit_zero
    cbz w0, Lcheck_and_or
    mov x0, x27
    bl is_bit_zero
    cbnz w0, Lcheck_rule4          // x00&y00, skip

Lcheck_and_or:
    // Check if out is used by OR
    mov x0, #0
    mov x1, #0                      // found flag

Land_or_check:
    cmp x0, x20
    b.ge Land_or_done

    mov x2, #40
    mul x2, x0, x2
    add x2, x19, x2

    ldr w3, [x2, #4]                // op
    cmp w3, #1                      // OR?
    b.ne Land_or_next

    ldr w3, [x2]                    // in1
    cmp w3, w28
    b.eq Land_or_found
    ldr w3, [x2, #8]                // in2
    cmp w3, w28
    b.eq Land_or_found
    b Land_or_next

Land_or_found:
    mov x1, #1
    b Land_or_done

Land_or_next:
    add x0, x0, #1
    b Land_or_check

Land_or_done:
    cbnz x1, Lcheck_rule4
    mov x0, x28
    bl add_swapped

Lcheck_rule4:
    // Rule 4: XOR of x,y (not bit 0) should feed into XOR and AND
    cmp w25, #2                     // XOR?
    b.ne Lp2_next

    // Check if in1 and in2 are x,y
    mov x0, x24
    mov w1, #'x'
    bl wire_starts_with
    mov x3, x0
    mov x0, x24
    mov w1, #'y'
    bl wire_starts_with
    orr w3, w3, w0
    cbz w3, Lp2_next

    mov x0, x27
    mov w1, #'x'
    bl wire_starts_with
    mov x3, x0
    mov x0, x27
    mov w1, #'y'
    bl wire_starts_with
    orr w3, w3, w0
    cbz w3, Lp2_next

    // Check if bit 0
    mov x0, x24
    bl is_bit_zero
    cbz w0, Lcheck_xy_usage
    mov x0, x27
    bl is_bit_zero
    cbnz w0, Lp2_next              // x00 XOR y00, skip

Lcheck_xy_usage:
    // Check if out is used by both XOR and AND
    mov x0, #0
    mov x3, #0                      // xor_found
    mov x4, #0                      // and_found

Lxy_usage_check:
    cmp x0, x20
    b.ge Lxy_usage_done

    mov x2, #40
    mul x2, x0, x2
    add x2, x19, x2

    ldr w5, [x2]                    // in1
    ldr w6, [x2, #8]                // in2
    cmp w5, w28
    b.eq Lxy_check_op
    cmp w6, w28
    b.ne Lxy_usage_next

Lxy_check_op:
    ldr w5, [x2, #4]                // op
    cmp w5, #2                      // XOR?
    b.ne 1f
    mov x3, #1
1:  cmp w5, #0                      // AND?
    b.ne Lxy_usage_next
    mov x4, #1

Lxy_usage_next:
    add x0, x0, #1
    b Lxy_usage_check

Lxy_usage_done:
    and x0, x3, x4
    cbnz x0, Lp2_next
    mov x0, x28
    bl add_swapped

Lp2_next:
    add x22, x22, #1
    b Lp2_loop

Lp2_done:
    ldp x27, x28, [sp, #80]
    ldp x25, x26, [sp, #64]
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #96
    ret

// Simple bubble sort for swapped names (4-byte entries)
sort_swapped:
    stp x29, x30, [sp, #-32]!
    mov x29, sp
    stp x19, x20, [sp, #16]

    adrp x19, swap_count@PAGE
    add x19, x19, swap_count@PAGEOFF
    ldr x19, [x19]
    cmp x19, #1
    b.le Lsort_done

    adrp x20, swapped@PAGE
    add x20, x20, swapped@PAGEOFF

Lsort_outer:
    mov x0, #0
    mov x1, #0                      // swapped flag

Lsort_inner:
    add x2, x0, #1
    cmp x2, x19
    b.ge Lsort_check

    lsl x3, x0, #2
    add x3, x20, x3
    lsl x4, x2, #2
    add x4, x20, x4

    // Compare names
    ldrb w5, [x3]
    ldrb w6, [x4]
    cmp w5, w6
    b.hi Ldo_swap
    b.lo Lsort_next
    ldrb w5, [x3, #1]
    ldrb w6, [x4, #1]
    cmp w5, w6
    b.hi Ldo_swap
    b.lo Lsort_next
    ldrb w5, [x3, #2]
    ldrb w6, [x4, #2]
    cmp w5, w6
    b.ls Lsort_next

Ldo_swap:
    ldr w5, [x3]
    ldr w6, [x4]
    str w6, [x3]
    str w5, [x4]
    mov x1, #1

Lsort_next:
    add x0, x0, #1
    b Lsort_inner

Lsort_check:
    cbnz x1, Lsort_outer

Lsort_done:
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #32
    ret

// Format part 2 output
format_part2:
    stp x29, x30, [sp, #-32]!
    mov x29, sp
    stp x19, x20, [sp, #16]

    adrp x19, output@PAGE
    add x19, x19, output@PAGEOFF
    adrp x20, swap_count@PAGE
    add x20, x20, swap_count@PAGEOFF
    ldr x20, [x20]
    adrp x0, swapped@PAGE
    add x0, x0, swapped@PAGEOFF

    mov x1, #0                      // index
    mov x2, x19                     // output pointer

Lfmt_loop:
    cmp x1, x20
    b.ge Lfmt_done

    // Add comma if not first
    cbz x1, Lfmt_name
    mov w3, #','
    strb w3, [x2]
    add x2, x2, #1

Lfmt_name:
    lsl x3, x1, #2
    add x3, x0, x3
    ldrb w4, [x3]
    strb w4, [x2]
    ldrb w4, [x3, #1]
    strb w4, [x2, #1]
    ldrb w4, [x3, #2]
    strb w4, [x2, #2]
    add x2, x2, #3
    add x1, x1, #1
    b Lfmt_loop

Lfmt_done:
    strb wzr, [x2]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #32
    ret

// Print number in x0
print_num:
    stp x29, x30, [sp, #-48]!
    mov x29, sp
    stp x19, x20, [sp, #16]

    mov x19, x0
    sub sp, sp, #32
    mov x20, sp
    add x20, x20, #31
    strb wzr, [x20]
    sub x20, x20, #1

Ldig:
    mov x0, x19
    mov x1, #10
    udiv x2, x0, x1
    msub x3, x2, x1, x0
    add w3, w3, #'0'
    strb w3, [x20]
    sub x20, x20, #1
    mov x19, x2
    cbnz x19, Ldig

    add x20, x20, #1
    mov x16, #4
    mov x0, #1
    mov x1, x20
    mov x2, sp
    add x2, x2, #31
    sub x2, x2, x1
    svc #0x80

    add sp, sp, #32
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #48
    ret

// Print string at x0
print_str:
    stp x29, x30, [sp, #-32]!
    mov x29, sp
    stp x19, x20, [sp, #16]

    mov x19, x0
    mov x20, #0

1:  ldrb w0, [x19, x20]
    cbz w0, 2f
    add x20, x20, #1
    b 1b

2:  mov x16, #4
    mov x0, #1
    mov x1, x19
    mov x2, x20
    svc #0x80

    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #32
    ret

_start:
    stp x29, x30, [sp, #-32]!
    mov x29, sp
    stp x19, x20, [sp, #16]

    // Open file
    mov x16, #5
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #0
    svc #0x80
    cmp x0, #0
    b.lt Lerr
    mov x19, x0

    // Read file
    mov x16, #3
    mov x0, x19
    adrp x1, buffer@PAGE
    add x1, x1, buffer@PAGEOFF
    mov x2, #40000
    svc #0x80
    mov x20, x0

    // Close file
    mov x16, #6
    mov x0, x19
    svc #0x80

    // Null terminate buffer
    adrp x0, buffer@PAGE
    add x0, x0, buffer@PAGEOFF
    strb wzr, [x0, x20]

    // Parse and simulate
    bl parse
    bl simulate
    mov x19, x0

    // Print Part 1
    adrp x0, msg_part1@PAGE
    add x0, x0, msg_part1@PAGEOFF
    bl print_str
    mov x0, x19
    bl print_num
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Part 2
    bl part2
    bl sort_swapped
    bl format_part2

    // Print Part 2
    adrp x0, msg_part2@PAGE
    add x0, x0, msg_part2@PAGEOFF
    bl print_str
    adrp x0, output@PAGE
    add x0, x0, output@PAGEOFF
    bl print_str
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Exit
    mov x16, #1
    mov x0, #0
    svc #0x80

Lerr:
    mov x16, #1
    mov x0, #1
    svc #0x80
