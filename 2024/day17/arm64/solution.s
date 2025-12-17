// Advent of Code 2024 - Day 17: Chronospatial Computer
// ARM64 Assembly for macOS
//
// A 3-bit VM emulator
// Part 1: Run VM and output comma-separated results
// Part 2: Find smallest A that makes program output itself (recursive search)

.global _start
.align 4

.data
input_file: .asciz "../input.txt"
msg_part1: .asciz "Part 1: "
msg_part2: .asciz "Part 2: "
msg_newline: .asciz "\n"
msg_comma: .asciz ","

.bss
.align 16
file_buffer: .space 4096
program: .space 128          // Max 64 instructions (each 1 byte as int)
program_len: .space 8
init_a: .space 8
init_b: .space 8
init_c: .space 8
output_buffer: .space 256    // VM output buffer
output_len: .space 8

.text

_start:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Read and parse input
    bl      read_input

    // Part 1: Run VM with initial registers
    adrp    x0, init_a@PAGE
    add     x0, x0, init_a@PAGEOFF
    ldr     x0, [x0]
    adrp    x1, init_b@PAGE
    add     x1, x1, init_b@PAGEOFF
    ldr     x1, [x1]
    adrp    x2, init_c@PAGE
    add     x2, x2, init_c@PAGEOFF
    ldr     x2, [x2]
    bl      run_vm

    // Print Part 1
    adrp    x0, msg_part1@PAGE
    add     x0, x0, msg_part1@PAGEOFF
    bl      print_str
    bl      print_output
    adrp    x0, msg_newline@PAGE
    add     x0, x0, msg_newline@PAGEOFF
    bl      print_str

    // Part 2: Find A that outputs the program itself
    bl      part2

    adrp    x1, msg_part2@PAGE
    add     x1, x1, msg_part2@PAGEOFF
    mov     x19, x0             // Save result
    mov     x0, x1
    bl      print_str
    mov     x0, x19
    bl      print_num
    adrp    x0, msg_newline@PAGE
    add     x0, x0, msg_newline@PAGEOFF
    bl      print_str

    mov     x0, #0
    mov     x16, #1
    svc     #0x80

// Read and parse input
// Parses: Register A: N, Register B: N, Register C: N, Program: x,y,z,...
read_input:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    // Open file
    mov     x16, #5
    adrp    x0, input_file@PAGE
    add     x0, x0, input_file@PAGEOFF
    mov     x1, #0
    mov     x2, #0
    svc     #0x80
    mov     x19, x0

    // Read file
    mov     x16, #3
    mov     x0, x19
    adrp    x1, file_buffer@PAGE
    add     x1, x1, file_buffer@PAGEOFF
    mov     x2, #4096
    svc     #0x80

    // Close file
    mov     x16, #6
    mov     x0, x19
    svc     #0x80

    // Parse input
    adrp    x19, file_buffer@PAGE
    add     x19, x19, file_buffer@PAGEOFF

    // Find "Register A: " and parse number
    bl      skip_to_number
    mov     x0, x19
    bl      parse_number
    mov     x19, x1
    adrp    x2, init_a@PAGE
    add     x2, x2, init_a@PAGEOFF
    str     x0, [x2]

    // Find "Register B: " and parse number
    bl      skip_to_number
    mov     x0, x19
    bl      parse_number
    mov     x19, x1
    adrp    x2, init_b@PAGE
    add     x2, x2, init_b@PAGEOFF
    str     x0, [x2]

    // Find "Register C: " and parse number
    bl      skip_to_number
    mov     x0, x19
    bl      parse_number
    mov     x19, x1
    adrp    x2, init_c@PAGE
    add     x2, x2, init_c@PAGEOFF
    str     x0, [x2]

    // Find "Program: " and parse comma-separated numbers
    bl      skip_to_number
    adrp    x20, program@PAGE
    add     x20, x20, program@PAGEOFF
    mov     x21, #0             // program length

.L_parse_prog:
    ldrb    w0, [x19]
    cbz     w0, .L_parse_prog_done
    cmp     w0, #'\n'
    b.eq    .L_parse_prog_done
    cmp     w0, #'0'
    b.lt    .L_parse_prog_skip
    cmp     w0, #'9'
    b.gt    .L_parse_prog_skip

    // Parse digit (single digit 0-7)
    sub     w0, w0, #'0'
    strb    w0, [x20, x21]
    add     x21, x21, #1
    add     x19, x19, #1
    b       .L_parse_prog

.L_parse_prog_skip:
    add     x19, x19, #1
    b       .L_parse_prog

.L_parse_prog_done:
    adrp    x0, program_len@PAGE
    add     x0, x0, program_len@PAGEOFF
    str     x21, [x0]

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Skip to next digit in input (uses x19)
skip_to_number:
.L_skip:
    ldrb    w0, [x19]
    cbz     w0, .L_skip_done
    cmp     w0, #'0'
    b.lt    .L_skip_next
    cmp     w0, #'9'
    b.le    .L_skip_done
.L_skip_next:
    add     x19, x19, #1
    b       .L_skip
.L_skip_done:
    ret

// Parse number from string
// x0 = pointer to string
// Returns: x0 = number, x1 = pointer past number
parse_number:
    mov     x2, #0              // result
    mov     x1, x0              // current pointer
.L_pn_loop:
    ldrb    w3, [x1]
    cmp     w3, #'0'
    b.lt    .L_pn_done
    cmp     w3, #'9'
    b.gt    .L_pn_done
    mov     x4, #10
    mul     x2, x2, x4
    sub     w3, w3, #'0'
    add     x2, x2, x3
    add     x1, x1, #1
    b       .L_pn_loop
.L_pn_done:
    mov     x0, x2
    ret

// Run VM
// x0 = A register, x1 = B register, x2 = C register
// Output stored in output_buffer, length in output_len
run_vm:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0             // A
    mov     x20, x1             // B
    mov     x21, x2             // C
    mov     x22, #0             // IP

    adrp    x23, program@PAGE
    add     x23, x23, program@PAGEOFF
    adrp    x24, program_len@PAGE
    add     x24, x24, program_len@PAGEOFF
    ldr     x24, [x24]          // program length

    adrp    x25, output_buffer@PAGE
    add     x25, x25, output_buffer@PAGEOFF
    mov     x26, #0             // output index

.L_vm_loop:
    cmp     x22, x24
    b.ge    .L_vm_done

    ldrb    w27, [x23, x22]     // opcode
    add     x28, x22, #1
    ldrb    w28, [x23, x28]     // operand (literal)

    // Get combo value in x0
    cmp     w28, #4
    b.lt    .L_combo_literal
    b.eq    .L_combo_a
    cmp     w28, #5
    b.eq    .L_combo_b
    cmp     w28, #6
    b.eq    .L_combo_c
    mov     x0, x28             // default to literal
    b       .L_combo_done
.L_combo_literal:
    mov     x0, x28
    b       .L_combo_done
.L_combo_a:
    mov     x0, x19
    b       .L_combo_done
.L_combo_b:
    mov     x0, x20
    b       .L_combo_done
.L_combo_c:
    mov     x0, x21
.L_combo_done:
    // x0 = combo value, x28 = literal operand

    // Execute opcode
    cmp     w27, #0
    b.eq    .L_op_adv
    cmp     w27, #1
    b.eq    .L_op_bxl
    cmp     w27, #2
    b.eq    .L_op_bst
    cmp     w27, #3
    b.eq    .L_op_jnz
    cmp     w27, #4
    b.eq    .L_op_bxc
    cmp     w27, #5
    b.eq    .L_op_out
    cmp     w27, #6
    b.eq    .L_op_bdv
    cmp     w27, #7
    b.eq    .L_op_cdv
    b       .L_vm_next

.L_op_adv:  // A = A >> combo
    lsr     x19, x19, x0
    b       .L_vm_next

.L_op_bxl:  // B = B XOR literal
    eor     x20, x20, x28
    b       .L_vm_next

.L_op_bst:  // B = combo % 8
    and     x20, x0, #7
    b       .L_vm_next

.L_op_jnz:  // jump if A != 0
    cbz     x19, .L_vm_next
    mov     x22, x28
    b       .L_vm_loop

.L_op_bxc:  // B = B XOR C
    eor     x20, x20, x21
    b       .L_vm_next

.L_op_out:  // output combo % 8
    and     x0, x0, #7
    strb    w0, [x25, x26]
    add     x26, x26, #1
    b       .L_vm_next

.L_op_bdv:  // B = A >> combo
    lsr     x20, x19, x0
    b       .L_vm_next

.L_op_cdv:  // C = A >> combo
    lsr     x21, x19, x0
    b       .L_vm_next

.L_vm_next:
    add     x22, x22, #2
    b       .L_vm_loop

.L_vm_done:
    adrp    x0, output_len@PAGE
    add     x0, x0, output_len@PAGEOFF
    str     x26, [x0]

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Print output buffer as comma-separated values
print_output:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    adrp    x19, output_buffer@PAGE
    add     x19, x19, output_buffer@PAGEOFF
    adrp    x20, output_len@PAGE
    add     x20, x20, output_len@PAGEOFF
    ldr     x20, [x20]
    mov     x21, #0

.L_po_loop:
    cmp     x21, x20
    b.ge    .L_po_done

    // Print comma if not first
    cbz     x21, .L_po_skip_comma
    adrp    x0, msg_comma@PAGE
    add     x0, x0, msg_comma@PAGEOFF
    bl      print_str

.L_po_skip_comma:
    ldrb    w0, [x19, x21]
    bl      print_digit
    add     x21, x21, #1
    b       .L_po_loop

.L_po_done:
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Print single digit (0-9)
print_digit:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    sub     sp, sp, #16

    add     w0, w0, #'0'
    strb    w0, [sp]

    mov     x0, #1
    mov     x1, sp
    mov     x2, #1
    mov     x16, #4
    svc     #0x80

    add     sp, sp, #16
    ldp     x29, x30, [sp], #16
    ret

// Part 2: Find A that makes program output itself
// Uses recursive backtracking, building A 3 bits at a time from the end
// Returns: x0 = smallest valid A
part2:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!

    adrp    x19, program_len@PAGE
    add     x19, x19, program_len@PAGEOFF
    ldr     x19, [x19]
    sub     x0, x19, #1         // target_idx = len - 1
    mov     x1, #0              // current_a = 0
    bl      search_a

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Recursive search for A
// x0 = target_idx (which output position we're trying to match)
// x1 = current_a
// Returns: x0 = valid A or -1 if not found
search_a:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    mov     x19, x0             // target_idx
    mov     x20, x1             // current_a

    // Base case: found valid A
    cmp     x19, #0
    b.lt    .L_search_found

    // Load program length for comparisons
    adrp    x21, program_len@PAGE
    add     x21, x21, program_len@PAGEOFF
    ldr     x21, [x21]

    // Try all 8 possible 3-bit values
    mov     x22, #0             // bits = 0

.L_search_bits:
    cmp     x22, #8
    b.ge    .L_search_not_found

    // candidate_a = (current_a << 3) | bits
    lsl     x23, x20, #3
    orr     x23, x23, x22

    // Skip if candidate_a == 0 and target_idx == program_len - 1
    // (A can't be 0 at start - would halt without output)
    cbnz    x23, .L_search_run_vm
    sub     x24, x21, #1
    cmp     x19, x24
    b.eq    .L_search_next_bits

.L_search_run_vm:
    // Run VM with candidate_a
    mov     x0, x23
    mov     x1, #0              // B = 0
    mov     x2, #0              // C = 0
    bl      run_vm

    // Check if output matches expected suffix
    // expected = program[target_idx:]
    // output should equal expected
    adrp    x24, output_len@PAGE
    add     x24, x24, output_len@PAGEOFF
    ldr     x24, [x24]          // output length

    // Expected length = program_len - target_idx
    sub     x25, x21, x19       // expected_len

    // Check lengths match
    cmp     x24, x25
    b.ne    .L_search_next_bits

    // Compare output with program[target_idx:]
    adrp    x24, output_buffer@PAGE
    add     x24, x24, output_buffer@PAGEOFF
    adrp    x25, program@PAGE
    add     x25, x25, program@PAGEOFF
    add     x25, x25, x19       // program + target_idx

    mov     x26, #0             // comparison index
.L_search_cmp:
    sub     x0, x21, x19        // expected_len
    cmp     x26, x0
    b.ge    .L_search_match

    ldrb    w0, [x24, x26]      // output[i]
    ldrb    w1, [x25, x26]      // program[target_idx + i]
    cmp     w0, w1
    b.ne    .L_search_next_bits
    add     x26, x26, #1
    b       .L_search_cmp

.L_search_match:
    // Output matches expected suffix - recurse
    sub     x0, x19, #1         // target_idx - 1
    mov     x1, x23             // candidate_a
    bl      search_a

    // Check if result is valid (not -1)
    cmn     x0, #1
    b.ne    .L_search_done      // Found valid result

.L_search_next_bits:
    add     x22, x22, #1
    b       .L_search_bits

.L_search_not_found:
    mov     x0, #-1
    b       .L_search_done

.L_search_found:
    mov     x0, x20             // return current_a
    b       .L_search_done

.L_search_done:
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Print null-terminated string
// x0 = pointer to string
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0

    // Find length
    mov     x20, #0
.L_strlen:
    ldrb    w1, [x19, x20]
    cbz     w1, .L_write
    add     x20, x20, #1
    b       .L_strlen

.L_write:
    mov     x0, #1              // stdout
    mov     x1, x19             // buffer
    mov     x2, x20             // length
    mov     x16, #4             // write syscall
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Print number (64-bit)
// x0 = number to print
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    sub     sp, sp, #32

    mov     x19, x0
    add     x20, sp, #31
    strb    wzr, [x20]

    // Handle zero case
    cbnz    x19, .L_pnum_convert
    sub     x20, x20, #1
    mov     w0, #'0'
    strb    w0, [x20]
    b       .L_pnum_print

.L_pnum_convert:
    cbz     x19, .L_pnum_print
    mov     x1, #10
    udiv    x2, x19, x1
    msub    x3, x2, x1, x19
    add     w3, w3, #'0'
    sub     x20, x20, #1
    strb    w3, [x20]
    mov     x19, x2
    b       .L_pnum_convert

.L_pnum_print:
    mov     x0, x20
    bl      print_str

    add     sp, sp, #32
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
