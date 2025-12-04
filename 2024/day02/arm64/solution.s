// ARM64 Assembly solution for AoC 2024 Day 2
// macOS syscalls

.global _main
.align 2

.equ STDOUT, 1
.equ MAX_REPORTS, 1000
.equ MAX_LEVELS, 10

.data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

.align 3
file_buffer: .skip 32768
// Each report: first 8 bytes = count, then up to MAX_LEVELS * 8 bytes for levels
reports: .skip MAX_REPORTS * (MAX_LEVELS + 1) * 8
num_reports: .skip 8
temp_report: .skip (MAX_LEVELS + 1) * 8
output_buffer: .skip 32

.text
_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    // Open file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0005
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #0  // O_RDONLY
    mov x2, #0
    svc #0x80
    cmp x0, #0
    b.lt exit_error
    mov x19, x0  // Save fd

    // Read file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0003
    mov x0, x19
    adrp x1, file_buffer@PAGE
    add x1, x1, file_buffer@PAGEOFF
    mov x2, #32768
    svc #0x80
    mov x20, x0  // Save bytes read

    // Close file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0006
    mov x0, x19
    svc #0x80

    // Parse input into reports
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20
    bl parse_input

    // Part 1: Count safe reports
    bl part1

    // Print Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_str
    mov x0, x21  // Part 1 result
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Part 2: Count safe reports with dampener
    bl part2

    // Print Part 2
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_str
    mov x0, x22  // Part 2 result
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    mov x0, #0
    ldp x29, x30, [sp], #16
    movz x16, #0x2000, lsl #16
    movk x16, #0x0001
    svc #0x80

exit_error:
    mov x0, #1
    ldp x29, x30, [sp], #16
    movz x16, #0x2000, lsl #16
    movk x16, #0x0001
    svc #0x80

// Parse input into reports
// x0 = buffer, x1 = length
parse_input:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0  // Buffer pointer
    mov x20, x1  // Buffer end
    add x20, x19, x20
    adrp x21, reports@PAGE
    add x21, x21, reports@PAGEOFF
    mov x22, #0  // Report count

parse_line_loop:
    cmp x19, x20
    b.ge parse_done

    mov x23, #0  // Level count for this report
    mov x24, x21  // Current report pointer
    add x24, x24, #8  // Skip count field

parse_number_loop:
    // Skip whitespace
    mov x0, x19
    bl skip_whitespace
    mov x19, x0

    // Check if we're at end or newline
    cmp x19, x20
    b.ge parse_line_done
    ldrb w0, [x19]
    cmp w0, #'\n'
    b.eq parse_line_done
    cmp w0, #0
    b.eq parse_line_done

    // Parse number
    mov x0, x19
    bl parse_number
    str x1, [x24], #8  // Store level
    mov x19, x0
    add x23, x23, #1
    b parse_number_loop

parse_line_done:
    // Store level count
    str x23, [x21]
    // Move to next report
    mov x0, #MAX_LEVELS
    add x0, x0, #1
    lsl x0, x0, #3
    add x21, x21, x0
    add x22, x22, #1

    // Skip newline
    cmp x19, x20
    b.ge parse_done
    ldrb w0, [x19]
    cmp w0, #'\n'
    b.ne parse_line_loop
    add x19, x19, #1
    b parse_line_loop

parse_done:
    adrp x0, num_reports@PAGE
    add x0, x0, num_reports@PAGEOFF
    str x22, [x0]

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Parse a number from string
// x0 = pointer, returns x0 = new pointer, x1 = number
parse_number:
    mov x1, #0  // Result
    mov x2, #10 // Base
parse_num_loop:
    ldrb w3, [x0]
    cmp w3, #'0'
    b.lt parse_num_done
    cmp w3, #'9'
    b.gt parse_num_done
    sub w3, w3, #'0'
    mul x1, x1, x2
    add x1, x1, x3
    add x0, x0, #1
    b parse_num_loop
parse_num_done:
    ret

// Skip whitespace
// x0 = pointer, returns x0 = new pointer
skip_whitespace:
skip_ws_loop:
    ldrb w1, [x0]
    cmp w1, #' '
    b.eq skip_ws_next
    cmp w1, #'\t'
    b.eq skip_ws_next
    ret
skip_ws_next:
    add x0, x0, #1
    b skip_ws_loop

// Check if report is safe
// x0 = pointer to report (count at [x0], levels starting at [x0+8])
// Returns 1 in x0 if safe, 0 otherwise
is_safe:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0  // Report pointer
    ldr x20, [x19]  // Level count
    cmp x20, #2
    b.lt is_safe_false  // Need at least 2 levels

    add x19, x19, #8  // Point to first level
    ldr x21, [x19]  // First level
    ldr x22, [x19, #8]  // Second level

    // Determine if increasing or decreasing
    cmp x21, x22
    b.eq is_safe_false  // Can't be equal
    b.lt check_increasing

check_decreasing:
    sub x1, x21, x22
    cmp x1, #1
    b.lt is_safe_false
    cmp x1, #3
    b.gt is_safe_false

    mov x2, #2  // Current index
check_dec_loop:
    cmp x2, x20
    b.ge is_safe_true
    ldr x3, [x19, x2, lsl #3]  // Current level
    sub x4, x2, #1
    ldr x4, [x19, x4, lsl #3]  // Previous level
    subs x5, x4, x3  // Diff
    cmp x5, #1
    b.lt is_safe_false
    cmp x5, #3
    b.gt is_safe_false
    add x2, x2, #1
    b check_dec_loop

check_increasing:
    sub x1, x22, x21
    cmp x1, #1
    b.lt is_safe_false
    cmp x1, #3
    b.gt is_safe_false

    mov x2, #2  // Current index
check_inc_loop:
    cmp x2, x20
    b.ge is_safe_true
    ldr x3, [x19, x2, lsl #3]  // Current level
    sub x4, x2, #1
    ldr x4, [x19, x4, lsl #3]  // Previous level
    subs x5, x3, x4  // Diff
    cmp x5, #1
    b.lt is_safe_false
    cmp x5, #3
    b.gt is_safe_false
    add x2, x2, #1
    b check_inc_loop

is_safe_true:
    mov x0, #1
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

is_safe_false:
    mov x0, #0
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Copy report to temp buffer, removing level at index
// x0 = source report pointer, x1 = index to remove
copy_report_remove:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    mov x19, x0  // Source
    mov x20, x1  // Index to remove
    ldr x2, [x19]  // Original count
    add x19, x19, #8  // Point to levels

    adrp x3, temp_report@PAGE
    add x3, x3, temp_report@PAGEOFF
    sub x4, x2, #1  // New count
    str x4, [x3]
    add x3, x3, #8  // Point to levels

    mov x5, #0  // Source index
    mov x6, #0  // Dest index
copy_loop:
    cmp x5, x2
    b.ge copy_done
    cmp x5, x20
    b.eq copy_skip
    ldr x7, [x19, x5, lsl #3]
    str x7, [x3, x6, lsl #3]
    add x6, x6, #1
copy_skip:
    add x5, x5, #1
    b copy_loop

copy_done:
    adrp x0, temp_report@PAGE
    add x0, x0, temp_report@PAGEOFF

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Check if report is safe with dampener
// x0 = pointer to report
// Returns 1 in x0 if safe with dampener, 0 otherwise
is_safe_with_dampener:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0  // Report pointer

    // First check if already safe
    bl is_safe
    cmp x0, #1
    b.eq dampener_true

    // Try removing each level
    ldr x20, [x19]  // Level count
    mov x21, #0  // Try index

dampener_loop:
    cmp x21, x20
    b.ge dampener_false

    mov x0, x19
    mov x1, x21
    bl copy_report_remove

    // Check if safe
    bl is_safe
    cmp x0, #1
    b.eq dampener_true

    add x21, x21, #1
    b dampener_loop

dampener_true:
    mov x0, #1
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

dampener_false:
    mov x0, #0
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 1: Count safe reports
part1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    adrp x19, reports@PAGE
    add x19, x19, reports@PAGEOFF
    adrp x0, num_reports@PAGE
    add x0, x0, num_reports@PAGEOFF
    ldr x20, [x0]
    mov x21, #0  // Safe count

part1_loop:
    cbz x20, part1_done

    mov x0, x19
    bl is_safe
    add x21, x21, x0

    // Move to next report
    mov x0, #MAX_LEVELS
    add x0, x0, #1
    lsl x0, x0, #3
    add x19, x19, x0
    sub x20, x20, #1
    b part1_loop

part1_done:
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 2: Count safe reports with dampener
part2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    adrp x19, reports@PAGE
    add x19, x19, reports@PAGEOFF
    adrp x0, num_reports@PAGE
    add x0, x0, num_reports@PAGEOFF
    ldr x20, [x0]
    mov x22, #0  // Safe count

part2_loop:
    cbz x20, part2_done

    mov x0, x19
    bl is_safe_with_dampener
    add x22, x22, x0

    // Move to next report
    mov x0, #MAX_LEVELS
    add x0, x0, #1
    lsl x0, x0, #3
    add x19, x19, x0
    sub x20, x20, #1
    b part2_loop

part2_done:
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print string
// x0 = string pointer
print_str:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    mov x19, x0
    mov x20, #0
ps_len_loop:
    ldrb w1, [x19, x20]
    cbz w1, ps_write
    add x20, x20, #1
    b ps_len_loop
ps_write:
    movz x16, #0x2000, lsl #16
    movk x16, #0x0004
    mov x0, #STDOUT
    mov x1, x19
    mov x2, x20
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print number
// x0 = number to print
print_number:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    adrp x19, output_buffer@PAGE
    add x19, x19, output_buffer@PAGEOFF
    add x19, x19, #31
    mov w1, #0
    strb w1, [x19]
    mov x20, x0
    mov x2, #10

pn_loop:
    udiv x3, x20, x2
    msub x4, x3, x2, x20
    add w4, w4, #'0'
    sub x19, x19, #1
    strb w4, [x19]
    mov x20, x3
    cbnz x20, pn_loop

    mov x0, x19
    bl print_str

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
