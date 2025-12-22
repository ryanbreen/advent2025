// Day 21: Keypad Conundrum - ARM64 Assembly for macOS
// Simplified implementation with runtime path computation

.global _start
.align 4

.section __DATA,__data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

// Key positions: numeric keypad (11 keys: 0-9, A)
// Format: row, col pairs
.align 2
numeric_pos:
    .byte 3, 1  // '0' at (3,1)
    .byte 2, 0  // '1' at (2,0)
    .byte 2, 1  // '2' at (2,1)
    .byte 2, 2  // '3' at (2,2)
    .byte 1, 0  // '4' at (1,0)
    .byte 1, 1  // '5' at (1,1)
    .byte 1, 2  // '6' at (1,2)
    .byte 0, 0  // '7' at (0,0)
    .byte 0, 1  // '8' at (0,1)
    .byte 0, 2  // '9' at (0,2)
    .byte 3, 2  // 'A' at (3,2)

// Directional keypad (5 keys: ^, A, <, v, >)
// Map: ^=0, A=1, <=2, v=3, >=4
.align 2
dir_pos:
    .byte 0, 1  // '^' at (0,1)
    .byte 0, 2  // 'A' at (0,2)
    .byte 1, 0  // '<' at (1,0)
    .byte 1, 1  // 'v' at (1,1)
    .byte 1, 2  // '>' at (1,2)

.section __DATA,__bss
.align 3
file_buf: .space 256
codes: .space 100
num_codes: .space 8

// Memo table: [from_idx][to_idx][depth][is_numeric]
// from_idx: 0-14 (11 numeric + 5 directional, but we use max 15)
// to_idx: 0-14
// depth: 0-25 (26 values)
// is_numeric: 0-1
// Size: 15 * 15 * 26 * 2 = 11700 entries * 8 bytes = 93600 bytes
memo_valid: .space 11700
memo_values: .space 93600

.section __TEXT,__text

_start:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    // Read input
    bl read_input

    // Clear memo
    bl clear_memo

    // Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_str

    mov w0, #2
    bl solve_all
    bl print_num

    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Clear memo for part 2
    bl clear_memo

    // Part 2
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_str

    mov w0, #25
    bl solve_all
    bl print_num

    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Exit
    mov x0, #0
    mov x16, #1
    svc #0x80

// Read and parse input
read_input:
    stp x29, x30, [sp, #-32]!
    stp x19, x20, [sp, #16]
    mov x29, sp

    // Open file
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #0
    mov x16, #5
    svc #0x80
    mov x19, x0  // fd

    // Read
    mov x0, x19
    adrp x1, file_buf@PAGE
    add x1, x1, file_buf@PAGEOFF
    mov x2, #255
    mov x16, #3
    svc #0x80
    mov x20, x0  // bytes read

    // Close
    mov x0, x19
    mov x16, #6
    svc #0x80

    // Parse codes
    adrp x0, file_buf@PAGE
    add x0, x0, file_buf@PAGEOFF
    adrp x1, codes@PAGE
    add x1, x1, codes@PAGEOFF
    mov x2, #0  // count
    mov x3, #0  // pos in current code

parse_loop:
    cmp x20, #0
    b.le parse_done
    ldrb w4, [x0], #1
    sub x20, x20, #1

    cmp w4, #'\n'
    b.eq next_code
    cmp w4, #'\r'
    b.eq parse_loop

    strb w4, [x1, x3]
    add x3, x3, #1
    b parse_loop

next_code:
    cbz x3, parse_loop
    strb wzr, [x1, x3]
    add x2, x2, #1
    add x1, x1, #10
    mov x3, #0
    b parse_loop

parse_done:
    // Store last code if any
    cbz x3, store_count
    add x2, x2, #1
store_count:
    adrp x0, num_codes@PAGE
    add x0, x0, num_codes@PAGEOFF
    str x2, [x0]

    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #32
    ret

// Clear memo table
clear_memo:
    adrp x0, memo_valid@PAGE
    add x0, x0, memo_valid@PAGEOFF
    mov x1, #11700
1:  cbz x1, 2f
    strb wzr, [x0], #1
    sub x1, x1, #1
    b 1b
2:  ret

// Char to index
// Input: w0 = char
// Output: w0 = index (0-10 for numeric, 0-4 for directional based on context)
char_to_num_idx:
    cmp w0, #'A'
    b.eq 1f
    sub w0, w0, #'0'
    ret
1:  mov w0, #10
    ret

char_to_dir_idx:
    cmp w0, #'^'
    b.eq 1f
    cmp w0, #'A'
    b.eq 2f
    cmp w0, #'<'
    b.eq 3f
    cmp w0, #'v'
    b.eq 4f
    // '>'
    mov w0, #4
    ret
1:  mov w0, #0
    ret
2:  mov w0, #1
    ret
3:  mov w0, #2
    ret
4:  mov w0, #3
    ret

// Get memo index
// w0 = from_idx, w1 = to_idx, w2 = depth, w3 = is_numeric
// Returns x0 = index
get_memo_idx:
    // index = from*15*26*2 + to*26*2 + depth*2 + is_numeric
    mov w4, #780  // 15*26*2
    mul w0, w0, w4
    mov w4, #52   // 26*2
    madd w0, w1, w4, w0
    add w0, w0, w2, lsl #1
    add w0, w0, w3
    ret

// min_presses: compute minimum button presses
// w0 = from_char, w1 = to_char, w2 = depth, w3 = is_numeric
// Returns x0 = cost
min_presses:
    stp x29, x30, [sp, #-96]!
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]
    stp x25, x26, [sp, #64]
    stp x27, x28, [sp, #80]
    mov x29, sp

    mov w19, w0  // from_char
    mov w20, w1  // to_char
    mov w21, w2  // depth
    mov w22, w3  // is_numeric

    // Same char? Just press A
    cmp w19, w20
    b.ne not_same
    cmp w21, #0
    b.eq base_same
    // Recurse for 'A' -> 'A' at depth-1
    mov w0, #'A'
    mov w1, #'A'
    sub w2, w21, #1
    mov w3, #0
    bl min_presses
    b done
base_same:
    mov x0, #1
    b done

not_same:
    // Convert to indices for memo lookup
    cbnz w22, from_numeric
    mov w0, w19
    bl char_to_dir_idx
    mov w23, w0  // from_idx
    mov w0, w20
    bl char_to_dir_idx
    mov w24, w0  // to_idx
    b check_memo
from_numeric:
    mov w0, w19
    bl char_to_num_idx
    mov w23, w0
    mov w0, w20
    bl char_to_num_idx
    mov w24, w0

check_memo:
    // Check memo
    mov w0, w23
    mov w1, w24
    mov w2, w21
    mov w3, w22
    bl get_memo_idx
    mov x25, x0  // memo index

    adrp x1, memo_valid@PAGE
    add x1, x1, memo_valid@PAGEOFF
    ldrb w2, [x1, x25]
    cbz w2, compute

    // Return cached
    adrp x1, memo_values@PAGE
    add x1, x1, memo_values@PAGEOFF
    ldr x0, [x1, x25, lsl #3]
    b done

compute:
    // Get positions
    cbz w22, get_dir_pos

    // Numeric positions
    adrp x0, numeric_pos@PAGE
    add x0, x0, numeric_pos@PAGEOFF
    lsl w1, w23, #1
    ldrb w26, [x0, w1, uxtw]  // from_row
    add w1, w1, #1
    ldrb w27, [x0, w1, uxtw]  // from_col
    lsl w1, w24, #1
    ldrb w28, [x0, w1, uxtw]  // to_row
    add w1, w1, #1
    ldrb w0, [x0, w1, uxtw]   // to_col
    mov w1, w0  // to_col in w1
    // Gap at (3,0)
    mov w2, #3
    mov w3, #0
    b compute_paths

get_dir_pos:
    adrp x0, dir_pos@PAGE
    add x0, x0, dir_pos@PAGEOFF
    lsl w1, w23, #1
    ldrb w26, [x0, w1, uxtw]  // from_row
    add w1, w1, #1
    ldrb w27, [x0, w1, uxtw]  // from_col
    lsl w1, w24, #1
    ldrb w28, [x0, w1, uxtw]  // to_row
    add w1, w1, #1
    ldrb w0, [x0, w1, uxtw]   // to_col
    mov w1, w0
    // Gap at (0,0)
    mov w2, #0
    mov w3, #0

compute_paths:
    // w26=from_r, w27=from_c, w28=to_r, w1=to_c, w2=gap_r, w3=gap_c
    mov w4, w1   // to_col
    sub w5, w28, w26  // dr = to_r - from_r
    sub w6, w4, w27   // dc = to_c - from_c

    // Check path validity BEFORE any path_cost calls
    // VH valid: NOT (from_col == gap_col AND to_row == gap_row)
    mov w7, #1   // vh_valid = true
    cmp w27, w3
    b.ne 1f
    cmp w28, w2
    b.ne 1f
    mov w7, #0   // vh_valid = false
1:
    // HV valid: NOT (from_row == gap_row AND to_col == gap_col)
    mov w8, #1   // hv_valid = true
    cmp w26, w2
    b.ne 2f
    cmp w4, w3
    b.ne 2f
    mov w8, #0   // hv_valid = false
2:

    // Save dr, dc to callee-saved registers (x27, x28 already saved in min_presses)
    // Actually we can save to stack
    stp x5, x6, [sp, #-16]!
    str x8, [sp, #-16]!   // save hv_valid

    // Try both path orderings, compute min cost
    mov x23, #0x7FFFFFFFFFFFFFFF  // best = MAX

    // Path 1: vertical then horizontal
    cbz w7, skip_vh
    mov w0, w5   // dr
    mov w1, w6   // dc
    mov w2, #0   // v-then-h
    bl path_cost
    cmp x0, x23
    csel x23, x0, x23, lt
skip_vh:

    // Restore hv_valid and dr, dc
    ldr x8, [sp], #16
    ldp x5, x6, [sp], #16

    // Path 2: horizontal then vertical
    cbz w8, skip_hv
    mov w0, w5   // dr
    mov w1, w6   // dc
    mov w2, #1   // h-then-v
    bl path_cost
    cmp x0, x23
    csel x23, x0, x23, lt
skip_hv:

    // Store in memo
    adrp x0, memo_valid@PAGE
    add x0, x0, memo_valid@PAGEOFF
    mov w1, #1
    strb w1, [x0, x25]

    adrp x0, memo_values@PAGE
    add x0, x0, memo_values@PAGEOFF
    str x23, [x0, x25, lsl #3]

    mov x0, x23

done:
    ldp x27, x28, [sp, #80]
    ldp x25, x26, [sp, #64]
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #96
    ret

// Compute cost of a path
// w0 = dr, w1 = dc, w2 = order (0=v-then-h, 1=h-then-v)
// Uses w21 (depth), w22 (is_numeric) from caller
// Returns x0 = cost
path_cost:
    stp x29, x30, [sp, #-64]!
    stp x19, x20, [sp, #16]
    stp x23, x24, [sp, #32]
    stp x25, x26, [sp, #48]
    mov x29, sp

    sxtw x19, w0  // dr
    sxtw x20, w1  // dc
    mov w23, w2   // order
    mov x24, #0   // cost
    mov w25, #'A' // current position

    // If depth == 0, just return path length + 1
    cbz w21, base_cost

    // Process moves based on order
    cbz w23, do_vert_first
    b do_horiz_first

do_vert_first:
    // Vertical moves first
    cmp x19, #0
    b.eq do_horiz_second
    b.lt vert_up
vert_down:
    mov w26, #'v'
    b vert_loop
vert_up:
    mov w26, #'^'
    neg x19, x19
vert_loop:
    cbz x19, do_horiz_second
    mov w0, w25
    mov w1, w26
    sub w2, w21, #1
    mov w3, #0
    bl min_presses
    add x24, x24, x0
    mov w25, w26
    sub x19, x19, #1
    b vert_loop

do_horiz_second:
    cmp x20, #0
    b.eq press_a
    b.lt horiz_left2
horiz_right2:
    mov w26, #'>'
    b horiz_loop2
horiz_left2:
    mov w26, #'<'
    neg x20, x20
horiz_loop2:
    cbz x20, press_a
    mov w0, w25
    mov w1, w26
    sub w2, w21, #1
    mov w3, #0
    bl min_presses
    add x24, x24, x0
    mov w25, w26
    sub x20, x20, #1
    b horiz_loop2

do_horiz_first:
    // Horizontal moves first
    cmp x20, #0
    b.eq do_vert_second
    b.lt horiz_left
horiz_right:
    mov w26, #'>'
    b horiz_loop
horiz_left:
    mov w26, #'<'
    neg x20, x20
horiz_loop:
    cbz x20, do_vert_second
    mov w0, w25
    mov w1, w26
    sub w2, w21, #1
    mov w3, #0
    bl min_presses
    add x24, x24, x0
    mov w25, w26
    sub x20, x20, #1
    b horiz_loop

do_vert_second:
    cmp x19, #0
    b.eq press_a
    b.lt vert_up2
vert_down2:
    mov w26, #'v'
    b vert_loop2
vert_up2:
    mov w26, #'^'
    neg x19, x19
vert_loop2:
    cbz x19, press_a
    mov w0, w25
    mov w1, w26
    sub w2, w21, #1
    mov w3, #0
    bl min_presses
    add x24, x24, x0
    mov w25, w26
    sub x19, x19, #1
    b vert_loop2

press_a:
    // Press A
    mov w0, w25
    mov w1, #'A'
    sub w2, w21, #1
    mov w3, #0
    bl min_presses
    add x24, x24, x0
    mov x0, x24
    b path_done

base_cost:
    // depth == 0: cost = |dr| + |dc| + 1
    cmp x19, #0
    cneg x19, x19, lt
    cmp x20, #0
    cneg x20, x20, lt
    add x0, x19, x20
    add x0, x0, #1

path_done:
    ldp x25, x26, [sp, #48]
    ldp x23, x24, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

// Solve one code
// x0 = code ptr, w1 = depth
// Returns x0 = presses * numeric_value
solve_code:
    stp x29, x30, [sp, #-64]!
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]
    mov x29, sp

    mov x19, x0  // code
    mov w20, w1  // depth
    mov x21, #0  // total presses
    mov w22, #'A' // current

code_loop:
    ldrb w0, [x19], #1
    cbz w0, code_done
    cmp w0, #'\n'
    b.eq code_done

    mov w1, w0   // to
    mov w23, w1  // save for later
    mov w0, w22  // from
    mov w2, w20  // depth
    mov w3, #1   // numeric
    bl min_presses
    add x21, x21, x0
    mov w22, w23
    b code_loop

code_done:
    // Get numeric part - go back to start of code
    sub x19, x19, #5  // back 5 bytes (4 chars + null)
    mov x0, #0
    mov x1, #10
num_loop:
    ldrb w2, [x19], #1
    cmp w2, #'0'
    b.lt num_done
    cmp w2, #'9'
    b.gt num_done
    mul x0, x0, x1
    sub w2, w2, #'0'
    add x0, x0, w2, uxtw
    b num_loop

num_done:
    mul x0, x21, x0

    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

// Solve all codes
// w0 = depth
// Returns x0 = total
solve_all:
    stp x29, x30, [sp, #-48]!
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    mov x29, sp

    mov w19, w0  // depth
    mov x20, #0  // total

    adrp x21, codes@PAGE
    add x21, x21, codes@PAGEOFF

    adrp x0, num_codes@PAGE
    add x0, x0, num_codes@PAGEOFF
    ldr x22, [x0]

all_loop:
    cbz x22, all_done
    mov x0, x21
    mov w1, w19
    bl solve_code
    add x20, x20, x0
    add x21, x21, #10
    sub x22, x22, #1
    b all_loop

all_done:
    mov x0, x20
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #48
    ret

// Print string
print_str:
    stp x29, x30, [sp, #-32]!
    str x19, [sp, #16]
    mov x29, sp
    mov x19, x0

    // strlen
    mov x1, #0
1:  ldrb w2, [x19, x1]
    cbz w2, 2f
    add x1, x1, #1
    b 1b

2:  mov x2, x1
    mov x0, #1
    mov x1, x19
    mov x16, #4
    svc #0x80

    ldr x19, [sp, #16]
    ldp x29, x30, [sp], #32
    ret

// Print number
print_num:
    stp x29, x30, [sp, #-48]!
    str x19, [sp, #16]
    mov x29, sp

    mov x19, x0
    sub sp, sp, #32
    add x1, sp, #31
    strb wzr, [x1]

    cbz x19, zero

    mov x2, #10
1:  cbz x19, print
    udiv x3, x19, x2
    msub x4, x3, x2, x19
    add w4, w4, #'0'
    sub x1, x1, #1
    strb w4, [x1]
    mov x19, x3
    b 1b

zero:
    sub x1, x1, #1
    mov w2, #'0'
    strb w2, [x1]

print:
    mov x0, x1
    bl print_str

    add sp, sp, #32
    ldr x19, [sp, #16]
    ldp x29, x30, [sp], #48
    ret
