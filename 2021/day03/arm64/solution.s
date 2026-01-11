// ARM64 Assembly solution for AoC 2021 Day 3 - Binary Diagnostic
// macOS syscalls

.global _main
.align 2

.equ STDOUT, 1
.equ MAX_NUMBERS, 1024
.equ NUM_BITS, 12

.data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

.align 3
file_buffer: .skip 16384
numbers: .skip MAX_NUMBERS * 4      // Store parsed binary numbers as 32-bit ints
candidates: .skip MAX_NUMBERS * 4   // Working array for Part 2
number_count: .skip 8               // How many numbers we have
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
    mov x1, #0              // O_RDONLY
    mov x2, #0
    svc #0x80
    cmp x0, #0
    b.lt exit_error
    mov x19, x0             // Save fd

    // Read file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0003
    mov x0, x19
    adrp x1, file_buffer@PAGE
    add x1, x1, file_buffer@PAGEOFF
    mov x2, #16384
    svc #0x80
    mov x20, x0             // Save bytes read

    // Close file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0006
    mov x0, x19
    svc #0x80

    // Parse input into numbers array
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20
    bl parse_input

    // Part 1: Calculate power consumption (gamma * epsilon)
    bl part1
    mov x21, x0             // Save Part 1 result

    // Print Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_str
    mov x0, x21
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Part 2: Calculate life support rating (oxygen * co2)
    bl part2
    mov x22, x0             // Save Part 2 result

    // Print Part 2
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_str
    mov x0, x22
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

// Parse input into array of binary numbers as integers
// Each line is a 12-bit binary number
// x0 = buffer, x1 = length
parse_input:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0             // Buffer pointer
    add x20, x19, x1        // Buffer end
    adrp x21, numbers@PAGE
    add x21, x21, numbers@PAGEOFF
    mov x22, #0             // Number count

parse_loop:
    cmp x19, x20
    b.ge parse_done

    // Skip any whitespace/newlines at start of line
    ldrb w23, [x19]
    cmp w23, #'\n'
    b.eq parse_skip
    cmp w23, #' '
    b.eq parse_skip
    cmp w23, #'\r'
    b.eq parse_skip
    cmp w23, #0
    b.eq parse_done

    // Check if it's a binary digit
    cmp w23, #'0'
    b.lt parse_skip
    cmp w23, #'1'
    b.gt parse_skip

    // Parse a binary number (12 bits)
    mov x24, #0             // Accumulated value
    mov x0, #NUM_BITS       // Bit counter

parse_binary:
    cbz x0, store_number
    ldrb w23, [x19]
    cmp w23, #'0'
    b.lt store_number
    cmp w23, #'1'
    b.gt store_number

    lsl x24, x24, #1        // Shift left
    sub w23, w23, #'0'      // Convert to 0 or 1
    orr x24, x24, x23       // Add bit
    add x19, x19, #1
    sub x0, x0, #1
    b parse_binary

store_number:
    str w24, [x21, x22, lsl #2]
    add x22, x22, #1
    b parse_loop

parse_skip:
    add x19, x19, #1
    b parse_loop

parse_done:
    adrp x0, number_count@PAGE
    add x0, x0, number_count@PAGEOFF
    str x22, [x0]

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 1: Calculate gamma and epsilon, return gamma * epsilon
// gamma = most common bit at each position
// epsilon = least common bit = ~gamma (within NUM_BITS)
part1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    adrp x19, numbers@PAGE
    add x19, x19, numbers@PAGEOFF
    adrp x20, number_count@PAGE
    add x20, x20, number_count@PAGEOFF
    ldr x20, [x20]          // Total count

    mov x21, #0             // gamma result
    mov x22, #NUM_BITS - 1  // Current bit position (11 down to 0)

part1_bit_loop:
    cmp x22, #0
    b.lt part1_calc_result

    // Count ones at this bit position
    mov x23, #0             // ones count
    mov x24, #0             // index
    mov x1, #1
    lsl x1, x1, x22         // bit mask

part1_count_loop:
    cmp x24, x20
    b.ge part1_count_done
    ldr w0, [x19, x24, lsl #2]
    and w0, w0, w1
    cbz w0, part1_count_next
    add x23, x23, #1
part1_count_next:
    add x24, x24, #1
    b part1_count_loop

part1_count_done:
    // If ones >= half, set this bit in gamma
    lsl x0, x23, #1         // ones * 2
    cmp x0, x20             // compare with total (ones*2 >= count means ones >= count/2)
    b.lt part1_next_bit

    mov x0, #1
    lsl x0, x0, x22
    orr x21, x21, x0        // Set bit in gamma

part1_next_bit:
    sub x22, x22, #1
    b part1_bit_loop

part1_calc_result:
    // epsilon = gamma XOR mask (flip all NUM_BITS bits)
    mov x0, #1
    lsl x0, x0, #NUM_BITS
    sub x0, x0, #1          // mask = (1 << NUM_BITS) - 1 = 0xFFF for 12 bits
    eor x22, x21, x0        // epsilon = gamma ^ mask

    mul x0, x21, x22        // Return gamma * epsilon

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 2: Calculate oxygen and CO2 ratings, return oxygen * co2
part2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    // Find oxygen rating (most common bit, prefer 1 on tie)
    mov x0, #1              // use_most_common = true
    bl find_rating
    mov x21, x0             // Save oxygen rating

    // Find CO2 rating (least common bit, prefer 0 on tie)
    mov x0, #0              // use_most_common = false
    bl find_rating
    mov x22, x0             // Save CO2 rating

    mul x0, x21, x22        // Return oxygen * co2

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Find rating based on bit criteria
// x0 = 1 for most common (oxygen), 0 for least common (co2)
// Returns rating in x0
find_rating:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    mov x27, x0             // Save use_most_common flag

    // Copy numbers to candidates array
    adrp x19, numbers@PAGE
    add x19, x19, numbers@PAGEOFF
    adrp x20, candidates@PAGE
    add x20, x20, candidates@PAGEOFF
    adrp x21, number_count@PAGE
    add x21, x21, number_count@PAGEOFF
    ldr x21, [x21]          // Total count
    mov x22, x21            // Candidate count

    mov x0, #0
copy_loop:
    cmp x0, x21
    b.ge copy_done
    ldr w1, [x19, x0, lsl #2]
    str w1, [x20, x0, lsl #2]
    add x0, x0, #1
    b copy_loop
copy_done:

    // Process each bit position from MSB to LSB
    mov x23, #NUM_BITS - 1  // Current bit position (11 down to 0)

bit_loop:
    cmp x22, #1
    b.le rating_found       // Only one candidate left
    cmp x23, #0
    b.lt rating_found       // No more bits to check

    // Count ones at this bit position among candidates
    mov x24, #0             // ones count
    mov x25, #0             // index
    mov x1, #1
    lsl x1, x1, x23         // bit mask
    mov x28, x1             // Save mask

count_ones_loop:
    cmp x25, x22
    b.ge count_ones_done
    ldr w0, [x20, x25, lsl #2]
    and w0, w0, w1
    cbz w0, count_ones_next
    add x24, x24, #1
count_ones_next:
    add x25, x25, #1
    b count_ones_loop

count_ones_done:
    // Determine target bit based on criteria
    // zeros = candidate_count - ones
    sub x26, x22, x24       // zeros count

    // For oxygen (most common, prefer 1): target = 1 if ones >= zeros
    // For co2 (least common, prefer 0): target = 0 if zeros <= ones (i.e., target = 1 if zeros > ones)
    cbnz x27, check_most_common

    // CO2: keep least common, prefer 0 on tie
    // target = 1 if ones < zeros, else 0
    cmp x24, x26
    b.lt target_one
    b target_zero

check_most_common:
    // Oxygen: keep most common, prefer 1 on tie
    // target = 1 if ones >= zeros, else 0
    cmp x24, x26
    b.ge target_one
    b target_zero

target_one:
    mov x26, #1
    b filter_candidates
target_zero:
    mov x26, #0

filter_candidates:
    // Filter candidates: keep only those with target bit at position x23
    mov x24, #0             // Read index
    mov x25, #0             // Write index

filter_loop:
    cmp x24, x22
    b.ge filter_done
    ldr w0, [x20, x24, lsl #2]
    and w1, w0, w28         // Get bit at position

    // Check if bit matches target
    cbz x26, check_zero_bit
    // Target is 1: keep if bit is non-zero
    cbnz w1, keep_candidate
    b next_candidate
check_zero_bit:
    // Target is 0: keep if bit is zero
    cbnz w1, next_candidate
keep_candidate:
    str w0, [x20, x25, lsl #2]
    add x25, x25, #1
next_candidate:
    add x24, x24, #1
    b filter_loop

filter_done:
    mov x22, x25            // Update candidate count
    sub x23, x23, #1        // Move to next bit
    b bit_loop

rating_found:
    ldr w0, [x20]           // Return first (only) candidate

    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
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
