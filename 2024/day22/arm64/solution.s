// Day 22: Monkey Market - ARM64 Assembly for macOS
// Pseudorandom number generation and price sequence optimization

.global _main
.align 4

// Constants
.equ SYS_EXIT,   0x2000001
.equ SYS_READ,   0x2000003
.equ SYS_WRITE,  0x2000004
.equ SYS_OPEN,   0x2000005

.equ O_RDONLY,   0x0000
.equ MASK_24BIT, 0xFFFFFF
.equ ITERATIONS, 2000
.equ MAX_BUYERS, 4096
.equ SEQUENCE_SIZE, 1048576 // 2^20 for bit-packed sequences (5 bits each, 4 changes)
.equ BITMAP_SIZE, 131072    // SEQUENCE_SIZE / 8

// Data section
.data
.align 8

input_path:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"

// BSS section - uninitialized data
.bss
.align 8

file_buffer:    .space 65536           // Input file buffer
buyers:         .space MAX_BUYERS * 8  // Initial secret numbers (64-bit)
buyer_count:    .space 8               // Number of buyers
part1_result:   .space 8               // Part 1 answer
part2_result:   .space 8               // Part 2 answer
output_buffer:  .space 32              // For number to string conversion
secrets:        .space 2001 * 8        // Secrets for one buyer (2001 numbers)
prices:         .space 2001 * 4        // Prices (last digit) for one buyer
changes:        .space 2000 * 4        // Price changes (signed 32-bit)
sequence_totals: .space SEQUENCE_SIZE * 8  // Total bananas per sequence (64-bit)
seen_bitmap:    .space BITMAP_SIZE  // Bitmap for seen sequences (1 bit per sequence)

// Text section
.text

// Main entry point
_main:
    // Save frame pointer and link register
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Open and read input file
    bl      read_input_file

    // Parse input into buyers array
    bl      parse_input

    // Part 1: Sum of 2000th secret for each buyer
    bl      solve_part1

    // Part 2: Find best 4-change sequence
    bl      solve_part2

    // Print results
    bl      print_results

    // Exit
    mov     x0, #0
    mov     x16, #1
    orr     x16, x16, #0x2000000
    svc     #0

// Read input file into buffer
// Returns: x0 = bytes read
read_input_file:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // open(path, O_RDONLY)
    adrp    x0, input_path@PAGE
    add     x0, x0, input_path@PAGEOFF
    mov     x1, #O_RDONLY
    mov     x2, #0
    mov     x16, #5
    orr     x16, x16, #0x2000000
    svc     #0

    // Check for error
    cmp     x0, #0
    b.lt    read_error
    mov     x19, x0          // Save fd

    // read(fd, buffer, size)
    mov     x0, x19
    adrp    x1, file_buffer@PAGE
    add     x1, x1, file_buffer@PAGEOFF
    mov     x2, #65536
    mov     x16, #3
    orr     x16, x16, #0x2000000
    svc     #0

    mov     x20, x0          // Save bytes read

    // Close file (we ignore errors here)
    mov     x0, x19

    mov     x0, x20          // Return bytes read

read_error:
    ldp     x29, x30, [sp], #16
    ret

// Parse input buffer into buyers array
// Input: file_buffer contains text, x0 = length
parse_input:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]

    adrp    x19, file_buffer@PAGE
    add     x19, x19, file_buffer@PAGEOFF
    mov     x20, x0          // Length
    mov     x21, #0          // Buyer index
    mov     x22, #0          // Current number

    adrp    x9, buyers@PAGE
    add     x9, x9, buyers@PAGEOFF

parse_loop:
    cbz     x20, parse_done

    ldrb    w10, [x19], #1
    sub     x20, x20, #1

    // Check for newline
    cmp     w10, #'\n'
    b.eq    parse_newline

    // Check for digit
    cmp     w10, #'0'
    b.lt    parse_loop
    cmp     w10, #'9'
    b.gt    parse_loop

    // Digit: accumulate
    sub     w10, w10, #'0'
    mov     x11, #10
    mul     x22, x22, x11
    add     x22, x22, x10
    b       parse_loop

parse_newline:
    // Store current number if non-zero
    cbz     x22, parse_loop

    str     x22, [x9, x21, lsl #3]
    add     x21, x21, #1
    mov     x22, #0
    b       parse_loop

parse_done:
    // Store last number if non-zero
    cbz     x22, parse_skip_last
    str     x22, [x9, x21, lsl #3]
    add     x21, x21, #1

parse_skip_last:
    // Store buyer count
    adrp    x9, buyer_count@PAGE
    add     x9, x9, buyer_count@PAGEOFF
    str     x21, [x9]

    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret

// Generate next secret number
// Input: x0 = current secret
// Output: x0 = next secret
// Preserves: All registers except x0, x1, x2
next_secret:
    mov     x2, #MASK_24BIT

    // Step 1: secret ^= (secret << 6); secret &= MASK
    lsl     x1, x0, #6
    eor     x0, x0, x1
    and     x0, x0, x2

    // Step 2: secret ^= (secret >> 5); secret &= MASK
    lsr     x1, x0, #5
    eor     x0, x0, x1
    and     x0, x0, x2

    // Step 3: secret ^= (secret << 11); secret &= MASK
    lsl     x1, x0, #11
    eor     x0, x0, x1
    and     x0, x0, x2

    ret

// Part 1: Sum of 2000th secret for each buyer
solve_part1:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]

    // Load buyer count
    adrp    x19, buyer_count@PAGE
    add     x19, x19, buyer_count@PAGEOFF
    ldr     x19, [x19]

    adrp    x20, buyers@PAGE
    add     x20, x20, buyers@PAGEOFF

    mov     x21, #0          // Sum
    mov     x22, #0          // Buyer index

part1_buyer_loop:
    cmp     x22, x19
    b.ge    part1_done

    // Load initial secret
    ldr     x0, [x20, x22, lsl #3]

    // Generate 2000 secrets
    mov     x9, #ITERATIONS
part1_secret_loop:
    cbz     x9, part1_buyer_done
    bl      next_secret
    sub     x9, x9, #1
    b       part1_secret_loop

part1_buyer_done:
    // Add to sum
    add     x21, x21, x0
    add     x22, x22, #1
    b       part1_buyer_loop

part1_done:
    // Store result
    adrp    x9, part1_result@PAGE
    add     x9, x9, part1_result@PAGEOFF
    str     x21, [x9]

    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret

// Part 2: Find best 4-change sequence
solve_part2:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    // Zero out sequence_totals array (required for correct results)
    adrp    x0, sequence_totals@PAGE
    add     x0, x0, sequence_totals@PAGEOFF
    // SEQUENCE_SIZE * 8 = 1048576 * 8 = 8388608 bytes = 0x800000
    mov     x1, #8388608
    bl      memzero

    // Load buyer count
    adrp    x19, buyer_count@PAGE
    add     x19, x19, buyer_count@PAGEOFF
    ldr     x19, [x19]

    adrp    x20, buyers@PAGE
    add     x20, x20, buyers@PAGEOFF

    mov     x21, #0          // Buyer index

part2_buyer_loop:
    cmp     x21, x19
    b.ge    part2_find_max

    // Generate secrets for this buyer
    ldr     x0, [x20, x21, lsl #3]
    bl      generate_buyer_secrets

    // Convert to prices and changes
    bl      compute_prices_changes

    // Process 4-change sequences for this buyer
    bl      process_sequences

    add     x21, x21, #1
    b       part2_buyer_loop

part2_find_max:
    // Find maximum value in sequence_totals
    adrp    x0, sequence_totals@PAGE
    add     x0, x0, sequence_totals@PAGEOFF
    movz    x1, #SEQUENCE_SIZE & 0xFFFF
    movk    x1, #(SEQUENCE_SIZE >> 16) & 0xFFFF, lsl #16
    bl      find_max_in_array

    // Store result
    adrp    x9, part2_result@PAGE
    add     x9, x9, part2_result@PAGEOFF
    str     x0, [x9]

    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// Generate 2001 secrets for a buyer
// Input: x0 = initial secret
generate_buyer_secrets:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]

    adrp    x19, secrets@PAGE
    add     x19, x19, secrets@PAGEOFF

    // Store initial
    str     x0, [x19]

    mov     x20, #ITERATIONS
    mov     x9, #8

gen_secrets_loop:
    cbz     x20, gen_secrets_done

    bl      next_secret
    str     x0, [x19, x9]
    add     x9, x9, #8
    sub     x20, x20, #1
    b       gen_secrets_loop

gen_secrets_done:
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// Compute prices (last digit) and changes
compute_prices_changes:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]

    adrp    x19, secrets@PAGE
    add     x19, x19, secrets@PAGEOFF

    adrp    x20, prices@PAGE
    add     x20, x20, prices@PAGEOFF

    // Compute prices (secret % 10)
    mov     x9, #0
price_loop:
    cmp     x9, #2001
    b.ge    price_done

    ldr     x10, [x19, x9, lsl #3]
    mov     x11, #10
    udiv    x12, x10, x11
    msub    w13, w12, w11, w10  // w13 = w10 - (w12 * w11) = w10 % 10
    str     w13, [x20, x9, lsl #2]

    add     x9, x9, #1
    b       price_loop

price_done:
    // Compute changes
    adrp    x20, prices@PAGE
    add     x20, x20, prices@PAGEOFF

    adrp    x21, changes@PAGE
    add     x21, x21, changes@PAGEOFF

    mov     x9, #0
change_loop:
    cmp     x9, #2000
    b.ge    change_done

    ldr     w10, [x20, x9, lsl #2]       // prices[i]
    add     x12, x9, #1
    ldr     w11, [x20, x12, lsl #2]      // prices[i+1]
    sub     w13, w11, w10                 // change = prices[i+1] - prices[i]
    str     w13, [x21, x9, lsl #2]

    add     x9, x9, #1
    b       change_loop

change_done:
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret

// Process 4-change sequences for current buyer
process_sequences:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    // Zero out seen bitmap
    adrp    x0, seen_bitmap@PAGE
    add     x0, x0, seen_bitmap@PAGEOFF
    mov     x1, #BITMAP_SIZE
    bl      memzero

    adrp    x19, changes@PAGE
    add     x19, x19, changes@PAGEOFF

    adrp    x20, prices@PAGE
    add     x20, x20, prices@PAGEOFF

    adrp    x21, seen_bitmap@PAGE
    add     x21, x21, seen_bitmap@PAGEOFF

    adrp    x22, sequence_totals@PAGE
    add     x22, x22, sequence_totals@PAGEOFF

    mov     x23, #0          // i

seq_loop:
    cmp     x23, #1997       // Need 1997 iterations (0..1996)
    b.ge    seq_done

    // Load 4 changes (with sign extension)
    ldrsw   x0, [x19, x23, lsl #2]
    add     x9, x23, #1
    ldrsw   x1, [x19, x9, lsl #2]
    add     x9, x23, #2
    ldrsw   x2, [x19, x9, lsl #2]
    add     x9, x23, #3
    ldrsw   x3, [x19, x9, lsl #2]

    // Pack into index using bit packing (5 bits per change)
    // index = ((c0+9) << 15) | ((c1+9) << 10) | ((c2+9) << 5) | (c3+9)
    add     x0, x0, #9
    add     x1, x1, #9
    add     x2, x2, #9
    add     x3, x3, #9

    lsl     x10, x0, #15    // c0 << 15
    lsl     x9, x1, #10     // c1 << 10
    orr     x10, x10, x9
    lsl     x9, x2, #5      // c2 << 5
    orr     x10, x10, x9
    orr     x10, x10, x3    // | c3

    // Check if seen
    lsr     x11, x10, #3    // byte index = index / 8
    and     x12, x10, #7    // bit index = index % 8
    ldrb    w13, [x21, x11]
    lsr     w13, w13, w12
    tst     w13, #1
    b.ne    seq_next

    // Mark as seen
    mov     w14, #1
    lsl     w14, w14, w12
    ldrb    w13, [x21, x11]
    orr     w13, w13, w14
    strb    w13, [x21, x11]

    // Add price to sequence_totals
    add     x9, x23, #4
    ldr     w15, [x20, x9, lsl #2]    // price at i+4
    uxtw    x15, w15                  // zero-extend to 64-bit
    ldr     x16, [x22, x10, lsl #3]   // current total
    add     x16, x16, x15
    str     x16, [x22, x10, lsl #3]

seq_next:
    add     x23, x23, #1
    b       seq_loop

seq_done:
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// Zero memory
// Input: x0 = address, x1 = size (in bytes)
memzero:
    cbz     x1, memzero_done

    // Zero in 8-byte chunks
    lsr     x2, x1, #3       // x2 = count of 8-byte chunks
    cbz     x2, memzero_remainder

memzero_loop:
    str     xzr, [x0], #8
    subs    x2, x2, #1
    b.ne    memzero_loop

memzero_remainder:
    // Handle remaining bytes
    and     x1, x1, #7
    cbz     x1, memzero_done

memzero_byte_loop:
    strb    wzr, [x0], #1
    subs    x1, x1, #1
    b.ne    memzero_byte_loop

memzero_done:
    ret

// Find maximum value in array
// Input: x0 = array address, x1 = count
// Output: x0 = max value
find_max_in_array:
    mov     x9, #0           // max value
    mov     x10, #0          // index

find_max_loop:
    cmp     x10, x1
    b.ge    find_max_done

    ldr     x11, [x0, x10, lsl #3]
    cmp     x11, x9
    csel    x9, x11, x9, gt

    add     x10, x10, #1
    b       find_max_loop

find_max_done:
    mov     x0, x9
    ret

// Print results
print_results:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Print "Part 1: "
    adrp    x0, part1_msg@PAGE
    add     x0, x0, part1_msg@PAGEOFF
    bl      print_string

    // Print part 1 result
    adrp    x9, part1_result@PAGE
    add     x9, x9, part1_result@PAGEOFF
    ldr     x0, [x9]
    bl      print_number

    // Print newline
    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    // Print "Part 2: "
    adrp    x0, part2_msg@PAGE
    add     x0, x0, part2_msg@PAGEOFF
    bl      print_string

    // Print part 2 result
    adrp    x9, part2_result@PAGE
    add     x9, x9, part2_result@PAGEOFF
    ldr     x0, [x9]
    bl      print_number

    // Print newline
    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    ldp     x29, x30, [sp], #16
    ret

// Print null-terminated string
// Input: x0 = string address
print_string:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    str     x19, [sp, #16]

    mov     x19, x0

    // Calculate length
    mov     x1, x0
strlen_loop:
    ldrb    w2, [x1], #1
    cbz     w2, strlen_done
    b       strlen_loop
strlen_done:
    sub     x2, x1, x0
    sub     x2, x2, #1

    // write(1, string, length)
    mov     x0, #1
    mov     x1, x19
    mov     x16, #4
    orr     x16, x16, #0x2000000
    svc     #0

    ldr     x19, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// Print number
// Input: x0 = number
print_number:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]

    mov     x19, x0

    // Convert to string
    adrp    x20, output_buffer@PAGE
    add     x20, x20, output_buffer@PAGEOFF
    add     x20, x20, #31    // Start at end of buffer
    strb    wzr, [x20]       // Null terminator

    mov     x9, #10
num_to_str_loop:
    sub     x20, x20, #1
    udiv    x10, x19, x9
    msub    x11, x10, x9, x19
    add     w11, w11, #'0'
    strb    w11, [x20]
    mov     x19, x10
    cbnz    x19, num_to_str_loop

    // Print string
    mov     x0, x20
    bl      print_string

    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret
