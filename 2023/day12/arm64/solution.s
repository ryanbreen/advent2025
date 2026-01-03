// ============================================================================
// Day 12: Hot Springs - ARM64 Assembly for macOS
// ============================================================================
//
// Algorithm Overview:
// ------------------
// This solution uses memoized dynamic programming to count valid arrangements
// of operational (.) and damaged (#) springs.
//
// DP State: (position, group_index, current_run_length)
// - position: current index in pattern string
// - group_index: which group we're currently trying to match
// - current_run: length of current run of # characters
//
// Part 1: Process patterns as-is
// Part 2: Unfold by repeating pattern 5 times (with ? separator) and groups 5 times
//
// Memory Layout for Memoization:
// - Max pattern length: 120 (Part 2: ~105 chars unfolded)
// - Max groups: 50 (Part 2: up to 30 groups * 5 = 150, but most lines have <10)
// - Max run length: 20 (group sizes rarely exceed this)
// - Memo table: 120 * 50 * 25 = 150,000 entries per line
// - Each entry: 8 bytes (value) + sentinel for "not computed" = -1
//
// ============================================================================

.global _start
.align 4

// ============================================================================
// Constants
// ============================================================================
.equ BUFFER_SIZE, 65536
.equ MAX_PATTERN, 130          // Max pattern length (unfolded)
.equ MAX_GROUPS, 60            // Max number of groups (unfolded)
.equ MAX_RUN, 25               // Max run length + 1
.equ MEMO_SIZE, 195000         // MAX_PATTERN * MAX_GROUPS * MAX_RUN = ~195K entries
.equ MEMO_BYTES, 1560000       // MEMO_SIZE * 8 bytes

// System call numbers (macOS ARM64)
.equ SYS_EXIT, 1
.equ SYS_READ, 3
.equ SYS_WRITE, 4
.equ SYS_OPEN, 5
.equ SYS_CLOSE, 6

// Sentinel value for "not computed" in memo
.equ NOT_COMPUTED, -2

// ============================================================================
// Macros
// ============================================================================

.macro load_addr reg, symbol
    adrp    \reg, \symbol@PAGE
    add     \reg, \reg, \symbol@PAGEOFF
.endm

.macro load_word reg, symbol, tmp
    adrp    \tmp, \symbol@PAGE
    add     \tmp, \tmp, \symbol@PAGEOFF
    ldr     \reg, [\tmp]
.endm

.macro store_word reg, symbol, tmp
    adrp    \tmp, \symbol@PAGE
    add     \tmp, \tmp, \symbol@PAGEOFF
    str     \reg, [\tmp]
.endm

// ============================================================================
// Data Section
// ============================================================================
.data
filename:       .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"

.align 4
buffer:         .space BUFFER_SIZE
pattern:        .space MAX_PATTERN      // Current pattern being processed
unfolded_pat:   .space MAX_PATTERN      // Unfolded pattern for Part 2
groups:         .space MAX_GROUPS * 4   // Array of group sizes (32-bit each)
unfolded_grp:   .space MAX_GROUPS * 4   // Unfolded groups for Part 2
memo:           .space MEMO_BYTES       // Memoization table
num_buffer:     .space 32               // For number to string conversion

// Global variables
pattern_len:    .quad 0
num_groups:     .quad 0
total_sum:      .quad 0

// ============================================================================
// Text Section
// ============================================================================
.text

_start:
    // Save frame pointer and link register
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Open input file
    mov     x16, #SYS_OPEN
    load_addr x0, filename
    mov     x1, #0              // O_RDONLY
    mov     x2, #0
    svc     #0x80

    // Check for error
    cmp     x0, #0
    b.lt    exit_error

    mov     x19, x0             // Save file descriptor

    // Read file contents
    mov     x16, #SYS_READ
    mov     x0, x19
    load_addr x1, buffer
    mov     x2, #BUFFER_SIZE
    svc     #0x80

    mov     x20, x0             // Save bytes read

    // Close file
    mov     x16, #SYS_CLOSE
    mov     x0, x19
    svc     #0x80

    // Null-terminate buffer
    load_addr x0, buffer
    strb    wzr, [x0, x20]

    // Part 1
    mov     x0, #0              // unfold = false
    bl      process_all_lines
    mov     x21, x0             // Save Part 1 result

    // Print Part 1
    load_addr x0, part1_msg
    mov     x1, #8
    bl      print_str

    mov     x0, x21
    bl      print_num

    load_addr x0, newline
    mov     x1, #1
    bl      print_str

    // Part 2
    mov     x0, #1              // unfold = true
    bl      process_all_lines
    mov     x22, x0             // Save Part 2 result

    // Print Part 2
    load_addr x0, part2_msg
    mov     x1, #8
    bl      print_str

    mov     x0, x22
    bl      print_num

    load_addr x0, newline
    mov     x1, #1
    bl      print_str

    // Exit success
    mov     x0, #0
    mov     x16, #SYS_EXIT
    svc     #0x80

exit_error:
    mov     x0, #1
    mov     x16, #SYS_EXIT
    svc     #0x80

// ============================================================================
// process_all_lines: Process all lines and return total sum
// Input: x0 = unfold flag (0 = Part 1, 1 = Part 2)
// Output: x0 = total sum of arrangements
// ============================================================================
process_all_lines:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    mov     x19, x0             // unfold flag
    load_addr x20, buffer       // current position in buffer
    mov     x21, #0             // total sum

process_line_loop:
    ldrb    w0, [x20]
    cbz     w0, process_done    // End of buffer

    // Skip blank lines
    cmp     w0, #10
    b.ne    process_not_blank
    add     x20, x20, #1
    b       process_line_loop

process_not_blank:
    // Parse the line: pattern + groups
    mov     x0, x20
    bl      parse_line
    mov     x20, x0             // Update position to after this line

    // If unfold, expand pattern and groups
    cbz     x19, process_no_unfold

    bl      unfold_data
    b       process_count

process_no_unfold:
    // Copy pattern to unfolded_pat (no change)
    load_addr x0, pattern
    load_addr x1, unfolded_pat
    load_word x2, pattern_len, x3
copy_pattern_loop:
    cbz     x2, copy_pattern_done
    ldrb    w3, [x0], #1
    strb    w3, [x1], #1
    sub     x2, x2, #1
    b       copy_pattern_loop
copy_pattern_done:
    strb    wzr, [x1]           // Null terminate

    // Copy groups to unfolded_grp (no change)
    load_addr x0, groups
    load_addr x1, unfolded_grp
    load_word x2, num_groups, x3
    lsl     x2, x2, #2          // * 4 bytes
copy_groups_loop:
    cbz     x2, copy_groups_done
    ldrb    w3, [x0], #1
    strb    w3, [x1], #1
    sub     x2, x2, #1
    b       copy_groups_loop
copy_groups_done:

process_count:
    // Clear memo table
    bl      clear_memo

    // Count arrangements using DP
    bl      count_arrangements

    // Add to total
    add     x21, x21, x0

    b       process_line_loop

process_done:
    mov     x0, x21

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// parse_line: Parse a line into pattern and groups
// Input: x0 = pointer to start of line
// Output: x0 = pointer to start of next line
// Sets: pattern, pattern_len, groups, num_groups
// ============================================================================
parse_line:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0             // Current position
    load_addr x20, pattern      // Pattern destination
    mov     x21, #0             // Pattern length

    // Read pattern until space
parse_pattern_loop:
    ldrb    w0, [x19]
    cmp     w0, #' '
    b.eq    parse_pattern_done
    cmp     w0, #10
    b.eq    parse_pattern_done
    cbz     w0, parse_pattern_done

    strb    w0, [x20, x21]
    add     x21, x21, #1
    add     x19, x19, #1
    b       parse_pattern_loop

parse_pattern_done:
    strb    wzr, [x20, x21]     // Null terminate
    store_word x21, pattern_len, x0

    // Skip space
    ldrb    w0, [x19]
    cmp     w0, #' '
    b.ne    parse_groups_start
    add     x19, x19, #1

parse_groups_start:
    load_addr x20, groups       // Groups destination
    mov     x21, #0             // Number of groups
    mov     x22, #0             // Current number

parse_groups_loop:
    ldrb    w0, [x19]
    cmp     w0, #10
    b.eq    parse_groups_done
    cbz     w0, parse_groups_done

    cmp     w0, #','
    b.eq    parse_groups_comma

    // Digit
    sub     w0, w0, #'0'
    mov     w1, #10
    mul     w22, w22, w1
    add     w22, w22, w0
    add     x19, x19, #1
    b       parse_groups_loop

parse_groups_comma:
    // Store current number
    str     w22, [x20, x21, lsl #2]
    add     x21, x21, #1
    mov     x22, #0
    add     x19, x19, #1
    b       parse_groups_loop

parse_groups_done:
    // Store last number
    str     w22, [x20, x21, lsl #2]
    add     x21, x21, #1
    store_word x21, num_groups, x0

    // Skip newline
    ldrb    w0, [x19]
    cmp     w0, #10
    b.ne    parse_return
    add     x19, x19, #1

parse_return:
    mov     x0, x19

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// unfold_data: Unfold pattern and groups for Part 2
// Pattern becomes: pattern?pattern?pattern?pattern?pattern
// Groups become: groups repeated 5 times
// ============================================================================
unfold_data:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    load_addr x19, pattern
    load_addr x20, unfolded_pat
    load_word x21, pattern_len, x0
    mov     x22, #0             // dest index
    mov     x23, #5             // repeat count

unfold_pattern_repeat:
    cbz     x23, unfold_pattern_done

    // Copy pattern
    mov     x24, #0
unfold_pattern_copy:
    cmp     x24, x21
    b.ge    unfold_pattern_sep
    ldrb    w0, [x19, x24]
    strb    w0, [x20, x22]
    add     x22, x22, #1
    add     x24, x24, #1
    b       unfold_pattern_copy

unfold_pattern_sep:
    sub     x23, x23, #1
    cbz     x23, unfold_pattern_done
    // Add separator '?'
    mov     w0, #'?'
    strb    w0, [x20, x22]
    add     x22, x22, #1
    b       unfold_pattern_repeat

unfold_pattern_done:
    strb    wzr, [x20, x22]     // Null terminate
    store_word x22, pattern_len, x0

    // Unfold groups
    load_addr x19, groups
    load_addr x20, unfolded_grp
    load_word x21, num_groups, x0
    mov     x22, #0             // dest index
    mov     x23, #5             // repeat count

unfold_groups_repeat:
    cbz     x23, unfold_groups_done

    mov     x24, #0
unfold_groups_copy:
    cmp     x24, x21
    b.ge    unfold_groups_next
    ldr     w0, [x19, x24, lsl #2]
    str     w0, [x20, x22, lsl #2]
    add     x22, x22, #1
    add     x24, x24, #1
    b       unfold_groups_copy

unfold_groups_next:
    sub     x23, x23, #1
    b       unfold_groups_repeat

unfold_groups_done:
    store_word x22, num_groups, x0

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// clear_memo: Clear memoization table to NOT_COMPUTED (-2)
// Optimized: 4x unrolled loop with paired stores (stp)
// Each stp writes 16 bytes (2 qwords), 4 stp = 8 qwords per iteration
// 195000 / 8 = 24375 iterations
// ============================================================================
clear_memo:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    load_addr x19, memo
    mov     x0, #NOT_COMPUTED
    movz    x20, #24375

clear_memo_loop:
    cbz     x20, clear_memo_done
    stp     x0, x0, [x19], #16
    stp     x0, x0, [x19], #16
    stp     x0, x0, [x19], #16
    stp     x0, x0, [x19], #16
    sub     x20, x20, #1
    b       clear_memo_loop

clear_memo_done:
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// count_arrangements: Main DP function to count valid arrangements
// Uses memoization with state (pos, group_idx, current_run)
// Output: x0 = number of valid arrangements
// ============================================================================
count_arrangements:
    stp     x29, x30, [sp, #-16]!

    // Start DP with pos=0, group_idx=0, current_run=0
    mov     x0, #0
    mov     x1, #0
    mov     x2, #0
    bl      dp

    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// dp: Recursive DP function with memoization
// Input: x0 = pos, x1 = group_idx, x2 = current_run
// Output: x0 = number of arrangements
// ============================================================================
dp:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0             // pos
    mov     x20, x1             // group_idx
    mov     x21, x2             // current_run

    // Check memo first
    // memo_idx = pos * (MAX_GROUPS * MAX_RUN) + group_idx * MAX_RUN + current_run
    mov     x0, #MAX_GROUPS
    mov     x1, #MAX_RUN
    mul     x0, x0, x1          // MAX_GROUPS * MAX_RUN
    mul     x22, x19, x0        // pos * (MAX_GROUPS * MAX_RUN)
    mul     x0, x20, x1         // group_idx * MAX_RUN
    add     x22, x22, x0
    add     x22, x22, x21       // + current_run
    lsl     x22, x22, #3        // * 8 bytes

    load_addr x23, memo
    ldr     x0, [x23, x22]
    mov     x1, #NOT_COMPUTED
    cmp     x0, x1
    b.ne    dp_return_memo

    // Load pattern length and num_groups
    load_word x24, pattern_len, x0
    load_word x25, num_groups, x0

    // Base case: reached end of pattern
    cmp     x19, x24
    b.ne    dp_not_end

    // At end: Valid if all groups matched and no partial run
    cmp     x20, x25
    b.ne    dp_check_last_group
    cmp     x21, #0
    b.ne    dp_return_zero
    mov     x0, #1
    b       dp_store_return

dp_check_last_group:
    // Or if on last group and run matches
    sub     x0, x25, #1
    cmp     x20, x0
    b.ne    dp_return_zero
    load_addr x0, unfolded_grp
    ldr     w0, [x0, x20, lsl #2]
    cmp     x21, x0
    b.ne    dp_return_zero
    mov     x0, #1
    b       dp_store_return

dp_return_zero:
    mov     x0, #0
    b       dp_store_return

dp_not_end:
    // Get current character
    load_addr x26, unfolded_pat
    ldrb    w27, [x26, x19]

    mov     x28, #0             // result accumulator

    // Option 1: Place operational spring (.)
    cmp     w27, #'.'
    b.eq    dp_try_dot
    cmp     w27, #'?'
    b.ne    dp_skip_dot

dp_try_dot:
    cmp     x21, #0
    b.ne    dp_try_end_run

    // No active run, just move forward
    add     x0, x19, #1
    mov     x1, x20
    mov     x2, #0
    bl      dp
    add     x28, x28, x0
    b       dp_skip_dot

dp_try_end_run:
    // Have active run - try to end it if it matches current group
    cmp     x20, x25
    b.ge    dp_skip_dot         // No more groups

    load_addr x0, unfolded_grp
    ldr     w0, [x0, x20, lsl #2]
    cmp     x21, x0
    b.ne    dp_skip_dot         // Run doesn't match group

    // End current run
    add     x0, x19, #1
    add     x1, x20, #1
    mov     x2, #0
    bl      dp
    add     x28, x28, x0

dp_skip_dot:
    // Option 2: Place damaged spring (#)
    cmp     w27, #'#'
    b.eq    dp_try_hash
    cmp     w27, #'?'
    b.ne    dp_skip_hash

dp_try_hash:
    // Check if we can extend current run
    cmp     x20, x25
    b.ge    dp_skip_hash        // No more groups

    load_addr x0, unfolded_grp
    ldr     w0, [x0, x20, lsl #2]
    cmp     x21, x0
    b.ge    dp_skip_hash        // Would exceed group size

    // Extend current run
    add     x0, x19, #1
    mov     x1, x20
    add     x2, x21, #1
    bl      dp
    add     x28, x28, x0

dp_skip_hash:
    mov     x0, x28

dp_store_return:
    // Store in memo
    str     x0, [x23, x22]

dp_return_memo:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print a string to stdout
// Input: x0 = string pointer, x1 = length
// ============================================================================
print_str:
    mov     x2, x1
    mov     x1, x0
    mov     x0, #1
    mov     x16, #SYS_WRITE
    svc     #0x80
    ret

// ============================================================================
// print_num: Print a decimal number to stdout
// Input: x0 = number to print
// ============================================================================
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0
    load_addr x20, num_buffer
    add     x20, x20, #30       // End of buffer
    mov     x1, #0              // Length

    // Handle 0 case
    cbnz    x19, print_num_loop
    mov     w2, #'0'
    sub     x20, x20, #1
    strb    w2, [x20]
    mov     x1, #1
    b       print_num_output

print_num_loop:
    cbz     x19, print_num_output
    mov     x2, #10
    udiv    x3, x19, x2
    msub    x4, x3, x2, x19     // remainder
    add     w4, w4, #'0'
    sub     x20, x20, #1
    strb    w4, [x20]
    add     x1, x1, #1
    mov     x19, x3
    b       print_num_loop

print_num_output:
    mov     x0, x20
    bl      print_str

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
