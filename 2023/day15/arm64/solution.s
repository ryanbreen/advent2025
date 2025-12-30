// =============================================================================
// Advent of Code 2023 - Day 15: Lens Library
// ARM64 Assembly Solution for macOS
//
// Problem: HASH algorithm and HASHMAP procedure
// Part 1: Sum of HASH values for all comma-separated steps
//         HASH: for each char, current = ((current + ASCII) * 17) % 256
// Part 2: HASHMAP procedure with 256 boxes containing ordered lens lists
//         - "label-": remove lens with label from box
//         - "label=N": add/replace lens with focal length N
//         - Focusing power = sum of (box+1) * slot * focal
// =============================================================================

.global _start
.align 4

// -----------------------------------------------------------------------------
// macOS ARM64 BSD syscall numbers (0x2000000 + syscall_number)
// -----------------------------------------------------------------------------
.equ SYSCALL_BASE_HI, 0x2000
.equ SYS_EXIT_LO,     0x0001
.equ SYS_READ_LO,     0x0003
.equ SYS_WRITE_LO,    0x0004
.equ SYS_OPEN_LO,     0x0005
.equ SYS_CLOSE_LO,    0x0006

.equ O_RDONLY, 0x0000

// Character codes
.equ CHAR_COMMA, 44       // ','
.equ CHAR_EQUALS, 61      // '='
.equ CHAR_DASH, 45        // '-'
.equ CHAR_NEWLINE, 10     // '\n'

// Buffer sizes
.equ MAX_FILE_SIZE, 65536
.equ MAX_LENSES_PER_BOX, 64   // max lenses per box
.equ MAX_LABEL_LEN, 8         // max label length

// Box structure:
// Each box has MAX_LENSES_PER_BOX entries
// Each entry: 8 bytes label (null-padded) + 8 bytes focal length (0 = empty)
.equ LENS_SIZE, 16
.equ BOX_SIZE, (MAX_LENSES_PER_BOX * LENS_SIZE)  // 1024 bytes per box
.equ TOTAL_BOX_SIZE, (256 * BOX_SIZE)            // 256 KB for all boxes

// =============================================================================
// DATA SECTION
// =============================================================================
.data
input_path: .asciz "../input.txt"
part1_msg:  .asciz "Part 1: "
part2_msg:  .asciz "Part 2: "
newline:    .asciz "\n"

// =============================================================================
// BSS SECTION
// =============================================================================
.bss
.align 4
file_buffer:    .skip MAX_FILE_SIZE
file_size:      .skip 8
output_buffer:  .skip 32

// 256 boxes, each with space for 64 lenses (label + focal)
boxes:          .skip TOTAL_BOX_SIZE
box_counts:     .skip 256 * 8    // Count of lenses in each box

// Temporary storage for current step
current_label:  .skip 16
current_focal:  .skip 8

// =============================================================================
// TEXT SECTION
// =============================================================================
.text

// -----------------------------------------------------------------------------
// _start: Program entry point
// -----------------------------------------------------------------------------
_start:
    // Open input file
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_OPEN_LO
    adrp    x0, input_path@PAGE
    add     x0, x0, input_path@PAGEOFF
    mov     x1, #O_RDONLY
    mov     x2, #0
    svc     #0
    mov     x19, x0                     // x19 = fd

    // Read file
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_READ_LO
    mov     x0, x19
    adrp    x1, file_buffer@PAGE
    add     x1, x1, file_buffer@PAGEOFF
    mov     x2, #MAX_FILE_SIZE
    svc     #0

    adrp    x1, file_size@PAGE
    add     x1, x1, file_size@PAGEOFF
    str     x0, [x1]

    // Close file
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_CLOSE_LO
    mov     x0, x19
    svc     #0

    // -------------------------------------------------------------------------
    // Part 1: Sum of HASH values for all steps
    // -------------------------------------------------------------------------
    adrp    x0, part1_msg@PAGE
    add     x0, x0, part1_msg@PAGEOFF
    bl      print_str

    bl      solve_part1
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    // -------------------------------------------------------------------------
    // Part 2: HASHMAP procedure
    // -------------------------------------------------------------------------
    adrp    x0, part2_msg@PAGE
    add     x0, x0, part2_msg@PAGEOFF
    bl      print_str

    bl      solve_part2
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    // Exit
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_EXIT_LO
    mov     x0, #0
    svc     #0

// =============================================================================
// hash_string: Compute HASH value for a string
// Input: x0 = start pointer, x1 = end pointer (exclusive)
// Output: x0 = hash value (0-255)
// =============================================================================
hash_string:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0             // start
    mov     x20, x1             // end
    mov     x0, #0              // current = 0

.L_hash_loop:
    cmp     x19, x20
    b.ge    .L_hash_done

    ldrb    w1, [x19]
    add     w0, w0, w1          // current += ASCII
    mov     w2, #17
    mul     w0, w0, w2          // current *= 17
    and     w0, w0, #255        // current %= 256

    add     x19, x19, #1
    b       .L_hash_loop

.L_hash_done:
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// solve_part1: Sum of HASH values for all comma-separated steps
// Output: x0 = total sum
// =============================================================================
solve_part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    adrp    x19, file_buffer@PAGE
    add     x19, x19, file_buffer@PAGEOFF   // x19 = current pos
    adrp    x20, file_size@PAGE
    add     x20, x20, file_size@PAGEOFF
    ldr     x20, [x20]
    add     x20, x19, x20                    // x20 = end of buffer

    mov     x21, #0                          // x21 = total sum
    mov     x22, x19                         // x22 = start of current step

.L_p1_loop:
    cmp     x19, x20
    b.ge    .L_p1_process_last

    ldrb    w23, [x19]

    // Check for comma or newline (end of step)
    cmp     w23, #CHAR_COMMA
    b.eq    .L_p1_end_step
    cmp     w23, #CHAR_NEWLINE
    b.eq    .L_p1_skip_newline

    add     x19, x19, #1
    b       .L_p1_loop

.L_p1_skip_newline:
    // Skip newlines without processing as step
    add     x19, x19, #1
    b       .L_p1_loop

.L_p1_end_step:
    // Hash the step from x22 to x19
    cmp     x22, x19
    b.ge    .L_p1_next_step     // Skip empty steps

    mov     x0, x22
    mov     x1, x19
    bl      hash_string
    add     x21, x21, x0        // total += hash

.L_p1_next_step:
    add     x19, x19, #1        // Skip comma
    mov     x22, x19            // Start of next step
    b       .L_p1_loop

.L_p1_process_last:
    // Process last step if any
    cmp     x22, x20
    b.ge    .L_p1_done

    // Check if last char is newline, adjust end
    sub     x0, x20, #1
    ldrb    w23, [x0]
    cmp     w23, #CHAR_NEWLINE
    b.ne    .L_p1_hash_last
    mov     x20, x0

.L_p1_hash_last:
    cmp     x22, x20
    b.ge    .L_p1_done

    mov     x0, x22
    mov     x1, x20
    bl      hash_string
    add     x21, x21, x0

.L_p1_done:
    mov     x0, x21

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// clear_boxes: Initialize all boxes to empty
// =============================================================================
clear_boxes:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    // Clear box_counts
    adrp    x19, box_counts@PAGE
    add     x19, x19, box_counts@PAGEOFF
    mov     x0, #0
    mov     x1, #256
.L_clear_counts:
    cbz     x1, .L_clear_boxes_data
    str     xzr, [x19], #8
    sub     x1, x1, #1
    b       .L_clear_counts

.L_clear_boxes_data:
    // Clear boxes (optional, but safe)
    // Actually we can rely on counts being 0

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// compare_labels: Compare two null-terminated labels
// Input: x0 = label1, x1 = label2
// Output: x0 = 1 if equal, 0 if not
// =============================================================================
compare_labels:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0
    mov     x20, x1

.L_cmp_loop:
    ldrb    w0, [x19]
    ldrb    w1, [x20]

    cmp     w0, w1
    b.ne    .L_cmp_not_equal

    cbz     w0, .L_cmp_equal    // Both null = equal

    add     x19, x19, #1
    add     x20, x20, #1
    b       .L_cmp_loop

.L_cmp_equal:
    mov     x0, #1
    b       .L_cmp_done

.L_cmp_not_equal:
    mov     x0, #0

.L_cmp_done:
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// solve_part2: HASHMAP procedure and calculate focusing power
// Output: x0 = focusing power
// =============================================================================
solve_part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    // Clear boxes
    bl      clear_boxes

    adrp    x19, file_buffer@PAGE
    add     x19, x19, file_buffer@PAGEOFF   // x19 = current pos
    adrp    x20, file_size@PAGE
    add     x20, x20, file_size@PAGEOFF
    ldr     x20, [x20]
    add     x20, x19, x20                    // x20 = end of buffer

    mov     x21, x19                         // x21 = start of current step

.L_p2_main_loop:
    cmp     x19, x20
    b.ge    .L_p2_process_last_step

    ldrb    w22, [x19]

    // Check for comma or newline (end of step)
    cmp     w22, #CHAR_COMMA
    b.eq    .L_p2_process_step
    cmp     w22, #CHAR_NEWLINE
    b.eq    .L_p2_skip_newline

    add     x19, x19, #1
    b       .L_p2_main_loop

.L_p2_skip_newline:
    add     x19, x19, #1
    b       .L_p2_main_loop

.L_p2_process_step:
    // Process step from x21 to x19
    cmp     x21, x19
    b.ge    .L_p2_next_step

    bl      process_step

.L_p2_next_step:
    add     x19, x19, #1        // Skip comma
    mov     x21, x19            // Start of next step
    b       .L_p2_main_loop

.L_p2_process_last_step:
    // Process last step if any
    cmp     x21, x20
    b.ge    .L_p2_calc_power

    // Check if last char is newline
    sub     x0, x20, #1
    ldrb    w22, [x0]
    cmp     w22, #CHAR_NEWLINE
    b.ne    .L_p2_do_last
    mov     x20, x0

.L_p2_do_last:
    cmp     x21, x20
    b.ge    .L_p2_calc_power

    bl      process_step

.L_p2_calc_power:
    // Calculate focusing power
    bl      calculate_focusing_power

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// process_step: Process a single step
// Uses x21 = start, x19 = end (from parent)
// Modifies boxes accordingly
// =============================================================================
process_step:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    // Find the operator (= or -)
    mov     x22, x21            // x22 = label start
    mov     x23, x21            // x23 = scan position

.L_find_operator:
    cmp     x23, x19
    b.ge    .L_step_done        // No operator found, skip

    ldrb    w24, [x23]
    cmp     w24, #CHAR_EQUALS
    b.eq    .L_found_equals
    cmp     w24, #CHAR_DASH
    b.eq    .L_found_dash

    add     x23, x23, #1
    b       .L_find_operator

.L_found_equals:
    // Copy label to current_label
    adrp    x25, current_label@PAGE
    add     x25, x25, current_label@PAGEOFF
    mov     x0, x22             // label start
    mov     x1, x23             // label end
    mov     x2, x25             // dest
    bl      copy_label

    // Parse focal length (single digit after =)
    add     x0, x23, #1         // position after '='
    ldrb    w26, [x0]
    sub     w26, w26, #'0'      // convert to number

    // Calculate box number (hash of label)
    mov     x0, x22
    mov     x1, x23
    bl      hash_string
    mov     x27, x0             // x27 = box number

    // Add or replace lens in box
    mov     x0, x27             // box number
    mov     x1, x25             // label
    mov     x2, x26             // focal length
    bl      add_or_replace_lens
    b       .L_step_done

.L_found_dash:
    // Copy label to current_label
    adrp    x25, current_label@PAGE
    add     x25, x25, current_label@PAGEOFF
    mov     x0, x22             // label start
    mov     x1, x23             // label end
    mov     x2, x25             // dest
    bl      copy_label

    // Calculate box number (hash of label)
    mov     x0, x22
    mov     x1, x23
    bl      hash_string
    mov     x27, x0             // x27 = box number

    // Remove lens from box
    mov     x0, x27             // box number
    mov     x1, x25             // label
    bl      remove_lens

.L_step_done:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// copy_label: Copy label from source range to destination
// Input: x0 = start, x1 = end, x2 = dest
// =============================================================================
copy_label:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0             // src
    mov     x20, x2             // dst

.L_copy_loop:
    cmp     x19, x1
    b.ge    .L_copy_done

    ldrb    w0, [x19]
    strb    w0, [x20]
    add     x19, x19, #1
    add     x20, x20, #1
    b       .L_copy_loop

.L_copy_done:
    // Null terminate
    strb    wzr, [x20]

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// add_or_replace_lens: Add or replace a lens in a box
// Input: x0 = box number, x1 = label pointer, x2 = focal length
// =============================================================================
add_or_replace_lens:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    mov     x19, x0             // box number
    mov     x20, x1             // label
    mov     x21, x2             // focal length

    // Get box count
    adrp    x22, box_counts@PAGE
    add     x22, x22, box_counts@PAGEOFF
    ldr     x23, [x22, x19, lsl #3]     // x23 = count

    // Get box base address
    adrp    x24, boxes@PAGE
    add     x24, x24, boxes@PAGEOFF
    mov     x0, #BOX_SIZE
    mul     x0, x0, x19
    add     x24, x24, x0        // x24 = base of this box

    // Search for existing lens with same label
    mov     x25, #0             // index
.L_search_lens:
    cmp     x25, x23
    b.ge    .L_add_new_lens

    // Get lens entry address
    mov     x0, #LENS_SIZE
    mul     x0, x0, x25
    add     x0, x24, x0         // x0 = lens entry address

    // Compare labels
    mov     x1, x20             // input label
    bl      compare_labels
    cbnz    x0, .L_replace_lens

    add     x25, x25, #1
    b       .L_search_lens

.L_replace_lens:
    // Replace focal length at this position
    mov     x0, #LENS_SIZE
    mul     x0, x0, x25
    add     x0, x24, x0
    add     x0, x0, #8          // offset to focal length
    str     x21, [x0]
    b       .L_aor_done

.L_add_new_lens:
    // Add new lens at end
    mov     x0, #LENS_SIZE
    mul     x0, x0, x23         // position = count * LENS_SIZE
    add     x26, x24, x0        // x26 = new entry address

    // Copy label (8 bytes)
    ldr     x0, [x20]
    str     x0, [x26]

    // Store focal length
    str     x21, [x26, #8]

    // Increment count
    add     x23, x23, #1
    str     x23, [x22, x19, lsl #3]

.L_aor_done:
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// remove_lens: Remove a lens from a box
// Input: x0 = box number, x1 = label pointer
// =============================================================================
remove_lens:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0             // box number
    mov     x20, x1             // label

    // Get box count
    adrp    x22, box_counts@PAGE
    add     x22, x22, box_counts@PAGEOFF
    ldr     x23, [x22, x19, lsl #3]     // x23 = count

    cbz     x23, .L_remove_done // Empty box

    // Get box base address
    adrp    x24, boxes@PAGE
    add     x24, x24, boxes@PAGEOFF
    mov     x0, #BOX_SIZE
    mul     x0, x0, x19
    add     x24, x24, x0        // x24 = base of this box

    // Search for lens with matching label
    mov     x25, #0             // index
.L_remove_search:
    cmp     x25, x23
    b.ge    .L_remove_done      // Not found

    // Get lens entry address
    mov     x0, #LENS_SIZE
    mul     x0, x0, x25
    add     x0, x24, x0         // x0 = lens entry address

    // Compare labels
    mov     x1, x20             // input label
    bl      compare_labels
    cbnz    x0, .L_remove_found

    add     x25, x25, #1
    b       .L_remove_search

.L_remove_found:
    // Shift all following lenses down
    mov     x26, x25            // src index = found + 1
    add     x26, x26, #1

.L_shift_loop:
    cmp     x26, x23
    b.ge    .L_shift_done

    // Copy lens[src] to lens[src-1]
    mov     x0, #LENS_SIZE
    mul     x27, x0, x26        // src offset
    add     x27, x24, x27       // src address

    sub     x28, x26, #1
    mul     x28, x0, x28        // dst offset
    add     x28, x24, x28       // dst address

    // Copy 16 bytes (label + focal)
    ldr     x0, [x27]
    str     x0, [x28]
    ldr     x0, [x27, #8]
    str     x0, [x28, #8]

    add     x26, x26, #1
    b       .L_shift_loop

.L_shift_done:
    // Decrement count
    sub     x23, x23, #1
    str     x23, [x22, x19, lsl #3]

.L_remove_done:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// calculate_focusing_power: Calculate total focusing power
// Output: x0 = focusing power
// =============================================================================
calculate_focusing_power:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    mov     x19, #0             // box number
    mov     x20, #0             // total power

    adrp    x21, box_counts@PAGE
    add     x21, x21, box_counts@PAGEOFF
    adrp    x22, boxes@PAGE
    add     x22, x22, boxes@PAGEOFF

.L_power_box_loop:
    cmp     x19, #256
    b.ge    .L_power_done

    // Get box count
    ldr     x23, [x21, x19, lsl #3]
    cbz     x23, .L_power_next_box

    // Get box base address
    mov     x0, #BOX_SIZE
    mul     x0, x0, x19
    add     x24, x22, x0        // x24 = base of this box

    // Iterate lenses
    mov     x25, #0             // slot index (0-based)
.L_power_lens_loop:
    cmp     x25, x23
    b.ge    .L_power_next_box

    // Get focal length
    mov     x0, #LENS_SIZE
    mul     x0, x0, x25
    add     x0, x24, x0
    ldr     x26, [x0, #8]       // focal length

    // power = (box+1) * (slot+1) * focal
    add     x0, x19, #1         // box + 1
    add     x1, x25, #1         // slot + 1
    mul     x0, x0, x1
    mul     x0, x0, x26
    add     x20, x20, x0

    add     x25, x25, #1
    b       .L_power_lens_loop

.L_power_next_box:
    add     x19, x19, #1
    b       .L_power_box_loop

.L_power_done:
    mov     x0, x20

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// UTILITY FUNCTIONS
// =============================================================================

// -----------------------------------------------------------------------------
// print_str: Write null-terminated string to stdout
// Input: x0 = string address
// -----------------------------------------------------------------------------
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0

    // Calculate length
    mov     x20, #0
.L_strlen:
    ldrb    w1, [x19, x20]
    cbz     w1, .L_strlen_done
    add     x20, x20, #1
    b       .L_strlen
.L_strlen_done:

    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_WRITE_LO
    mov     x0, #1
    mov     x1, x19
    mov     x2, x20
    svc     #0

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// -----------------------------------------------------------------------------
// print_num: Write decimal number to stdout
// Input: x0 = number to print
// -----------------------------------------------------------------------------
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0
    adrp    x20, output_buffer@PAGE
    add     x20, x20, output_buffer@PAGEOFF
    add     x20, x20, #31
    mov     x21, #0

    // Handle zero
    cbnz    x19, .L_num_loop
    mov     w22, #'0'
    strb    w22, [x20, #-1]!
    mov     x21, #1
    b       .L_num_print

.L_num_loop:
    cbz     x19, .L_num_print

    mov     x1, #10
    udiv    x2, x19, x1
    msub    x3, x2, x1, x19

    add     w3, w3, #'0'
    strb    w3, [x20, #-1]!
    add     x21, x21, #1

    mov     x19, x2
    b       .L_num_loop

.L_num_print:
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_WRITE_LO
    mov     x0, #1
    mov     x1, x20
    mov     x2, x21
    svc     #0

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
