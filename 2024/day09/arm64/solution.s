// Day 9: Disk Fragmenter - ARM64 Assembly (macOS)
// OPTIMIZED: Span-based approach (no block expansion)
//
// Key insight: Work with spans (file_id, position, length) instead of
// expanding to individual blocks. Calculate checksums mathematically.
//
// Checksum contribution for file f at position p with length len:
//   f * (p + p+1 + ... + p+len-1) = f * len * p + f * len * (len-1) / 2

.global _start
.align 4

// Constants
.equ MAX_INPUT_SIZE, 20480
.equ MAX_SPANS, 10000          // Max number of file/free spans

// Macro for loading addresses from data section
.macro LOAD_ADDR reg, label
    adrp    \reg, \label@PAGE
    add     \reg, \reg, \label@PAGEOFF
.endm

// ============================================================================
// Data Section
// ============================================================================
.data

// String constants
input_path:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"
error_msg:      .asciz "Error reading file\n"

.align 3
// File I/O buffer
file_buffer:    .space MAX_INPUT_SIZE

// Span arrays: position and length for each file/free space
file_pos:       .space MAX_SPANS * 8    // 64-bit positions
file_len:       .space MAX_SPANS * 8    // 64-bit lengths
free_pos:       .space MAX_SPANS * 8
free_len:       .space MAX_SPANS * 8

// Working copies for Part 2
p2_file_pos:    .space MAX_SPANS * 8
p2_free_pos:    .space MAX_SPANS * 8
p2_free_len:    .space MAX_SPANS * 8

// Counters
max_file_id:    .quad 0
num_free:       .quad 0
total_blocks:   .quad 0

// ============================================================================
// Code Section
// ============================================================================
.text

// ============================================================================
// Main entry point
// ============================================================================
_start:
    // Open and read input file
    LOAD_ADDR x0, input_path
    mov     x1, #0                          // O_RDONLY
    mov     x2, #0
    mov     x16, #5                         // open() syscall
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit

    mov     x19, x0                         // Save fd

    // Read file
    mov     x0, x19
    LOAD_ADDR x1, file_buffer
    mov     x2, #MAX_INPUT_SIZE
    mov     x16, #3                         // read() syscall
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit

    mov     x20, x0                         // Save bytes read

    // Close file
    mov     x0, x19
    mov     x16, #6                         // close() syscall
    svc     #0x80

    // Parse disk map into spans
    bl      parse_spans

    // Solve Part 1
    bl      solve_part1
    mov     x21, x0                         // Save part1 result

    // Solve Part 2
    bl      solve_part2
    mov     x22, x0                         // Save part2 result

    // Print results
    LOAD_ADDR x0, part1_msg
    bl      print_str
    mov     x0, x21
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    LOAD_ADDR x0, part2_msg
    bl      print_str
    mov     x0, x22
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Exit
    mov     x0, #0
    mov     x16, #1                         // exit() syscall
    svc     #0x80

error_exit:
    LOAD_ADDR x0, error_msg
    bl      print_str
    mov     x0, #1
    mov     x16, #1
    svc     #0x80

// ============================================================================
// parse_spans: Parse disk map into file and free space spans
// Input: file_buffer contains disk map
// Output: file_pos[], file_len[], free_pos[], free_len[] filled
// ============================================================================
parse_spans:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    LOAD_ADDR x19, file_buffer              // Input pointer
    LOAD_ADDR x20, file_pos                 // File position array
    LOAD_ADDR x21, file_len                 // File length array
    LOAD_ADDR x22, free_pos                 // Free position array
    LOAD_ADDR x23, free_len                 // Free length array

    mov     x24, #0                         // Current position
    mov     x25, #0                         // File ID counter
    mov     x26, #0                         // Free span counter
    mov     x27, #1                         // is_file flag
    mov     x28, #0                         // Total file blocks

parse_loop:
    ldrb    w0, [x19], #1                   // Load next character

    // Check for end of input
    cbz     w0, parse_done
    cmp     w0, #'\n'
    b.eq    parse_done
    cmp     w0, #'0'
    b.lt    parse_done
    cmp     w0, #'9'
    b.gt    parse_done

    // Convert ASCII digit to number
    sub     x1, x0, #'0'                    // x1 = length

    // Check if file or free space
    cbz     x27, handle_free

handle_file:
    // Store file span if length > 0
    cbz     x1, toggle_flag
    str     x24, [x20, x25, lsl #3]         // file_pos[file_id] = pos
    str     x1, [x21, x25, lsl #3]          // file_len[file_id] = len
    add     x28, x28, x1                    // total_blocks += len
    add     x25, x25, #1                    // file_id++
    b       update_pos

handle_free:
    // Store free span if length > 0
    cbz     x1, toggle_flag
    str     x24, [x22, x26, lsl #3]         // free_pos[num_free] = pos
    str     x1, [x23, x26, lsl #3]          // free_len[num_free] = len
    add     x26, x26, #1                    // num_free++

update_pos:
    add     x24, x24, x1                    // pos += len

toggle_flag:
    eor     x27, x27, #1                    // Toggle is_file
    b       parse_loop

parse_done:
    // Save counters
    sub     x25, x25, #1                    // max_file_id = last file_id
    LOAD_ADDR x0, max_file_id
    str     x25, [x0]
    LOAD_ADDR x0, num_free
    str     x26, [x0]
    LOAD_ADDR x0, total_blocks
    str     x28, [x0]

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// checksum_span: Calculate checksum contribution for a span
// Input: x0 = file_id, x1 = position, x2 = length
// Output: x0 = checksum contribution
// Formula: file_id * (len * pos + len * (len-1) / 2)
// ============================================================================
checksum_span:
    // x0 = file_id, x1 = pos, x2 = len
    // Result = file_id * (len * pos + len * (len-1) / 2)

    mul     x3, x2, x1                      // x3 = len * pos
    sub     x4, x2, #1                      // x4 = len - 1
    mul     x4, x2, x4                      // x4 = len * (len-1)
    lsr     x4, x4, #1                      // x4 = len * (len-1) / 2
    add     x3, x3, x4                      // x3 = len * pos + len * (len-1) / 2
    mul     x0, x0, x3                      // x0 = file_id * sum
    ret

// ============================================================================
// solve_part1: Block-by-block compaction using span logic
// ============================================================================
solve_part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    LOAD_ADDR x19, max_file_id
    ldr     x19, [x19]                      // x19 = max_file_id
    LOAD_ADDR x20, total_blocks
    ldr     x20, [x20]                      // x20 = cutoff (total file blocks)
    LOAD_ADDR x21, file_pos
    LOAD_ADDR x22, file_len
    LOAD_ADDR x23, num_free
    ldr     x23, [x23]                      // x23 = num_free
    LOAD_ADDR x24, free_pos
    LOAD_ADDR x25, free_len

    mov     x26, #0                         // part1 checksum
    mov     x27, #0                         // Current file_id

    // Process file blocks that stay (files before cutoff)
stay_loop:
    cmp     x27, x19
    b.gt    fill_free_spans

    ldr     x0, [x21, x27, lsl #3]          // file_pos[fid]
    cmp     x0, x20
    b.ge    fill_free_spans                 // File entirely after cutoff

    ldr     x2, [x22, x27, lsl #3]          // file_len[fid]
    add     x1, x0, x2                      // end = pos + len
    cmp     x1, x20
    b.le    stay_full

    // Partial stay
    sub     x2, x20, x0                     // stay_len = cutoff - pos

stay_full:
    // Add checksum for this span
    mov     x1, x0                          // position
    mov     x0, x27                         // file_id
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!
    bl      checksum_span
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    add     x26, x26, x0                    // part1 += checksum

    add     x27, x27, #1
    b       stay_loop

fill_free_spans:
    // Fill free spans with file blocks from the right
    mov     x27, x19                        // take_fid = max_file_id
    mov     x28, #0                         // take_offset (blocks taken from current file)
    mov     x10, #0                         // free_index (0-based, we use 0 to num_free-1)

free_loop:
    cmp     x10, x23
    b.ge    part1_done

    ldr     x11, [x24, x10, lsl #3]         // fpos = free_pos[f]
    cmp     x11, x20
    b.ge    part1_done                      // Free span after cutoff

    ldr     x12, [x25, x10, lsl #3]         // flen = free_len[f]
    add     x13, x11, x12                   // end = fpos + flen
    cmp     x13, x20
    b.le    1f
    sub     x12, x20, x11                   // flen = cutoff - fpos

1:  mov     x14, #0                         // filled = 0

fill_loop:
    cmp     x14, x12
    b.ge    next_free

    // Find next file to take from (must be at or after cutoff)
find_take:
    cmp     x27, #0
    b.lt    next_free                       // No more files

    ldr     x15, [x21, x27, lsl #3]         // file_pos[take_fid]
    cmp     x15, x20
    b.ge    found_take
    sub     x27, x27, #1
    mov     x28, #0
    b       find_take

found_take:
    // Calculate available and needed blocks
    ldr     x15, [x22, x27, lsl #3]         // file_len[take_fid]
    sub     x15, x15, x28                   // avail = file_len - take_offset
    sub     x16, x12, x14                   // need = flen - filled
    cmp     x15, x16
    csel    x17, x15, x16, lt               // use = min(avail, need)

    // Add checksum for this span at fpos+filled
    add     x1, x11, x14                    // position = fpos + filled
    mov     x0, x27                         // file_id = take_fid
    mov     x2, x17                         // length = use

    stp     x10, x11, [sp, #-16]!
    stp     x12, x13, [sp, #-16]!
    stp     x14, x15, [sp, #-16]!
    stp     x16, x17, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!
    bl      checksum_span
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x16, x17, [sp], #16
    ldp     x14, x15, [sp], #16
    ldp     x12, x13, [sp], #16
    ldp     x10, x11, [sp], #16
    add     x26, x26, x0                    // part1 += checksum

    add     x14, x14, x17                   // filled += use
    add     x28, x28, x17                   // take_offset += use

    // Check if we exhausted this file
    ldr     x15, [x22, x27, lsl #3]
    cmp     x28, x15
    b.lt    fill_loop
    sub     x27, x27, #1
    mov     x28, #0
    b       fill_loop

next_free:
    add     x10, x10, #1
    b       free_loop

part1_done:
    mov     x0, x26                         // Return checksum

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_part2: Whole-file compaction
// ============================================================================
solve_part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    LOAD_ADDR x19, max_file_id
    ldr     x19, [x19]                      // x19 = max_file_id
    LOAD_ADDR x20, num_free
    ldr     x20, [x20]                      // x20 = num_free
    LOAD_ADDR x21, file_pos
    LOAD_ADDR x22, file_len
    LOAD_ADDR x23, free_pos
    LOAD_ADDR x24, free_len
    LOAD_ADDR x25, p2_file_pos
    LOAD_ADDR x26, p2_free_pos
    LOAD_ADDR x27, p2_free_len

    // Copy file positions to working array
    mov     x0, #0
copy_file_pos:
    cmp     x0, x19
    b.gt    copy_free
    ldr     x1, [x21, x0, lsl #3]
    str     x1, [x25, x0, lsl #3]
    add     x0, x0, #1
    b       copy_file_pos

copy_free:
    // Copy free spans to working arrays
    mov     x0, #0
copy_free_loop:
    cmp     x0, x20
    b.ge    process_files
    ldr     x1, [x23, x0, lsl #3]
    str     x1, [x26, x0, lsl #3]           // p2_free_pos[i] = free_pos[i]
    ldr     x1, [x24, x0, lsl #3]
    str     x1, [x27, x0, lsl #3]           // p2_free_len[i] = free_len[i]
    add     x0, x0, #1
    b       copy_free_loop

process_files:
    // Process files from max_file_id down to 0
    mov     x28, x19                        // fid = max_file_id

file_loop:
    cmp     x28, #0
    b.lt    calc_part2

    ldr     x10, [x22, x28, lsl #3]         // flen = file_len[fid]
    ldr     x11, [x25, x28, lsl #3]         // fpos = p2_file_pos[fid]

    // Find leftmost free span that fits (must be left of file)
    mov     x0, #0                          // free index
    mov     x12, #-1                        // best = -1 (none found)

find_span:
    cmp     x0, x20
    b.ge    check_move

    ldr     x1, [x26, x0, lsl #3]           // p2_free_pos[f]
    cmp     x1, x11
    b.ge    check_move                      // Only look left

    ldr     x2, [x27, x0, lsl #3]           // p2_free_len[f]
    cmp     x2, x10
    b.lt    next_span                       // Doesn't fit

    mov     x12, x0                         // Found suitable span
    b       check_move

next_span:
    add     x0, x0, #1
    b       find_span

check_move:
    cmp     x12, #0
    b.lt    next_file                       // No suitable span found

    // Move file to this free span
    ldr     x1, [x26, x12, lsl #3]          // p2_free_pos[best]
    str     x1, [x25, x28, lsl #3]          // p2_file_pos[fid] = p2_free_pos[best]

    // Update free span
    add     x1, x1, x10                     // p2_free_pos[best] += flen
    str     x1, [x26, x12, lsl #3]
    ldr     x2, [x27, x12, lsl #3]          // p2_free_len[best]
    sub     x2, x2, x10                     // p2_free_len[best] -= flen
    str     x2, [x27, x12, lsl #3]
next_file:
    sub     x28, x28, #1
    b       file_loop

calc_part2:
    // Calculate checksum
    mov     x28, #0                         // part2 checksum
    mov     x10, #0                         // fid

checksum_loop:
    cmp     x10, x19
    b.gt    part2_done

    ldr     x1, [x25, x10, lsl #3]          // p2_file_pos[fid]
    ldr     x2, [x22, x10, lsl #3]          // file_len[fid]
    mov     x0, x10                         // file_id

    stp     x10, x19, [sp, #-16]!
    stp     x20, x21, [sp, #-16]!
    stp     x22, x23, [sp, #-16]!
    stp     x24, x25, [sp, #-16]!
    stp     x26, x27, [sp, #-16]!
    str     x28, [sp, #-16]!
    bl      checksum_span
    ldr     x28, [sp], #16
    ldp     x26, x27, [sp], #16
    ldp     x24, x25, [sp], #16
    ldp     x22, x23, [sp], #16
    ldp     x20, x21, [sp], #16
    ldp     x10, x19, [sp], #16
    add     x28, x28, x0                    // part2 += checksum

    add     x10, x10, #1
    b       checksum_loop

part2_done:
    mov     x0, x28                         // Return checksum

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print a null-terminated string
// ============================================================================
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0

    // Find length
    mov     x20, #0
1:  ldrb    w1, [x19, x20]
    cbz     w1, 2f
    add     x20, x20, #1
    b       1b

2:  // Write to stdout
    mov     x0, #1
    mov     x1, x19
    mov     x2, x20
    mov     x16, #4
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_num: Print a number
// ============================================================================
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    sub     sp, sp, #32

    mov     x19, x0
    add     x20, sp, #31
    strb    wzr, [x20]

    // Handle zero case
    cbnz    x19, 1f
    sub     x20, x20, #1
    mov     w0, #'0'
    strb    w0, [x20]
    b       2f

1:  cbz     x19, 2f
    mov     x1, #10
    udiv    x2, x19, x1
    msub    x3, x2, x1, x19
    add     w3, w3, #'0'
    sub     x20, x20, #1
    strb    w3, [x20]
    mov     x19, x2
    b       1b

2:  mov     x0, x20
    bl      print_str

    add     sp, sp, #32
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
