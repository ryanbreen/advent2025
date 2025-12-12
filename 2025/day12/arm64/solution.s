// Day 12: Christmas Tree Farm - ARM64 Assembly for macOS
// Algorithm: Parse shapes (count '#'), parse regions, check if total cells <= available
// Optimized for ARM64 ABI compliance and performance
// Assemble: as -o solution.o solution.s && ld -o solution solution.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64
// Run: ./solution

.global _start
.align 4

// Constants
.equ O_RDONLY, 0
.equ BUFFER_SIZE, 131072
.equ MAX_SHAPES, 10

.section __DATA,__data
.align 3
input_path: .asciz "../input.txt"
part1_fmt: .asciz "Part 1: %lld\n"
part2_fmt: .asciz "Part 2: %lld\n"

.section __DATA,__bss
.align 3
buffer: .space BUFFER_SIZE
shape_cells: .space MAX_SHAPES * 8  // Array of cell counts per shape

.section __TEXT,__text

// Check if character is whitespace
// Input: w0 = character
// Output: Z flag set if whitespace
// Clobbers: w1
is_whitespace:
    cmp w0, #' '
    b.eq .Lis_ws
    cmp w0, #'\n'
    b.eq .Lis_ws
    cmp w0, #'\r'
    b.eq .Lis_ws
    cmp w0, #'\t'
.Lis_ws:
    ret

// Parse unsigned integer from buffer
// Input: x23 = pointer to string
// Output: x0 = parsed number, x23 = pointer after number
// Clobbers: w1, w2, x3
parse_uint:
    mov x0, #0
.Lparse_digit:
    ldrb w1, [x23]
    sub w2, w1, #'0'
    cmp w2, #9
    b.hi .Lparse_done
    // x0 = x0 * 10 + digit
    // Optimize: 10*x = 8*x + 2*x = (x<<3) + (x<<1)
    add x3, x0, x0, lsl #2  // x3 = x0 * 5
    add x0, x2, x3, lsl #1  // x0 = digit + (x0*5)*2 = digit + x0*10
    add x23, x23, #1
    b .Lparse_digit
.Lparse_done:
    ret

// Main function
// Register allocation:
// x19 = fd
// x20 = buffer base
// x21 = bytes read
// x22 = shape_cells array base
// x23 = parse pointer
// x24 = current shape index / temp
// x25 = fit count / hash count
// x26 = region width
// x27 = region height
_start:
    // Prologue - ARM64 ABI requires 16-byte stack alignment
    sub sp, sp, #96
    stp x29, x30, [sp, #80]
    add x29, sp, #80
    stp x19, x20, [sp, #64]
    stp x21, x22, [sp, #48]
    stp x23, x24, [sp, #32]
    stp x25, x26, [sp, #16]
    str x27, [sp, #8]

    // Open file
    adrp x0, input_path@PAGE
    add x0, x0, input_path@PAGEOFF
    mov w1, #O_RDONLY
    bl _open
    sxtw x19, w0            // x19 = fd

    cmp x19, #0
    b.lt exit_error

    // Get buffer address
    adrp x20, buffer@PAGE
    add x20, x20, buffer@PAGEOFF

    // Read file
    mov x0, x19
    mov x1, x20
    mov x2, #BUFFER_SIZE
    bl _read
    mov x21, x0             // x21 = bytes read

    // Close file
    mov x0, x19
    bl _close

    cmp x21, #0
    b.le exit_error

    // Null-terminate
    strb wzr, [x20, x21]

    // Get shape_cells array address
    adrp x22, shape_cells@PAGE
    add x22, x22, shape_cells@PAGEOFF

    // ===== Parse Shapes =====
    mov x23, x20            // x23 = parse pointer
    mov x24, #0             // x24 = current shape index

parse_shapes_loop:
    ldrb w0, [x23]
    cbz w0, parse_done

    // Skip whitespace
    cmp w0, #' '
    b.eq skip_char
    cmp w0, #'\n'
    b.eq skip_char
    cmp w0, #'\r'
    b.eq skip_char
    cmp w0, #'\t'
    b.eq skip_char

    // Check for digit (shape index)
    sub w1, w0, #'0'
    cmp w1, #9
    b.hi not_shape_start

    // Parse shape index using optimized subroutine
    bl parse_uint
    mov x24, x0             // x24 = current shape index

    // Skip colon and whitespace
3:  ldrb w0, [x23]
    cmp w0, #':'
    b.eq 4f
    cmp w0, #' '
    b.eq 4f
    cmp w0, #'\n'
    b.eq 4f
    cmp w0, #'\r'
    b.eq 4f
    cmp w0, #'\t'
    b.eq 4f
    b parse_shapes_loop
4:  add x23, x23, #1
    b 3b

not_shape_start:
    // Check for '#' or '.' (shape data)
    cmp w0, #'#'
    b.eq count_hash
    cmp w0, #'.'
    b.eq skip_dot

    // Check for 'x' (start of regions)
    cmp w0, #'x'
    b.eq parse_regions_start

skip_char:
    add x23, x23, #1
    b parse_shapes_loop

skip_dot:
    add x23, x23, #1
    b parse_shapes_loop

count_hash:
    // Count '#' for current shape
    mov x25, #0             // x25 = hash count for this shape line/section

count_hash_loop:
    ldrb w0, [x23]
    cbz w0, save_hash_count

    cmp w0, #'#'
    b.ne check_end_shape

    add x25, x25, #1
    add x23, x23, #1
    b count_hash_loop

check_end_shape:
    // Continue counting if we see '.' or newline within shape
    cmp w0, #'.'
    b.eq count_hash_continue
    cmp w0, #'\n'
    b.eq check_next_line
    cmp w0, #'\r'
    b.eq count_hash_continue

    // If we hit something else, save and continue
    b save_hash_count

count_hash_continue:
    add x23, x23, #1
    b count_hash_loop

check_next_line:
    add x23, x23, #1
    ldrb w0, [x23]
    cbz w0, save_hash_count

    // If next line starts with '#' or '.', continue counting
    cmp w0, #'#'
    b.eq count_hash_loop
    cmp w0, #'.'
    b.eq count_hash_continue

    // Otherwise, end of shape
    b save_hash_count

save_hash_count:
    // Save hash count for this shape
    lsl x0, x24, #3         // x0 = shape_index * 8
    str x25, [x22, x0]      // shape_cells[index] = count
    b parse_shapes_loop

parse_regions_start:
    // Back up to find the digit before 'x'
    sub x23, x23, #1
1:  ldrb w0, [x23]
    sub w1, w0, #'0'
    cmp w1, #9
    b.ls 2f
    sub x23, x23, #1
    b 1b
2:
    // Now at start of width, fall through to parse regions

parse_done:
    // ===== Parse Regions and Count Fits =====
    mov x25, #0             // x25 = fit count

parse_regions_loop:
    ldrb w0, [x23]
    cbz w0, print_results

    // Skip whitespace
    cmp w0, #' '
    b.eq skip_region_char
    cmp w0, #'\n'
    b.eq skip_region_char
    cmp w0, #'\r'
    b.eq skip_region_char
    cmp w0, #'\t'
    b.eq skip_region_char

    // Check for digit (start of region)
    sub w1, w0, #'0'
    cmp w1, #9
    b.hi skip_region_char

    // Parse width
    bl parse_uint
    mov x26, x0             // x26 = width

    // Skip 'x'
    ldrb w0, [x23]
    cmp w0, #'x'
    b.ne skip_region_char
    add x23, x23, #1

    // Parse height
    bl parse_uint
    mov x27, x0             // x27 = height

    // Calculate available cells
    mul x28, x26, x27       // x28 = available cells

    // Skip colon and whitespace
3:  ldrb w0, [x23]
    cmp w0, #':'
    b.eq 4f
    cmp w0, #' '
    b.eq 4f
    cmp w0, #'\n'
    b.ne 5f
4:  add x23, x23, #1
    b 3b
5:

    // Parse counts and calculate total cells needed
    // Use x24 (callee-saved) for total, x26 for shape index temporarily
    mov x24, #0             // x24 = total cells needed
    mov x1, #0              // x1 = shape index

parse_counts_loop:
    ldrb w2, [x23]
    cbz w2, check_region_fit

    // Check for newline (end of region)
    cmp w2, #'\n'
    b.eq check_region_fit

    // Skip whitespace
    cmp w2, #' '
    b.eq skip_count_char
    cmp w2, #'\t'
    b.eq skip_count_char
    cmp w2, #'\r'
    b.eq skip_count_char

    // Check for digit
    sub w3, w2, #'0'
    cmp w3, #9
    b.hi skip_count_char

    // Save shape index before call
    stp x1, x24, [sp, #-16]!
    // Parse count using optimized subroutine
    bl parse_uint
    // Restore after call
    ldp x1, x24, [sp], #16

    // x0 = count for current shape
    // Get shape cell count
    lsl x2, x1, #3
    ldr x3, [x22, x2]       // x3 = shape_cells[shape_index]

    // Add count * shape_cells to total using MADD (fused multiply-add)
    madd x24, x0, x3, x24   // x24 += (count * shape_cells) in one instruction

    add x1, x1, #1          // Next shape
    b parse_counts_loop

skip_count_char:
    add x23, x23, #1
    b parse_counts_loop

check_region_fit:
    // Check if total_cells_needed <= available
    cmp x24, x28
    b.hi skip_region_increment
    add x25, x25, #1        // Increment fit count

skip_region_increment:
    add x23, x23, #1
    b parse_regions_loop

skip_region_char:
    add x23, x23, #1
    b parse_regions_loop

print_results:
    // Print Part 1 - varargs on stack for ARM64 macOS
    str x25, [sp]
    adrp x0, part1_fmt@PAGE
    add x0, x0, part1_fmt@PAGEOFF
    bl _printf

    // Print Part 2 (always 0)
    mov x0, #0
    str x0, [sp]
    adrp x0, part2_fmt@PAGE
    add x0, x0, part2_fmt@PAGEOFF
    bl _printf

    mov x0, #0
    b exit_program

exit_error:
    mov x0, #1

exit_program:
    // Epilogue
    ldr x27, [sp, #8]
    ldp x25, x26, [sp, #16]
    ldp x23, x24, [sp, #32]
    ldp x21, x22, [sp, #48]
    ldp x19, x20, [sp, #64]
    ldp x29, x30, [sp, #80]
    add sp, sp, #96
    bl _exit
