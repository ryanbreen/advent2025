// ARM64 Assembly solution for AoC 2022 Day 7 - No Space Left On Device
// macOS syscalls

.global _start
.align 2

.equ STDOUT, 1
.equ MAX_DIRS, 256
.equ MAX_PATH_DEPTH, 32
.equ THRESHOLD, 100000
.equ TOTAL_SPACE, 70000000
.equ NEEDED_SPACE, 30000000

.data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

.align 3
file_buffer: .skip 16384
// Directory sizes array - we don't care about names, just accumulate sizes
dir_sizes: .skip MAX_DIRS * 8       // Array of directory sizes
dir_count: .skip 8                   // Number of directories
// Path stack: stores indices into dir_sizes for current path
path_stack: .skip MAX_PATH_DEPTH * 8 // Stack of directory indices
path_depth: .skip 8                  // Current depth
output_buffer: .skip 32

.text
_start:
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

    // Parse input
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20
    bl parse_input

    // Part 1: Sum of directory sizes <= 100000
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

    // Part 2: Find smallest directory >= needed space
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

// Parse terminal output and build directory sizes
// x0 = buffer, x1 = length
parse_input:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    mov x19, x0             // Buffer pointer
    add x20, x19, x1        // Buffer end

    // Initialize
    adrp x21, dir_sizes@PAGE
    add x21, x21, dir_sizes@PAGEOFF
    adrp x22, path_stack@PAGE
    add x22, x22, path_stack@PAGEOFF
    adrp x23, dir_count@PAGE
    add x23, x23, dir_count@PAGEOFF
    adrp x24, path_depth@PAGE
    add x24, x24, path_depth@PAGEOFF

    mov x25, #0             // Directory count
    str x25, [x23]          // dir_count = 0
    str x25, [x24]          // path_depth = 0

parse_loop:
    cmp x19, x20
    b.ge parse_done

    // Check if line starts with '$'
    ldrb w26, [x19]
    cmp w26, #'$'
    b.eq handle_command

    // Check if line starts with 'd' (dir entry)
    cmp w26, #'d'
    b.eq skip_line

    // Must be a file with size - parse the number
    cmp w26, #'0'
    b.lt skip_line
    cmp w26, #'9'
    b.gt skip_line

    // Parse file size
    mov x0, x19
    bl parse_number
    mov x19, x0             // Update pointer
    mov x27, x1             // Save file size

    // Add this file size to all directories in the path stack
    ldr x26, [x24]          // path_depth
    cbz x26, skip_to_eol

    mov x28, #0             // Index
add_size_loop:
    cmp x28, x26
    b.ge skip_to_eol
    ldr x0, [x22, x28, lsl #3]   // Get dir index from path stack
    ldr x1, [x21, x0, lsl #3]    // Get current size
    add x1, x1, x27              // Add file size
    str x1, [x21, x0, lsl #3]    // Store back
    add x28, x28, #1
    b add_size_loop

skip_to_eol:
    // Skip to end of line
    mov x0, x19
    bl skip_to_newline
    mov x19, x0
    b parse_loop

handle_command:
    // Skip "$ "
    add x19, x19, #2

    // Check if 'cd' or 'ls'
    ldrb w26, [x19]
    cmp w26, #'c'
    b.eq handle_cd
    // Otherwise it's 'ls' - skip
    b skip_line

handle_cd:
    // Skip "cd "
    add x19, x19, #3

    // Check target: '/' or '..' or name
    ldrb w26, [x19]
    cmp w26, #'/'
    b.eq cd_root
    cmp w26, #'.'
    b.eq cd_parent

    // cd into subdirectory - create new dir entry
    ldr x27, [x23]          // dir_count
    // Initialize new directory size to 0
    mov x28, #0
    str x28, [x21, x27, lsl #3]
    // Push to path stack
    ldr x28, [x24]          // path_depth
    str x27, [x22, x28, lsl #3]  // path_stack[depth] = dir_index
    add x28, x28, #1
    str x28, [x24]          // path_depth++
    add x27, x27, #1
    str x27, [x23]          // dir_count++
    b skip_line

cd_root:
    // cd / - reset path, create root directory
    ldr x27, [x23]          // dir_count
    mov x28, #0
    str x28, [x21, x27, lsl #3]  // Initialize root dir size to 0
    str x28, [x22]               // path_stack[0] = dir_index
    mov x28, #1
    str x28, [x24]               // path_depth = 1
    add x27, x27, #1
    str x27, [x23]               // dir_count++
    b skip_line

cd_parent:
    // cd .. - pop from path stack
    ldr x28, [x24]          // path_depth
    cmp x28, #1
    b.le skip_line          // Don't go above root
    sub x28, x28, #1
    str x28, [x24]          // path_depth--
    b skip_line

skip_line:
    mov x0, x19
    bl skip_to_newline
    mov x19, x0
    b parse_loop

parse_done:
    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Parse a number from string
// x0 = pointer, returns x0 = new pointer, x1 = number
parse_number:
    mov x1, #0              // Result
    mov x2, #10             // Base
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

// Skip to newline
// x0 = pointer, returns x0 = new pointer (after newline)
skip_to_newline:
skip_nl_loop:
    ldrb w1, [x0]
    cmp w1, #'\n'
    b.eq skip_nl_found
    cmp w1, #0
    b.eq skip_nl_end
    add x0, x0, #1
    b skip_nl_loop
skip_nl_found:
    add x0, x0, #1
skip_nl_end:
    ret

// Part 1: Sum of all directory sizes <= 100000
// Returns sum in x0
part1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    adrp x19, dir_sizes@PAGE
    add x19, x19, dir_sizes@PAGEOFF
    adrp x20, dir_count@PAGE
    add x20, x20, dir_count@PAGEOFF
    ldr x20, [x20]          // Count

    mov x21, #0             // Sum
    // Load 100000 = 0x186A0
    movz x22, #0x86A0
    movk x22, #0x1, lsl #16

part1_loop:
    cbz x20, part1_done
    ldr x0, [x19], #8       // Get directory size
    cmp x0, x22
    b.gt part1_skip         // Skip if > 100000
    add x21, x21, x0        // Add to sum
part1_skip:
    sub x20, x20, #1
    b part1_loop

part1_done:
    mov x0, x21
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 2: Find smallest directory >= space needed to free
// Returns size in x0
part2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    adrp x19, dir_sizes@PAGE
    add x19, x19, dir_sizes@PAGEOFF
    adrp x20, dir_count@PAGE
    add x20, x20, dir_count@PAGEOFF
    ldr x20, [x20]          // Count

    // Root directory is at index 0, its size is total used
    ldr x21, [x19]          // used_space = dir_sizes[0]

    // Calculate how much we need to free
    // free_space = TOTAL_SPACE - used_space
    // need_to_free = NEEDED_SPACE - free_space
    // need_to_free = NEEDED_SPACE - (TOTAL_SPACE - used_space)
    // need_to_free = NEEDED_SPACE - TOTAL_SPACE + used_space
    // need_to_free = used_space - (TOTAL_SPACE - NEEDED_SPACE)
    // need_to_free = used_space - 40000000
    // Load 40000000 = 0x2625A00
    movz x22, #0x5A00
    movk x22, #0x262, lsl #16
    sub x22, x21, x22       // need_to_free = used_space - 40000000

    // Find smallest directory >= need_to_free
    mov x23, x21            // Start with root size as candidate (largest possible)
    mov x24, x20            // Counter

part2_loop:
    cbz x24, part2_done
    ldr x0, [x19], #8       // Get directory size
    cmp x0, x22
    b.lt part2_skip         // Skip if < need_to_free
    cmp x0, x23
    b.ge part2_skip         // Skip if >= current minimum
    mov x23, x0             // New minimum
part2_skip:
    sub x24, x24, #1
    b part2_loop

part2_done:
    mov x0, x23
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
