// Day 12: Hill Climbing Algorithm - ARM64 Assembly (macOS)
//
// Part 1: BFS from S to E (shortest path)
// Part 2: BFS from any 'a' (or S) to E (shortest path from lowest elevations)
// Movement constraint: destination height at most 1 higher than current

.global _start
.align 4

// Constants
.equ MAX_ROWS, 50
.equ MAX_COLS, 80
.equ MAX_CELLS, 4000           // 50 * 80
.equ MAX_QUEUE, 8000
.equ BUFFER_SIZE, 5000

// Macro to load address
.macro load_addr reg, symbol
    adrp \reg, \symbol\()@PAGE
    add \reg, \reg, \symbol\()@PAGEOFF
.endm

.data
input_path: .asciz "../input.txt"
part1_str: .asciz "Part 1: "
part2_str: .asciz "Part 2: "
newline: .asciz "\n"

.bss
.align 4
grid: .skip MAX_CELLS              // Grid storage (heights 0-25)
visited: .skip MAX_CELLS           // Visited array
queue_r: .skip MAX_QUEUE * 4       // Queue rows
queue_c: .skip MAX_QUEUE * 4       // Queue cols
queue_d: .skip MAX_QUEUE * 4       // Queue distances
buffer: .skip BUFFER_SIZE          // Input file buffer
rows: .skip 4
cols: .skip 4
start_r: .skip 4
start_c: .skip 4
end_r: .skip 4
end_c: .skip 4

.text
_start:
    // Open and read input file
    mov x16, #5                 // open syscall
    load_addr x0, input_path
    mov x1, #0                  // O_RDONLY
    mov x2, #0
    svc #0x80

    mov x19, x0                 // Save file descriptor

    // Read file
    mov x16, #3                 // read syscall
    mov x0, x19
    load_addr x1, buffer
    mov x2, #BUFFER_SIZE
    svc #0x80

    mov x20, x0                 // Save file size

    // Close file
    mov x16, #6                 // close syscall
    mov x0, x19
    svc #0x80

    // Parse grid
    bl parse_grid

    // Part 1: BFS from S to E
    bl part1
    mov x21, x0                 // Save part1 result

    // Print "Part 1: "
    mov x16, #4                 // write syscall
    mov x0, #1
    load_addr x1, part1_str
    mov x2, #8
    svc #0x80

    mov x0, x21
    bl print_num

    // Print newline
    mov x16, #4
    mov x0, #1
    load_addr x1, newline
    mov x2, #1
    svc #0x80

    // Part 2: BFS from all 'a' positions to E
    bl part2
    mov x22, x0                 // Save part2 result

    // Print "Part 2: "
    mov x16, #4
    mov x0, #1
    load_addr x1, part2_str
    mov x2, #8
    svc #0x80

    mov x0, x22
    bl print_num

    // Print newline
    mov x16, #4
    mov x0, #1
    load_addr x1, newline
    mov x2, #1
    svc #0x80

    // Exit
    mov x16, #1                 // exit syscall
    mov x0, #0
    svc #0x80

// Parse grid from buffer
// Converts letters to heights (a=0, b=1, ..., z=25)
// S -> height 0, records start position
// E -> height 25, records end position
parse_grid:
    stp x29, x30, [sp, #-48]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]

    load_addr x0, buffer
    mov x1, #0                  // row count
    mov x2, #0                  // col count in current row
    mov x3, #0                  // grid index
    load_addr x4, grid

parse_loop:
    ldrb w5, [x0], #1           // Load byte

    cmp w5, #0                  // End of buffer
    b.eq parse_done

    cmp w5, #'\n'
    b.eq parse_newline

    // Check for S (start)
    cmp w5, #'S'
    b.ne check_end
    // Store start position
    load_addr x6, start_r
    str w1, [x6]
    load_addr x6, start_c
    str w2, [x6]
    mov w5, #0                  // S has height 'a' = 0
    b store_char

check_end:
    cmp w5, #'E'
    b.ne convert_height
    // Store end position
    load_addr x6, end_r
    str w1, [x6]
    load_addr x6, end_c
    str w2, [x6]
    mov w5, #25                 // E has height 'z' = 25
    b store_char

convert_height:
    // Convert a-z to 0-25
    sub w5, w5, #'a'

store_char:
    strb w5, [x4, x3]
    add x3, x3, #1
    add x2, x2, #1              // Increment column
    b parse_loop

parse_newline:
    cmp x2, #0                  // Skip empty lines
    b.eq parse_loop

    // Save cols on first line
    cmp x1, #0
    b.ne skip_save_cols
    load_addr x6, cols
    str w2, [x6]

skip_save_cols:
    add x1, x1, #1              // Increment row
    mov x2, #0                  // Reset column
    b parse_loop

parse_done:
    load_addr x6, rows
    str w1, [x6]

    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #48
    ret

// Part 1: BFS from start to end
// Returns shortest path distance
part1:
    stp x29, x30, [sp, #-32]!
    mov x29, sp
    stp x19, x20, [sp, #16]

    // Clear visited array
    load_addr x0, visited
    mov x1, #0
    mov x2, #MAX_CELLS
clear_visited1:
    strb w1, [x0], #1
    subs x2, x2, #1
    b.ne clear_visited1

    // Initialize queue with start position
    load_addr x0, queue_r
    load_addr x1, start_r
    ldr w2, [x1]
    str w2, [x0]                // queue_r[0] = start_r
    mov x19, x2                 // save start_r

    load_addr x0, queue_c
    load_addr x1, start_c
    ldr w2, [x1]
    str w2, [x0]                // queue_c[0] = start_c
    mov x20, x2                 // save start_c

    load_addr x0, queue_d
    str wzr, [x0]               // queue_d[0] = 0

    // Mark start as visited
    mov x0, x19
    mov x1, x20
    bl get_index
    load_addr x1, visited
    mov w2, #1
    strb w2, [x1, x0]

    mov x0, #0                  // queue_start
    mov x1, #1                  // queue_end
    bl bfs

    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #32
    ret

// Part 2: BFS from all 'a' positions to end
// Returns shortest path distance from any 'a'
part2:
    stp x29, x30, [sp, #-64]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]

    // Clear visited array
    load_addr x0, visited
    mov x1, #0
    mov x2, #MAX_CELLS
clear_visited2:
    strb w1, [x0], #1
    subs x2, x2, #1
    b.ne clear_visited2

    // Find all cells with height 0 (a) and add to queue
    load_addr x19, rows
    ldr w19, [x19]              // rows
    load_addr x20, cols
    ldr w20, [x20]              // cols

    mov x21, #0                 // queue_end
    mov x22, #0                 // r
    load_addr x23, grid

part2_find_a:
    cmp x22, x19
    b.ge part2_bfs

    mov x24, #0                 // c
part2_find_a_col:
    cmp x24, x20
    b.ge part2_next_row

    // Check if height is 0 (a)
    mov x0, x22
    mov x1, x24
    bl get_index
    ldrb w2, [x23, x0]
    cbnz w2, part2_next_col

    // Add to queue
    load_addr x3, queue_r
    str w22, [x3, x21, lsl #2]
    load_addr x3, queue_c
    str w24, [x3, x21, lsl #2]
    load_addr x3, queue_d
    str wzr, [x3, x21, lsl #2]

    // Mark as visited
    mov x0, x22
    mov x1, x24
    bl get_index
    load_addr x3, visited
    mov w4, #1
    strb w4, [x3, x0]

    add x21, x21, #1

part2_next_col:
    add x24, x24, #1
    b part2_find_a_col

part2_next_row:
    add x22, x22, #1
    b part2_find_a

part2_bfs:
    mov x0, #0                  // queue_start
    mov x1, x21                 // queue_end
    bl bfs

    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

// BFS: x0 = queue_start, x1 = queue_end
// Returns distance to end position, or -1 if not found
bfs:
    stp x29, x30, [sp, #-128]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]
    stp x25, x26, [sp, #64]
    stp x27, x28, [sp, #80]

    mov x19, x0                 // queue_start
    mov x20, x1                 // queue_end

    load_addr x21, queue_r
    load_addr x22, queue_c
    load_addr x23, queue_d

    load_addr x24, rows
    ldr w24, [x24]              // rows
    load_addr x25, cols
    ldr w25, [x25]              // cols

    load_addr x0, end_r
    ldr w26, [x0]               // end_r
    load_addr x0, end_c
    ldr w27, [x0]               // end_c

bfs_loop:
    cmp x19, x20
    b.ge bfs_not_found

    // Dequeue
    ldr w9, [x21, x19, lsl #2]  // cr = queue_r[queue_start]
    ldr w10, [x22, x19, lsl #2] // cc = queue_c[queue_start]
    ldr w11, [x23, x19, lsl #2] // dist = queue_d[queue_start]
    add x19, x19, #1

    // Check if we reached the end
    cmp w9, w26
    b.ne bfs_try_neighbors
    cmp w10, w27
    b.ne bfs_try_neighbors
    // Found end!
    mov x0, x11
    b bfs_done

bfs_try_neighbors:
    // Get current height
    mov x0, x9
    mov x1, x10
    // Save all BFS state before call
    stp x9, x10, [sp, #96]
    stp x11, x12, [sp, #112]
    bl get_index
    load_addr x6, grid
    ldrb w28, [x6, x0]          // current_height
    // Restore BFS state
    ldp x9, x10, [sp, #96]
    ldp x11, x12, [sp, #112]

    // Try all 4 directions: up, down, left, right
    // new_dist = dist + 1
    add w12, w11, #1

    // Direction 0: up (r-1, c)
    sub w13, w9, #1             // nr = r - 1
    mov w14, w10                // nc = c
    // Check bounds
    tbnz w13, #31, bfs_dir1     // if nr < 0 (signed negative)
    cmp w13, w24
    b.ge bfs_dir1
    // col is same as current, always valid
    // Check visited
    mov x0, x13
    mov x1, x14
    stp x9, x10, [sp, #96]
    stp x11, x12, [sp, #112]
    bl get_index
    mov x15, x0                 // save index
    load_addr x1, visited
    ldrb w2, [x1, x0]
    ldp x9, x10, [sp, #96]
    ldp x11, x12, [sp, #112]
    cbnz w2, bfs_dir1
    // Check height
    load_addr x1, grid
    ldrb w2, [x1, x15]          // next_height
    add w3, w28, #1             // current_height + 1
    cmp w2, w3
    b.gt bfs_dir1
    // Valid - mark visited and enqueue
    load_addr x1, visited
    mov w2, #1
    strb w2, [x1, x15]
    str w13, [x21, x20, lsl #2]
    str w14, [x22, x20, lsl #2]
    str w12, [x23, x20, lsl #2]
    add x20, x20, #1

bfs_dir1:
    // Direction 1: down (r+1, c)
    add w13, w9, #1             // nr = r + 1
    mov w14, w10                // nc = c
    cmp w13, w24
    b.ge bfs_dir2
    // col is same as current, always valid
    mov x0, x13
    mov x1, x14
    stp x9, x10, [sp, #96]
    stp x11, x12, [sp, #112]
    bl get_index
    mov x15, x0
    load_addr x1, visited
    ldrb w2, [x1, x0]
    ldp x9, x10, [sp, #96]
    ldp x11, x12, [sp, #112]
    cbnz w2, bfs_dir2
    load_addr x1, grid
    ldrb w2, [x1, x15]
    add w3, w28, #1
    cmp w2, w3
    b.gt bfs_dir2
    load_addr x1, visited
    mov w2, #1
    strb w2, [x1, x15]
    str w13, [x21, x20, lsl #2]
    str w14, [x22, x20, lsl #2]
    str w12, [x23, x20, lsl #2]
    add x20, x20, #1

bfs_dir2:
    // Direction 2: left (r, c-1)
    mov w13, w9                 // nr = r
    sub w14, w10, #1            // nc = c - 1
    tbnz w14, #31, bfs_dir3     // if nc < 0 (signed negative)
    // row is same as current, always valid
    mov x0, x13
    mov x1, x14
    stp x9, x10, [sp, #96]
    stp x11, x12, [sp, #112]
    bl get_index
    mov x15, x0
    load_addr x1, visited
    ldrb w2, [x1, x0]
    ldp x9, x10, [sp, #96]
    ldp x11, x12, [sp, #112]
    cbnz w2, bfs_dir3
    load_addr x1, grid
    ldrb w2, [x1, x15]
    add w3, w28, #1
    cmp w2, w3
    b.gt bfs_dir3
    load_addr x1, visited
    mov w2, #1
    strb w2, [x1, x15]
    str w13, [x21, x20, lsl #2]
    str w14, [x22, x20, lsl #2]
    str w12, [x23, x20, lsl #2]
    add x20, x20, #1

bfs_dir3:
    // Direction 3: right (r, c+1)
    mov w13, w9                 // nr = r
    add w14, w10, #1            // nc = c + 1
    cmp w14, w25
    b.ge bfs_next
    // row is same as current, always valid
    mov x0, x13
    mov x1, x14
    stp x9, x10, [sp, #96]
    stp x11, x12, [sp, #112]
    bl get_index
    mov x15, x0
    load_addr x1, visited
    ldrb w2, [x1, x0]
    ldp x9, x10, [sp, #96]
    ldp x11, x12, [sp, #112]
    cbnz w2, bfs_next
    load_addr x1, grid
    ldrb w2, [x1, x15]
    add w3, w28, #1
    cmp w2, w3
    b.gt bfs_next
    load_addr x1, visited
    mov w2, #1
    strb w2, [x1, x15]
    str w13, [x21, x20, lsl #2]
    str w14, [x22, x20, lsl #2]
    str w12, [x23, x20, lsl #2]
    add x20, x20, #1

bfs_next:
    b bfs_loop

bfs_not_found:
    mov x0, #-1

bfs_done:
    ldp x27, x28, [sp, #80]
    ldp x25, x26, [sp, #64]
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #128
    ret

// Get index from (r, c): r * cols + c
get_index:
    load_addr x2, cols
    ldr w3, [x2]
    mul w2, w0, w3
    add w0, w2, w1
    ret

// Print number in x0
print_num:
    stp x29, x30, [sp, #-48]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    str x21, [sp, #32]

    mov x19, x0
    add x20, sp, #40
    mov x21, #0

    // Handle negative numbers
    cmp x19, #0
    b.ge print_positive
    // Print minus sign
    mov x16, #4
    mov x0, #1
    sub sp, sp, #16
    mov w1, #'-'
    strb w1, [sp]
    mov x1, sp
    mov x2, #1
    svc #0x80
    add sp, sp, #16
    neg x19, x19

print_positive:
    cbz x19, print_zero

convert_loop:
    cbz x19, print_digits
    mov x0, x19
    mov x1, #10
    udiv x2, x0, x1
    msub x3, x2, x1, x0
    add w3, w3, #'0'
    sub x20, x20, #1
    strb w3, [x20]
    add x21, x21, #1
    mov x19, x2
    b convert_loop

print_zero:
    mov w3, #'0'
    sub x20, x20, #1
    strb w3, [x20]
    mov x21, #1

print_digits:
    mov x16, #4
    mov x0, #1
    mov x1, x20
    mov x2, x21
    svc #0x80

    ldr x21, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #48
    ret
