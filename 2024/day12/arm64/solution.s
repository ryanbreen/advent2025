.global _start
.align 4

// Macro to load address
.macro load_addr reg, symbol
    adrp \reg, \symbol\()@PAGE
    add \reg, \reg, \symbol\()@PAGEOFF
.endm

// Constants
.equ MAX_GRID_SIZE, 150
.equ MAX_CELLS, 22500       // 150 * 150
.equ MAX_QUEUE, 22500
.equ BUFFER_SIZE, 25000

.data
input_path: .asciz "../input.txt"
part1_str: .asciz "Part 1: "
part2_str: .asciz "Part 2: "
newline: .asciz "\n"

.bss
.align 4
grid: .skip MAX_CELLS           // Grid storage (flat array)
visited: .skip MAX_CELLS        // Visited array (0/1)
region: .skip MAX_CELLS         // Current region (0/1)
queue_r: .skip MAX_QUEUE * 4    // Queue rows
queue_c: .skip MAX_QUEUE * 4    // Queue cols
buffer: .skip BUFFER_SIZE       // Input file buffer
rows: .skip 4
cols: .skip 4

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

    // Part 1
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

    // Part 2
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
parse_grid:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    load_addr x0, buffer
    mov x1, #0                  // row count
    mov x2, #0                  // col count
    mov x3, #0                  // grid index
    load_addr x4, grid

parse_loop:
    ldrb w5, [x0], #1           // Load byte

    cmp w5, #0                  // End of buffer
    b.eq parse_done

    cmp w5, #'\n'
    b.eq parse_newline

    // Store character in grid
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

    ldp x29, x30, [sp], #16
    ret

// Part 1: Calculate sum of area * perimeter
part1:
    stp x29, x30, [sp, #-64]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]

    mov x19, #0                 // total price

    // Clear visited array
    load_addr x0, visited
    mov x1, #0
    mov x2, #MAX_CELLS
clear_visited1:
    strb w1, [x0], #1
    subs x2, x2, #1
    b.ne clear_visited1

    load_addr x5, rows
    ldr w20, [x5]               // rows
    load_addr x5, cols
    ldr w21, [x5]               // cols

    mov x22, #0                 // r
part1_row_loop:
    cmp x22, x20
    b.ge part1_done

    mov x23, #0                 // c
part1_col_loop:
    cmp x23, x21
    b.ge part1_next_row

    // Check if visited
    mov x0, x22
    mov x1, x23
    bl get_index
    load_addr x1, visited
    ldrb w2, [x1, x0]
    cbnz w2, part1_next_col

    // BFS to find region
    mov x0, x22
    mov x1, x23
    bl bfs_region_part1

    add x19, x19, x0            // Add to total

part1_next_col:
    add x23, x23, #1
    b part1_col_loop

part1_next_row:
    add x22, x22, #1
    b part1_row_loop

part1_done:
    mov x0, x19
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

// BFS region for part 1, returns area * perimeter
bfs_region_part1:
    stp x29, x30, [sp, #-96]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]
    stp x25, x26, [sp, #64]
    stp x27, x28, [sp, #80]

    mov x19, x0                 // start_r
    mov x20, x1                 // start_c

    // Clear region array
    load_addr x0, region
    mov x1, #0
    mov x2, #MAX_CELLS
clear_region_p1:
    strb w1, [x0], #1
    subs x2, x2, #1
    b.ne clear_region_p1

    // Get plant type
    mov x0, x19
    mov x1, x20
    bl get_cell
    mov x21, x0                 // plant type

    // Initialize queue
    load_addr x22, queue_r
    load_addr x23, queue_c
    str w19, [x22]              // queue[0] = start_r
    str w20, [x23]              // queue[0] = start_c
    mov x24, #0                 // queue_start
    mov x25, #1                 // queue_end

bfs_p1_loop:
    cmp x24, x25
    b.ge bfs_p1_done

    // Dequeue
    ldr w26, [x22, x24, lsl #2] // cr
    ldr w27, [x23, x24, lsl #2] // cc
    add x24, x24, #1

    // Check if visited
    mov x0, x26
    mov x1, x27
    bl get_index
    load_addr x1, visited
    ldrb w2, [x1, x0]
    cbnz w2, bfs_p1_loop

    // Check bounds
    cmp x26, #0
    b.lt bfs_p1_loop
    load_addr x1, rows
    ldr w2, [x1]
    cmp x26, x2
    b.ge bfs_p1_loop
    cmp x27, #0
    b.lt bfs_p1_loop
    load_addr x1, cols
    ldr w2, [x1]
    cmp x27, x2
    b.ge bfs_p1_loop

    // Check plant type
    mov x0, x26
    mov x1, x27
    bl get_cell
    cmp x0, x21
    b.ne bfs_p1_loop

    // Mark visited and in region
    mov x0, x26
    mov x1, x27
    bl get_index
    load_addr x1, visited
    mov w2, #1
    strb w2, [x1, x0]
    load_addr x1, region
    strb w2, [x1, x0]

    // Enqueue neighbors
    // Right
    mov x0, x26
    add x1, x27, #1
    mov x2, x22
    mov x3, x23
    mov x4, x25
    bl try_enqueue
    mov x25, x0

    // Left
    mov x0, x26
    sub x1, x27, #1
    mov x2, x22
    mov x3, x23
    mov x4, x25
    bl try_enqueue
    mov x25, x0

    // Down
    add x0, x26, #1
    mov x1, x27
    mov x2, x22
    mov x3, x23
    mov x4, x25
    bl try_enqueue
    mov x25, x0

    // Up
    sub x0, x26, #1
    mov x1, x27
    mov x2, x22
    mov x3, x23
    mov x4, x25
    bl try_enqueue
    mov x25, x0

    b bfs_p1_loop

bfs_p1_done:
    // Calculate area and perimeter
    bl calculate_area_perimeter

    ldp x27, x28, [sp, #80]
    ldp x25, x26, [sp, #64]
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #96
    ret

// Try to enqueue if not visited
// x0=r, x1=c, x2=queue_r, x3=queue_c, x4=queue_end
// Returns new queue_end
try_enqueue:
    stp x29, x30, [sp, #-64]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    str x23, [sp, #48]

    mov x19, x0                 // r
    mov x20, x1                 // c
    mov x21, x2                 // queue_r
    mov x22, x3                 // queue_c
    mov x23, x4                 // queue_end

    // Check if visited
    mov x0, x19
    mov x1, x20
    bl get_index
    load_addr x1, visited
    ldrb w5, [x1, x0]
    cbnz w5, try_enqueue_skip

    // Enqueue
    str w19, [x21, x23, lsl #2]
    str w20, [x22, x23, lsl #2]
    add x23, x23, #1

try_enqueue_skip:
    mov x0, x23
    ldr x23, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

// Calculate area * perimeter for current region
calculate_area_perimeter:
    stp x29, x30, [sp, #-48]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]

    mov x19, #0                 // area
    mov x20, #0                 // perimeter

    load_addr x5, rows
    ldr w21, [x5]
    load_addr x5, cols
    ldr w22, [x5]

    mul w5, w21, w22
    mov x6, #0

calc_ap_loop:
    cmp x6, x5
    b.ge calc_ap_done

    load_addr x1, region
    ldrb w2, [x1, x6]
    cbz w2, calc_ap_next

    // In region
    add x19, x19, #1

    // Calculate perimeter contribution
    mov x0, x6
    bl get_row_col
    mov x7, x0                  // r
    mov x8, x1                  // c

    // Check 4 neighbors
    // Right
    add x1, x8, #1
    mov x0, x7
    bl is_in_region
    cbz x0, calc_ap_perim1
    b calc_ap_check2
calc_ap_perim1:
    add x20, x20, #1

calc_ap_check2:
    // Left
    sub x1, x8, #1
    mov x0, x7
    bl is_in_region
    cbz x0, calc_ap_perim2
    b calc_ap_check3
calc_ap_perim2:
    add x20, x20, #1

calc_ap_check3:
    // Down
    add x0, x7, #1
    mov x1, x8
    bl is_in_region
    cbz x0, calc_ap_perim3
    b calc_ap_check4
calc_ap_perim3:
    add x20, x20, #1

calc_ap_check4:
    // Up
    sub x0, x7, #1
    mov x1, x8
    bl is_in_region
    cbz x0, calc_ap_perim4
    b calc_ap_next
calc_ap_perim4:
    add x20, x20, #1

calc_ap_next:
    add x6, x6, #1
    b calc_ap_loop

calc_ap_done:
    mul x0, x19, x20

    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #48
    ret

// Check if (r, c) is in region
is_in_region:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    // Check bounds
    cmp x0, #0
    b.lt is_in_region_no
    load_addr x2, rows
    ldr w3, [x2]
    cmp x0, x3
    b.ge is_in_region_no
    cmp x1, #0
    b.lt is_in_region_no
    load_addr x2, cols
    ldr w3, [x2]
    cmp x1, x3
    b.ge is_in_region_no

    bl get_index
    load_addr x1, region
    ldrb w0, [x1, x0]

    ldp x29, x30, [sp], #16
    ret

is_in_region_no:
    mov x0, #0
    ldp x29, x30, [sp], #16
    ret

// Part 2: Calculate sum of area * sides
part2:
    stp x29, x30, [sp, #-64]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]

    mov x19, #0                 // total price

    // Clear visited array
    load_addr x0, visited
    mov x1, #0
    mov x2, #MAX_CELLS
clear_visited2:
    strb w1, [x0], #1
    subs x2, x2, #1
    b.ne clear_visited2

    load_addr x5, rows
    ldr w20, [x5]               // rows
    load_addr x5, cols
    ldr w21, [x5]               // cols

    mov x22, #0                 // r
part2_row_loop:
    cmp x22, x20
    b.ge part2_done

    mov x23, #0                 // c
part2_col_loop:
    cmp x23, x21
    b.ge part2_next_row

    // Check if visited
    mov x0, x22
    mov x1, x23
    bl get_index
    load_addr x1, visited
    ldrb w2, [x1, x0]
    cbnz w2, part2_next_col

    // BFS to find region
    mov x0, x22
    mov x1, x23
    bl bfs_region_part2

    add x19, x19, x0            // Add to total

part2_next_col:
    add x23, x23, #1
    b part2_col_loop

part2_next_row:
    add x22, x22, #1
    b part2_row_loop

part2_done:
    mov x0, x19
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

// BFS region for part 2, returns area * sides
bfs_region_part2:
    stp x29, x30, [sp, #-96]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]
    stp x25, x26, [sp, #64]
    stp x27, x28, [sp, #80]

    mov x19, x0                 // start_r
    mov x20, x1                 // start_c

    // Clear region array
    load_addr x0, region
    mov x1, #0
    mov x2, #MAX_CELLS
clear_region_p2:
    strb w1, [x0], #1
    subs x2, x2, #1
    b.ne clear_region_p2

    // Get plant type
    mov x0, x19
    mov x1, x20
    bl get_cell
    mov x21, x0                 // plant type

    // Initialize queue
    load_addr x22, queue_r
    load_addr x23, queue_c
    str w19, [x22]
    str w20, [x23]
    mov x24, #0                 // queue_start
    mov x25, #1                 // queue_end

bfs_p2_loop:
    cmp x24, x25
    b.ge bfs_p2_done

    // Dequeue
    ldr w26, [x22, x24, lsl #2]
    ldr w27, [x23, x24, lsl #2]
    add x24, x24, #1

    // Check if visited
    mov x0, x26
    mov x1, x27
    bl get_index
    load_addr x1, visited
    ldrb w2, [x1, x0]
    cbnz w2, bfs_p2_loop

    // Check bounds
    cmp x26, #0
    b.lt bfs_p2_loop
    load_addr x1, rows
    ldr w2, [x1]
    cmp x26, x2
    b.ge bfs_p2_loop
    cmp x27, #0
    b.lt bfs_p2_loop
    load_addr x1, cols
    ldr w2, [x1]
    cmp x27, x2
    b.ge bfs_p2_loop

    // Check plant type
    mov x0, x26
    mov x1, x27
    bl get_cell
    cmp x0, x21
    b.ne bfs_p2_loop

    // Mark visited and in region
    mov x0, x26
    mov x1, x27
    bl get_index
    load_addr x1, visited
    mov w2, #1
    strb w2, [x1, x0]
    load_addr x1, region
    strb w2, [x1, x0]

    // Enqueue neighbors (same as part1)
    mov x0, x26
    add x1, x27, #1
    mov x2, x22
    mov x3, x23
    mov x4, x25
    bl try_enqueue
    mov x25, x0

    mov x0, x26
    sub x1, x27, #1
    mov x2, x22
    mov x3, x23
    mov x4, x25
    bl try_enqueue
    mov x25, x0

    add x0, x26, #1
    mov x1, x27
    mov x2, x22
    mov x3, x23
    mov x4, x25
    bl try_enqueue
    mov x25, x0

    sub x0, x26, #1
    mov x1, x27
    mov x2, x22
    mov x3, x23
    mov x4, x25
    bl try_enqueue
    mov x25, x0

    b bfs_p2_loop

bfs_p2_done:
    // Calculate area and sides
    bl calculate_area_sides

    ldp x27, x28, [sp, #80]
    ldp x25, x26, [sp, #64]
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #96
    ret

// Calculate area * sides for current region (sides = corners)
calculate_area_sides:
    stp x29, x30, [sp, #-64]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]

    mov x19, #0                 // area
    mov x20, #0                 // corners

    load_addr x5, rows
    ldr w21, [x5]
    load_addr x5, cols
    ldr w22, [x5]

    mul w5, w21, w22
    mov x6, #0

calc_as_loop:
    cmp x6, x5
    b.ge calc_as_done

    load_addr x1, region
    ldrb w2, [x1, x6]
    cbz w2, calc_as_next

    // In region
    add x19, x19, #1

    // Count corners for this cell
    mov x0, x6
    bl get_row_col
    mov x23, x0                 // r
    mov x24, x1                 // c

    bl count_corners_at
    add x20, x20, x0

calc_as_next:
    add x6, x6, #1
    b calc_as_loop

calc_as_done:
    mul x0, x19, x20

    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

// Count corners at position (x23, x24)
count_corners_at:
    stp x29, x30, [sp, #-64]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    str x25, [sp, #48]

    mov x25, #0                 // corner count

    // Check 8 neighbors
    sub x0, x23, #1
    mov x1, x24
    bl is_in_region
    mov x19, x0                 // up

    add x0, x23, #1
    mov x1, x24
    bl is_in_region
    mov x20, x0                 // down

    mov x0, x23
    sub x1, x24, #1
    bl is_in_region
    mov x21, x0                 // left

    mov x0, x23
    add x1, x24, #1
    bl is_in_region
    mov x22, x0                 // right

    sub x0, x23, #1
    sub x1, x24, #1
    bl is_in_region
    str x0, [sp, #56]           // up_left

    sub x0, x23, #1
    add x1, x24, #1
    bl is_in_region
    mov x10, x0                 // up_right

    add x0, x23, #1
    sub x1, x24, #1
    bl is_in_region
    mov x11, x0                 // down_left

    add x0, x23, #1
    add x1, x24, #1
    bl is_in_region
    mov x12, x0                 // down_right

    // Top-left corner
    // Convex: !up && !left
    cbz x19, check_tl_convex
    b check_tl_concave
check_tl_convex:
    cbz x21, tl_corner
    b check_tr
check_tl_concave:
    // Concave: up && left && !up_left
    cbz x21, check_tr
    ldr x0, [sp, #56]
    cbnz x0, check_tr
tl_corner:
    add x25, x25, #1

check_tr:
    // Top-right corner
    cbz x19, check_tr_convex
    b check_tr_concave
check_tr_convex:
    cbz x22, tr_corner
    b check_bl
check_tr_concave:
    cbz x22, check_bl
    cbz x10, tr_corner
    b check_bl
tr_corner:
    add x25, x25, #1

check_bl:
    // Bottom-left corner
    cbz x20, check_bl_convex
    b check_bl_concave
check_bl_convex:
    cbz x21, bl_corner
    b check_br
check_bl_concave:
    cbz x21, check_br
    cbz x11, bl_corner
    b check_br
bl_corner:
    add x25, x25, #1

check_br:
    // Bottom-right corner
    cbz x20, check_br_convex
    b check_br_concave
check_br_convex:
    cbz x22, br_corner
    b corners_done
check_br_concave:
    cbz x22, corners_done
    cbz x12, br_corner
    b corners_done
br_corner:
    add x25, x25, #1

corners_done:
    mov x0, x25
    ldr x25, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

// Get cell at (r, c)
get_cell:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    bl get_index
    load_addr x1, grid
    ldrb w0, [x1, x0]

    ldp x29, x30, [sp], #16
    ret

// Get index from (r, c)
get_index:
    load_addr x2, cols
    ldr w3, [x2]
    mul w2, w0, w3
    add w0, w2, w1
    ret

// Get (r, c) from index
get_row_col:
    load_addr x2, cols
    ldr w3, [x2]
    udiv w1, w0, w3
    msub w2, w1, w3, w0
    mov x0, x1
    mov x1, x2
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
