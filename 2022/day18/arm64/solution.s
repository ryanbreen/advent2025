// Day 18: Boiling Boulders - ARM64 Assembly (macOS)
//
// Part 1: Count all exposed cube faces (not touching another cube)
// Part 2: Count only exterior surface area (BFS flood fill from outside)
//
// Algorithm:
// - Parse cube coordinates into a 3D grid
// - Part 1: For each cube, count neighbors not in the grid (6 - adjacent_count)
// - Part 2: BFS from corner of bounding box to find all exterior air cells
//           Then count cube faces that touch exterior air

.global _start
.align 4

// Constants - grid dimensions need to cover coordinate range with padding
.equ GRID_SIZE, 25              // Max coordinate + 2 for padding
.equ GRID_DIM, 25
.equ GRID_TOTAL, 15625          // 25 * 25 * 25
.equ MAX_CUBES, 3000
.equ MAX_QUEUE, 20000           // For BFS, needs to handle full exterior
.equ BUFFER_SIZE, 20000

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
buffer: .skip BUFFER_SIZE           // Input file buffer
cubes_x: .skip MAX_CUBES * 4        // X coordinates (scaled +1)
cubes_y: .skip MAX_CUBES * 4        // Y coordinates (scaled +1)
cubes_z: .skip MAX_CUBES * 4        // Z coordinates (scaled +1)
cube_count: .skip 4                 // Number of cubes
grid: .skip GRID_TOTAL              // 3D grid: 1 if cube exists
exterior: .skip GRID_TOTAL          // 3D grid: 1 if exterior air
queue_x: .skip MAX_QUEUE * 4        // BFS queue X
queue_y: .skip MAX_QUEUE * 4        // BFS queue Y
queue_z: .skip MAX_QUEUE * 4        // BFS queue Z
// Bounds for BFS
min_x: .skip 4
max_x: .skip 4
min_y: .skip 4
max_y: .skip 4
min_z: .skip 4
max_z: .skip 4

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

    // Parse input
    bl parse_input

    // Part 1: Count exposed faces
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

    // Part 2: Count exterior surface area
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

// Parse input: extract x,y,z coordinates
// Each line is "x,y,z\n"
// We add 1 to each coordinate to create padding at 0
parse_input:
    stp x29, x30, [sp, #-64]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]

    load_addr x0, buffer
    add x1, x0, x20             // End of buffer

    load_addr x2, cubes_x
    load_addr x3, cubes_y
    load_addr x4, cubes_z
    mov x5, #0                  // Cube count

    // Initialize bounds
    mov x19, #GRID_SIZE         // min values start high
    mov x20, #0                 // max values start low
    mov x21, #GRID_SIZE
    mov x22, #0
    mov x23, #GRID_SIZE
    mov x24, #0

parse_loop:
    cmp x0, x1
    b.ge parse_done

    // Skip whitespace
    ldrb w6, [x0]
    cmp w6, #'\n'
    b.eq parse_skip_ws
    cmp w6, #' '
    b.eq parse_skip_ws
    cmp w6, #'\r'
    b.eq parse_skip_ws
    cmp w6, #0
    b.eq parse_done

    // Parse x
    mov x7, #0                  // x value
parse_x:
    ldrb w6, [x0], #1
    cmp w6, #','
    b.eq parse_x_done
    sub w6, w6, #'0'
    mov x8, #10
    mul x7, x7, x8
    add x7, x7, x6
    b parse_x

parse_x_done:
    add x7, x7, #1              // Add 1 for padding
    str w7, [x2, x5, lsl #2]    // Store x
    // Update bounds
    cmp x7, x19
    csel x19, x7, x19, lt
    cmp x7, x20
    csel x20, x7, x20, gt

    // Parse y
    mov x8, #0                  // y value
parse_y:
    ldrb w6, [x0], #1
    cmp w6, #','
    b.eq parse_y_done
    sub w6, w6, #'0'
    mov x9, #10
    mul x8, x8, x9
    add x8, x8, x6
    b parse_y

parse_y_done:
    add x8, x8, #1              // Add 1 for padding
    str w8, [x3, x5, lsl #2]    // Store y
    // Update bounds
    cmp x8, x21
    csel x21, x8, x21, lt
    cmp x8, x22
    csel x22, x8, x22, gt

    // Parse z
    mov x9, #0                  // z value
parse_z:
    ldrb w6, [x0], #1
    cmp w6, #'\n'
    b.eq parse_z_done
    cmp w6, #'\r'
    b.eq parse_z_cr
    cmp w6, #0
    b.eq parse_z_done
    sub w6, w6, #'0'
    mov x10, #10
    mul x9, x9, x10
    add x9, x9, x6
    b parse_z

parse_z_cr:
    // Skip the \r and continue to check for \n
    b parse_z

parse_z_done:
    add x9, x9, #1              // Add 1 for padding
    str w9, [x4, x5, lsl #2]    // Store z
    // Update bounds
    cmp x9, x23
    csel x23, x9, x23, lt
    cmp x9, x24
    csel x24, x9, x24, gt

    add x5, x5, #1              // Increment cube count
    b parse_loop

parse_skip_ws:
    add x0, x0, #1
    b parse_loop

parse_done:
    // Save cube count
    load_addr x0, cube_count
    str w5, [x0]

    // Save bounds with 1 unit padding
    sub x19, x19, #1            // min - 1
    add x20, x20, #1            // max + 1
    sub x21, x21, #1
    add x22, x22, #1
    sub x23, x23, #1
    add x24, x24, #1

    load_addr x0, min_x
    str w19, [x0]
    load_addr x0, max_x
    str w20, [x0]
    load_addr x0, min_y
    str w21, [x0]
    load_addr x0, max_y
    str w22, [x0]
    load_addr x0, min_z
    str w23, [x0]
    load_addr x0, max_z
    str w24, [x0]

    // Build the 3D grid
    bl build_grid

    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

// Build 3D grid from cube coordinates
build_grid:
    stp x29, x30, [sp, #-48]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]

    // Clear grid
    load_addr x0, grid
    mov x1, #0
    mov x2, #GRID_TOTAL
clear_grid:
    strb w1, [x0], #1
    subs x2, x2, #1
    b.ne clear_grid

    // Mark cube positions in grid
    load_addr x19, cubes_x
    load_addr x20, cubes_y
    load_addr x21, cubes_z
    load_addr x0, cube_count
    ldr w22, [x0]               // cube_count

    mov x0, #0                  // i = 0
build_loop:
    cmp w0, w22
    b.ge build_done

    ldr w1, [x19, x0, lsl #2]   // x
    ldr w2, [x20, x0, lsl #2]   // y
    ldr w3, [x21, x0, lsl #2]   // z

    // Calculate index: x * GRID_DIM * GRID_DIM + y * GRID_DIM + z
    mov x4, #GRID_DIM
    mul x5, x1, x4              // x * GRID_DIM
    mul x5, x5, x4              // x * GRID_DIM * GRID_DIM
    mul x6, x2, x4              // y * GRID_DIM
    add x5, x5, x6              // + y * GRID_DIM
    add x5, x5, x3              // + z

    load_addr x6, grid
    mov w7, #1
    strb w7, [x6, x5]

    add x0, x0, #1
    b build_loop

build_done:
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #48
    ret

// Get grid index for (x, y, z)
// x0 = x, x1 = y, x2 = z
// Returns index in x0
get_index:
    mov x3, #GRID_DIM
    mul x4, x0, x3              // x * GRID_DIM
    mul x4, x4, x3              // x * GRID_DIM * GRID_DIM
    mul x5, x1, x3              // y * GRID_DIM
    add x4, x4, x5              // + y * GRID_DIM
    add x0, x4, x2              // + z
    ret

// Part 1: Count total exposed surface area
// For each cube, count neighbors that are NOT in the grid
part1:
    stp x29, x30, [sp, #-80]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]
    stp x25, x26, [sp, #64]

    load_addr x19, cubes_x
    load_addr x20, cubes_y
    load_addr x21, cubes_z
    load_addr x0, cube_count
    ldr w22, [x0]               // cube_count
    load_addr x23, grid

    mov x24, #0                 // surface_area
    mov x25, #0                 // i = 0

part1_loop:
    cmp w25, w22
    b.ge part1_done

    ldr w9, [x19, x25, lsl #2]  // x
    ldr w10, [x20, x25, lsl #2] // y
    ldr w11, [x21, x25, lsl #2] // z

    // Check 6 neighbors
    // Neighbor 1: x+1, y, z
    add w0, w9, #1
    mov w1, w10
    mov w2, w11
    bl get_index
    ldrb w3, [x23, x0]
    cbz w3, part1_add1
    b part1_check2
part1_add1:
    add x24, x24, #1

part1_check2:
    // Neighbor 2: x-1, y, z
    sub w0, w9, #1
    mov w1, w10
    mov w2, w11
    bl get_index
    ldrb w3, [x23, x0]
    cbz w3, part1_add2
    b part1_check3
part1_add2:
    add x24, x24, #1

part1_check3:
    // Neighbor 3: x, y+1, z
    mov w0, w9
    add w1, w10, #1
    mov w2, w11
    bl get_index
    ldrb w3, [x23, x0]
    cbz w3, part1_add3
    b part1_check4
part1_add3:
    add x24, x24, #1

part1_check4:
    // Neighbor 4: x, y-1, z
    mov w0, w9
    sub w1, w10, #1
    mov w2, w11
    bl get_index
    ldrb w3, [x23, x0]
    cbz w3, part1_add4
    b part1_check5
part1_add4:
    add x24, x24, #1

part1_check5:
    // Neighbor 5: x, y, z+1
    mov w0, w9
    mov w1, w10
    add w2, w11, #1
    bl get_index
    ldrb w3, [x23, x0]
    cbz w3, part1_add5
    b part1_check6
part1_add5:
    add x24, x24, #1

part1_check6:
    // Neighbor 6: x, y, z-1
    mov w0, w9
    mov w1, w10
    sub w2, w11, #1
    bl get_index
    ldrb w3, [x23, x0]
    cbz w3, part1_add6
    b part1_next
part1_add6:
    add x24, x24, #1

part1_next:
    add x25, x25, #1
    b part1_loop

part1_done:
    mov x0, x24

    ldp x25, x26, [sp, #64]
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #80
    ret

// Part 2: Count exterior surface area
// BFS from outside bounding box to find all exterior air cells
// Then count cube faces touching exterior air
part2:
    stp x29, x30, [sp, #-112]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]
    stp x25, x26, [sp, #64]
    stp x27, x28, [sp, #80]

    // Clear exterior grid
    load_addr x0, exterior
    mov x1, #0
    mov x2, #GRID_TOTAL
clear_exterior:
    strb w1, [x0], #1
    subs x2, x2, #1
    b.ne clear_exterior

    // Load bounds
    load_addr x0, min_x
    ldr w19, [x0]               // min_x
    load_addr x0, max_x
    ldr w20, [x0]               // max_x
    load_addr x0, min_y
    ldr w21, [x0]               // min_y
    load_addr x0, max_y
    ldr w22, [x0]               // max_y
    load_addr x0, min_z
    ldr w23, [x0]               // min_z
    load_addr x0, max_z
    ldr w24, [x0]               // max_z

    // Initialize BFS queue with starting position (min_x, min_y, min_z)
    load_addr x25, queue_x
    load_addr x26, queue_y
    load_addr x27, queue_z

    str w19, [x25]              // queue_x[0] = min_x
    str w21, [x26]              // queue_y[0] = min_y
    str w23, [x27]              // queue_z[0] = min_z

    // Mark starting position as exterior
    mov w0, w19
    mov w1, w21
    mov w2, w23
    bl get_index
    load_addr x1, exterior
    mov w2, #1
    strb w2, [x1, x0]

    mov x9, #0                  // queue_start
    mov x10, #1                 // queue_end

    load_addr x11, grid
    load_addr x12, exterior

    // BFS loop
bfs_loop:
    cmp x9, x10
    b.ge bfs_done

    // Dequeue
    ldr w13, [x25, x9, lsl #2]  // x
    ldr w14, [x26, x9, lsl #2]  // y
    ldr w15, [x27, x9, lsl #2]  // z
    add x9, x9, #1

    // Try 6 neighbors
    // Save state for neighbor checking
    str x9, [sp, #96]
    str x10, [sp, #104]

    // Neighbor 1: x+1, y, z
    add w0, w13, #1
    mov w1, w14
    mov w2, w15
    cmp w0, w20                 // x+1 <= max_x
    b.gt bfs_n2
    bl try_enqueue

bfs_n2:
    ldr x9, [sp, #96]
    ldr x10, [sp, #104]
    // Neighbor 2: x-1, y, z
    sub w0, w13, #1
    mov w1, w14
    mov w2, w15
    cmp w0, w19                 // x-1 >= min_x
    b.lt bfs_n3
    bl try_enqueue

bfs_n3:
    ldr x9, [sp, #96]
    ldr x10, [sp, #104]
    // Neighbor 3: x, y+1, z
    mov w0, w13
    add w1, w14, #1
    mov w2, w15
    cmp w1, w22                 // y+1 <= max_y
    b.gt bfs_n4
    bl try_enqueue

bfs_n4:
    ldr x9, [sp, #96]
    ldr x10, [sp, #104]
    // Neighbor 4: x, y-1, z
    mov w0, w13
    sub w1, w14, #1
    mov w2, w15
    cmp w1, w21                 // y-1 >= min_y
    b.lt bfs_n5
    bl try_enqueue

bfs_n5:
    ldr x9, [sp, #96]
    ldr x10, [sp, #104]
    // Neighbor 5: x, y, z+1
    mov w0, w13
    mov w1, w14
    add w2, w15, #1
    cmp w2, w24                 // z+1 <= max_z
    b.gt bfs_n6
    bl try_enqueue

bfs_n6:
    ldr x9, [sp, #96]
    ldr x10, [sp, #104]
    // Neighbor 6: x, y, z-1
    mov w0, w13
    mov w1, w14
    sub w2, w15, #1
    cmp w2, w23                 // z-1 >= min_z
    b.lt bfs_next
    bl try_enqueue

bfs_next:
    ldr x9, [sp, #96]
    ldr x10, [sp, #104]
    b bfs_loop

bfs_done:
    // Count exterior surface area
    // For each cube, count neighbors that are in exterior
    load_addr x19, cubes_x
    load_addr x20, cubes_y
    load_addr x21, cubes_z
    load_addr x0, cube_count
    ldr w22, [x0]
    load_addr x23, exterior

    mov x24, #0                 // surface_area
    mov x25, #0                 // i = 0

part2_count_loop:
    cmp w25, w22
    b.ge part2_done

    ldr w9, [x19, x25, lsl #2]  // x
    ldr w10, [x20, x25, lsl #2] // y
    ldr w11, [x21, x25, lsl #2] // z

    // Check 6 neighbors for exterior
    // Neighbor 1: x+1
    add w0, w9, #1
    mov w1, w10
    mov w2, w11
    bl get_index
    ldrb w3, [x23, x0]
    cbz w3, part2_c2
    add x24, x24, #1

part2_c2:
    sub w0, w9, #1
    mov w1, w10
    mov w2, w11
    bl get_index
    ldrb w3, [x23, x0]
    cbz w3, part2_c3
    add x24, x24, #1

part2_c3:
    mov w0, w9
    add w1, w10, #1
    mov w2, w11
    bl get_index
    ldrb w3, [x23, x0]
    cbz w3, part2_c4
    add x24, x24, #1

part2_c4:
    mov w0, w9
    sub w1, w10, #1
    mov w2, w11
    bl get_index
    ldrb w3, [x23, x0]
    cbz w3, part2_c5
    add x24, x24, #1

part2_c5:
    mov w0, w9
    mov w1, w10
    add w2, w11, #1
    bl get_index
    ldrb w3, [x23, x0]
    cbz w3, part2_c6
    add x24, x24, #1

part2_c6:
    mov w0, w9
    mov w1, w10
    sub w2, w11, #1
    bl get_index
    ldrb w3, [x23, x0]
    cbz w3, part2_next
    add x24, x24, #1

part2_next:
    add x25, x25, #1
    b part2_count_loop

part2_done:
    mov x0, x24

    ldp x27, x28, [sp, #80]
    ldp x25, x26, [sp, #64]
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #112
    ret

// Try to enqueue a position for BFS
// x0 = x, x1 = y, x2 = z
// Uses: x11 = grid, x12 = exterior, x25-x27 = queues, x10 = queue_end
try_enqueue:
    stp x29, x30, [sp, #-48]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]

    mov w19, w0                 // x
    mov w20, w1                 // y
    mov w21, w2                 // z

    // Get index
    bl get_index
    mov x22, x0                 // save index

    // Check if it's a cube (skip if so)
    load_addr x1, grid
    ldrb w2, [x1, x0]
    cbnz w2, try_enqueue_done

    // Check if already visited (skip if so)
    load_addr x1, exterior
    ldrb w2, [x1, x0]
    cbnz w2, try_enqueue_done

    // Mark as exterior
    mov w2, #1
    strb w2, [x1, x22]

    // Enqueue
    // Load queue_end from parent's saved location
    ldr x10, [x29, #48+104]     // Access parent's stack frame

    load_addr x1, queue_x
    str w19, [x1, x10, lsl #2]
    load_addr x1, queue_y
    str w20, [x1, x10, lsl #2]
    load_addr x1, queue_z
    str w21, [x1, x10, lsl #2]

    add x10, x10, #1

    // Save back queue_end
    str x10, [x29, #48+104]

try_enqueue_done:
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #48
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

    // Handle zero
    cbnz x19, convert_loop
    mov w3, #'0'
    sub x20, x20, #1
    strb w3, [x20]
    mov x21, #1
    b print_digits

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
