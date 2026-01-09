// Day 14: Regolith Reservoir - ARM64 Assembly (macOS)
//
// Falling sand simulation:
// - Parse rock paths from input
// - Part 1: Count sand units that come to rest before sand falls into the abyss
// - Part 2: Count sand units until source (500,0) is blocked (with floor at max_y + 2)
//
// Grid representation:
// - Use a 2D byte array for the cave grid
// - Grid width: 700 (to handle potential spread), origin at x=0
// - Grid height: 200 (enough for max_y + floor)
// - 0 = air, 1 = rock, 2 = sand

.global _main
.align 4

// Constants
.equ BUFFER_SIZE, 32768         // Input file buffer
.equ GRID_WIDTH, 700            // Cave grid width
.equ GRID_HEIGHT, 200           // Cave grid height
.equ GRID_SIZE, 140000          // GRID_WIDTH * GRID_HEIGHT
.equ SAND_SOURCE_X, 500         // Sand source x coordinate
.equ SAND_SOURCE_Y, 0           // Sand source y coordinate

// Grid cell values
.equ CELL_AIR, 0
.equ CELL_ROCK, 1
.equ CELL_SAND, 2

// Macro for loading addresses
.macro LOAD_ADDR reg, label
    adrp    \reg, \label@PAGE
    add     \reg, \reg, \label@PAGEOFF
.endm

// ============================================================================
// Data Section
// ============================================================================
.data

input_path:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"
error_msg:      .asciz "Error reading file\n"

.align 3
file_buffer:    .space BUFFER_SIZE
buffer_len:     .quad 0
buffer_pos:     .quad 0         // Current parse position

// Cave grid
.align 4
cave_grid:      .space GRID_SIZE
cave_grid_bak:  .space GRID_SIZE  // Backup for part 2

// Max Y coordinate found in rocks
max_y:          .quad 0

// ============================================================================
// Code Section
// ============================================================================
.text

// ============================================================================
// Main entry point
// ============================================================================
_main:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

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
    mov     x2, #BUFFER_SIZE
    mov     x16, #3                         // read() syscall
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit

    LOAD_ADDR x1, buffer_len
    str     x0, [x1]

    // Close file
    mov     x0, x19
    mov     x16, #6                         // close() syscall
    svc     #0x80

    // Clear the grid
    bl      clear_grid

    // Parse rock paths and draw them on the grid
    bl      parse_and_draw_rocks

    // Make a backup of the grid for part 2
    bl      backup_grid

    // Part 1: Simulate sand falling
    bl      part1
    mov     x19, x0                         // Save part1 result

    // Print Part 1
    LOAD_ADDR x0, part1_msg
    bl      print_str
    mov     x0, x19
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Restore grid from backup
    bl      restore_grid

    // Part 2: Simulate with floor
    bl      part2
    mov     x20, x0                         // Save part2 result

    // Print Part 2
    LOAD_ADDR x0, part2_msg
    bl      print_str
    mov     x0, x20
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Exit
    mov     x0, #0
    ldp     x29, x30, [sp], #16
    ret

error_exit:
    LOAD_ADDR x0, error_msg
    bl      print_str
    mov     x0, #1
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// clear_grid: Initialize grid to all air
// ============================================================================
clear_grid:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    LOAD_ADDR x0, cave_grid
    mov     x1, #(GRID_SIZE & 0xFFFF)
    movk    x1, #(GRID_SIZE >> 16), lsl #16
    mov     w2, #CELL_AIR

clear_loop:
    cbz     x1, clear_done
    strb    w2, [x0], #1
    subs    x1, x1, #1
    b.ne    clear_loop

clear_done:
    // Also reset max_y
    LOAD_ADDR x0, max_y
    str     xzr, [x0]

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// backup_grid: Copy grid to backup
// ============================================================================
backup_grid:
    stp     x29, x30, [sp, #-16]!

    LOAD_ADDR x0, cave_grid
    LOAD_ADDR x1, cave_grid_bak
    mov     x2, #(GRID_SIZE & 0xFFFF)
    movk    x2, #(GRID_SIZE >> 16), lsl #16

backup_loop:
    cbz     x2, backup_done
    ldrb    w3, [x0], #1
    strb    w3, [x1], #1
    sub     x2, x2, #1
    b       backup_loop

backup_done:
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// restore_grid: Restore grid from backup
// ============================================================================
restore_grid:
    stp     x29, x30, [sp, #-16]!

    LOAD_ADDR x0, cave_grid_bak
    LOAD_ADDR x1, cave_grid
    mov     x2, #(GRID_SIZE & 0xFFFF)
    movk    x2, #(GRID_SIZE >> 16), lsl #16

restore_loop:
    cbz     x2, restore_done
    ldrb    w3, [x0], #1
    strb    w3, [x1], #1
    sub     x2, x2, #1
    b       restore_loop

restore_done:
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// get_cell: Get cell value at (x, y)
// Input: x0 = x, x1 = y
// Output: x0 = cell value
// ============================================================================
get_cell:
    LOAD_ADDR x2, cave_grid
    mov     x3, #GRID_WIDTH
    mul     x3, x1, x3
    add     x3, x3, x0
    ldrb    w0, [x2, x3]
    ret

// ============================================================================
// set_cell: Set cell value at (x, y)
// Input: x0 = x, x1 = y, x2 = value
// ============================================================================
set_cell:
    LOAD_ADDR x3, cave_grid
    mov     x4, #GRID_WIDTH
    mul     x4, x1, x4
    add     x4, x4, x0
    strb    w2, [x3, x4]
    ret

// ============================================================================
// parse_and_draw_rocks: Parse input and draw rock lines
// ============================================================================
parse_and_draw_rocks:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    LOAD_ADDR x19, buffer_pos
    str     xzr, [x19]                      // Reset position

    LOAD_ADDR x20, file_buffer
    LOAD_ADDR x21, buffer_len
    ldr     x21, [x21]

parse_line_loop:
    // Check if at end
    ldr     x0, [x19]
    cmp     x0, x21
    b.ge    parse_rocks_done

    // Skip whitespace at start of line
    bl      skip_whitespace
    ldr     x0, [x19]
    cmp     x0, x21
    b.ge    parse_rocks_done

    // Check if line is empty (newline at start)
    ldrb    w1, [x20, x0]
    cmp     w1, #'\n'
    b.eq    skip_empty_line
    cmp     w1, #'\r'
    b.eq    skip_empty_line

    // Parse first coordinate pair
    bl      parse_number
    mov     x22, x0                         // prev_x

    // Skip comma
    ldr     x0, [x19]
    add     x0, x0, #1
    str     x0, [x19]

    bl      parse_number
    mov     x23, x0                         // prev_y

    // Update max_y
    LOAD_ADDR x0, max_y
    ldr     x1, [x0]
    cmp     x23, x1
    csel    x1, x23, x1, gt
    str     x1, [x0]

parse_path_loop:
    // Check for ' -> '
    ldr     x0, [x19]
    cmp     x0, x21
    b.ge    next_line

    ldrb    w1, [x20, x0]
    cmp     w1, #' '
    b.ne    next_line

    add     x0, x0, #1
    ldrb    w1, [x20, x0]
    cmp     w1, #'-'
    b.ne    next_line

    add     x0, x0, #1
    ldrb    w1, [x20, x0]
    cmp     w1, #'>'
    b.ne    next_line

    add     x0, x0, #1
    ldrb    w1, [x20, x0]
    cmp     w1, #' '
    b.ne    next_line

    // Skip ' -> '
    add     x0, x0, #1
    str     x0, [x19]

    // Parse next coordinate
    bl      parse_number
    mov     x24, x0                         // cur_x

    // Skip comma
    ldr     x0, [x19]
    add     x0, x0, #1
    str     x0, [x19]

    bl      parse_number
    mov     x25, x0                         // cur_y

    // Update max_y
    LOAD_ADDR x0, max_y
    ldr     x1, [x0]
    cmp     x25, x1
    csel    x1, x25, x1, gt
    str     x1, [x0]

    // Draw line from (prev_x, prev_y) to (cur_x, cur_y)
    mov     x0, x22
    mov     x1, x23
    mov     x2, x24
    mov     x3, x25
    bl      draw_rock_line

    // Update prev to cur
    mov     x22, x24
    mov     x23, x25

    b       parse_path_loop

skip_empty_line:
    ldr     x0, [x19]
    add     x0, x0, #1
    str     x0, [x19]
    b       parse_line_loop

next_line:
    // Skip to end of line
    ldr     x0, [x19]
skip_to_eol:
    cmp     x0, x21
    b.ge    parse_rocks_done
    ldrb    w1, [x20, x0]
    cmp     w1, #'\n'
    b.eq    found_eol
    add     x0, x0, #1
    b       skip_to_eol

found_eol:
    add     x0, x0, #1
    str     x0, [x19]
    b       parse_line_loop

parse_rocks_done:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// skip_whitespace: Skip spaces and tabs (not newlines)
// ============================================================================
skip_whitespace:
    stp     x29, x30, [sp, #-16]!

    LOAD_ADDR x0, buffer_pos
    ldr     x1, [x0]
    LOAD_ADDR x2, buffer_len
    ldr     x3, [x2]
    LOAD_ADDR x4, file_buffer

skip_ws_loop2:
    cmp     x1, x3
    b.ge    skip_ws_done2
    ldrb    w5, [x4, x1]
    cmp     w5, #' '
    b.eq    skip_ws_next2
    cmp     w5, #'\t'
    b.eq    skip_ws_next2
    b       skip_ws_done2

skip_ws_next2:
    add     x1, x1, #1
    b       skip_ws_loop2

skip_ws_done2:
    LOAD_ADDR x0, buffer_pos
    str     x1, [x0]

    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// parse_number: Parse a decimal number
// Output: x0 = parsed number
// ============================================================================
parse_number:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    LOAD_ADDR x19, buffer_pos
    ldr     x0, [x19]
    LOAD_ADDR x1, file_buffer

    mov     x2, #0                          // Result

parse_num_loop:
    ldrb    w3, [x1, x0]
    cmp     w3, #'0'
    b.lt    parse_num_done
    cmp     w3, #'9'
    b.gt    parse_num_done

    sub     w3, w3, #'0'
    and     x3, x3, #0xFF
    mov     x4, #10
    mul     x2, x2, x4
    add     x2, x2, x3
    add     x0, x0, #1
    b       parse_num_loop

parse_num_done:
    str     x0, [x19]
    mov     x0, x2

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// draw_rock_line: Draw a rock line from (x1,y1) to (x2,y2)
// Input: x0 = x1, x1 = y1, x2 = x2, x3 = y2
// ============================================================================
draw_rock_line:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    mov     x19, x0                         // x1
    mov     x20, x1                         // y1
    mov     x21, x2                         // x2
    mov     x22, x3                         // y2

    // Determine direction
    cmp     x19, x21
    b.eq    draw_vertical

    // Horizontal line (y is constant)
    // Ensure x19 <= x21
    cmp     x19, x21
    b.le    horiz_ordered
    mov     x23, x19
    mov     x19, x21
    mov     x21, x23

horiz_ordered:
    mov     x23, x19
draw_horiz_loop:
    cmp     x23, x21
    b.gt    draw_line_done

    mov     x0, x23
    mov     x1, x20
    mov     x2, #CELL_ROCK
    bl      set_cell

    add     x23, x23, #1
    b       draw_horiz_loop

draw_vertical:
    // Vertical line (x is constant)
    // Ensure y20 <= y22
    cmp     x20, x22
    b.le    vert_ordered
    mov     x23, x20
    mov     x20, x22
    mov     x22, x23

vert_ordered:
    mov     x23, x20
draw_vert_loop:
    cmp     x23, x22
    b.gt    draw_line_done

    mov     x0, x19
    mov     x1, x23
    mov     x2, #CELL_ROCK
    bl      set_cell

    add     x23, x23, #1
    b       draw_vert_loop

draw_line_done:
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// simulate_sand_part1: Simulate one unit of sand for part 1
// Output: x0 = 1 if sand came to rest, 0 if fell into abyss
// ============================================================================
simulate_sand_part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    // Start at sand source
    mov     x19, #SAND_SOURCE_X             // x
    mov     x20, #SAND_SOURCE_Y             // y

    // Get max_y
    LOAD_ADDR x0, max_y
    ldr     x21, [x0]

sand_fall_loop1:
    // Check if sand has fallen below all rocks (into abyss)
    cmp     x20, x21
    b.gt    sand_abyss

    // Try to move down
    add     x1, x20, #1
    mov     x0, x19
    bl      get_cell
    cbz     x0, move_down1

    // Try to move down-left
    sub     x0, x19, #1
    add     x1, x20, #1
    bl      get_cell
    cbz     x0, move_down_left1

    // Try to move down-right
    add     x0, x19, #1
    add     x1, x20, #1
    bl      get_cell
    cbz     x0, move_down_right1

    // Sand comes to rest
    mov     x0, x19
    mov     x1, x20
    mov     x2, #CELL_SAND
    bl      set_cell
    mov     x0, #1
    b       sand_done1

move_down1:
    add     x20, x20, #1
    b       sand_fall_loop1

move_down_left1:
    sub     x19, x19, #1
    add     x20, x20, #1
    b       sand_fall_loop1

move_down_right1:
    add     x19, x19, #1
    add     x20, x20, #1
    b       sand_fall_loop1

sand_abyss:
    mov     x0, #0

sand_done1:
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// part1: Count sand units that come to rest before sand falls into abyss
// Output: x0 = count
// ============================================================================
part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, #0                         // Count

part1_loop:
    bl      simulate_sand_part1
    cbz     x0, part1_done

    add     x19, x19, #1
    b       part1_loop

part1_done:
    mov     x0, x19

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// simulate_sand_part2: Simulate one unit of sand for part 2 (with floor)
// Output: x0 = 1 if sand came to rest, 0 if source is blocked
//         x1 = x position where sand came to rest
//         x2 = y position where sand came to rest
// ============================================================================
simulate_sand_part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    // Start at sand source
    mov     x19, #SAND_SOURCE_X             // x
    mov     x20, #SAND_SOURCE_Y             // y

    // Get floor_y = max_y + 2
    LOAD_ADDR x0, max_y
    ldr     x21, [x0]
    add     x21, x21, #2

sand_fall_loop2:
    // Check if we hit the floor
    add     x22, x20, #1
    cmp     x22, x21
    b.ge    sand_rest2

    // Try to move down
    mov     x0, x19
    mov     x1, x22
    bl      get_cell
    cbz     x0, move_down2

    // Try to move down-left
    sub     x0, x19, #1
    mov     x1, x22
    bl      get_cell
    cbz     x0, move_down_left2

    // Try to move down-right
    add     x0, x19, #1
    mov     x1, x22
    bl      get_cell
    cbz     x0, move_down_right2

    // Sand comes to rest at current position
sand_rest2:
    mov     x0, x19
    mov     x1, x20
    mov     x2, #CELL_SAND
    bl      set_cell

    mov     x0, #1
    mov     x1, x19
    mov     x2, x20
    b       sand_done2

move_down2:
    add     x20, x20, #1
    b       sand_fall_loop2

move_down_left2:
    sub     x19, x19, #1
    add     x20, x20, #1
    b       sand_fall_loop2

move_down_right2:
    add     x19, x19, #1
    add     x20, x20, #1
    b       sand_fall_loop2

sand_done2:
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// part2: Count sand units until source is blocked (with floor)
// Output: x0 = count
// ============================================================================
part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, #0                         // Count

part2_loop:
    bl      simulate_sand_part2

    add     x19, x19, #1

    // Check if sand came to rest at source (500, 0)
    cmp     x1, #SAND_SOURCE_X
    b.ne    part2_loop
    cmp     x2, #SAND_SOURCE_Y
    b.ne    part2_loop

    // Source is blocked
    mov     x0, x19

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print null-terminated string
// Input: x0 = string address
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

2:  // Write
    mov     x0, #1
    mov     x1, x19
    mov     x2, x20
    mov     x16, #4
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_num: Print 64-bit unsigned number
// Input: x0 = number
// ============================================================================
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    sub     sp, sp, #32

    mov     x19, x0
    add     x20, sp, #31
    strb    wzr, [x20]

    // Handle zero
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
