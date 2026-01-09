// ARM64 Assembly solution for Advent of Code 2022 Day 24: Blizzard Basin
// BFS through a grid with moving blizzards

.global _start
.align 4

// System call numbers
.equ SYS_exit, 1
.equ SYS_read, 3
.equ SYS_write, 4
.equ SYS_open, 5
.equ SYS_close, 6

// File flags
.equ O_RDONLY, 0

// Constants
.equ MAX_BLIZZARDS, 20000
.equ MAX_WIDTH, 130
.equ MAX_HEIGHT, 30
.equ MAX_PERIOD, 700         // LCM can be up to ~600
.equ QUEUE_SIZE, 2000000
.equ VISITED_SIZE, 40000000  // MAX_PERIOD * MAX_HEIGHT * MAX_WIDTH

.data
.align 4
filename:       .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"

// Global state variables
g_width:        .quad 0
g_height:       .quad 0
g_inner_h:      .quad 0
g_inner_w:      .quad 0
g_start_col:    .quad 0
g_end_col:      .quad 0
g_num_bliz:     .quad 0
g_period:       .quad 0

.bss
.align 4
input_buf:      .skip 50000
bliz_row:       .skip MAX_BLIZZARDS * 4     // Original row (int)
bliz_col:       .skip MAX_BLIZZARDS * 4     // Original col (int)
bliz_dir:       .skip MAX_BLIZZARDS         // Direction (char: 0=^, 1=v, 2=<, 3=>)
bliz_cache:     .skip MAX_PERIOD * MAX_HEIGHT * MAX_WIDTH  // Precomputed blizzard positions
visited:        .skip VISITED_SIZE          // visited[t % period][row][col]
queue:          .skip QUEUE_SIZE * 12       // Each entry: time(4), row(4), col(4)
num_buf:        .skip 32

.text

// Print null-terminated string at x0
print_cstr:
    stp x29, x30, [sp, #-16]!
    mov x1, x0
    mov x2, #0
1:  ldrb w3, [x1, x2]
    cbz w3, 2f
    add x2, x2, #1
    b 1b
2:  mov x1, x0
    mov x0, #1
    mov x16, #SYS_write
    svc #0x80
    ldp x29, x30, [sp], #16
    ret

// Print integer in x0
print_int:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    mov x19, x0             // save number
    adrp x20, num_buf@PAGE
    add x20, x20, num_buf@PAGEOFF
    add x20, x20, #30       // end of buffer
    strb wzr, [x20]         // null terminator

    // Handle 0
    cbnz x19, 1f
    sub x20, x20, #1
    mov w1, #'0'
    strb w1, [x20]
    b 3f

1:  // Convert digits
    mov x1, #10
2:  cbz x19, 3f
    udiv x2, x19, x1
    msub x3, x2, x1, x19    // remainder
    add w3, w3, #'0'
    sub x20, x20, #1
    strb w3, [x20]
    mov x19, x2
    b 2b

3:  mov x0, x20
    bl print_cstr

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// GCD: x0 = gcd(x0, x1)
gcd:
    cbz x1, 1f
2:  udiv x2, x0, x1
    msub x0, x2, x1, x0     // x0 = x0 % x1
    cbz x0, 3f
    // swap
    mov x2, x0
    mov x0, x1
    mov x1, x2
    b 2b
3:  mov x0, x1
1:  ret

// LCM: x0 = lcm(x0, x1)
lcm_func:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    mov x19, x0
    mov x20, x1
    bl gcd
    // lcm = (a * b) / gcd
    mul x1, x19, x20
    udiv x0, x1, x0
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Modulo that handles negative numbers: x0 = mod(x0, x1) where result is always non-negative
mod_pos:
    sdiv x2, x0, x1
    msub x0, x2, x1, x0     // x0 = x0 - (x0/x1)*x1
    // If negative, add x1
    tbnz x0, #63, 1f
    ret
1:  add x0, x0, x1
    ret

_start:
    // Open file
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #O_RDONLY
    mov x16, #SYS_open
    svc #0x80
    mov x19, x0             // fd

    // Read file
    adrp x1, input_buf@PAGE
    add x1, x1, input_buf@PAGEOFF
    mov x2, #50000
    mov x16, #SYS_read
    svc #0x80
    mov x20, x0             // bytes read

    // Close file
    mov x0, x19
    mov x16, #SYS_close
    svc #0x80

    // Parse input
    // Find width (length of first line)
    adrp x0, input_buf@PAGE
    add x0, x0, input_buf@PAGEOFF
    mov x1, #0              // width counter
1:  ldrb w2, [x0, x1]
    cmp w2, #'\n'
    b.eq 2f
    add x1, x1, #1
    b 1b
2:  mov x21, x1             // width

    // Find height
    mov x1, #0              // position
    mov x2, #0              // height counter
3:  cmp x1, x20
    b.ge 4f
    ldrb w3, [x0, x1]
    cmp w3, #'\n'
    b.ne 5f
    add x2, x2, #1
5:  add x1, x1, #1
    b 3b
4:  mov x22, x2             // height

    // Inner dimensions
    sub x23, x22, #2        // inner_h
    sub x24, x21, #2        // inner_w

    // Find start column (position of '.' in first row)
    mov x1, #0
6:  ldrb w2, [x0, x1]
    cmp w2, #'.'
    b.eq 7f
    add x1, x1, #1
    b 6b
7:  mov x25, x1             // start_col

    // Find end column (position of '.' in last row)
    sub x1, x22, #1         // last row index
    add x2, x21, #1         // stride (width + newline)
    mul x1, x1, x2          // offset to last row
8:  ldrb w2, [x0, x1]
    cmp w2, #'.'
    b.eq 9f
    add x1, x1, #1
    b 8b
9:  // Calculate column from position
    sub x2, x22, #1
    add x3, x21, #1
    mul x2, x2, x3
    sub x26, x1, x2         // end_col

    // Store globals
    adrp x0, g_width@PAGE
    add x0, x0, g_width@PAGEOFF
    str x21, [x0]
    adrp x0, g_height@PAGE
    add x0, x0, g_height@PAGEOFF
    str x22, [x0]
    adrp x0, g_inner_h@PAGE
    add x0, x0, g_inner_h@PAGEOFF
    str x23, [x0]
    adrp x0, g_inner_w@PAGE
    add x0, x0, g_inner_w@PAGEOFF
    str x24, [x0]
    adrp x0, g_start_col@PAGE
    add x0, x0, g_start_col@PAGEOFF
    str x25, [x0]
    adrp x0, g_end_col@PAGE
    add x0, x0, g_end_col@PAGEOFF
    str x26, [x0]

    // Parse blizzards
    adrp x10, bliz_row@PAGE
    add x10, x10, bliz_row@PAGEOFF
    adrp x11, bliz_col@PAGE
    add x11, x11, bliz_col@PAGEOFF
    adrp x12, bliz_dir@PAGE
    add x12, x12, bliz_dir@PAGEOFF

    adrp x0, input_buf@PAGE
    add x0, x0, input_buf@PAGEOFF

    mov x13, #0             // blizzard count
    mov x14, #0             // current row
    mov x15, #0             // current col
    mov x1, #0              // position

parse_loop:
    cmp x1, x20
    b.ge parse_done
    ldrb w2, [x0, x1]

    cmp w2, #'\n'
    b.ne 10f
    add x14, x14, #1        // next row
    mov x15, #0             // reset col
    add x1, x1, #1
    b parse_loop

10: // Check for blizzard characters
    mov w3, #0              // direction
    cmp w2, #'^'
    b.eq found_bliz
    mov w3, #1
    cmp w2, #'v'
    b.eq found_bliz
    mov w3, #2
    cmp w2, #'<'
    b.eq found_bliz
    mov w3, #3
    cmp w2, #'>'
    b.eq found_bliz
    // Not a blizzard
    add x15, x15, #1
    add x1, x1, #1
    b parse_loop

found_bliz:
    // Store blizzard
    str w14, [x10, x13, lsl #2]     // row
    str w15, [x11, x13, lsl #2]     // col
    strb w3, [x12, x13]             // direction
    add x13, x13, #1
    add x15, x15, #1
    add x1, x1, #1
    b parse_loop

parse_done:
    // Store num_blizzards
    adrp x0, g_num_bliz@PAGE
    add x0, x0, g_num_bliz@PAGEOFF
    str x13, [x0]

    // Calculate period = lcm(inner_h, inner_w)
    mov x0, x23
    mov x1, x24
    bl lcm_func
    mov x27, x0             // period
    adrp x1, g_period@PAGE
    add x1, x1, g_period@PAGEOFF
    str x27, [x1]

    // Precompute blizzard positions
    bl precompute_blizzards

    // BFS Part 1: start (0, start_col) to end (height-1, end_col)
    adrp x0, g_start_col@PAGE
    add x0, x0, g_start_col@PAGEOFF
    ldr x1, [x0]            // start_col
    adrp x0, g_height@PAGE
    add x0, x0, g_height@PAGEOFF
    ldr x2, [x0]            // height
    sub x2, x2, #1          // end_row
    adrp x0, g_end_col@PAGE
    add x0, x0, g_end_col@PAGEOFF
    ldr x3, [x0]            // end_col

    mov x0, #0              // start_row
    mov x4, #0              // start_time
    bl bfs
    mov x28, x0             // save part1 result

    // Print Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_cstr
    mov x0, x28
    bl print_int
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_cstr

    // Part 2: trip back to start
    // Load globals again
    adrp x0, g_height@PAGE
    add x0, x0, g_height@PAGEOFF
    ldr x22, [x0]
    adrp x0, g_start_col@PAGE
    add x0, x0, g_start_col@PAGEOFF
    ldr x25, [x0]
    adrp x0, g_end_col@PAGE
    add x0, x0, g_end_col@PAGEOFF
    ldr x26, [x0]

    // Trip 2: end to start
    sub x0, x22, #1         // start from end_row
    mov x1, x26             // end_col
    mov x2, #0              // to start_row
    mov x3, x25             // start_col
    mov x4, x28             // start at time t1
    bl bfs
    mov x28, x0             // t2

    // Trip 3: start to end again
    mov x0, #0              // start_row
    mov x1, x25             // start_col
    sub x2, x22, #1         // end_row
    mov x3, x26             // end_col
    mov x4, x28             // start at time t2
    bl bfs

    // Print Part 2
    mov x28, x0
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_cstr
    mov x0, x28
    bl print_int
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_cstr

    // Exit
    mov x0, #0
    mov x16, #SYS_exit
    svc #0x80

// Precompute blizzard positions for all times
precompute_blizzards:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    // Load globals
    adrp x0, g_period@PAGE
    add x0, x0, g_period@PAGEOFF
    ldr x19, [x0]           // period
    adrp x0, g_height@PAGE
    add x0, x0, g_height@PAGEOFF
    ldr x20, [x0]           // height
    adrp x0, g_width@PAGE
    add x0, x0, g_width@PAGEOFF
    ldr x21, [x0]           // width
    adrp x0, g_inner_h@PAGE
    add x0, x0, g_inner_h@PAGEOFF
    ldr x22, [x0]           // inner_h
    adrp x0, g_inner_w@PAGE
    add x0, x0, g_inner_w@PAGEOFF
    ldr x23, [x0]           // inner_w
    adrp x0, g_num_bliz@PAGE
    add x0, x0, g_num_bliz@PAGEOFF
    ldr x24, [x0]           // num_blizzards

    // Get cache pointer
    adrp x9, bliz_cache@PAGE
    add x9, x9, bliz_cache@PAGEOFF

    // Clear cache
    mov x0, x19             // period
    mul x0, x0, x20         // * height
    mul x0, x0, x21         // * width
    mov x1, #0
clear_loop:
    cmp x1, x0
    b.ge clear_done
    strb wzr, [x9, x1]
    add x1, x1, #1
    b clear_loop
clear_done:

    // For each time t from 0 to period-1
    mov x25, #0             // t
time_loop:
    cmp x25, x19
    b.ge time_done

    // For each blizzard
    mov x26, #0             // blizzard index
bliz_loop:
    cmp x26, x24
    b.ge bliz_done

    // Load blizzard info
    adrp x10, bliz_row@PAGE
    add x10, x10, bliz_row@PAGEOFF
    adrp x11, bliz_col@PAGE
    add x11, x11, bliz_col@PAGEOFF
    adrp x12, bliz_dir@PAGE
    add x12, x12, bliz_dir@PAGEOFF

    ldr w14, [x10, x26, lsl #2]  // row
    ldr w15, [x11, x26, lsl #2]  // col
    ldrb w16, [x12, x26]         // direction

    // Convert to inner coordinates
    sub x14, x14, #1        // ir = row - 1
    sub x15, x15, #1        // ic = col - 1

    // Calculate new position based on direction
    // 0=^, 1=v, 2=<, 3=>
    mov x17, x14            // nr = ir
    mov x18, x15            // nc = ic

    cmp w16, #0             // ^
    b.ne check_down_pre
    // nr = (ir - t) % inner_h
    sub x0, x14, x25
    mov x1, x22
    bl mod_pos
    mov x17, x0
    b calc_done_pre

check_down_pre:
    cmp w16, #1             // v
    b.ne check_left_pre
    // nr = (ir + t) % inner_h
    add x0, x14, x25
    mov x1, x22
    bl mod_pos
    mov x17, x0
    b calc_done_pre

check_left_pre:
    cmp w16, #2             // <
    b.ne check_right_pre
    // nc = (ic - t) % inner_w
    sub x0, x15, x25
    mov x1, x23
    bl mod_pos
    mov x18, x0
    b calc_done_pre

check_right_pre:
    // > : nc = (ic + t) % inner_w
    add x0, x15, x25
    mov x1, x23
    bl mod_pos
    mov x18, x0

calc_done_pre:
    // Convert back to full coordinates
    add x17, x17, #1        // nr + 1
    add x18, x18, #1        // nc + 1

    // Mark in cache: cache[t * height * width + nr * width + nc] = 1
    mov x0, x25             // t
    mul x0, x0, x20         // t * height
    add x0, x0, x17         // + nr
    mul x0, x0, x21         // * width
    add x0, x0, x18         // + nc
    mov w1, #1
    strb w1, [x9, x0]

    add x26, x26, #1
    b bliz_loop

bliz_done:
    add x25, x25, #1
    b time_loop

time_done:
    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// BFS function
// x0 = start_row, x1 = start_col, x2 = end_row, x3 = end_col, x4 = start_time
// Returns time to reach end in x0
bfs:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    mov x19, x0             // start_row
    mov x20, x1             // start_col
    mov x21, x2             // end_row
    mov x22, x3             // end_col
    mov x23, x4             // start_time

    // Load globals
    adrp x0, g_period@PAGE
    add x0, x0, g_period@PAGEOFF
    ldr x24, [x0]           // period
    adrp x0, g_width@PAGE
    add x0, x0, g_width@PAGEOFF
    ldr x25, [x0]           // width
    adrp x0, g_height@PAGE
    add x0, x0, g_height@PAGEOFF
    ldr x26, [x0]           // height

    // Clear visited array
    adrp x0, visited@PAGE
    add x0, x0, visited@PAGEOFF
    mov x1, x24             // period
    mul x1, x1, x26         // * height
    mul x1, x1, x25         // * width
    mov x2, #0
clear_visited:
    cmp x2, x1
    b.ge visited_cleared
    strb wzr, [x0, x2]
    add x2, x2, #1
    b clear_visited
visited_cleared:

    // Initialize queue
    adrp x27, queue@PAGE
    add x27, x27, queue@PAGEOFF
    mov x28, #0             // queue head
    mov x9, #0              // queue tail (next write position)

    // Enqueue start state
    str w23, [x27, x9]      // time
    add x9, x9, #4
    str w19, [x27, x9]      // row
    add x9, x9, #4
    str w20, [x27, x9]      // col
    add x9, x9, #4

    // Mark start as visited
    // visited[(time % period) * height * width + row * width + col] = 1
    udiv x0, x23, x24
    msub x0, x0, x24, x23   // time % period
    mul x0, x0, x26         // * height
    add x0, x0, x19         // + row
    mul x0, x0, x25         // * width
    add x0, x0, x20         // + col
    adrp x1, visited@PAGE
    add x1, x1, visited@PAGEOFF
    mov w2, #1
    strb w2, [x1, x0]

bfs_loop:
    cmp x28, x9             // head >= tail means empty
    b.ge bfs_not_found

    // Dequeue
    ldr w10, [x27, x28]     // time
    add x28, x28, #4
    ldr w11, [x27, x28]     // row
    add x28, x28, #4
    ldr w12, [x27, x28]     // col
    add x28, x28, #4

    // Check if reached end
    cmp w11, w21
    b.ne not_end
    cmp w12, w22
    b.ne not_end
    // Found!
    mov x0, x10
    b bfs_return

not_end:
    // next_time = time + 1
    add x13, x10, #1

    // Get blizzard positions for next_time
    udiv x0, x13, x24
    msub x14, x0, x24, x13  // next_time % period

    // Try all 5 directions: (0,0), (-1,0), (1,0), (0,-1), (0,1)
    // Direction 0: wait (0, 0)
    mov x16, x11            // nr = row
    mov x17, x12            // nc = col
    bl try_move

    // Direction 1: up (-1, 0)
    sub x16, x11, #1        // nr = row - 1
    mov x17, x12            // nc = col
    bl try_move

    // Direction 2: down (1, 0)
    add x16, x11, #1        // nr = row + 1
    mov x17, x12            // nc = col
    bl try_move

    // Direction 3: left (0, -1)
    mov x16, x11            // nr = row
    sub x17, x12, #1        // nc = col - 1
    bl try_move

    // Direction 4: right (0, 1)
    mov x16, x11            // nr = row
    add x17, x12, #1        // nc = col + 1
    bl try_move

    b bfs_loop

// try_move: Check if position (x16, x17) at time x13 is valid and enqueue if so
// Uses: x14 (next_time % period), x19-x26 (globals), x9 (queue tail), x27 (queue base)
try_move:
    stp x29, x30, [sp, #-16]!

    // Check if this is start or end position
    cmp x16, x19
    b.ne check_end_pos_tm
    cmp x17, x20
    b.eq bounds_ok_tm

check_end_pos_tm:
    cmp x16, x21
    b.ne check_bounds_tm
    cmp x17, x22
    b.eq bounds_ok_tm

check_bounds_tm:
    // Check if in inner area: 0 < nr < height-1 and 0 < nc < width-1
    cmp x16, #0
    b.le try_move_done
    sub x0, x26, #1          // height - 1
    cmp x16, x0
    b.ge try_move_done
    cmp x17, #0
    b.le try_move_done
    sub x0, x25, #1          // width - 1
    cmp x17, x0
    b.ge try_move_done

bounds_ok_tm:
    // Check blizzards at (nr, nc) at next_time
    adrp x0, bliz_cache@PAGE
    add x0, x0, bliz_cache@PAGEOFF
    mov x1, x14             // next_time % period
    mul x1, x1, x26         // * height
    add x1, x1, x16         // + nr
    mul x1, x1, x25         // * width
    add x1, x1, x17         // + nc
    ldrb w2, [x0, x1]
    cbnz w2, try_move_done  // blizzard present

    // Check visited
    adrp x0, visited@PAGE
    add x0, x0, visited@PAGEOFF
    mov x1, x14             // next_time % period
    mul x1, x1, x26         // * height
    add x1, x1, x16         // + nr
    mul x1, x1, x25         // * width
    add x1, x1, x17         // + nc
    ldrb w2, [x0, x1]
    cbnz w2, try_move_done  // already visited

    // Mark as visited
    mov w2, #1
    strb w2, [x0, x1]

    // Enqueue
    str w13, [x27, x9]      // next_time
    add x9, x9, #4
    str w16, [x27, x9]      // nr
    add x9, x9, #4
    str w17, [x27, x9]      // nc
    add x9, x9, #4

try_move_done:
    ldp x29, x30, [sp], #16
    ret

bfs_not_found:
    mov x0, #-1

bfs_return:
    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
