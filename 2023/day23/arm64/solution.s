// Day 23: A Long Walk - ARM64 Assembly for macOS
// Find longest path through hiking trails using graph compression and DFS

.global _start
.align 4

// Constants
.equ MAX_GRID_SIZE, 150        // Max rows/cols
.equ MAX_JUNCTIONS, 64         // Max junction points
.equ MAX_EDGES, 16             // Max edges per junction
.equ BUFFER_SIZE, 32768        // Input buffer size
.equ DFS_STACK_SIZE, 8192      // DFS stack entries

// macOS AArch64 syscall convention: syscall number in x16
// Arguments in x0-x5, return value in x0
// Add 0x2000000 to standard Unix syscall numbers

.section __DATA,__data
.align 4
filename:   .asciz "../input.txt"
part1_msg:  .asciz "Part 1: "
part2_msg:  .asciz "Part 2: "
newline:    .asciz "\n"

.section __DATA,__bss
.align 4
input_buf:      .skip BUFFER_SIZE           // Raw input
grid:           .skip MAX_GRID_SIZE * MAX_GRID_SIZE  // Grid data
grid_rows:      .skip 8                     // Number of rows
grid_cols:      .skip 8                     // Number of cols
start_pos:      .skip 8                     // Start position (row << 32 | col)
end_pos:        .skip 8                     // End position

// Junction data
junctions:      .skip MAX_JUNCTIONS * 8     // Array of (row << 32 | col)
junction_count: .skip 8                     // Number of junctions
junction_index: .skip MAX_GRID_SIZE * MAX_GRID_SIZE  // -1 (0xFF) if not junction, else index

// Graph adjacency list: for each junction, store (neighbor_idx, distance) pairs
// graph[i][j] = (neighbor_index << 32) | distance
graph:          .skip MAX_JUNCTIONS * MAX_EDGES * 8
graph_sizes:    .skip MAX_JUNCTIONS * 8     // Number of edges per junction

// BFS data
bfs_queue:      .skip 65536 * 24            // Queue entries: (row, col, dist) - 24 bytes each
bfs_visited:    .skip MAX_GRID_SIZE * MAX_GRID_SIZE

// DFS data
dfs_visited:    .skip 8                     // Bitmask (64 junctions max)
dfs_stack:      .skip DFS_STACK_SIZE * 24   // (junction_idx, edge_idx, current_dist)

// Output buffer
output_buf:     .skip 64

.section __TEXT,__text

// ============================================================================
// Main entry point
// ============================================================================
_start:
    // Read input file
    bl _read_input

    // Parse grid
    bl _parse_grid

    // Find junctions
    bl _find_junctions

    // Part 1: Build graph respecting slopes
    mov x0, #1                  // respect_slopes = true
    bl _build_graph

    // Find longest path for Part 1
    bl _longest_path_dfs
    mov x19, x0                 // Save Part 1 result

    // Print Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl _print_string
    mov x0, x19
    bl _print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl _print_string

    // Part 2: Rebuild graph ignoring slopes
    mov x0, #0                  // respect_slopes = false
    bl _build_graph

    // Find longest path for Part 2
    bl _longest_path_dfs
    mov x20, x0                 // Save Part 2 result

    // Print Part 2
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl _print_string
    mov x0, x20
    bl _print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl _print_string

    // Exit
    mov x0, #0
    mov x16, #1
    movk x16, #0x200, lsl #16
    svc #0x80

// ============================================================================
// Read input file into buffer
// ============================================================================
_read_input:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    // Open file
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #0                  // O_RDONLY
    mov x2, #0
    mov x16, #5
    movk x16, #0x200, lsl #16
    svc #0x80

    cmp x0, #0
    b.lt _exit_error

    mov x19, x0                 // Save fd

    // Read file
    mov x0, x19
    adrp x1, input_buf@PAGE
    add x1, x1, input_buf@PAGEOFF
    mov x2, #BUFFER_SIZE
    mov x16, #3
    movk x16, #0x200, lsl #16
    svc #0x80

    adrp x1, input_buf@PAGE
    add x1, x1, input_buf@PAGEOFF
    add x1, x1, x0
    strb wzr, [x1]              // Null terminate

    // Close file
    mov x0, x19
    mov x16, #6
    movk x16, #0x200, lsl #16
    svc #0x80

    ldp x29, x30, [sp], #16
    ret

_exit_error:
    mov x0, #1
    mov x16, #1
    movk x16, #0x200, lsl #16
    svc #0x80

// ============================================================================
// Parse grid from input buffer
// ============================================================================
_parse_grid:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    adrp x0, input_buf@PAGE
    add x0, x0, input_buf@PAGEOFF   // Source
    adrp x1, grid@PAGE
    add x1, x1, grid@PAGEOFF        // Dest
    mov x2, #0                  // Row count
    mov x3, #0                  // Col count
    mov x4, #0                  // Current col

_parse_loop:
    ldrb w5, [x0], #1
    cbz w5, _parse_done

    cmp w5, #'\n'
    b.eq _parse_newline

    // Store character
    strb w5, [x1], #1
    add x4, x4, #1
    b _parse_loop

_parse_newline:
    // First row determines cols
    cmp x2, #0
    b.ne _skip_cols_set
    mov x3, x4
_skip_cols_set:
    add x2, x2, #1
    mov x4, #0
    b _parse_loop

_parse_done:
    // Handle last row if no trailing newline
    cmp x4, #0
    b.eq _no_extra_row
    add x2, x2, #1
_no_extra_row:

    adrp x0, grid_rows@PAGE
    add x0, x0, grid_rows@PAGEOFF
    str x2, [x0]
    adrp x0, grid_cols@PAGE
    add x0, x0, grid_cols@PAGEOFF
    str x3, [x0]

    ldp x29, x30, [sp], #16
    ret

// ============================================================================
// Find all junction points (start, end, cells with 3+ neighbors)
// Also builds junction_index lookup table for O(1) junction checks
// ============================================================================
_find_junctions:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!
    mov x29, sp

    adrp x19, grid@PAGE
    add x19, x19, grid@PAGEOFF
    adrp x0, grid_rows@PAGE
    add x0, x0, grid_rows@PAGEOFF
    ldr x20, [x0]               // rows
    adrp x0, grid_cols@PAGE
    add x0, x0, grid_cols@PAGEOFF
    ldr x21, [x0]               // cols

    // Initialize junction_index to 0xFF (-1 as byte) - use unrolled 128-byte clears
    adrp x27, junction_index@PAGE
    add x27, x27, junction_index@PAGEOFF
    mov x0, x27
    mov x1, #0xFFFFFFFFFFFFFFFF  // All 0xFF bytes
    mov x2, #MAX_GRID_SIZE * MAX_GRID_SIZE / 128  // 175 iterations
_clear_junction_index:
    stp x1, x1, [x0], #16
    stp x1, x1, [x0], #16
    stp x1, x1, [x0], #16
    stp x1, x1, [x0], #16
    stp x1, x1, [x0], #16
    stp x1, x1, [x0], #16
    stp x1, x1, [x0], #16
    stp x1, x1, [x0], #16
    subs x2, x2, #1
    b.gt _clear_junction_index
    // Clear remaining 100 bytes
    stp x1, x1, [x0], #16
    stp x1, x1, [x0], #16
    stp x1, x1, [x0], #16
    stp x1, x1, [x0], #16
    stp x1, x1, [x0], #16
    stp x1, x1, [x0], #16
    str w1, [x0]

    adrp x22, junctions@PAGE
    add x22, x22, junctions@PAGEOFF
    mov x23, #0                 // junction count

    // Find start (first '.' in row 0)
    mov x0, #0
_find_start:
    ldrb w1, [x19, x0]
    cmp w1, #'.'
    b.eq _found_start
    add x0, x0, #1
    b _find_start
_found_start:
    // Store start position (row=0, col=x0)
    mov x1, x0                  // col
    mov x28, x0                 // save col in x28
    mov x0, #0                  // row = 0
    orr x2, x1, x0, lsl #32     // Pack as (row << 32 | col)
    str x2, [x22], #8
    // Store in junction_index[0][col] = 0
    strb w23, [x27, x28]        // junction_index[col] = 0 (first junction)
    add x23, x23, #1
    adrp x3, start_pos@PAGE
    add x3, x3, start_pos@PAGEOFF
    str x2, [x3]

    // Find end (first '.' in last row)
    sub x0, x20, #1             // Last row
    mul x1, x0, x21             // Offset to last row
    add x1, x19, x1
    mov x4, #0
_find_end:
    ldrb w5, [x1, x4]
    cmp w5, #'.'
    b.eq _found_end
    add x4, x4, #1
    b _find_end
_found_end:
    // Store end position
    sub x0, x20, #1             // row = rows-1
    orr x2, x4, x0, lsl #32
    str x2, [x22], #8
    // Store in junction_index[row][col] = 1
    mul x5, x0, x21
    add x5, x5, x4              // offset = row * cols + col
    strb w23, [x27, x5]         // junction_index[row][col] = 1
    add x23, x23, #1
    adrp x3, end_pos@PAGE
    add x3, x3, end_pos@PAGEOFF
    str x2, [x3]

    // Find intersections (cells with 3+ walkable neighbors)
    mov x24, #0                 // row
_junction_row_loop:
    cmp x24, x20
    b.ge _junction_done

    mov x25, #0                 // col
_junction_col_loop:
    cmp x25, x21
    b.ge _junction_next_row

    // Get grid[row][col]
    mul x0, x24, x21
    add x0, x0, x25
    ldrb w1, [x19, x0]

    cmp w1, #'#'
    b.eq _junction_next_col

    // Count walkable neighbors
    mov x26, #0                 // neighbor count

    // Check up (row-1)
    cmp x24, #0
    b.eq _skip_up
    sub x0, x24, #1
    mul x0, x0, x21
    add x0, x0, x25
    ldrb w1, [x19, x0]
    cmp w1, #'#'
    b.eq _skip_up
    add x26, x26, #1
_skip_up:

    // Check down (row+1)
    add x0, x24, #1
    cmp x0, x20
    b.ge _skip_down
    mul x0, x0, x21
    add x0, x0, x25
    ldrb w1, [x19, x0]
    cmp w1, #'#'
    b.eq _skip_down
    add x26, x26, #1
_skip_down:

    // Check left (col-1)
    cmp x25, #0
    b.eq _skip_left
    mul x0, x24, x21
    sub x1, x25, #1
    add x0, x0, x1
    ldrb w1, [x19, x0]
    cmp w1, #'#'
    b.eq _skip_left
    add x26, x26, #1
_skip_left:

    // Check right (col+1)
    add x0, x25, #1
    cmp x0, x21
    b.ge _skip_right
    mul x0, x24, x21
    add x1, x25, #1
    add x0, x0, x1
    ldrb w1, [x19, x0]
    cmp w1, #'#'
    b.eq _skip_right
    add x26, x26, #1
_skip_right:

    // If 3+ neighbors, add as junction
    cmp x26, #3
    b.lt _junction_next_col

    // Check junction_index to see if already added
    mul x0, x24, x21
    add x0, x0, x25
    ldrb w1, [x27, x0]
    cmp w1, #0xFF
    b.ne _junction_next_col      // Already exists (not 0xFF)

    // Add junction
    orr x2, x25, x24, lsl #32
    str x2, [x22], #8
    // Store in junction_index
    strb w23, [x27, x0]
    add x23, x23, #1

_junction_next_col:
    add x25, x25, #1
    b _junction_col_loop

_junction_next_row:
    add x24, x24, #1
    b _junction_row_loop

_junction_done:
    adrp x0, junction_count@PAGE
    add x0, x0, junction_count@PAGEOFF
    str x23, [x0]

    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================================
// Build graph from junctions
// x0 = respect_slopes (1 or 0)
// ============================================================================
_build_graph:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #48
    mov x29, sp

    str x0, [sp, #0]            // respect_slopes

    // Clear graph (8192 bytes)
    // Unrolled 4x: clear 64 bytes per iteration (128 * 64 = 8192)
    adrp x0, graph@PAGE
    add x0, x0, graph@PAGEOFF
    mov x2, #128                 // 8192 / 64 = 128
_clear_graph:
    stp xzr, xzr, [x0]
    stp xzr, xzr, [x0, #16]
    stp xzr, xzr, [x0, #32]
    stp xzr, xzr, [x0, #48]
    add x0, x0, #64
    subs x2, x2, #1
    b.gt _clear_graph

    // Clear graph_sizes (512 bytes)
    // Unrolled 4x: clear 64 bytes per iteration (8 * 64 = 512)
    adrp x0, graph_sizes@PAGE
    add x0, x0, graph_sizes@PAGEOFF
    mov x2, #8                   // 512 / 64 = 8
_clear_sizes:
    stp xzr, xzr, [x0]
    stp xzr, xzr, [x0, #16]
    stp xzr, xzr, [x0, #32]
    stp xzr, xzr, [x0, #48]
    add x0, x0, #64
    subs x2, x2, #1
    b.gt _clear_sizes

    adrp x19, grid@PAGE
    add x19, x19, grid@PAGEOFF
    adrp x0, grid_rows@PAGE
    add x0, x0, grid_rows@PAGEOFF
    ldr x20, [x0]               // rows
    adrp x0, grid_cols@PAGE
    add x0, x0, grid_cols@PAGEOFF
    ldr x21, [x0]               // cols
    adrp x22, junctions@PAGE
    add x22, x22, junctions@PAGEOFF
    adrp x0, junction_count@PAGE
    add x0, x0, junction_count@PAGEOFF
    ldr x23, [x0]               // num junctions
    // Store junction_index pointer for O(1) lookups
    adrp x0, junction_index@PAGE
    add x0, x0, junction_index@PAGEOFF
    str x0, [sp, #32]           // junction_index base

    // For each junction, BFS to find reachable junctions
    mov x24, #0                 // junction index

_build_junction_loop:
    cmp x24, x23
    b.ge _build_done

    // Get junction position
    ldr x25, [x22, x24, lsl #3] // start_junction

    // Clear BFS visited (22,500 bytes)
    // Unrolled 8x: clear 128 bytes per iteration (175 * 128 = 22400, remainder = 100)
    adrp x0, bfs_visited@PAGE
    add x0, x0, bfs_visited@PAGEOFF
    mov x2, #175
_clear_visited:
    stp xzr, xzr, [x0], #16
    stp xzr, xzr, [x0], #16
    stp xzr, xzr, [x0], #16
    stp xzr, xzr, [x0], #16
    stp xzr, xzr, [x0], #16
    stp xzr, xzr, [x0], #16
    stp xzr, xzr, [x0], #16
    stp xzr, xzr, [x0], #16
    subs x2, x2, #1
    b.gt _clear_visited
    // Clear remaining 100 bytes (6*16 + 4 = 100)
    stp xzr, xzr, [x0], #16
    stp xzr, xzr, [x0], #16
    stp xzr, xzr, [x0], #16
    stp xzr, xzr, [x0], #16
    stp xzr, xzr, [x0], #16
    stp xzr, xzr, [x0], #16
    str wzr, [x0]

    // Initialize stack with start junction (use LIFO for cache locality)
    adrp x26, bfs_queue@PAGE
    add x26, x26, bfs_queue@PAGEOFF  // stack base
    mov x28, #1                 // stack size (starts at 1)

    // Extract row and col
    lsr x0, x25, #32            // row
    and x1, x25, #0xFFFFFFFF    // col

    // Push to stack: (row, col, dist=0) at position 0
    str x0, [x26, #0]
    str x1, [x26, #8]
    str xzr, [x26, #16]

    // Mark as visited
    mul x2, x0, x21
    add x2, x2, x1
    adrp x3, bfs_visited@PAGE
    add x3, x3, bfs_visited@PAGEOFF
    mov w4, #1
    strb w4, [x3, x2]

_bfs_loop:
    cmp x28, #0
    b.eq _bfs_done

    // Pop from stack - get top element at index (size-1)
    sub x0, x28, #1
    mov x27, #24
    mul x0, x0, x27
    add x0, x26, x0

    ldr x1, [x0, #0]            // row
    ldr x2, [x0, #8]            // col
    ldr x3, [x0, #16]           // dist
    sub x28, x28, #1            // decrement stack size

    str x1, [sp, #8]            // current row
    str x2, [sp, #16]           // current col
    str x3, [sp, #24]           // current dist

    // If dist > 0 and this is a junction, record edge
    cmp x3, #0
    b.eq _explore_neighbors

    // O(1) junction lookup using junction_index[row][col]
    mul x4, x1, x21             // row * cols
    add x4, x4, x2              // + col
    ldr x6, [sp, #32]           // junction_index base
    ldrb w5, [x6, x4]           // junction_index[row * cols + col]
    cmp w5, #0xFF
    b.eq _explore_neighbors     // Not a junction (0xFF = -1)

_found_junction_neighbor:
    // Don't record edge to self
    cmp x5, x24
    b.eq _bfs_continue

    // Add edge: graph[x24][size] = (x5 << 32) | dist
    adrp x6, graph_sizes@PAGE
    add x6, x6, graph_sizes@PAGEOFF
    ldr x7, [x6, x24, lsl #3]   // current size

    // Calculate offset: x24 * MAX_EDGES + x7
    // MAX_EDGES = 16, so x24 * 16 = x24 << 4
    lsl x9, x24, #4
    add x9, x9, x7

    // Store edge
    orr x10, x3, x5, lsl #32    // (neighbor_idx << 32) | distance
    adrp x11, graph@PAGE
    add x11, x11, graph@PAGEOFF
    str x10, [x11, x9, lsl #3]

    // Increment size
    add x7, x7, #1
    str x7, [x6, x24, lsl #3]

    b _bfs_continue              // Don't explore past junctions

_explore_neighbors:
    // Try all 4 directions
    mov x12, #0                 // direction index

_try_direction:
    cmp x12, #4
    b.ge _bfs_continue

    ldr x1, [sp, #8]            // row
    ldr x2, [sp, #16]           // col
    ldr x3, [sp, #24]           // dist

    // Calculate dr, dc
    cmp x12, #0
    b.ne _dir1
    mov x13, #-1                // dr = -1
    mov x14, #0                 // dc = 0
    b _check_dir
_dir1:
    cmp x12, #1
    b.ne _dir2
    mov x13, #1                 // dr = 1
    mov x14, #0                 // dc = 0
    b _check_dir
_dir2:
    cmp x12, #2
    b.ne _dir3
    mov x13, #0                 // dr = 0
    mov x14, #-1                // dc = -1
    b _check_dir
_dir3:
    mov x13, #0                 // dr = 0
    mov x14, #1                 // dc = 1

_check_dir:
    // nr = row + dr, nc = col + dc
    adds x15, x1, x13           // nr
    b.mi _next_dir               // nr < 0
    cmp x15, x20
    b.ge _next_dir               // nr >= rows

    adds x16, x2, x14           // nc
    b.mi _next_dir               // nc < 0
    cmp x16, x21
    b.ge _next_dir               // nc >= cols

    // Check if forest
    mul x17, x15, x21
    add x17, x17, x16
    ldrb w18, [x19, x17]
    cmp w18, #'#'
    b.eq _next_dir

    // Check if visited
    adrp x0, bfs_visited@PAGE
    add x0, x0, bfs_visited@PAGEOFF
    ldrb w4, [x0, x17]
    cmp w4, #0
    b.ne _next_dir

    // Check slope constraints if respect_slopes
    ldr x0, [sp, #0]            // respect_slopes
    cbz x0, _no_slope_check

    // Get current cell
    mul x4, x1, x21
    add x4, x4, x2
    ldrb w5, [x19, x4]

    // Check if current cell is a slope
    cmp w5, #'^'
    b.ne _check_v
    // Must go up (dr=-1, dc=0)
    cmn x13, #1                 // x13 == -1?
    b.ne _next_dir
    cmp x14, #0
    b.ne _next_dir
    b _no_slope_check

_check_v:
    cmp w5, #'v'
    b.ne _check_lt
    // Must go down (dr=1, dc=0)
    cmp x13, #1
    b.ne _next_dir
    cmp x14, #0
    b.ne _next_dir
    b _no_slope_check

_check_lt:
    cmp w5, #'<'
    b.ne _check_gt
    // Must go left (dr=0, dc=-1)
    cmp x13, #0
    b.ne _next_dir
    cmn x14, #1                 // x14 == -1?
    b.ne _next_dir
    b _no_slope_check

_check_gt:
    cmp w5, #'>'
    b.ne _no_slope_check
    // Must go right (dr=0, dc=1)
    cmp x13, #0
    b.ne _next_dir
    cmp x14, #1
    b.ne _next_dir

_no_slope_check:
    // Mark as visited
    adrp x0, bfs_visited@PAGE
    add x0, x0, bfs_visited@PAGEOFF
    mov w4, #1
    strb w4, [x0, x17]

    // Push to stack (nr, nc, dist+1)
    mov x4, #24
    mul x4, x28, x4             // stack_size * 24
    add x4, x26, x4             // stack base + offset

    str x15, [x4, #0]           // nr
    str x16, [x4, #8]           // nc
    add x5, x3, #1
    str x5, [x4, #16]           // dist + 1

    add x28, x28, #1            // increment stack size

_next_dir:
    add x12, x12, #1
    b _try_direction

_bfs_continue:
    b _bfs_loop

_bfs_done:
    add x24, x24, #1
    b _build_junction_loop

_build_done:
    add sp, sp, #48
    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================================
// Find longest path using DFS with backtracking (recursive implementation)
// Returns longest distance in x0
// ============================================================================
_longest_path_dfs:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    mov x29, sp

    // Set up constants for recursive DFS
    adrp x19, graph@PAGE
    add x19, x19, graph@PAGEOFF
    adrp x20, graph_sizes@PAGE
    add x20, x20, graph_sizes@PAGEOFF

    // Call recursive DFS: dfs(node=0, visited=0)
    // x0 = node, x1 = visited_mask
    // x19 = graph base, x20 = graph_sizes base, x21 = end_idx (1)
    mov x21, #1                 // end_idx constant
    mov x0, #0                  // start node
    mov x1, #0                  // visited = empty (self-marking)
    bl _dfs_recursive

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Recursive DFS function (C-style: returns distance from node to end)
// x0 = node, x1 = visited_mask
// x19 = graph base (preserved), x20 = graph_sizes base (preserved), x21 = end_idx (preserved)
// Returns max distance from node to end in x0, or -1 if no path
_dfs_recursive:
    // Check if at end
    cmp x0, x21
    b.ne _dfs_not_end
    mov x0, #0                  // Return 0 at end
    ret

_dfs_not_end:
    stp x29, x30, [sp, #-16]!
    stp x22, x23, [sp, #-16]!
    stp x24, x25, [sp, #-16]!
    stp x26, x27, [sp, #-16]!
    mov x29, sp

    // Mark self as visited
    mov x3, #1
    lsl x3, x3, x0
    orr x23, x1, x3             // x23 = visited_mask with self marked

    mov x24, #-1                // max_dist = -1

    // Get number of edges: graph_sizes[node]
    ldr x25, [x20, x0, lsl #3]  // count
    cbz x25, _dfs_edge_done     // Skip if no edges

    // Calculate edge pointer: graph + (node * 16) * 8 = graph + node * 128
    lsl x26, x0, #7             // node * 128
    add x26, x19, x26           // edge_ptr = graph + node * 128

    // Calculate end pointer: edge_ptr + count * 8
    add x27, x26, x25, lsl #3   // edge_end = edge_ptr + count * 8

_dfs_edge_loop:
    // Load edge
    ldr x0, [x26], #8           // load and advance

    // Extract neighbor and weight
    lsr x1, x0, #32             // neighbor_idx
    and x22, x0, #0xFFFFFFFF    // weight (x22 available since we saved it)

    // Check if neighbor visited (optimized: shift visited right by neighbor_idx, test bit 0)
    lsr x3, x23, x1
    tbnz x3, #0, _dfs_check_loop  // Skip if bit 0 set (visited)

    // Recursive call
    mov x0, x1                  // neighbor
    mov x1, x23                 // visited
    bl _dfs_recursive

    // Check result
    cmp x0, #0
    b.lt _dfs_check_loop        // Skip if no path

    // total = weight + result
    add x0, x22, x0

    // Update max_dist if better
    cmp x0, x24
    csel x24, x0, x24, gt

_dfs_check_loop:
    cmp x26, x27
    b.lt _dfs_edge_loop

_dfs_edge_done:
    mov x0, x24                 // Return max_dist

    ldp x26, x27, [sp], #16
    ldp x24, x25, [sp], #16
    ldp x22, x23, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================================
// Print string (null-terminated)
// x0 = string address
// ============================================================================
_print_string:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    mov x29, sp

    mov x19, x0

    // Find length
    mov x1, #0
_strlen_loop:
    ldrb w2, [x19, x1]
    cbz w2, _strlen_done
    add x1, x1, #1
    b _strlen_loop
_strlen_done:

    // Write
    mov x0, #1                  // stdout
    mov x2, x1                  // length
    mov x1, x19                 // buffer
    mov x16, #4
    movk x16, #0x200, lsl #16
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================================
// Print number
// x0 = number to print
// ============================================================================
_print_number:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    mov x29, sp

    adrp x19, output_buf@PAGE
    add x19, x19, output_buf@PAGEOFF
    add x19, x19, #32           // Start at end of buffer
    mov x20, x19                // Save end position

    cbz x0, _print_zero

    mov x1, #10
_convert_loop:
    cbz x0, _print_converted
    udiv x2, x0, x1             // x2 = x0 / 10
    msub x3, x2, x1, x0         // x3 = x0 - (x2 * 10) = remainder
    add w3, w3, #'0'
    sub x19, x19, #1
    strb w3, [x19]
    mov x0, x2
    b _convert_loop

_print_zero:
    sub x19, x19, #1
    mov w0, #'0'
    strb w0, [x19]

_print_converted:
    // Calculate length
    sub x2, x20, x19

    // Write
    mov x0, #1
    mov x1, x19
    mov x16, #4
    movk x16, #0x200, lsl #16
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
