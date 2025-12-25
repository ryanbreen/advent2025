// Day 23: LAN Party - ARM64 Assembly for macOS
// Find triangles and maximum clique using simple greedy algorithm

.global _start
.align 4

// Constants
.equ O_RDONLY, 0x0000
.equ MAX_NODES, 1000
.equ NODE_LEN, 3

// Data section
.data
.align 8

input_path:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"
comma:          .asciz ","

// BSS section - uninitialized data
.bss
.align 8

file_buffer:    .space 65536
nodes:          .space MAX_NODES * NODE_LEN    // Node names (2-char + null)
node_count:     .space 8
adj_matrix:     .space MAX_NODES * MAX_NODES   // Adjacency matrix (bytes)
part1_result:   .space 8
part2_result:   .space 2048                    // Password string
output_buffer:  .space 32

// Working memory for Part 2
max_clique:     .space MAX_NODES * 4           // Max clique node indices
max_clique_size: .space 8
current_clique: .space MAX_NODES * 4           // Current clique being built
current_size:   .space 8

// Text section
.text

// Main entry point
_start:
    // Save frame pointer and link register
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Open and read input file
    bl      read_input_file

    // Parse input and build graph
    bl      parse_input

    // Part 1: Find triangles with 't' node
    bl      solve_part1

    // Print Part 1 result
    adrp    x0, part1_msg@PAGE
    add     x0, x0, part1_msg@PAGEOFF
    bl      print_string
    adrp    x9, part1_result@PAGE
    add     x9, x9, part1_result@PAGEOFF
    ldr     x0, [x9]
    bl      print_number
    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    // Part 2: Find maximum clique
    bl      solve_part2

    // Print Part 2 result
    adrp    x0, part2_msg@PAGE
    add     x0, x0, part2_msg@PAGEOFF
    bl      print_string
    adrp    x0, part2_result@PAGE
    add     x0, x0, part2_result@PAGEOFF
    bl      print_string
    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    // Exit
    mov     x0, #0
    mov     x16, #1
    orr     x16, x16, #0x2000000
    svc     #0

// Read input file into buffer
// Returns: x0 = bytes read
read_input_file:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // open(path, O_RDONLY)
    adrp    x0, input_path@PAGE
    add     x0, x0, input_path@PAGEOFF
    mov     x1, #O_RDONLY
    mov     x2, #0
    mov     x16, #5
    orr     x16, x16, #0x2000000
    svc     #0

    cmp     x0, #0
    b.lt    read_error
    mov     x19, x0          // Save fd

    // read(fd, buffer, size)
    mov     x0, x19
    adrp    x1, file_buffer@PAGE
    add     x1, x1, file_buffer@PAGEOFF
    mov     x2, #65536
    mov     x16, #3
    orr     x16, x16, #0x2000000
    svc     #0

    mov     x20, x0          // Save bytes read

    // Close file
    mov     x0, x19

    mov     x0, x20          // Return bytes read

read_error:
    ldp     x29, x30, [sp], #16
    ret

// Parse input and build graph
// Input: file_buffer contains connections
parse_input:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    // Zero node_count
    adrp    x19, node_count@PAGE
    add     x19, x19, node_count@PAGEOFF
    str     xzr, [x19]

    // Zero adjacency matrix
    adrp    x0, adj_matrix@PAGE
    add     x0, x0, adj_matrix@PAGEOFF
    mov     x1, #MAX_NODES
    mov     x2, #MAX_NODES
    mul     x1, x1, x2
    bl      memzero

    // Parse connections
    adrp    x20, file_buffer@PAGE
    add     x20, x20, file_buffer@PAGEOFF

parse_line_loop:
    ldrb    w21, [x20]
    cbz     w21, parse_done

    // Read first node (2 chars)
    ldrb    w21, [x20], #1       // First char
    cbz     w21, parse_done
    cmp     w21, #'\n'
    b.eq    parse_line_loop

    ldrb    w22, [x20], #1       // Second char
    cbz     w22, parse_done

    // Get or add first node
    mov     w0, w21
    mov     w1, w22
    bl      get_node_index
    mov     x23, x0              // Save first node index

    // Skip '-'
    ldrb    w21, [x20], #1

    // Read second node (2 chars)
    ldrb    w21, [x20], #1       // First char
    cbz     w21, parse_done

    ldrb    w22, [x20], #1       // Second char
    cbz     w22, parse_done

    // Get or add second node
    mov     w0, w21
    mov     w1, w22
    bl      get_node_index
    mov     x24, x0              // Save second node index

    // Add edge to adjacency matrix (both directions)
    adrp    x9, adj_matrix@PAGE
    add     x9, x9, adj_matrix@PAGEOFF
    mov     x10, #MAX_NODES
    mul     x11, x23, x10        // row offset for node1
    add     x11, x11, x24        // + col for node2
    mov     w12, #1
    strb    w12, [x9, x11]       // adj[node1][node2] = 1

    mul     x11, x24, x10        // row offset for node2
    add     x11, x11, x23        // + col for node1
    strb    w12, [x9, x11]       // adj[node2][node1] = 1

    // Skip to next line
    ldrb    w21, [x20], #1
    cbnz    w21, parse_line_loop

parse_done:
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// Get or add node to graph
// Input: w0, w1 = two-character node name
// Output: x0 = node index
get_node_index:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]

    mov     w19, w0              // Save chars
    mov     w20, w1

    // Search existing nodes
    adrp    x9, nodes@PAGE
    add     x9, x9, nodes@PAGEOFF
    adrp    x10, node_count@PAGE
    add     x10, x10, node_count@PAGEOFF
    ldr     x11, [x10]           // Current count
    mov     x12, #0              // Index

find_node_loop:
    cmp     x12, x11
    b.ge    add_new_node

    // Check if node matches
    mov     x13, #NODE_LEN
    mul     x14, x12, x13
    ldrb    w15, [x9, x14]
    cmp     w15, w19
    b.ne    find_node_next
    add     x14, x14, #1
    ldrb    w15, [x9, x14]
    cmp     w15, w20
    b.eq    find_node_found

find_node_next:
    add     x12, x12, #1
    b       find_node_loop

find_node_found:
    mov     x0, x12
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

add_new_node:
    // Add new node at index x11
    mov     x13, #NODE_LEN
    mul     x14, x11, x13
    strb    w19, [x9, x14]
    add     x14, x14, #1
    strb    w20, [x9, x14]
    add     x14, x14, #1
    strb    wzr, [x9, x14]       // Null terminator

    // Increment count
    add     x11, x11, #1
    str     x11, [x10]

    mov     x0, x11
    sub     x0, x0, #1           // Return new index

    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// Part 1: Find all triangles and count those with 't' node
solve_part1:
    stp     x29, x30, [sp, #-80]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]

    // Load node count
    adrp    x19, node_count@PAGE
    add     x19, x19, node_count@PAGEOFF
    ldr     x19, [x19]

    adrp    x20, adj_matrix@PAGE
    add     x20, x20, adj_matrix@PAGEOFF

    mov     x21, #0              // Triangle count
    mov     x22, #0              // a

tri_loop_a:
    cmp     x22, x19
    b.ge    tri_done
    mov     x23, x22
    add     x23, x23, #1         // b = a + 1

tri_loop_b:
    cmp     x23, x19
    b.ge    tri_next_a

    // Check if adj[a][b]
    mov     x9, #MAX_NODES
    mul     x10, x22, x9
    add     x10, x10, x23
    ldrb    w11, [x20, x10]
    cbz     w11, tri_next_b

    mov     x24, x23
    add     x24, x24, #1         // c = b + 1

tri_loop_c:
    cmp     x24, x19
    b.ge    tri_next_b

    // Check if adj[a][c] && adj[b][c]
    mov     x9, #MAX_NODES
    mul     x10, x22, x9
    add     x10, x10, x24
    ldrb    w11, [x20, x10]
    cbz     w11, tri_next_c

    mul     x10, x23, x9
    add     x10, x10, x24
    ldrb    w12, [x20, x10]
    cbz     w12, tri_next_c

    // Found triangle (a, b, c)
    // Check if any starts with 't'
    adrp    x9, nodes@PAGE
    add     x9, x9, nodes@PAGEOFF

    mov     x10, #NODE_LEN
    mul     x11, x22, x10
    ldrb    w13, [x9, x11]
    cmp     w13, #'t'
    b.eq    tri_has_t

    mul     x11, x23, x10
    ldrb    w13, [x9, x11]
    cmp     w13, #'t'
    b.eq    tri_has_t

    mul     x11, x24, x10
    ldrb    w13, [x9, x11]
    cmp     w13, #'t'
    b.ne    tri_next_c

tri_has_t:
    add     x21, x21, #1         // Increment count

tri_next_c:
    add     x24, x24, #1
    b       tri_loop_c

tri_next_b:
    add     x23, x23, #1
    b       tri_loop_b

tri_next_a:
    add     x22, x22, #1
    b       tri_loop_a

tri_done:
    // Store result
    adrp    x9, part1_result@PAGE
    add     x9, x9, part1_result@PAGEOFF
    str     x21, [x9]

    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #80
    ret

// Part 2: Find maximum clique using greedy algorithm
// For each node, try to build the largest clique starting from that node
solve_part2:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Initialize max_clique_size to 0
    adrp    x9, max_clique_size@PAGE
    add     x9, x9, max_clique_size@PAGEOFF
    str     xzr, [x9]

    // Load node count
    adrp    x19, node_count@PAGE
    add     x19, x19, node_count@PAGEOFF
    ldr     x19, [x19]

    // Try starting from each node
    mov     x20, #0              // start_node

try_each_start:
    cmp     x20, x19
    b.ge    part2_done

    // Build clique starting from x20
    mov     x0, x20
    bl      find_clique_from_node

    add     x20, x20, #1
    b       try_each_start

part2_done:
    // Build password string from max_clique
    bl      build_password

    ldp     x29, x30, [sp], #16
    ret

// Find largest clique starting from a given node
// Input: x0 = starting node index
find_clique_from_node:
    stp     x29, x30, [sp, #-80]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]

    mov     x19, x0              // start_node

    // Initialize current_clique with start_node
    adrp    x20, current_clique@PAGE
    add     x20, x20, current_clique@PAGEOFF
    str     w19, [x20]           // clique[0] = start_node

    adrp    x21, current_size@PAGE
    add     x21, x21, current_size@PAGEOFF
    mov     x9, #1
    str     x9, [x21]            // size = 1

    // Get node count
    adrp    x22, node_count@PAGE
    add     x22, x22, node_count@PAGEOFF
    ldr     x22, [x22]

    // Get adjacency matrix
    adrp    x23, adj_matrix@PAGE
    add     x23, x23, adj_matrix@PAGEOFF

    // Try to add each other node to the clique
    mov     x24, #0              // candidate node

try_add_loop:
    cmp     x24, x22
    b.ge    check_if_best

    // Skip if candidate is already in clique
    cmp     x24, x19
    b.eq    try_add_next

    // Check if candidate is connected to ALL nodes in current clique
    ldr     x25, [x21]           // current_size
    mov     x26, #0              // clique_idx

check_connections:
    cmp     x26, x25
    b.ge    add_to_clique        // All connections checked, can add

    // Get node from clique
    ldr     w9, [x20, x26, lsl #2]

    // Check if adj[candidate][node]
    mov     x10, #MAX_NODES
    mul     x11, x24, x10
    add     x11, x11, x9
    ldrb    w12, [x23, x11]
    cbz     w12, try_add_next    // Not connected, skip candidate

    add     x26, x26, #1
    b       check_connections

add_to_clique:
    // Add candidate to clique
    ldr     x25, [x21]
    str     w24, [x20, x25, lsl #2]
    add     x25, x25, #1
    str     x25, [x21]

try_add_next:
    add     x24, x24, #1
    b       try_add_loop

check_if_best:
    // Check if this clique is larger than max_clique
    adrp    x9, max_clique_size@PAGE
    add     x9, x9, max_clique_size@PAGEOFF
    ldr     x10, [x9]
    ldr     x11, [x21]           // current_size

    cmp     x11, x10
    b.le    find_clique_done

    // Save as new max_clique
    str     x11, [x9]
    adrp    x12, max_clique@PAGE
    add     x12, x12, max_clique@PAGEOFF

    mov     x13, #0
copy_clique:
    cmp     x13, x11
    b.ge    find_clique_done
    ldr     w14, [x20, x13, lsl #2]
    str     w14, [x12, x13, lsl #2]
    add     x13, x13, #1
    b       copy_clique

find_clique_done:
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #80
    ret

// Build password from max_clique
build_password:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]

    // Sort max_clique indices by node name
    adrp    x19, max_clique@PAGE
    add     x19, x19, max_clique@PAGEOFF
    adrp    x20, max_clique_size@PAGE
    add     x20, x20, max_clique_size@PAGEOFF
    ldr     x20, [x20]

    // Simple bubble sort
    cmp     x20, #1
    b.le    sort_done

    mov     x21, x20
    sub     x21, x21, #1         // outer = size - 1

sort_outer:
    cbz     x21, sort_done
    mov     x22, #0              // inner

sort_inner:
    cmp     x22, x21
    b.ge    sort_outer_next

    ldr     w9, [x19, x22, lsl #2]
    add     x10, x22, #1
    ldr     w11, [x19, x10, lsl #2]

    // Compare node names
    adrp    x12, nodes@PAGE
    add     x12, x12, nodes@PAGEOFF
    mov     x13, #NODE_LEN
    mul     x14, x9, x13
    mul     x15, x11, x13

    ldrb    w16, [x12, x14]      // First char of node1
    ldrb    w17, [x12, x15]      // First char of node2
    cmp     w16, w17
    b.lt    sort_no_swap
    b.gt    sort_swap

    add     x14, x14, #1
    add     x15, x15, #1
    ldrb    w16, [x12, x14]      // Second char of node1
    ldrb    w17, [x12, x15]      // Second char of node2
    cmp     w16, w17
    b.le    sort_no_swap

sort_swap:
    str     w11, [x19, x22, lsl #2]
    str     w9, [x19, x10, lsl #2]

sort_no_swap:
    add     x22, x22, #1
    b       sort_inner

sort_outer_next:
    sub     x21, x21, #1
    b       sort_outer

sort_done:
    // Build password string
    adrp    x19, part2_result@PAGE
    add     x19, x19, part2_result@PAGEOFF
    adrp    x20, max_clique@PAGE
    add     x20, x20, max_clique@PAGEOFF
    adrp    x21, max_clique_size@PAGE
    add     x21, x21, max_clique_size@PAGEOFF
    ldr     x21, [x21]

    mov     x22, #0              // Index in clique

build_pwd_loop:
    cmp     x22, x21
    b.ge    build_pwd_done

    // Add comma if not first
    cbz     x22, build_pwd_no_comma
    mov     w9, #','
    strb    w9, [x19], #1

build_pwd_no_comma:
    // Get node index
    ldr     w9, [x20, x22, lsl #2]

    // Get node name
    adrp    x10, nodes@PAGE
    add     x10, x10, nodes@PAGEOFF
    mov     x11, #NODE_LEN
    mul     x12, x9, x11

    // Copy 2 chars
    ldrb    w13, [x10, x12]
    strb    w13, [x19], #1
    add     x12, x12, #1
    ldrb    w13, [x10, x12]
    strb    w13, [x19], #1

    add     x22, x22, #1
    b       build_pwd_loop

build_pwd_done:
    // Null terminate
    strb    wzr, [x19]

    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret

// Memory operations
memzero:
    cbz     x1, memzero_done

memzero_loop:
    strb    wzr, [x0], #1
    subs    x1, x1, #1
    b.ne    memzero_loop

memzero_done:
    ret

// Print null-terminated string
print_string:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    str     x19, [sp, #16]

    mov     x19, x0

    // Calculate length
    mov     x1, x0
strlen_loop:
    ldrb    w2, [x1], #1
    cbz     w2, strlen_done
    b       strlen_loop
strlen_done:
    sub     x2, x1, x0
    sub     x2, x2, #1

    // write(1, string, length)
    mov     x0, #1
    mov     x1, x19
    mov     x16, #4
    orr     x16, x16, #0x2000000
    svc     #0

    ldr     x19, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// Print number
print_number:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]

    mov     x19, x0

    // Convert to string
    adrp    x20, output_buffer@PAGE
    add     x20, x20, output_buffer@PAGEOFF
    add     x20, x20, #31
    strb    wzr, [x20]

    mov     x9, #10
num_to_str_loop:
    sub     x20, x20, #1
    udiv    x10, x19, x9
    msub    x11, x10, x9, x19
    add     w11, w11, #'0'
    strb    w11, [x20]
    mov     x19, x10
    cbnz    x19, num_to_str_loop

    // Print string
    mov     x0, x20
    bl      print_string

    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret
