// Day 23: LAN Party - ARM64 Assembly for macOS
// Find triangles and maximum clique using Bron-Kerbosch algorithm

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
debug_msg:      .asciz "Clique size: "
debug_msg2:     .asciz "Cliques found: "
debug_msg3:     .asciz "BK calls: "
newline:        .asciz "\n"
comma:          .asciz ","

// BSS section - uninitialized data
.bss
.align 8

file_buffer:    .space 65536
nodes:          .space MAX_NODES * NODE_LEN    // Node names (2-char + null)
node_count:     .space 8
adj_matrix:     .space MAX_NODES * MAX_NODES   // Adjacency matrix (bytes)
triangles:      .space 100000 * 12             // Triangle storage (3 ints each)
triangle_count: .space 8
part1_result:   .space 8
part2_result:   .space 2048                    // Password string
output_buffer:  .space 32

// Bron-Kerbosch working memory
max_clique:     .space MAX_NODES * 4           // Max clique node indices
max_clique_size: .space 8
r_set:          .space MAX_NODES               // Current clique
p_set:          .space MAX_NODES               // Candidates
x_set:          .space MAX_NODES               // Processed
temp_set1:      .space MAX_NODES               // Temp working set
temp_set2:      .space MAX_NODES               // Temp working set
temp_set3:      .space MAX_NODES               // Temp working set
debug_clique_count: .space 8                   // Debug counter
debug_call_count: .space 8                     // Call counter

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

    // Part 2: Find maximum clique
    bl      solve_part2

    // Print results
    bl      print_results

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

// Part 2: Find maximum clique using Bron-Kerbosch
solve_part2:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Initialize max_clique_size to 0
    adrp    x9, max_clique_size@PAGE
    add     x9, x9, max_clique_size@PAGEOFF
    str     xzr, [x9]

    // Initialize R = empty
    adrp    x0, r_set@PAGE
    add     x0, x0, r_set@PAGEOFF
    mov     x1, #MAX_NODES
    bl      memzero

    // Initialize X = empty
    adrp    x0, x_set@PAGE
    add     x0, x0, x_set@PAGEOFF
    mov     x1, #MAX_NODES
    bl      memzero

    // Initialize P = all nodes
    adrp    x0, p_set@PAGE
    add     x0, x0, p_set@PAGEOFF
    mov     x1, #MAX_NODES
    bl      memzero

    adrp    x9, node_count@PAGE
    add     x9, x9, node_count@PAGEOFF
    ldr     x10, [x9]
    mov     x11, #0

init_p_loop:
    cmp     x11, x10
    b.ge    init_p_done
    mov     w12, #1
    strb    w12, [x0, x11]
    add     x11, x11, #1
    b       init_p_loop

init_p_done:
    // Call Bron-Kerbosch
    bl      bron_kerbosch

    // Debug: print call count
    adrp    x0, debug_msg3@PAGE
    add     x0, x0, debug_msg3@PAGEOFF
    bl      print_string
    adrp    x9, debug_call_count@PAGE
    add     x9, x9, debug_call_count@PAGEOFF
    ldr     x0, [x9]
    bl      print_number
    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    // Debug: print clique count
    adrp    x0, debug_msg2@PAGE
    add     x0, x0, debug_msg2@PAGEOFF
    bl      print_string
    adrp    x9, debug_clique_count@PAGE
    add     x9, x9, debug_clique_count@PAGEOFF
    ldr     x0, [x9]
    bl      print_number
    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    // Debug: print clique size
    adrp    x0, debug_msg@PAGE
    add     x0, x0, debug_msg@PAGEOFF
    bl      print_string
    adrp    x9, max_clique_size@PAGE
    add     x9, x9, max_clique_size@PAGEOFF
    ldr     x0, [x9]
    bl      print_number
    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    // Build password string from max_clique
    bl      build_password
    ldp     x29, x30, [sp], #16
    ret

// Bron-Kerbosch algorithm (with proper state management)
// Allocate sets on stack to avoid corruption
bron_kerbosch:
    // Debug: increment call counter
    adrp    x9, debug_call_count@PAGE
    add     x9, x9, debug_call_count@PAGEOFF
    ldr     x10, [x9]
    add     x10, x10, #1
    str     x10, [x9]

    // Save registers first
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    // Allocate space for local copies of R, P, X (3 * MAX_NODES = 3000 bytes)
    sub     sp, sp, #3008

    // Local set offsets relative to current sp
    // r_local = sp + 0
    // p_local = sp + 1000
    // x_local = sp + 2000

    // Copy global R to local
    mov     x0, sp               // r_local
    adrp    x1, r_set@PAGE
    add     x1, x1, r_set@PAGEOFF
    mov     x2, #MAX_NODES
    bl      memcpy_bytes

    // Copy global P to local
    add     x0, sp, #1000        // p_local
    adrp    x1, p_set@PAGE
    add     x1, x1, p_set@PAGEOFF
    mov     x2, #MAX_NODES
    bl      memcpy_bytes

    // Copy global X to local
    add     x0, sp, #2000        // x_local
    adrp    x1, x_set@PAGE
    add     x1, x1, x_set@PAGEOFF
    mov     x2, #MAX_NODES
    bl      memcpy_bytes

    // Get node count
    adrp    x21, node_count@PAGE
    add     x21, x21, node_count@PAGEOFF
    ldr     x21, [x21]

    // Check if P and X are both empty
    mov     x19, sp              // r_local
    add     x20, sp, #1000       // p_local
    add     x22, sp, #2000       // x_local

    mov     x9, #0
    mov     x10, #0              // p_size
    mov     x11, #0              // x_size

bk_check_empty_loop:
    cmp     x9, x21
    b.ge    bk_check_empty_done
    ldrb    w12, [x20, x9]
    add     x10, x10, x12
    ldrb    w12, [x22, x9]
    add     x11, x11, x12
    add     x9, x9, #1
    b       bk_check_empty_loop

bk_check_empty_done:
    // If both empty (p_size == 0 AND x_size == 0), we found a maximal clique
    orr     x12, x10, x11
    cbnz    x12, bk_not_maximal
    // If we get here, both are empty

    // Debug: we found a clique, count it
    // Increment a counter (we'll use a global for this)
    adrp    x25, debug_clique_count@PAGE
    add     x25, x25, debug_clique_count@PAGEOFF
    ldr     x26, [x25]
    add     x26, x26, #1
    str     x26, [x25]

    // Count size of R
    mov     x9, #0
    mov     x23, #0              // r_size

bk_count_r_loop:
    cmp     x9, x21
    b.ge    bk_count_r_done
    ldrb    w12, [x19, x9]
    add     x23, x23, x12
    add     x9, x9, #1
    b       bk_count_r_loop

bk_count_r_done:
    // Compare with max_clique_size
    adrp    x24, max_clique_size@PAGE
    add     x24, x24, max_clique_size@PAGEOFF
    ldr     x25, [x24]
    cmp     x23, x25
    b.le    bk_return

    // Update max clique
    str     x23, [x24]
    adrp    x24, max_clique@PAGE
    add     x24, x24, max_clique@PAGEOFF
    mov     x9, #0
    mov     x10, #0              // clique index

bk_save_clique_loop:
    cmp     x9, x21
    b.ge    bk_return
    ldrb    w12, [x19, x9]
    cbz     w12, bk_save_clique_next
    str     w9, [x24, x10, lsl #2]
    add     x10, x10, #1

bk_save_clique_next:
    add     x9, x9, #1
    b       bk_save_clique_loop

bk_not_maximal:
    // Iterate over P
    mov     x23, #0              // node index

bk_iter_loop:
    cmp     x23, x21
    b.ge    bk_return

    ldrb    w24, [x20, x23]      // Check if in P
    cbz     w24, bk_iter_next

    // Get neighbors of v into temp_set1
    adrp    x10, temp_set1@PAGE
    add     x10, x10, temp_set1@PAGEOFF
    mov     x0, x23
    mov     x1, x10
    bl      get_neighbors

    // Copy global sets for recursion
    // R_new = R ∪ {v}
    adrp    x0, r_set@PAGE
    add     x0, x0, r_set@PAGEOFF
    mov     x1, x19              // r_local
    mov     x2, #MAX_NODES
    bl      memcpy_bytes
    adrp    x9, r_set@PAGE
    add     x9, x9, r_set@PAGEOFF
    mov     w11, #1
    strb    w11, [x9, x23]

    // P_new = P ∩ N(v)
    adrp    x0, p_set@PAGE
    add     x0, x0, p_set@PAGEOFF
    adrp    x10, temp_set1@PAGE
    add     x10, x10, temp_set1@PAGEOFF
    mov     x9, #0

bk_intersect_p_loop:
    cmp     x9, x21
    b.ge    bk_intersect_p_done
    ldrb    w11, [x20, x9]
    ldrb    w12, [x10, x9]
    and     w11, w11, w12
    strb    w11, [x0, x9]
    add     x9, x9, #1
    b       bk_intersect_p_loop

bk_intersect_p_done:
    // X_new = X ∩ N(v)
    adrp    x0, x_set@PAGE
    add     x0, x0, x_set@PAGEOFF
    mov     x9, #0

bk_intersect_x_loop:
    cmp     x9, x21
    b.ge    bk_intersect_x_done
    ldrb    w11, [x22, x9]
    ldrb    w12, [x10, x9]
    and     w11, w11, w12
    strb    w11, [x0, x9]
    add     x9, x9, #1
    b       bk_intersect_x_loop

bk_intersect_x_done:
    // Recursive call
    bl      bron_kerbosch

    // After recursion, update local P and X
    // Remove v from P
    strb    wzr, [x20, x23]

    // Add v to X
    mov     w11, #1
    strb    w11, [x22, x23]

bk_iter_next:
    add     x23, x23, #1
    b       bk_iter_loop

bk_return:
    // Restore stack
    add     sp, sp, #3008
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// Get neighbors of a node
// Input: x0 = node index, x1 = output set address
get_neighbors:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    str     x19, [sp, #16]

    mov     x19, x1              // Save output address

    // Zero output
    mov     x2, #MAX_NODES
    bl      memzero

    // Load adj matrix
    adrp    x9, adj_matrix@PAGE
    add     x9, x9, adj_matrix@PAGEOFF
    adrp    x10, node_count@PAGE
    add     x10, x10, node_count@PAGEOFF
    ldr     x10, [x10]

    // Get row in adj matrix
    mov     x11, #MAX_NODES
    mul     x12, x0, x11
    add     x9, x9, x12

    // Copy row to output
    mov     x11, #0
neighbor_loop:
    cmp     x11, x10
    b.ge    neighbor_done
    ldrb    w12, [x9, x11]
    strb    w12, [x19, x11]
    add     x11, x11, #1
    b       neighbor_loop

neighbor_done:
    ldr     x19, [sp, #16]
    ldp     x29, x30, [sp], #32
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

memcpy_bytes:
    cbz     x2, memcpy_done

memcpy_loop:
    ldrb    w9, [x1], #1
    strb    w9, [x0], #1
    subs    x2, x2, #1
    b.ne    memcpy_loop

memcpy_done:
    ret

// Print results
print_results:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Print "Part 1: "
    adrp    x0, part1_msg@PAGE
    add     x0, x0, part1_msg@PAGEOFF
    bl      print_string

    // Print part 1 result
    adrp    x9, part1_result@PAGE
    add     x9, x9, part1_result@PAGEOFF
    ldr     x0, [x9]
    bl      print_number

    // Print newline
    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    // Print "Part 2: "
    adrp    x0, part2_msg@PAGE
    add     x0, x0, part2_msg@PAGEOFF
    bl      print_string

    // Print part 2 result
    adrp    x0, part2_result@PAGE
    add     x0, x0, part2_result@PAGEOFF
    bl      print_string

    // Print newline
    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    ldp     x29, x30, [sp], #16
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
