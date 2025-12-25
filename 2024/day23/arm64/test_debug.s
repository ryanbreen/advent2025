.global _start
.align 4

.data
msg: .asciz "Max clique size: "
newline: .asciz "\n"

.bss
.align 8
file_buffer:    .space 65536
nodes:          .space 3000
node_count:     .space 8
adj_matrix:     .space 1000000
max_clique_size: .space 8

.text
_start:
    // Quick test - just print max_clique_size location
    adrp    x9, max_clique_size@PAGE
    add     x9, x9, max_clique_size@PAGEOFF
    ldr     x0, [x9]
    
    // Exit
    mov     x0, #0
    mov     x16, #1
    orr     x16, x16, #0x2000000
    svc     #0
