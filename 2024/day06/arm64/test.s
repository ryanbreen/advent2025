.global _main
.align 4

.data
msg: .asciz "Value: %d\n"

.text
_main:
    stp x29, x30, [sp, #-32]!
    mov x29, sp

    // Store argument on stack like C compiler does
    mov x8, #42
    str x8, [sp]

    adrp x0, msg@page
    add x0, x0, msg@pageoff
    bl _printf

    mov x0, #0
    ldp x29, x30, [sp], #32
    ret
