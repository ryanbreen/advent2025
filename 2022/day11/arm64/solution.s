// Day 11: Monkey in the Middle - ARM64 Assembly (macOS)
//
// Algorithm:
//   Parse monkey definitions from input
//   Part 1: 20 rounds, divide worry by 3 after each inspection
//   Part 2: 10000 rounds, no divide by 3, use product of all divisors as modulo
//   Return product of top 2 inspection counts

.global _start
.align 4

// Constants
.equ BUFFER_SIZE, 16384         // Input file buffer size
.equ MAX_MONKEYS, 16            // Maximum number of monkeys
.equ MAX_ITEMS, 64              // Max items per monkey
.equ ITEM_SIZE, 8               // 64-bit items

// Monkey structure offsets (each monkey = 1024 bytes total)
// Items are stored as a circular queue
.equ M_ITEMS, 0                 // Items array: 64 * 8 = 512 bytes
.equ M_ITEM_COUNT, 512          // Number of items: 8 bytes
.equ M_OP_TYPE, 520             // Operation type: 0=add, 1=mul, 2=square: 8 bytes
.equ M_OP_VALUE, 528            // Operand value: 8 bytes
.equ M_DIVISOR, 536             // Test divisor: 8 bytes
.equ M_TRUE_TARGET, 544         // Target monkey if true: 8 bytes
.equ M_FALSE_TARGET, 552        // Target monkey if false: 8 bytes
.equ M_INSPECTIONS, 560         // Inspection count: 8 bytes
.equ MONKEY_SIZE, 1024          // Padded size for alignment

// Macro for loading addresses from data section
.macro LOAD_ADDR reg, label
    adrp    \reg, \label@PAGE
    add     \reg, \reg, \label@PAGEOFF
.endm

// ============================================================================
// Data Section
// ============================================================================
.data

// String constants
input_path:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"
error_msg:      .asciz "Error reading file\n"

.align 3
// File buffer for input
file_buffer:    .space BUFFER_SIZE
buffer_len:     .quad 0

// Number of monkeys
num_monkeys:    .quad 0

// Product of all divisors (for Part 2 modulo)
lcm_divisors:   .quad 1

// Monkey data - primary copy
.align 4
monkeys:        .space MONKEY_SIZE * MAX_MONKEYS

// Monkey data - backup copy for Part 2
.align 4
monkeys_backup: .space MONKEY_SIZE * MAX_MONKEYS

// ============================================================================
// Code Section
// ============================================================================
.text

// ============================================================================
// Main entry point
// ============================================================================
_start:
    // Open and read input file
    LOAD_ADDR x0, input_path
    mov     x1, #0                          // O_RDONLY
    mov     x2, #0                          // mode (not used for O_RDONLY)
    mov     x16, #5                         // open() syscall
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit

    mov     x19, x0                         // Save fd in x19

    // Read file
    mov     x0, x19
    LOAD_ADDR x1, file_buffer
    mov     x2, #BUFFER_SIZE
    mov     x16, #3                         // read() syscall
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit

    // Save buffer length
    LOAD_ADDR x1, buffer_len
    str     x0, [x1]

    // Close file
    mov     x0, x19
    mov     x16, #6                         // close() syscall
    svc     #0x80

    // Parse monkeys
    bl      parse_monkeys

    // Backup monkey state for Part 2
    bl      backup_monkeys

    // Part 1: 20 rounds with relief (divide by 3)
    mov     x0, #20
    mov     x1, #3                          // relief divisor
    mov     x2, #0                          // don't use modulo
    bl      simulate

    bl      monkey_business
    mov     x19, x0                         // Save Part 1 result

    // Restore monkeys for Part 2
    bl      restore_monkeys

    // Part 2: 10000 rounds without relief
    mov     x0, #10000
    mov     x1, #1                          // no relief (divide by 1)
    mov     x2, #1                          // use modulo
    bl      simulate

    bl      monkey_business
    mov     x20, x0                         // Save Part 2 result

    // Print Part 1
    LOAD_ADDR x0, part1_msg
    bl      print_str

    mov     x0, x19
    bl      print_num

    LOAD_ADDR x0, newline
    bl      print_str

    // Print Part 2
    LOAD_ADDR x0, part2_msg
    bl      print_str

    mov     x0, x20
    bl      print_num

    LOAD_ADDR x0, newline
    bl      print_str

    // Exit
    mov     x0, #0
    mov     x16, #1                         // exit() syscall
    svc     #0x80

error_exit:
    LOAD_ADDR x0, error_msg
    bl      print_str
    mov     x0, #1
    mov     x16, #1
    svc     #0x80

// ============================================================================
// parse_monkeys: Parse input into monkey structures
// ============================================================================
parse_monkeys:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    // x19 = current position in buffer
    // x20 = end of buffer
    LOAD_ADDR x19, file_buffer
    LOAD_ADDR x0, buffer_len
    ldr     x20, [x0]
    add     x20, x19, x20

    // x21 = monkey count
    mov     x21, #0

    // x22 = monkeys base address
    LOAD_ADDR x22, monkeys

    // x23 = lcm accumulator
    mov     x23, #1

parse_monkey_loop:
    cmp     x19, x20
    b.ge    parse_done

    // Skip whitespace
    bl      skip_whitespace
    cmp     x19, x20
    b.ge    parse_done

    // Expect "Monkey N:"
    ldrb    w0, [x19]
    cmp     w0, #'M'
    b.ne    parse_done

    // Skip to end of "Monkey N:" line
    bl      skip_to_newline
    bl      skip_whitespace

    // Calculate monkey address
    mov     x24, #MONKEY_SIZE
    mul     x24, x21, x24
    add     x24, x22, x24               // x24 = address of current monkey

    // Initialize monkey
    str     xzr, [x24, #M_ITEM_COUNT]
    str     xzr, [x24, #M_INSPECTIONS]

    // Parse "Starting items: N, N, ..."
    // Skip to first digit
    bl      skip_to_digit

    // Parse items
    mov     x25, #0                     // item index
parse_items_loop:
    cmp     x19, x20
    b.ge    items_done
    ldrb    w0, [x19]
    cmp     w0, #'\n'
    b.eq    items_done
    cmp     w0, #'0'
    b.lt    skip_item_char
    cmp     w0, #'9'
    b.gt    skip_item_char

    // Parse number
    bl      parse_number               // x0 = number

    // Store item
    mov     x1, #ITEM_SIZE
    mul     x1, x25, x1
    str     x0, [x24, x1]              // Store at items[x25]
    add     x25, x25, #1
    b       parse_items_loop

skip_item_char:
    add     x19, x19, #1
    b       parse_items_loop

items_done:
    str     x25, [x24, #M_ITEM_COUNT]

    // Parse "Operation: new = old OP VALUE"
    bl      skip_to_newline
    bl      skip_whitespace

    // Find the operator (+ or *)
    bl      skip_to_operator
    ldrb    w0, [x19]

    cmp     w0, #'+'
    b.eq    op_add
    // Must be multiply
    add     x19, x19, #2               // Skip "* "
    ldrb    w0, [x19]
    cmp     w0, #'o'                    // Check for "old" (square)
    b.eq    op_square

    // Multiply by constant
    mov     x0, #1
    str     x0, [x24, #M_OP_TYPE]
    bl      parse_number
    str     x0, [x24, #M_OP_VALUE]
    b       op_done

op_add:
    mov     x0, #0
    str     x0, [x24, #M_OP_TYPE]
    add     x19, x19, #2               // Skip "+ "
    bl      parse_number
    str     x0, [x24, #M_OP_VALUE]
    b       op_done

op_square:
    mov     x0, #2
    str     x0, [x24, #M_OP_TYPE]
    str     xzr, [x24, #M_OP_VALUE]

op_done:
    // Parse "Test: divisible by N"
    bl      skip_to_newline
    bl      skip_whitespace
    bl      skip_to_digit
    bl      parse_number
    str     x0, [x24, #M_DIVISOR]

    // Multiply into LCM
    mul     x23, x23, x0

    // Parse "If true: throw to monkey N"
    bl      skip_to_newline
    bl      skip_whitespace
    bl      skip_to_digit
    bl      parse_number
    str     x0, [x24, #M_TRUE_TARGET]

    // Parse "If false: throw to monkey N"
    bl      skip_to_newline
    bl      skip_whitespace
    bl      skip_to_digit
    bl      parse_number
    str     x0, [x24, #M_FALSE_TARGET]

    // Move to next monkey
    add     x21, x21, #1
    bl      skip_to_newline
    b       parse_monkey_loop

parse_done:
    // Save monkey count
    LOAD_ADDR x0, num_monkeys
    str     x21, [x0]

    // Save LCM
    LOAD_ADDR x0, lcm_divisors
    str     x23, [x0]

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// Helper: skip_whitespace
// ============================================================================
skip_whitespace:
1:  cmp     x19, x20
    b.ge    2f
    ldrb    w0, [x19]
    cmp     w0, #' '
    b.eq    3f
    cmp     w0, #'\t'
    b.eq    3f
    cmp     w0, #'\n'
    b.eq    3f
    cmp     w0, #'\r'
    b.eq    3f
2:  ret
3:  add     x19, x19, #1
    b       1b

// ============================================================================
// Helper: skip_to_newline
// ============================================================================
skip_to_newline:
1:  cmp     x19, x20
    b.ge    2f
    ldrb    w0, [x19]
    add     x19, x19, #1
    cmp     w0, #'\n'
    b.ne    1b
2:  ret

// ============================================================================
// Helper: skip_to_digit
// ============================================================================
skip_to_digit:
1:  cmp     x19, x20
    b.ge    2f
    ldrb    w0, [x19]
    cmp     w0, #'0'
    b.lt    3f
    cmp     w0, #'9'
    b.le    2f
3:  add     x19, x19, #1
    b       1b
2:  ret

// ============================================================================
// Helper: skip_to_operator (+ or *)
// ============================================================================
skip_to_operator:
1:  cmp     x19, x20
    b.ge    2f
    ldrb    w0, [x19]
    cmp     w0, #'+'
    b.eq    2f
    cmp     w0, #'*'
    b.eq    2f
    add     x19, x19, #1
    b       1b
2:  ret

// ============================================================================
// Helper: parse_number (returns in x0)
// Modifies: x19 (advances past number)
// ============================================================================
parse_number:
    mov     x0, #0
1:  cmp     x19, x20
    b.ge    2f
    ldrb    w1, [x19]
    cmp     w1, #'0'
    b.lt    2f
    cmp     w1, #'9'
    b.gt    2f
    sub     w1, w1, #'0'
    and     x1, x1, #0xFF               // Zero extend
    mov     x2, #10
    mul     x0, x0, x2
    add     x0, x0, x1
    add     x19, x19, #1
    b       1b
2:  ret

// ============================================================================
// backup_monkeys: Copy monkey state to backup
// ============================================================================
backup_monkeys:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x19, monkeys
    LOAD_ADDR x20, monkeys_backup
    LOAD_ADDR x0, num_monkeys
    ldr     x21, [x0]
    mov     x22, #MONKEY_SIZE
    mul     x21, x21, x22               // Total bytes to copy

    mov     x0, #0
1:  cmp     x0, x21
    b.ge    2f
    ldr     x1, [x19, x0]
    str     x1, [x20, x0]
    add     x0, x0, #8
    b       1b

2:  ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// restore_monkeys: Restore monkey state from backup
// ============================================================================
restore_monkeys:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x19, monkeys
    LOAD_ADDR x20, monkeys_backup
    LOAD_ADDR x0, num_monkeys
    ldr     x21, [x0]
    mov     x22, #MONKEY_SIZE
    mul     x21, x21, x22               // Total bytes to copy

    mov     x0, #0
1:  cmp     x0, x21
    b.ge    2f
    ldr     x1, [x20, x0]
    str     x1, [x19, x0]
    add     x0, x0, #8
    b       1b

2:  ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// simulate: Run simulation
// x0 = number of rounds
// x1 = relief divisor (3 for Part 1, 1 for Part 2)
// x2 = use_modulo (0 = no, 1 = yes)
// ============================================================================
simulate:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0                     // rounds
    mov     x20, x1                     // relief divisor
    mov     x21, x2                     // use_modulo flag

    LOAD_ADDR x22, monkeys
    LOAD_ADDR x0, num_monkeys
    ldr     x23, [x0]                   // num_monkeys

    LOAD_ADDR x0, lcm_divisors
    ldr     x24, [x0]                   // modulo value

round_loop:
    cbz     x19, simulate_done
    sub     x19, x19, #1

    // Process each monkey
    mov     x25, #0                     // monkey index
monkey_loop:
    cmp     x25, x23
    b.ge    round_loop

    // Get monkey address
    mov     x0, #MONKEY_SIZE
    mul     x0, x25, x0
    add     x26, x22, x0                // x26 = current monkey address

    // Process all items this monkey has
item_loop:
    ldr     x0, [x26, #M_ITEM_COUNT]
    cbz     x0, next_monkey

    // Get first item
    ldr     x27, [x26]                  // x27 = worry level

    // Shift remaining items down
    mov     x1, #1
    sub     x0, x0, #1
    str     x0, [x26, #M_ITEM_COUNT]
shift_items:
    cbz     x0, shift_done
    mov     x2, #ITEM_SIZE
    mul     x3, x1, x2
    ldr     x4, [x26, x3]
    sub     x3, x3, #ITEM_SIZE
    str     x4, [x26, x3]
    add     x1, x1, #1
    sub     x0, x0, #1
    b       shift_items
shift_done:

    // Increment inspection count
    ldr     x0, [x26, #M_INSPECTIONS]
    add     x0, x0, #1
    str     x0, [x26, #M_INSPECTIONS]

    // Apply operation
    ldr     x0, [x26, #M_OP_TYPE]
    ldr     x1, [x26, #M_OP_VALUE]

    cmp     x0, #0
    b.eq    do_add
    cmp     x0, #1
    b.eq    do_mul
    // Square
    mul     x27, x27, x27
    b       op_applied

do_add:
    add     x27, x27, x1
    b       op_applied

do_mul:
    mul     x27, x27, x1

op_applied:
    // Apply relief (divide by relief_divisor)
    udiv    x27, x27, x20

    // Apply modulo if needed
    cbz     x21, no_modulo
    udiv    x0, x27, x24
    msub    x27, x0, x24, x27           // x27 = x27 % lcm
no_modulo:

    // Test divisibility
    ldr     x0, [x26, #M_DIVISOR]
    udiv    x1, x27, x0
    msub    x1, x1, x0, x27             // x1 = x27 % divisor

    // Get target monkey
    cbnz    x1, test_false
    ldr     x28, [x26, #M_TRUE_TARGET]
    b       throw_item
test_false:
    ldr     x28, [x26, #M_FALSE_TARGET]

throw_item:
    // Get target monkey address
    mov     x0, #MONKEY_SIZE
    mul     x0, x28, x0
    add     x0, x22, x0                 // x0 = target monkey address

    // Add item to target monkey's queue
    ldr     x1, [x0, #M_ITEM_COUNT]
    mov     x2, #ITEM_SIZE
    mul     x2, x1, x2
    str     x27, [x0, x2]               // Store item
    add     x1, x1, #1
    str     x1, [x0, #M_ITEM_COUNT]

    b       item_loop

next_monkey:
    add     x25, x25, #1
    b       monkey_loop

simulate_done:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// monkey_business: Find product of top 2 inspection counts
// Returns: x0 = result
// ============================================================================
monkey_business:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x19, monkeys
    LOAD_ADDR x0, num_monkeys
    ldr     x20, [x0]                   // num_monkeys

    mov     x21, #0                     // max1
    mov     x22, #0                     // max2

    mov     x0, #0                      // index
find_max_loop:
    cmp     x0, x20
    b.ge    find_max_done

    mov     x1, #MONKEY_SIZE
    mul     x1, x0, x1
    add     x1, x19, x1
    ldr     x1, [x1, #M_INSPECTIONS]

    // Update top 2
    cmp     x1, x21
    b.le    check_second
    mov     x22, x21                    // max2 = max1
    mov     x21, x1                     // max1 = new value
    b       next_max

check_second:
    cmp     x1, x22
    b.le    next_max
    mov     x22, x1

next_max:
    add     x0, x0, #1
    b       find_max_loop

find_max_done:
    mul     x0, x21, x22

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print a null-terminated string
// Input: x0 = address of string
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

2:  // Write to stdout
    mov     x0, #1
    mov     x1, x19
    mov     x2, x20
    mov     x16, #4
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_num: Print a 64-bit unsigned number
// Input: x0 = number to print
// ============================================================================
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    sub     sp, sp, #32

    mov     x19, x0
    add     x20, sp, #31
    strb    wzr, [x20]

    // Handle zero case
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
