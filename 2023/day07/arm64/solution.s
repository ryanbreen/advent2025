// ============================================================================
// Advent of Code 2023 - Day 7: Camel Cards
// ARM64 Assembly Solution for macOS
// ============================================================================
//
// ALGORITHM:
// Part 1: Rank poker-like hands by type, then by card values (A>K>Q>J>T>9>...>2)
// Part 2: J becomes wildcard for type, but weakest for tiebreakers
//
// Hand types (0=high card to 6=five of a kind):
//   [5]       = 6 (five of a kind)
//   [4,1]     = 5 (four of a kind)
//   [3,2]     = 4 (full house)
//   [3,1,1]   = 3 (three of a kind)
//   [2,2,1]   = 2 (two pair)
//   [2,1,1,1] = 1 (one pair)
//   [1,1,1,1,1] = 0 (high card)
//
// ============================================================================

.global _start
.align 4

// ============================================================================
// DATA SECTION
// ============================================================================
.data
    .align 4
    filename:   .asciz "../input.txt"
    msg_part1:  .asciz "Part 1: "
    msg_part2:  .asciz "Part 2: "
    newline:    .asciz "\n"

    .align 4
    buffer:     .space 16384          // Input file buffer

    .align 4
    num_buffer: .space 32             // Number printing buffer

    // Card strength lookup tables (indexed by ASCII - '2')
    // Part 1: 23456789TJQKA -> 0,1,2,3,4,5,6,7,8,9,10,11,12
    // ASCII: '2'=50, '3'=51, ..., '9'=57, ':'=58, ..., 'A'=65, ..., 'J'=74, ..., 'K'=75, ..., 'Q'=81, 'T'=84
    // We'll use direct mapping

    .align 4
    // Part 1 card values: A=12, K=11, Q=10, J=9, T=8, 9=7, 8=6, 7=5, 6=4, 5=3, 4=2, 3=1, 2=0
    card_value_p1:
        .byte 0, 1, 2, 3, 4, 5, 6, 7   // 2-9 at indices 0-7 (ASCII 50-57 minus 50)
        .byte 0, 0, 0, 0, 0, 0, 0      // : ; < = > ? @  (indices 8-14)
        .byte 12                       // A at index 15 (65-50)
        .byte 0, 0, 0, 0, 0, 0, 0, 0   // B-I (indices 16-23)
        .byte 9                        // J at index 24 (74-50)
        .byte 11                       // K at index 25 (75-50)
        .byte 0, 0, 0, 0, 0            // L, M, N, O, P (indices 26-30)
        .byte 10                       // Q at index 31 (81-50)
        .byte 0, 0                     // R, S (indices 32-33)
        .byte 8                        // T at index 34 (84-50)
        .space 20

    .align 4
    // Part 2 card values: A=12, K=11, Q=10, T=9, 9=8, 8=7, 7=6, 6=5, 5=4, 4=3, 3=2, 2=1, J=0
    card_value_p2:
        .byte 1, 2, 3, 4, 5, 6, 7, 8   // 2-9 at indices 0-7 (values 1-8)
        .byte 0, 0, 0, 0, 0, 0, 0      // : ; < = > ? @
        .byte 12                       // A at index 15
        .byte 0, 0, 0, 0, 0, 0, 0, 0   // B-I
        .byte 0                        // J at index 24 (weakest = 0)
        .byte 11                       // K at index 25
        .byte 0, 0, 0, 0, 0            // L, M, N, O, P
        .byte 10                       // Q at index 31
        .byte 0, 0                     // R, S
        .byte 9                        // T at index 34 (value 9)
        .space 20

    .align 4
    // Storage for hands: each hand is 16 bytes
    // Bytes 0-4: card characters
    // Byte 5: hand type (for sorting)
    // Bytes 6-7: padding
    // Bytes 8-15: bid (64-bit)
    hands:      .space 16384          // 1024 hands * 16 bytes
    hand_count: .quad 0

    // Temporary card count array (for calculating hand type)
    .align 4
    card_counts: .space 16            // 13 cards max, with padding

    // Sorted counts array
    .align 4
    sorted_counts: .space 8

// ============================================================================
// TEXT SECTION
// ============================================================================
.text

_start:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    bl      read_file
    cbz     x0, exit_error

    // Part 1
    mov     x0, #0                    // part = 0 (Part 1)
    bl      parse_input

    adrp    x0, msg_part1@PAGE
    add     x0, x0, msg_part1@PAGEOFF
    bl      print_str

    mov     x0, #0                    // part = 0
    bl      solve
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    // Part 2
    mov     x0, #1                    // part = 1 (Part 2)
    bl      parse_input

    adrp    x0, msg_part2@PAGE
    add     x0, x0, msg_part2@PAGEOFF
    bl      print_str

    mov     x0, #1                    // part = 1
    bl      solve
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    mov     x0, #0
    ldp     x29, x30, [sp], #16
    mov     x16, #1
    svc     #0x80

exit_error:
    mov     x0, #1
    ldp     x29, x30, [sp], #16
    mov     x16, #1
    svc     #0x80

// ============================================================================
// FILE I/O
// ============================================================================
read_file:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    adrp    x0, filename@PAGE
    add     x0, x0, filename@PAGEOFF
    mov     x1, #0                    // O_RDONLY
    mov     x16, #5                   // open
    svc     #0x80
    cmp     x0, #0
    b.lt    read_error
    mov     x19, x0                   // fd

    mov     x0, x19
    adrp    x1, buffer@PAGE
    add     x1, x1, buffer@PAGEOFF
    mov     x2, #16384
    mov     x16, #3                   // read
    svc     #0x80
    mov     x20, x0                   // bytes read

    mov     x0, x19
    mov     x16, #6                   // close
    svc     #0x80

    mov     x0, x20
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

read_error:
    mov     x0, #0
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// PARSING
// ============================================================================

// parse_number: parse integer from string
// Input: x0 = string pointer
// Output: x0 = parsed number, x1 = updated pointer
parse_number:
    mov     x3, x0
    mov     x0, #0
1:  ldrb    w4, [x3]
    cmp     w4, #'0'
    b.lt    2f
    cmp     w4, #'9'
    b.gt    2f
    mov     x5, #10
    mul     x0, x0, x5
    sub     w4, w4, #'0'
    add     x0, x0, x4
    add     x3, x3, #1
    b       1b
2:  mov     x1, x3
    ret

// get_hand_type: Calculate hand type for 5 cards
// Input: x0 = pointer to 5 card bytes, x1 = part (0=P1, 1=P2 with jokers)
// Output: x0 = hand type (0-6)
get_hand_type:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    mov     x19, x0                   // cards pointer
    mov     x20, x1                   // part

    // Clear card counts (13 slots, but we use 16 for alignment)
    adrp    x21, card_counts@PAGE
    add     x21, x21, card_counts@PAGEOFF
    str     xzr, [x21]
    str     xzr, [x21, #8]

    mov     x22, #0                   // joker count

    // Count each card
    mov     x23, #0                   // i = 0
count_cards_loop:
    cmp     x23, #5
    b.ge    count_cards_done

    ldrb    w24, [x19, x23]           // card char

    // Check if it's a joker in Part 2
    cmp     x20, #1
    b.ne    not_joker_check
    cmp     w24, #'J'
    b.ne    not_joker_check
    add     x22, x22, #1              // joker_count++
    b       next_card

not_joker_check:
    // Map card to index 0-12
    // 2=0, 3=1, 4=2, 5=3, 6=4, 7=5, 8=6, 9=7, T=8, J=9, Q=10, K=11, A=12
    cmp     w24, #'A'
    b.eq    card_A
    cmp     w24, #'K'
    b.eq    card_K
    cmp     w24, #'Q'
    b.eq    card_Q
    cmp     w24, #'J'
    b.eq    card_J
    cmp     w24, #'T'
    b.eq    card_T
    // Digit 2-9
    sub     w24, w24, #'2'
    b       increment_count

card_A:
    mov     w24, #12
    b       increment_count
card_K:
    mov     w24, #11
    b       increment_count
card_Q:
    mov     w24, #10
    b       increment_count
card_J:
    mov     w24, #9
    b       increment_count
card_T:
    mov     w24, #8
    b       increment_count

increment_count:
    ldrb    w0, [x21, x24]
    add     w0, w0, #1
    strb    w0, [x21, x24]

next_card:
    add     x23, x23, #1
    b       count_cards_loop

count_cards_done:
    // If all 5 are jokers (Part 2), return five of a kind
    cmp     x22, #5
    b.ne    not_all_jokers
    mov     x0, #6
    b       get_hand_type_done

not_all_jokers:
    // Sort counts in descending order
    // First, collect non-zero counts
    adrp    x23, sorted_counts@PAGE
    add     x23, x23, sorted_counts@PAGEOFF
    str     xzr, [x23]

    mov     x0, #0                    // count of non-zero entries
    mov     x1, #0                    // i
collect_counts:
    cmp     x1, #13
    b.ge    counts_collected
    ldrb    w2, [x21, x1]
    cbz     w2, skip_zero_count
    strb    w2, [x23, x0]
    add     x0, x0, #1
skip_zero_count:
    add     x1, x1, #1
    b       collect_counts

counts_collected:
    mov     x24, x0                   // num_counts

    // Bubble sort counts descending
    cmp     x24, #2
    b.lt    sort_done

    mov     x0, #0                    // i
outer_sort:
    cmp     x0, x24
    b.ge    sort_done
    add     x1, x0, #1                // j = i+1
inner_sort:
    cmp     x1, x24
    b.ge    next_outer
    ldrb    w2, [x23, x0]             // counts[i]
    ldrb    w3, [x23, x1]             // counts[j]
    cmp     w2, w3
    b.ge    no_swap
    // swap
    strb    w3, [x23, x0]
    strb    w2, [x23, x1]
no_swap:
    add     x1, x1, #1
    b       inner_sort
next_outer:
    add     x0, x0, #1
    b       outer_sort

sort_done:
    // Add jokers to highest count (Part 2)
    cbz     x22, no_jokers_to_add
    ldrb    w0, [x23]
    add     w0, w0, w22
    strb    w0, [x23]

no_jokers_to_add:
    // Determine hand type from sorted counts
    ldrb    w0, [x23]                 // highest count
    ldrb    w1, [x23, #1]             // second highest (or 0)

    cmp     w0, #5
    b.eq    type_five

    cmp     w0, #4
    b.eq    type_four

    cmp     w0, #3
    b.ne    check_two
    cmp     w1, #2
    b.eq    type_full_house
    b       type_three

check_two:
    cmp     w0, #2
    b.ne    type_high_card
    cmp     w1, #2
    b.eq    type_two_pair
    b       type_one_pair

type_five:
    mov     x0, #6
    b       get_hand_type_done
type_four:
    mov     x0, #5
    b       get_hand_type_done
type_full_house:
    mov     x0, #4
    b       get_hand_type_done
type_three:
    mov     x0, #3
    b       get_hand_type_done
type_two_pair:
    mov     x0, #2
    b       get_hand_type_done
type_one_pair:
    mov     x0, #1
    b       get_hand_type_done
type_high_card:
    mov     x0, #0

get_hand_type_done:
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// parse_input: Parse all hands from buffer
// Input: x0 = part (0 or 1)
parse_input:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    mov     x24, x0                   // part

    adrp    x19, buffer@PAGE
    add     x19, x19, buffer@PAGEOFF

    adrp    x20, hands@PAGE
    add     x20, x20, hands@PAGEOFF

    mov     x21, #0                   // hand count

parse_loop:
    ldrb    w0, [x19]
    cbz     w0, parse_done
    cmp     w0, #'\n'
    b.eq    skip_newline_only

    // Check if we have a valid card character
    cmp     w0, #'2'
    b.lt    parse_done
    cmp     w0, #'Z'
    b.gt    parse_done

    // Copy 5 cards to hand structure
    lsl     x22, x21, #4              // offset = hand_count * 16
    add     x23, x20, x22             // hand pointer

    ldrb    w0, [x19]
    strb    w0, [x23, #0]
    ldrb    w0, [x19, #1]
    strb    w0, [x23, #1]
    ldrb    w0, [x19, #2]
    strb    w0, [x23, #2]
    ldrb    w0, [x19, #3]
    strb    w0, [x23, #3]
    ldrb    w0, [x19, #4]
    strb    w0, [x23, #4]

    // Calculate and store hand type
    mov     x0, x23                   // cards pointer
    mov     x1, x24                   // part
    bl      get_hand_type
    strb    w0, [x23, #5]             // store type

    // Skip past cards and space
    add     x19, x19, #6              // 5 cards + 1 space

    // Parse bid
    mov     x0, x19
    bl      parse_number
    mov     x19, x1

    // Store bid at offset 8
    str     x0, [x23, #8]

    add     x21, x21, #1

    // Skip to next line
skip_to_eol:
    ldrb    w0, [x19]
    cbz     w0, parse_done
    cmp     w0, #'\n'
    b.eq    found_newline
    add     x19, x19, #1
    b       skip_to_eol

found_newline:
    add     x19, x19, #1
    b       parse_loop

skip_newline_only:
    add     x19, x19, #1
    b       parse_loop

parse_done:
    adrp    x0, hand_count@PAGE
    add     x0, x0, hand_count@PAGEOFF
    str     x21, [x0]

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// SORTING
// ============================================================================

// compare_hands: Compare two hands for sorting
// Input: x0 = hand1 ptr, x1 = hand2 ptr, x2 = part (0 or 1)
// Output: x0 = -1 if hand1 < hand2, 0 if equal, 1 if hand1 > hand2
compare_hands:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0                   // hand1
    mov     x20, x1                   // hand2
    mov     x21, x2                   // part

    // Compare types first
    ldrb    w0, [x19, #5]             // type1
    ldrb    w1, [x20, #5]             // type2
    cmp     w0, w1
    b.lt    hand1_less
    b.gt    hand1_greater

    // Types equal, compare card by card
    // Get card value lookup table
    cmp     x21, #0
    b.eq    use_p1_table
    adrp    x22, card_value_p2@PAGE
    add     x22, x22, card_value_p2@PAGEOFF
    b       compare_cards
use_p1_table:
    adrp    x22, card_value_p1@PAGE
    add     x22, x22, card_value_p1@PAGEOFF

compare_cards:
    mov     x0, #0                    // i
compare_card_loop:
    cmp     x0, #5
    b.ge    hands_equal

    ldrb    w1, [x19, x0]             // card1 char
    ldrb    w2, [x20, x0]             // card2 char

    sub     w1, w1, #'2'              // index into table
    sub     w2, w2, #'2'

    ldrb    w1, [x22, x1]             // value1
    ldrb    w2, [x22, x2]             // value2

    cmp     w1, w2
    b.lt    hand1_less
    b.gt    hand1_greater

    add     x0, x0, #1
    b       compare_card_loop

hands_equal:
    mov     x0, #0
    b       compare_done

hand1_less:
    mov     x0, #-1
    b       compare_done

hand1_greater:
    mov     x0, #1

compare_done:
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// sort_hands: Sort hands array using insertion sort
// Input: x0 = part (0 or 1)
sort_hands:
    stp     x29, x30, [sp, #-64]!
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    mov     x24, x0                   // part

    adrp    x19, hands@PAGE
    add     x19, x19, hands@PAGEOFF

    adrp    x0, hand_count@PAGE
    add     x0, x0, hand_count@PAGEOFF
    ldr     x20, [x0]                 // n

    cmp     x20, #2
    b.lt    sort_done_ret

    // Insertion sort
    mov     x21, #1                   // i = 1
outer_loop:
    cmp     x21, x20
    b.ge    sort_done_ret

    // Save hands[i] to temp (on stack)
    lsl     x0, x21, #4
    add     x0, x19, x0               // hands[i]

    // Copy 16 bytes to temp storage at [sp, #-16]
    sub     sp, sp, #16
    ldr     x1, [x0]
    str     x1, [sp]
    ldr     x1, [x0, #8]
    str     x1, [sp, #8]

    mov     x22, x21                  // j = i

inner_loop:
    cbz     x22, insert_here

    // Get hands[j-1]
    sub     x0, x22, #1
    lsl     x0, x0, #4
    add     x0, x19, x0               // hands[j-1]

    // Compare hands[j-1] with temp
    mov     x1, sp                    // temp
    mov     x2, x24                   // part
    bl      compare_hands

    cmp     x0, #1                    // if hands[j-1] > temp
    b.ne    insert_here

    // Shift hands[j-1] to hands[j]
    sub     x0, x22, #1
    lsl     x0, x0, #4
    add     x0, x19, x0               // src = hands[j-1]

    lsl     x1, x22, #4
    add     x1, x19, x1               // dst = hands[j]

    ldr     x2, [x0]
    str     x2, [x1]
    ldr     x2, [x0, #8]
    str     x2, [x1, #8]

    sub     x22, x22, #1
    b       inner_loop

insert_here:
    // Insert temp into hands[j]
    lsl     x0, x22, #4
    add     x0, x19, x0

    ldr     x1, [sp]
    str     x1, [x0]
    ldr     x1, [sp, #8]
    str     x1, [x0, #8]

    add     sp, sp, #16

    add     x21, x21, #1
    b       outer_loop

sort_done_ret:
    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x23, x24, [sp, #48]
    ldp     x29, x30, [sp], #64
    ret

// ============================================================================
// SOLVE
// ============================================================================
// solve: Sort hands and calculate total winnings
// Input: x0 = part (0 or 1)
// Output: x0 = total winnings
solve:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0                   // part

    // Sort hands
    mov     x0, x19
    bl      sort_hands

    // Calculate total winnings
    adrp    x20, hands@PAGE
    add     x20, x20, hands@PAGEOFF

    adrp    x0, hand_count@PAGE
    add     x0, x0, hand_count@PAGEOFF
    ldr     x21, [x0]                 // n

    mov     x22, #0                   // total = 0
    mov     x0, #0                    // i = 0

calc_loop:
    cmp     x0, x21
    b.ge    calc_done

    // rank = i + 1
    add     x1, x0, #1

    // bid = hands[i].bid
    lsl     x2, x0, #4
    add     x2, x20, x2
    ldr     x3, [x2, #8]              // bid

    // total += rank * bid
    mul     x3, x1, x3
    add     x22, x22, x3

    add     x0, x0, #1
    b       calc_loop

calc_done:
    mov     x0, x22

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// OUTPUT
// ============================================================================
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    mov     x19, x0

    mov     x1, #0
1:  ldrb    w2, [x19, x1]
    cbz     w2, 2f
    add     x1, x1, #1
    b       1b

2:  mov     x2, x1
    mov     x1, x19
    mov     x0, #1
    mov     x16, #4                   // write
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0
    adrp    x20, num_buffer@PAGE
    add     x20, x20, num_buffer@PAGEOFF
    add     x20, x20, #30
    strb    wzr, [x20]

    cbz     x19, print_zero

    mov     x2, #10
1:  sub     x20, x20, #1
    udiv    x3, x19, x2
    msub    x4, x3, x2, x19
    add     w4, w4, #'0'
    strb    w4, [x20]
    mov     x19, x3
    cbnz    x19, 1b
    b       print_done

print_zero:
    sub     x20, x20, #1
    mov     w4, #'0'
    strb    w4, [x20]

print_done:
    mov     x0, x20
    bl      print_str

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
