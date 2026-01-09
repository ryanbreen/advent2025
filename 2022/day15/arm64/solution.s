// =============================================================================
// Advent of Code 2022 - Day 15: Beacon Exclusion Zone
// ARM64 Assembly Solution for macOS
//
// Problem: Sensor coverage using Manhattan distance
// Part 1: Count positions in row y=2000000 that cannot contain a beacon
// Part 2: Find the one uncovered position in 0-4000000 range, return x*4000000+y
//
// Algorithm: For each row, compute coverage ranges from each sensor and merge
// overlapping ranges.
// =============================================================================

.global _main
.align 4

// Constants
.equ BUFFER_SIZE, 32768         // Input file buffer
.equ MAX_SENSORS, 64            // Maximum number of sensors
.equ MAX_RANGES, 128            // Maximum ranges for merging
.equ TARGET_ROW, 2000000        // Part 1 target row
.equ MAX_COORD, 4000000         // Part 2 search limit
.equ TUNING_MULT, 4000000       // Tuning frequency multiplier

// Sensor structure: 5 x 8 bytes = 40 bytes per sensor
// [sx, sy, bx, by, dist]
.equ SENSOR_SIZE, 40

// Range structure: 2 x 8 bytes = 16 bytes per range
// [start, end]
.equ RANGE_SIZE, 16

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

.align 3
file_buffer:    .space BUFFER_SIZE
buffer_len:     .quad 0
buffer_pos:     .quad 0

// Sensors array: each sensor is (sx, sy, bx, by, dist) - all signed 64-bit
.align 3
sensors:        .space (MAX_SENSORS * SENSOR_SIZE)
sensor_count:   .quad 0

// Ranges array for merging
.align 3
ranges:         .space (MAX_RANGES * RANGE_SIZE)
range_count:    .quad 0

// Temporary storage for sorting
.align 3
merged_ranges:  .space (MAX_RANGES * RANGE_SIZE)

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

    // Parse sensors
    bl      parse_sensors

    // Part 1
    bl      part1
    mov     x19, x0                         // Save part1 result

    // Print Part 1
    LOAD_ADDR x0, part1_msg
    bl      print_str
    mov     x0, x19
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Part 2
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
    mov     x0, #1
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// parse_sensors: Parse input and populate sensors array
// ============================================================================
parse_sensors:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    // Reset buffer position and sensor count
    LOAD_ADDR x19, buffer_pos
    str     xzr, [x19]
    LOAD_ADDR x20, sensor_count
    str     xzr, [x20]

    LOAD_ADDR x21, file_buffer
    LOAD_ADDR x22, buffer_len
    ldr     x22, [x22]

    LOAD_ADDR x23, sensors              // Current sensor pointer
    mov     x24, #0                     // Sensor count

parse_line_loop:
    // Get current position
    ldr     x0, [x19]
    cmp     x0, x22
    b.ge    parse_done

    // Skip to 'x=' in "Sensor at x="
    bl      skip_to_number
    ldr     x0, [x19]
    cmp     x0, x22
    b.ge    parse_done

    // Parse sensor x
    bl      parse_signed_number
    str     x0, [x23, #0]               // sx

    // Skip to next number (sensor y)
    bl      skip_to_number
    bl      parse_signed_number
    str     x0, [x23, #8]               // sy

    // Skip to beacon x
    bl      skip_to_number
    bl      parse_signed_number
    str     x0, [x23, #16]              // bx

    // Skip to beacon y
    bl      skip_to_number
    bl      parse_signed_number
    str     x0, [x23, #24]              // by

    // Calculate Manhattan distance
    ldr     x0, [x23, #0]               // sx
    ldr     x1, [x23, #16]              // bx
    subs    x2, x0, x1
    b.ge    1f
    neg     x2, x2                      // abs(sx - bx)
1:
    ldr     x0, [x23, #8]               // sy
    ldr     x1, [x23, #24]              // by
    subs    x3, x0, x1
    b.ge    2f
    neg     x3, x3                      // abs(sy - by)
2:
    add     x2, x2, x3                  // dist = |sx-bx| + |sy-by|
    str     x2, [x23, #32]              // dist

    // Move to next sensor
    add     x23, x23, #SENSOR_SIZE
    add     x24, x24, #1

    // Skip to next line
    bl      skip_to_eol

    b       parse_line_loop

parse_done:
    // Store sensor count
    LOAD_ADDR x0, sensor_count
    str     x24, [x0]

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// skip_to_number: Skip until we find a digit or minus sign
// ============================================================================
skip_to_number:
    stp     x29, x30, [sp, #-16]!

    LOAD_ADDR x0, buffer_pos
    ldr     x1, [x0]
    LOAD_ADDR x2, buffer_len
    ldr     x3, [x2]
    LOAD_ADDR x4, file_buffer

1:  cmp     x1, x3
    b.ge    2f
    ldrb    w5, [x4, x1]
    cmp     w5, #'-'
    b.eq    2f
    cmp     w5, #'0'
    b.lt    3f
    cmp     w5, #'9'
    b.le    2f
3:  add     x1, x1, #1
    b       1b

2:  LOAD_ADDR x0, buffer_pos
    str     x1, [x0]

    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// parse_signed_number: Parse a signed decimal number
// Output: x0 = parsed number (signed)
// ============================================================================
parse_signed_number:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x19, buffer_pos
    ldr     x0, [x19]
    LOAD_ADDR x1, file_buffer
    LOAD_ADDR x2, buffer_len
    ldr     x2, [x2]

    mov     x20, #0                     // Result
    mov     x21, #0                     // Negative flag

    // Check for minus sign
    cmp     x0, x2
    b.ge    parse_signed_done
    ldrb    w3, [x1, x0]
    cmp     w3, #'-'
    b.ne    parse_digits
    mov     x21, #1
    add     x0, x0, #1

parse_digits:
    cmp     x0, x2
    b.ge    parse_signed_done
    ldrb    w3, [x1, x0]
    cmp     w3, #'0'
    b.lt    parse_signed_done
    cmp     w3, #'9'
    b.gt    parse_signed_done

    sub     w3, w3, #'0'
    and     x3, x3, #0xFF
    mov     x4, #10
    mul     x20, x20, x4
    add     x20, x20, x3
    add     x0, x0, #1
    b       parse_digits

parse_signed_done:
    str     x0, [x19]

    // Apply negative if needed
    cbz     x21, 1f
    neg     x20, x20
1:
    mov     x0, x20

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// skip_to_eol: Skip to end of line (past newline)
// ============================================================================
skip_to_eol:
    stp     x29, x30, [sp, #-16]!

    LOAD_ADDR x0, buffer_pos
    ldr     x1, [x0]
    LOAD_ADDR x2, buffer_len
    ldr     x3, [x2]
    LOAD_ADDR x4, file_buffer

1:  cmp     x1, x3
    b.ge    2f
    ldrb    w5, [x4, x1]
    add     x1, x1, #1
    cmp     w5, #'\n'
    b.ne    1b

2:  LOAD_ADDR x0, buffer_pos
    str     x1, [x0]

    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// get_coverage_at_row: Get coverage ranges for a specific row
// Input: x0 = row number
// Uses ranges array, sets range_count
// ============================================================================
get_coverage_at_row:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    mov     x19, x0                     // row

    LOAD_ADDR x20, sensors
    LOAD_ADDR x21, sensor_count
    ldr     x21, [x21]

    LOAD_ADDR x22, ranges
    mov     x23, #0                     // range count

    mov     x24, #0                     // sensor index

coverage_loop:
    cmp     x24, x21
    b.ge    coverage_done

    // Calculate offset
    mov     x0, #SENSOR_SIZE
    mul     x0, x0, x24
    add     x25, x20, x0                // sensor ptr

    // Load sensor data
    ldr     x0, [x25, #8]               // sy
    ldr     x1, [x25, #32]              // dist

    // row_dist = abs(sy - row)
    subs    x2, x0, x19
    b.ge    1f
    neg     x2, x2
1:
    // Check if sensor reaches this row
    cmp     x2, x1
    b.gt    next_sensor

    // x_spread = dist - row_dist
    sub     x3, x1, x2

    // range = [sx - x_spread, sx + x_spread]
    ldr     x4, [x25, #0]               // sx
    sub     x5, x4, x3                  // start
    add     x6, x4, x3                  // end

    // Store range
    mov     x0, #RANGE_SIZE
    mul     x0, x0, x23
    add     x0, x22, x0
    str     x5, [x0, #0]                // start
    str     x6, [x0, #8]                // end
    add     x23, x23, #1

next_sensor:
    add     x24, x24, #1
    b       coverage_loop

coverage_done:
    // Store range count
    LOAD_ADDR x0, range_count
    str     x23, [x0]

    // Merge overlapping ranges
    bl      merge_ranges

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// merge_ranges: Sort and merge overlapping ranges in-place
// Uses range_count, modifies ranges array
// ============================================================================
merge_ranges:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    LOAD_ADDR x19, range_count
    ldr     x20, [x19]

    // Need at least 2 ranges to merge
    cmp     x20, #2
    b.lt    merge_done

    LOAD_ADDR x21, ranges

    // Simple bubble sort by start value
    mov     x22, x20
sort_outer:
    sub     x22, x22, #1
    cbz     x22, sort_done

    mov     x23, #0
sort_inner:
    cmp     x23, x22
    b.ge    sort_outer

    // Compare ranges[i] and ranges[i+1]
    mov     x0, #RANGE_SIZE
    mul     x24, x0, x23
    add     x24, x21, x24               // &ranges[i]
    add     x25, x24, #RANGE_SIZE       // &ranges[i+1]

    ldr     x26, [x24, #0]              // ranges[i].start
    ldr     x27, [x25, #0]              // ranges[i+1].start

    cmp     x26, x27
    b.le    no_swap

    // Swap
    ldr     x0, [x24, #0]
    ldr     x1, [x24, #8]
    ldr     x2, [x25, #0]
    ldr     x3, [x25, #8]
    str     x2, [x24, #0]
    str     x3, [x24, #8]
    str     x0, [x25, #0]
    str     x1, [x25, #8]

no_swap:
    add     x23, x23, #1
    b       sort_inner

sort_done:
    // Now merge overlapping ranges
    // merged[0] = ranges[0]
    LOAD_ADDR x22, merged_ranges
    ldr     x0, [x21, #0]
    str     x0, [x22, #0]
    ldr     x0, [x21, #8]
    str     x0, [x22, #8]

    mov     x23, #1                     // merged count
    mov     x24, #1                     // input index

merge_loop:
    cmp     x24, x20
    b.ge    merge_copy_back

    // Get current input range
    mov     x0, #RANGE_SIZE
    mul     x0, x0, x24
    add     x25, x21, x0
    ldr     x26, [x25, #0]              // start
    ldr     x27, [x25, #8]              // end

    // Get last merged range
    sub     x0, x23, #1
    mov     x1, #RANGE_SIZE
    mul     x0, x0, x1
    add     x28, x22, x0
    ldr     x0, [x28, #0]               // merged.start
    ldr     x1, [x28, #8]               // merged.end

    // Check if overlapping: start <= merged.end + 1
    add     x2, x1, #1
    cmp     x26, x2
    b.gt    add_new_range

    // Overlapping: extend merged range
    cmp     x27, x1
    csel    x27, x27, x1, gt
    str     x27, [x28, #8]
    add     x24, x24, #1
    b       merge_loop

add_new_range:
    // Add new merged range
    mov     x0, #RANGE_SIZE
    mul     x0, x0, x23
    add     x0, x22, x0
    str     x26, [x0, #0]
    str     x27, [x0, #8]
    add     x23, x23, #1
    add     x24, x24, #1
    b       merge_loop

merge_copy_back:
    // Copy merged ranges back to ranges array
    LOAD_ADDR x19, range_count
    str     x23, [x19]

    mov     x24, #0
copy_back_loop:
    cmp     x24, x23
    b.ge    merge_done

    mov     x0, #RANGE_SIZE
    mul     x0, x0, x24
    add     x25, x22, x0
    add     x26, x21, x0

    ldr     x0, [x25, #0]
    str     x0, [x26, #0]
    ldr     x0, [x25, #8]
    str     x0, [x26, #8]

    add     x24, x24, #1
    b       copy_back_loop

merge_done:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// part1: Count positions that cannot contain a beacon at row y=2000000
// Output: x0 = count
// ============================================================================
part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    // Get coverage at target row
    mov     w0, #(TARGET_ROW & 0xFFFF)
    movk    w0, #(TARGET_ROW >> 16), lsl #16
    sxtw    x0, w0
    bl      get_coverage_at_row

    // Sum up range lengths
    LOAD_ADDR x19, ranges
    LOAD_ADDR x20, range_count
    ldr     x20, [x20]

    mov     x21, #0                     // total count
    mov     x22, #0                     // index

sum_ranges_loop:
    cmp     x22, x20
    b.ge    sum_ranges_done

    mov     x0, #RANGE_SIZE
    mul     x0, x0, x22
    add     x0, x19, x0
    ldr     x23, [x0, #0]               // start
    ldr     x24, [x0, #8]               // end

    sub     x25, x24, x23
    add     x25, x25, #1                // length = end - start + 1
    add     x21, x21, x25

    add     x22, x22, #1
    b       sum_ranges_loop

sum_ranges_done:
    // Subtract beacons that are on this row
    LOAD_ADDR x19, sensors
    LOAD_ADDR x20, sensor_count
    ldr     x20, [x20]

    // Count unique beacons on target row
    // Simple approach: check each beacon, only count if not already counted
    mov     x22, #0                     // sensor index
    mov     w0, #(TARGET_ROW & 0xFFFF)
    movk    w0, #(TARGET_ROW >> 16), lsl #16
    sxtw    x25, w0                     // target row

    // For simplicity, use a small array to track unique beacon x positions
    // Max 64 beacons on one row should be plenty
    sub     sp, sp, #512                // space for 64 beacon x values
    mov     x26, sp                     // beacon x array
    mov     x27, #0                     // unique beacon count

check_beacons_loop:
    cmp     x22, x20
    b.ge    check_beacons_done

    mov     x0, #SENSOR_SIZE
    mul     x0, x0, x22
    add     x0, x19, x0
    ldr     x23, [x0, #24]              // by

    cmp     x23, x25
    b.ne    next_beacon

    // Beacon is on target row, check if unique
    ldr     x24, [x0, #16]              // bx

    mov     x28, #0                     // check index
check_unique_loop:
    cmp     x28, x27
    b.ge    add_unique_beacon

    ldr     x0, [x26, x28, lsl #3]
    cmp     x0, x24
    b.eq    next_beacon                 // already counted

    add     x28, x28, #1
    b       check_unique_loop

add_unique_beacon:
    str     x24, [x26, x27, lsl #3]
    add     x27, x27, #1

next_beacon:
    add     x22, x22, #1
    b       check_beacons_loop

check_beacons_done:
    // Subtract unique beacons from total
    sub     x21, x21, x27
    add     sp, sp, #512

    mov     x0, x21

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// part2: Find the distress beacon's tuning frequency
// Output: x0 = x * 4000000 + y
// ============================================================================
part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, #0                     // row = 0
    mov     w20, #(MAX_COORD & 0xFFFF)
    movk    w20, #(MAX_COORD >> 16), lsl #16
    sxtw    x20, w20                    // max_coord

row_scan_loop:
    cmp     x19, x20
    b.gt    part2_not_found

    // Get coverage at this row
    mov     x0, x19
    bl      get_coverage_at_row

    // Clip ranges to [0, max_coord] and check for gaps
    LOAD_ADDR x21, ranges
    LOAD_ADDR x22, range_count
    ldr     x22, [x22]

    // If no ranges, the beacon could be anywhere (unlikely)
    cbz     x22, next_row

    // Check if full row [0, max_coord] is covered
    // After merging, ranges are sorted by start
    // Check first range starts at or before 0
    ldr     x23, [x21, #0]              // first range start
    ldr     x24, [x21, #8]              // first range end

    // Clip first range start to 0
    cmp     x23, #0
    csel    x23, x23, xzr, gt

    // Check if first range covers from 0
    cmp     x23, #0
    b.gt    found_gap_at_start

    // Check if there are multiple ranges (gap between them)
    cmp     x22, #1
    b.eq    check_single_range

    // Multiple ranges - gap is between first and second
    // x = first_range.end + 1
    add     x25, x24, #1

    // Check if gap is within bounds
    cmp     x25, x20
    b.gt    next_row                    // Gap is beyond max_coord

    cmp     x25, #0
    b.lt    next_row                    // Gap is before 0

    // Found the beacon!
    b       found_beacon

check_single_range:
    // Single merged range - check if it covers full row
    cmp     x24, x20
    b.ge    next_row                    // Covers full row

    // Gap at end
    add     x25, x24, #1
    b       found_beacon

found_gap_at_start:
    // Gap at start (x = 0)
    mov     x25, #0
    b       found_beacon

next_row:
    add     x19, x19, #1
    b       row_scan_loop

found_beacon:
    // x25 = beacon x, x19 = beacon y
    // tuning frequency = x * 4000000 + y
    mov     w0, #(TUNING_MULT & 0xFFFF)
    movk    w0, #(TUNING_MULT >> 16), lsl #16
    sxtw    x0, w0
    mul     x0, x25, x0
    add     x0, x0, x19

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

part2_not_found:
    mov     x0, #0

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
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
