// ============================================================================
// Advent of Code 2023 - Day 5: If You Give A Seed A Fertilizer
// ARM64 Assembly Solution for macOS
// ============================================================================
//
// PROBLEM SUMMARY:
// Given a list of seeds and 7 transformation maps (seed->soil, soil->fertilizer,
// etc.), find the minimum location number after passing all seeds through
// the transformation pipeline.
//
// Part 1: Each seed is treated as an individual value
// Part 2: Seeds are interpreted as (start, length) pairs defining ranges
//
// ALGORITHM OVERVIEW:
// Part 1: For each seed, apply all 7 maps sequentially to get its location.
//         Track the minimum location found.
//
// Part 2: Use interval arithmetic to track ranges through transformations.
//         Each map may split an interval into multiple pieces:
//         - Parts that overlap with a mapping rule get transformed
//         - Parts that don't overlap any rule pass through unchanged
//
// TRANSFORMATION RULE FORMAT:
// Each rule is a triple (dst_start, src_start, length) meaning:
//   values in [src_start, src_start+length) map to [dst_start, dst_start+length)
//
// ============================================================================

.global _main
.align 4

// ============================================================================
// CONSTANTS
// ============================================================================
.equ MAX_SEEDS, 32              // Maximum number of seeds
.equ MAX_RANGES_PER_MAP, 64     // Maximum transformation rules per map
.equ MAX_INTERVALS, 256         // Maximum intervals for Part 2
.equ NUM_MAPS, 7                // Number of transformation maps
.equ RANGE_SIZE, 24             // Size of each range (3 x 8 bytes)
.equ MAP_STRIDE, 1544           // Map count (8) + ranges (64 * 24)
.equ INTERVAL_SIZE, 16          // Size of interval (start, end)

// ============================================================================
// DATA SECTION
// ============================================================================
.data
    .align 4

    // --- String Constants ---
    filename:   .asciz "../input.txt"
    msg_part1:  .asciz "Part 1: "
    msg_part2:  .asciz "Part 2: "
    newline:    .asciz "\n"

    // --- File I/O Buffer ---
    .align 4
    buffer:     .space 32768            // Raw file contents

    // --- Number Output Buffer ---
    num_buffer: .space 32               // For integer-to-string conversion

    // --- Seed Storage ---
    .align 4
    seeds:      .space 256              // Up to 32 seeds (8 bytes each)
    seed_count: .quad 0

    // --- Transformation Maps ---
    // Each map: count (8 bytes) + ranges array (64 * 24 bytes)
    // Maps represent: seed->soil, soil->fertilizer, fertilizer->water,
    //                 water->light, light->temperature, temperature->humidity,
    //                 humidity->location
    map0_count:  .quad 0
    map0_ranges: .space 1536            // seed-to-soil
    map1_count:  .quad 0
    map1_ranges: .space 1536            // soil-to-fertilizer
    map2_count:  .quad 0
    map2_ranges: .space 1536            // fertilizer-to-water
    map3_count:  .quad 0
    map3_ranges: .space 1536            // water-to-light
    map4_count:  .quad 0
    map4_ranges: .space 1536            // light-to-temperature
    map5_count:  .quad 0
    map5_ranges: .space 1536            // temperature-to-humidity
    map6_count:  .quad 0
    map6_ranges: .space 1536            // humidity-to-location

    // --- Part 2: Interval Double-Buffering ---
    // We alternate between these two arrays during map application
    intervals_a:        .space 4096     // 256 intervals * 16 bytes
    intervals_b:        .space 4096
    intervals_a_count:  .quad 0
    intervals_b_count:  .quad 0

    // --- Part 2: Working Buffers for Interval Splitting ---
    // During map application, intervals may be split by multiple rules
    remaining_arr:      .space 2048     // Unmapped portions of current interval
    remaining_count:    .quad 0
    new_remaining_arr:  .space 2048     // New unmapped portions after one rule
    new_remaining_count: .quad 0

// ============================================================================
// TEXT SECTION
// ============================================================================
.text

// ============================================================================
// MAIN ENTRY POINT
// ============================================================================
// Register usage in _main:
//   x0: return values and syscall arguments
//   x29: frame pointer
//   x30: link register
// ============================================================================
_main:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // --- Load Input ---
    bl      read_file
    cbz     x0, exit_error              // Exit if file read failed

    // --- Parse Input ---
    bl      parse_input

    // --- Solve Part 1 ---
    adrp    x0, msg_part1@PAGE
    add     x0, x0, msg_part1@PAGEOFF
    bl      print_str

    bl      solve_part1
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    // --- Solve Part 2 ---
    adrp    x0, msg_part2@PAGE
    add     x0, x0, msg_part2@PAGEOFF
    bl      print_str

    bl      solve_part2
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    // --- Exit Success ---
    mov     x0, #0
    ldp     x29, x30, [sp], #16
    mov     x16, #1                     // SYS_EXIT
    svc     #0x80

exit_error:
    mov     x0, #1
    ldp     x29, x30, [sp], #16
    mov     x16, #1                     // SYS_EXIT
    svc     #0x80

// ============================================================================
// FILE I/O
// ============================================================================

// ----------------------------------------------------------------------------
// read_file: Read input file into buffer
// Input:  None
// Output: x0 = bytes read (0 on error)
// Clobbers: x1, x2, x16
// Uses: x19 (fd), x20 (bytes read)
// ----------------------------------------------------------------------------
read_file:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    // Open file
    adrp    x0, filename@PAGE
    add     x0, x0, filename@PAGEOFF
    mov     x1, #0                      // O_RDONLY
    mov     x16, #5                     // SYS_OPEN
    svc     #0x80
    cmp     x0, #0
    b.lt    read_error
    mov     x19, x0                     // Save file descriptor

    // Read file contents
    mov     x0, x19
    adrp    x1, buffer@PAGE
    add     x1, x1, buffer@PAGEOFF
    mov     x2, #32768
    mov     x16, #3                     // SYS_READ
    svc     #0x80
    mov     x20, x0                     // Save bytes read

    // Close file
    mov     x0, x19
    mov     x16, #6                     // SYS_CLOSE
    svc     #0x80

    mov     x0, x20                     // Return bytes read
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

read_error:
    mov     x0, #0
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// PARSING UTILITIES
// ============================================================================

// ----------------------------------------------------------------------------
// parse_number: Parse decimal integer from buffer
// Input:  x0 = pointer to buffer
// Output: x0 = parsed number, x1 = pointer after number
// Clobbers: x2, x3, x4
// ----------------------------------------------------------------------------
parse_number:
    mov     x2, x0                      // Current position
    mov     x0, #0                      // Accumulator

    // Skip leading spaces
1:  ldrb    w3, [x2]
    cmp     w3, #' '
    b.ne    2f
    add     x2, x2, #1
    b       1b

    // Accumulate digits: result = result * 10 + digit
2:  ldrb    w3, [x2]
    cmp     w3, #'0'
    b.lt    3f
    cmp     w3, #'9'
    b.gt    3f

    mov     x4, #10
    mul     x0, x0, x4                  // result *= 10
    sub     w3, w3, #'0'                // Convert ASCII to digit
    add     x0, x0, x3                  // result += digit
    add     x2, x2, #1
    b       2b

3:  mov     x1, x2                      // Return end position
    ret

// ----------------------------------------------------------------------------
// skip_to_char: Skip to specified character
// Input:  x0 = pointer, w1 = character to find
// Output: x0 = pointer after character
// Clobbers: w2
// ----------------------------------------------------------------------------
skip_to_char:
1:  ldrb    w2, [x0]
    cbz     w2, 2f                      // Stop at null terminator
    cmp     w2, w1
    b.eq    2f
    add     x0, x0, #1
    b       1b
2:  add     x0, x0, #1                  // Skip past the character
    ret

// ----------------------------------------------------------------------------
// skip_ws: Skip whitespace (spaces, newlines, tabs)
// Input:  x0 = pointer
// Output: x0 = pointer after whitespace
// Clobbers: w1
// ----------------------------------------------------------------------------
skip_ws:
1:  ldrb    w1, [x0]
    cmp     w1, #' '
    b.eq    2f
    cmp     w1, #'\n'
    b.eq    2f
    cmp     w1, #'\t'
    b.eq    2f
    ret
2:  add     x0, x0, #1
    b       1b

// ============================================================================
// INPUT PARSING
// ============================================================================

// ----------------------------------------------------------------------------
// parse_input: Parse seeds and all 7 transformation maps
// Input:  None (reads from buffer)
// Output: None (populates seeds, seed_count, map*_count, map*_ranges)
// Register allocation:
//   x19: current buffer position
//   x20: seeds array pointer
//   x21: seed count
//   x22: current map index (0-6)
//   x23: current map count pointer
//   x24: current map ranges pointer
//   x25: range count for current map
//   x26: dst_start (temp)
//   x27: src_start (temp)
// ----------------------------------------------------------------------------
parse_input:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    // Initialize buffer pointer
    adrp    x19, buffer@PAGE
    add     x19, x19, buffer@PAGEOFF

    // Skip "seeds: " header
    mov     x0, x19
    mov     w1, #':'
    bl      skip_to_char
    bl      skip_ws
    mov     x19, x0

    // --- Parse Seeds ---
    adrp    x20, seeds@PAGE
    add     x20, x20, seeds@PAGEOFF
    mov     x21, #0                     // Seed count

parse_seeds_loop:
    ldrb    w0, [x19]
    cmp     w0, #'\n'
    b.eq    seeds_done
    cbz     w0, seeds_done
    cmp     w0, #'0'
    b.lt    seeds_done
    cmp     w0, #'9'
    b.gt    seeds_done

    // Parse and store seed value
    mov     x0, x19
    bl      parse_number
    str     x0, [x20, x21, lsl #3]      // seeds[count] = value
    add     x21, x21, #1
    mov     x19, x1

    // Skip to next number
    mov     x0, x19
    bl      skip_ws
    mov     x19, x0
    b       parse_seeds_loop

seeds_done:
    adrp    x0, seed_count@PAGE
    add     x0, x0, seed_count@PAGEOFF
    str     x21, [x0]

    // --- Parse 7 Transformation Maps ---
    mov     x22, #0                     // Map index (0-6)

parse_maps_loop:
    cmp     x22, #7
    b.ge    parse_complete

    // Calculate pointers for current map
    // Each map occupies MAP_STRIDE (1544) bytes
    adrp    x23, map0_count@PAGE
    add     x23, x23, map0_count@PAGEOFF
    mov     x0, #MAP_STRIDE
    mul     x0, x22, x0
    add     x23, x23, x0                // x23 = count pointer
    add     x24, x23, #8                // x24 = ranges pointer

    // Skip empty line between sections (except for first map)
    cbz     x22, skip_empty_line_done
    add     x19, x19, #1                // Skip newline

skip_empty_line_done:
    // Skip header line (e.g., "seed-to-soil map:")
    mov     x0, x19
    mov     w1, #'\n'
    bl      skip_to_char
    mov     x19, x0

    mov     x25, #0                     // Range count for this map

    // --- Parse Transformation Rules ---
parse_ranges_loop:
    ldrb    w0, [x19]
    cbz     w0, map_section_done
    cmp     w0, #'\n'
    b.eq    map_section_done
    cmp     w0, #'0'
    b.lt    map_section_done
    cmp     w0, #'9'
    b.gt    map_section_done

    // Parse: dst_start src_start length
    mov     x0, x19
    bl      parse_number
    mov     x26, x0                     // dst_start
    mov     x19, x1

    mov     x0, x19
    bl      parse_number
    mov     x27, x0                     // src_start
    mov     x19, x1

    mov     x0, x19
    bl      parse_number                // x0 = length
    mov     x19, x1

    // Store range at ranges[count]
    mov     x1, #RANGE_SIZE
    mul     x1, x25, x1
    add     x1, x24, x1
    str     x26, [x1]                   // dst_start
    str     x27, [x1, #8]               // src_start
    str     x0, [x1, #16]               // length

    add     x25, x25, #1

    // Skip to next line
    mov     x0, x19
    mov     w1, #'\n'
    bl      skip_to_char
    mov     x19, x0
    b       parse_ranges_loop

map_section_done:
    str     x25, [x23]                  // Store range count

    add     x22, x22, #1
    b       parse_maps_loop

parse_complete:
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// PART 1: SINGLE VALUE MAPPING
// ============================================================================

// ----------------------------------------------------------------------------
// apply_map: Apply single transformation map to a value
// Input:  x0 = value, x1 = map count pointer
// Output: x0 = mapped value
// Algorithm:
//   For each range (dst, src, len):
//     if src <= value < src + len:
//       return dst + (value - src)
//   return value  // No matching range, identity mapping
// Register allocation:
//   x19: input value
//   x20: number of ranges
//   x21: ranges base pointer
//   x22: current range index
// ----------------------------------------------------------------------------
apply_map:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0                     // Save input value
    ldr     x20, [x1]                   // Load range count
    add     x21, x1, #8                 // Point to ranges array

    mov     x22, #0                     // Range index

apply_map_loop:
    cmp     x22, x20
    b.ge    apply_map_not_found

    // Load current range
    mov     x0, #RANGE_SIZE
    mul     x0, x22, x0
    add     x0, x21, x0

    ldr     x1, [x0]                    // dst_start
    ldr     x2, [x0, #8]                // src_start
    ldr     x3, [x0, #16]               // length

    // Check: src_start <= value < src_start + length
    cmp     x19, x2
    b.lo    next_range                  // value < src_start
    add     x4, x2, x3                  // src_end = src_start + length
    cmp     x19, x4
    b.hs    next_range                  // value >= src_end

    // Found match: return dst_start + (value - src_start)
    sub     x0, x19, x2                 // offset = value - src_start
    add     x0, x0, x1                  // result = dst_start + offset

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

next_range:
    add     x22, x22, #1
    b       apply_map_loop

apply_map_not_found:
    mov     x0, x19                     // Identity mapping

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ----------------------------------------------------------------------------
// seed_to_location: Convert seed to location through all 7 maps
// Input:  x0 = seed value
// Output: x0 = location value
// Register allocation:
//   x19: current value
//   x20: base pointer to maps
//   x21: map index (0-6)
//   x22: scratch for map offset
// ----------------------------------------------------------------------------
seed_to_location:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0                     // Current value
    adrp    x20, map0_count@PAGE
    add     x20, x20, map0_count@PAGEOFF
    mov     x21, #0                     // Map index

s2l_loop:
    cmp     x21, #7
    b.ge    s2l_done

    // Calculate pointer to current map
    mov     x22, #MAP_STRIDE
    mul     x22, x21, x22
    add     x1, x20, x22

    mov     x0, x19
    bl      apply_map
    mov     x19, x0                     // Update current value

    add     x21, x21, #1
    b       s2l_loop

s2l_done:
    mov     x0, x19                     // Return final location

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ----------------------------------------------------------------------------
// solve_part1: Find minimum location for all individual seeds
// Input:  None
// Output: x0 = minimum location
// Register allocation:
//   x19: seeds array pointer
//   x20: seed count
//   x21: current seed index
//   x22: minimum location found
// ----------------------------------------------------------------------------
solve_part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    adrp    x19, seeds@PAGE
    add     x19, x19, seeds@PAGEOFF

    adrp    x0, seed_count@PAGE
    add     x0, x0, seed_count@PAGEOFF
    ldr     x20, [x0]                   // Load seed count

    mov     x21, #0                     // Seed index
    mov     x22, #-1                    // Min = UINT64_MAX initially

part1_loop:
    cmp     x21, x20
    b.ge    part1_done

    // Get location for current seed
    ldr     x0, [x19, x21, lsl #3]
    bl      seed_to_location

    // Update minimum
    cmp     x0, x22
    csel    x22, x0, x22, lo            // min = min(location, min)

    add     x21, x21, #1
    b       part1_loop

part1_done:
    mov     x0, x22

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// PART 2: INTERVAL MAPPING
// ============================================================================

// ----------------------------------------------------------------------------
// add_interval: Add interval to array (skips empty intervals)
// Input:  x0 = start, x1 = end, x2 = array ptr, x3 = count ptr
// Output: None (updates array and count in memory)
// Clobbers: x4, x5
// ----------------------------------------------------------------------------
add_interval:
    cmp     x0, x1
    b.ge    add_interval_skip           // Skip if start >= end (empty)

    ldr     x4, [x3]                    // Load count
    lsl     x5, x4, #4                  // offset = count * 16
    add     x5, x2, x5
    str     x0, [x5]                    // Store start
    str     x1, [x5, #8]                // Store end
    add     x4, x4, #1
    str     x4, [x3]                    // Increment count

add_interval_skip:
    ret

// ----------------------------------------------------------------------------
// apply_map_to_intervals: Apply transformation map to set of intervals
// Input:  x0 = map count ptr
//         x1 = input array, x2 = input count ptr
//         x3 = output array, x4 = output count ptr
// Output: None (populates output array)
//
// Algorithm:
// For each input interval:
//   remaining = [interval]
//   For each map range (dst, src, len):
//     new_remaining = []
//     For each r in remaining:
//       Split r into three parts:
//       1. [r.start, min(r.end, src)) -> add to new_remaining (unmapped)
//       2. [max(r.start, src), min(r.end, src+len)) -> transform and add to output
//       3. [max(r.start, src+len), r.end) -> add to new_remaining (unmapped)
//     remaining = new_remaining
//   Add all remaining intervals to output (identity mapped)
//
// Register allocation (preserved across calls):
//   x19: map count pointer
//   x20: input array
//   x21: input count pointer
//   x22: output array
//   x23: output count pointer
//   x24: map range count
//   x25: map ranges base
//   x26: input interval count
//   x27: input interval index
//   x28: map range index
// Scratch registers (saved/restored around calls):
//   x9-x18: various temporary values
// ----------------------------------------------------------------------------
apply_map_to_intervals:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    // Save parameters
    mov     x19, x0                     // Map count pointer
    mov     x20, x1                     // Input array
    mov     x21, x2                     // Input count pointer
    mov     x22, x3                     // Output array
    mov     x23, x4                     // Output count pointer

    ldr     x24, [x19]                  // Map range count
    add     x25, x19, #8                // Map ranges base

    str     xzr, [x23]                  // Clear output count

    ldr     x26, [x21]                  // Input interval count
    mov     x27, #0                     // Input interval index

    // Get static buffer pointers
    adrp    x9, remaining_arr@PAGE
    add     x9, x9, remaining_arr@PAGEOFF
    adrp    x10, remaining_count@PAGE
    add     x10, x10, remaining_count@PAGEOFF
    adrp    x11, new_remaining_arr@PAGE
    add     x11, x11, new_remaining_arr@PAGEOFF
    adrp    x12, new_remaining_count@PAGE
    add     x12, x12, new_remaining_count@PAGEOFF

process_interval:
    cmp     x27, x26
    b.ge    intervals_done

    // Load current input interval
    lsl     x0, x27, #4
    add     x0, x20, x0
    ldr     x13, [x0]                   // interval start
    ldr     x14, [x0, #8]               // interval end

    // Initialize remaining with this interval
    mov     x0, #1
    str     x0, [x10]                   // remaining_count = 1
    str     x13, [x9]                   // remaining[0].start
    str     x14, [x9, #8]               // remaining[0].end

    mov     x28, #0                     // Map range index

    // --- Process Each Map Range ---
process_map_range:
    cmp     x28, x24
    b.ge    add_remaining               // All ranges processed

    // Load map range: (dst_start, src_start, length)
    mov     x0, #RANGE_SIZE
    mul     x0, x28, x0
    add     x0, x25, x0
    ldr     x13, [x0]                   // dst_start
    ldr     x14, [x0, #8]               // src_start
    ldr     x15, [x0, #16]              // length
    add     x16, x14, x15               // src_end = src_start + length

    // Process remaining intervals against this range
    ldr     x17, [x10]                  // remaining count
    cbz     x17, next_map_range

    str     xzr, [x12]                  // new_remaining_count = 0

    mov     x18, #0                     // remaining index

    // --- Split Each Remaining Interval ---
process_remaining_interval:
    cmp     x18, x17
    b.ge    copy_new_remaining

    // Load remaining interval
    lsl     x0, x18, #4
    add     x0, x9, x0
    ldr     x5, [x0]                    // r_start
    ldr     x6, [x0, #8]                // r_end

    // --- Part 1: Before source range (unmapped) ---
    // [r_start, min(r_end, src_start))
    cmp     x5, x14
    b.hs    skip_before_part            // r_start >= src_start
    cmp     x6, x14
    csel    x1, x6, x14, ls             // end = min(r_end, src_start)
    mov     x0, x5

    // Add to new_remaining
    ldr     x2, [x12]
    lsl     x3, x2, #4
    add     x3, x11, x3
    str     x0, [x3]
    str     x1, [x3, #8]
    add     x2, x2, #1
    str     x2, [x12]

skip_before_part:
    // --- Part 2: Overlapping part (mapped) ---
    // [max(r_start, src_start), min(r_end, src_end))
    cmp     x5, x14
    csel    x7, x5, x14, hi             // overlap_start = max(r_start, src_start)
    cmp     x6, x16
    csel    x8, x6, x16, lo             // overlap_end = min(r_end, src_end)

    cmp     x7, x8
    b.hs    skip_overlap_part           // No overlap

    // Save caller-saved registers before function call
    sub     sp, sp, #96
    stp     x5, x6, [sp]
    stp     x9, x10, [sp, #16]
    stp     x11, x12, [sp, #32]
    stp     x13, x14, [sp, #48]
    stp     x15, x16, [sp, #64]
    stp     x17, x18, [sp, #80]

    // Transform overlap and add to output
    // mapped_start = overlap_start + (dst_start - src_start)
    // mapped_end = overlap_end + (dst_start - src_start)
    sub     x0, x13, x14                // offset = dst_start - src_start
    add     x0, x7, x0                  // mapped_start
    sub     x1, x13, x14
    add     x1, x8, x1                  // mapped_end
    mov     x2, x22
    mov     x3, x23
    bl      add_interval

    // Restore registers
    ldp     x5, x6, [sp]
    ldp     x9, x10, [sp, #16]
    ldp     x11, x12, [sp, #32]
    ldp     x13, x14, [sp, #48]
    ldp     x15, x16, [sp, #64]
    ldp     x17, x18, [sp, #80]
    add     sp, sp, #96

skip_overlap_part:
    // --- Part 3: After source range (unmapped) ---
    // [max(r_start, src_end), r_end)
    cmp     x6, x16
    b.ls    skip_after_part             // r_end <= src_end
    cmp     x5, x16
    csel    x0, x5, x16, hi             // start = max(r_start, src_end)
    mov     x1, x6

    // Add to new_remaining
    ldr     x2, [x12]
    lsl     x3, x2, #4
    add     x3, x11, x3
    str     x0, [x3]
    str     x1, [x3, #8]
    add     x2, x2, #1
    str     x2, [x12]

skip_after_part:
    add     x18, x18, #1
    b       process_remaining_interval

copy_new_remaining:
    // Copy new_remaining to remaining
    ldr     x17, [x12]
    str     x17, [x10]

    mov     x18, #0
copy_rem_loop:
    cmp     x18, x17
    b.ge    next_map_range

    lsl     x0, x18, #4
    add     x1, x11, x0                 // src
    ldr     x2, [x1]
    ldr     x3, [x1, #8]

    add     x0, x9, x0                  // dst
    str     x2, [x0]
    str     x3, [x0, #8]

    add     x18, x18, #1
    b       copy_rem_loop

next_map_range:
    add     x28, x28, #1
    b       process_map_range

add_remaining:
    // Add all remaining intervals to output (identity mapped)
    ldr     x17, [x10]
    mov     x18, #0

add_rem_loop:
    cmp     x18, x17
    b.ge    next_interval

    lsl     x0, x18, #4
    add     x0, x9, x0
    ldr     x5, [x0]                    // start
    ldr     x6, [x0, #8]                // end

    // Save registers before call
    sub     sp, sp, #32
    stp     x9, x10, [sp]
    stp     x17, x18, [sp, #16]

    mov     x0, x5
    mov     x1, x6
    mov     x2, x22
    mov     x3, x23
    bl      add_interval

    // Restore registers
    ldp     x9, x10, [sp]
    ldp     x17, x18, [sp, #16]
    add     sp, sp, #32

    add     x18, x18, #1
    b       add_rem_loop

next_interval:
    add     x27, x27, #1
    b       process_interval

intervals_done:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ----------------------------------------------------------------------------
// solve_part2: Find minimum location for seed ranges
// Input:  None
// Output: x0 = minimum location
//
// Algorithm:
// 1. Convert seed pairs (start, length) to intervals (start, start+length)
// 2. Apply all 7 maps to intervals, alternating between buffers A and B
// 3. Find minimum start value among all final intervals
//
// Register allocation:
//   x19: seeds array / maps base
//   x20: seed count
//   x21: intervals_a pointer
//   x22: intervals_a_count pointer
//   x23: intervals_b pointer / intervals_b_count pointer
//   x24: current map index
//   x25: minimum location
//   x26: interval index
// ----------------------------------------------------------------------------
solve_part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    // --- Convert Seed Pairs to Intervals ---
    adrp    x19, seeds@PAGE
    add     x19, x19, seeds@PAGEOFF

    adrp    x0, seed_count@PAGE
    add     x0, x0, seed_count@PAGEOFF
    ldr     x20, [x0]

    adrp    x21, intervals_a@PAGE
    add     x21, x21, intervals_a@PAGEOFF

    adrp    x22, intervals_a_count@PAGE
    add     x22, x22, intervals_a_count@PAGEOFF

    str     xzr, [x22]                  // Clear intervals_a count

    mov     x23, #0                     // Seed pair index

convert_seeds_loop:
    cmp     x23, x20
    b.ge    seeds_converted

    // Load (start, length) pair and create interval (start, start+length)
    ldr     x0, [x19, x23, lsl #3]      // start
    add     x1, x23, #1
    ldr     x1, [x19, x1, lsl #3]       // length
    add     x1, x0, x1                  // end = start + length

    mov     x2, x21
    mov     x3, x22
    bl      add_interval

    add     x23, x23, #2                // Next pair
    b       convert_seeds_loop

seeds_converted:
    // --- Apply All 7 Maps ---
    // We alternate: A->B, B->A, A->B, B->A, A->B, B->A, A->B
    // After 7 maps, result is in B (even indices output to B)
    adrp    x19, map0_count@PAGE
    add     x19, x19, map0_count@PAGEOFF

    adrp    x20, intervals_a@PAGE
    add     x20, x20, intervals_a@PAGEOFF
    adrp    x21, intervals_a_count@PAGE
    add     x21, x21, intervals_a_count@PAGEOFF

    adrp    x22, intervals_b@PAGE
    add     x22, x22, intervals_b@PAGEOFF
    adrp    x23, intervals_b_count@PAGE
    add     x23, x23, intervals_b_count@PAGEOFF

    mov     x24, #0                     // Map index

apply_maps_loop:
    cmp     x24, #7
    b.ge    maps_done

    // Calculate current map pointer
    mov     x0, #MAP_STRIDE
    mul     x0, x24, x0
    add     x0, x19, x0

    // Determine input/output based on map index parity
    tst     x24, #1
    b.ne    use_b_as_input

    // Even: input=A, output=B
    mov     x1, x20
    mov     x2, x21
    mov     x3, x22
    mov     x4, x23
    b       do_apply_map

use_b_as_input:
    // Odd: input=B, output=A
    mov     x1, x22
    mov     x2, x23
    mov     x3, x20
    mov     x4, x21

do_apply_map:
    bl      apply_map_to_intervals

    add     x24, x24, #1
    b       apply_maps_loop

maps_done:
    // Final result is in intervals_b (7 maps = odd count, but:
    // map 0: A->B, map 1: B->A, map 2: A->B, map 3: B->A,
    // map 4: A->B, map 5: B->A, map 6: A->B => result in B)

    ldr     x24, [x23]                  // intervals_b count

    mov     x25, #-1                    // min = UINT64_MAX
    mov     x26, #0                     // interval index

find_min_loop:
    cmp     x26, x24
    b.ge    find_min_done

    lsl     x0, x26, #4
    add     x0, x22, x0
    ldr     x0, [x0]                    // interval start

    cmp     x0, x25
    csel    x25, x0, x25, lo            // min = min(start, min)

    add     x26, x26, #1
    b       find_min_loop

find_min_done:
    mov     x0, x25

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// OUTPUT UTILITIES
// ============================================================================

// ----------------------------------------------------------------------------
// print_str: Print null-terminated string to stdout
// Input:  x0 = string pointer
// Output: None
// Clobbers: x1, x2, x16
// Uses: x19, x20 (saved)
// ----------------------------------------------------------------------------
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    mov     x19, x0

    // Calculate string length
    mov     x1, #0
1:  ldrb    w2, [x19, x1]
    cbz     w2, 2f
    add     x1, x1, #1
    b       1b

2:  mov     x2, x1                      // length
    mov     x1, x19                     // buffer
    mov     x0, #1                      // stdout
    mov     x16, #4                     // SYS_WRITE
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ----------------------------------------------------------------------------
// print_num: Print 64-bit unsigned integer to stdout
// Input:  x0 = number to print
// Output: None
// Algorithm: Build decimal string in buffer from right to left
// Uses: x19 (number), x20 (buffer pointer)
// ----------------------------------------------------------------------------
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0
    adrp    x20, num_buffer@PAGE
    add     x20, x20, num_buffer@PAGEOFF
    add     x20, x20, #30               // Start at end of buffer
    strb    wzr, [x20]                  // Null terminator

    mov     x2, #10
1:  sub     x20, x20, #1
    udiv    x3, x19, x2                 // quotient
    msub    x4, x3, x2, x19             // remainder = n - (q * 10)
    add     w4, w4, #'0'                // Convert to ASCII
    strb    w4, [x20]
    mov     x19, x3
    cbnz    x19, 1b

    mov     x0, x20
    bl      print_str

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
