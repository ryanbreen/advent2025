# Day 9: Disk Fragmenter

## Problem Summary

An amphipod needs help compacting files on a fragmented disk. The input is a **disk map** - a dense format where digits alternate between file length and free space length. Files are assigned sequential IDs (0, 1, 2, ...) based on their original order.

For example, `12345` means:
- File 0: 1 block
- Free space: 2 blocks
- File 1: 3 blocks
- Free space: 4 blocks
- File 2: 5 blocks

Expanded: `0..111....22222`

### Part 1: Block-by-Block Compaction
Move file blocks one at a time from the **end** of the disk to the **leftmost free space**, continuing until no gaps remain between file blocks.

### Part 2: Whole-File Compaction
Move entire files (not individual blocks), processing files in **decreasing order of file ID**. Each file attempts to move to the leftmost span of free space large enough to contain it. Files only move left, never right. A file that can't fit anywhere stays in place.

After compaction, calculate a **checksum**: sum of (position × file_id) for each non-free block.

## Input Format

A single line of digits (no newlines). The puzzle input is ~20,000 characters, expanding to ~95,000 blocks.

## Algorithmic Approach

### Part 1: Two-Pointer Technique

The classic two-pointer approach:
1. `left` pointer finds the next free space (scanning right)
2. `right` pointer finds the next file block (scanning left)
3. Swap when both found, continue until pointers meet

```
Time: O(n) - single pass through the block array
Space: O(n) - store expanded block representation
```

### Part 2: Greedy File Placement

Process files from highest ID to lowest:
1. For each file, scan left-to-right for the first contiguous free span large enough
2. If found (and it's to the left of current position), move the entire file
3. Update bookkeeping and continue to next file

```
Time: O(f × n) where f = number of files - for each file, scan for free space
Space: O(n) - same block array plus file metadata
```

### Key Insight

The dense disk map format is elegant but requires expansion for manipulation. The choice to use `-1` for free space (vs. null or a special object) enables simple integer array operations that work well across all languages.

For Part 2, the "decreasing file ID order" constraint prevents the problem from being NP-hard (like bin packing). We simply greedily place each file in the first spot that fits.

## Data Structures Used

- **Array/List**: Expanded block representation where each index is a disk position
- **Map/Dictionary**: File ID → (start position, length) for Part 2 file tracking
- **Two pointers**: For Part 1's O(n) swap algorithm

## Programming Techniques Highlighted

- **Two-pointer technique**: A fundamental algorithm pattern for in-place array manipulation
- **Greedy algorithms**: Part 2's approach of always taking the first valid placement
- **Span/segment scanning**: Finding contiguous regions in arrays
- **State parsing**: Converting dense format to expanded representation

## Language Notes

### Fast Performers
- **ARM64 asm** (19ms): Span-based approach with register-rich architecture
- **C, Rust, C++, Zig** (80-150ms): Block expansion with direct array manipulation
- **Go** (153ms): Efficient slices, though GC shows in memory
- **Node.js** (265ms): V8's array optimization works well here

### Notable Performance Gaps
- **Python, Ruby, Perl** (8-12 seconds): Interpreted languages struggle with ~95,000 array mutations
- **Bash** (6 seconds with span optimization, 9 minutes with block expansion): See optimization section below

### Implementation Considerations
- **Memory**: The block array is ~95KB for file IDs, but languages with object overhead (Java, Clojure) use significantly more
- **64-bit integers**: Checksums exceed 32-bit range (6+ trillion), requiring 64-bit integers or big integers
  - Java: `long` (not `int`)
  - JavaScript: Safe up to 2^53
  - Bash: Native arithmetic handles it
  - PHP: Works on 64-bit systems

### Bash - Span-Based Optimization (90x Speedup)
The naive Bash implementation that expands to ~95,000 blocks takes **9 minutes** due to slow array operations. The optimized version works with **spans** instead:

1. **No block expansion**: Keep the compressed representation (~10,000 spans vs ~95,000 blocks)
2. **Mathematical checksum**: For a file at position `p` with length `L` and ID `f`:
   ```
   contribution = f × (p + p+1 + ... + p+L-1)
                = f × L × p + f × L × (L-1) / 2
   ```
3. **AWK for heavy lifting**: AWK's associative arrays are orders of magnitude faster than Bash's

Result: **6 seconds** instead of 9 minutes.

### ARM64 Assembly - Span-Based (15x Speedup, Fastest Overall)
The original ARM64 implementation used block expansion like C, resulting in 290ms. By adopting the span-based approach:

- **Before**: 290ms (block expansion, ~760KB memory for two arrays)
- **After**: 19ms (span-based, ~240KB for span metadata)
- **Result**: 4x faster than C, fastest implementation overall

ARM64 excels here because:
- 31 general-purpose registers keep span metadata in registers
- Single-instruction array indexing: `ldr x0, [x21, x27, lsl #3]`
- All span data fits in L1 cache (no cache misses)
- No function call overhead (inlined critical paths)

## Answers

- **Part 1**: 6291146824486
- **Part 2**: 6307279963620
