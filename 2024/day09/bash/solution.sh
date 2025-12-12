#!/bin/bash
# Day 9: Disk Fragmenter - Span-based approach (no block expansion)
#
# Key insight: Work with spans (file_id, position, length) instead of
# expanding to individual blocks. Calculate checksums mathematically.
#
# Checksum contribution for file f at position p with length len:
#   f * (p + p+1 + ... + p+len-1) = f * len * p + f * len * (len-1) / 2

awk '
function checksum_span(file_id, pos, len) {
    return file_id * (len * pos + len * (len - 1) / 2)
}

{
    # Parse disk map into file and free span arrays
    n = split($0, chars, "")

    num_files = 0
    num_free = 0
    pos = 0
    file_id = 0
    is_file = 1
    total_file_blocks = 0

    for (i = 1; i <= n; i++) {
        len = int(chars[i])
        if (is_file) {
            if (len > 0) {
                num_files++
                file_pos[file_id] = pos
                file_len[file_id] = len
                total_file_blocks += len
            }
            file_id++
        } else {
            if (len > 0) {
                num_free++
                free_pos[num_free] = pos
                free_len[num_free] = len
            }
        }
        pos += len
        is_file = 1 - is_file
    }

    max_file_id = file_id - 1

    # Part 1: Block-by-block compaction
    # After compaction, positions 0 to total_file_blocks-1 contain all file blocks
    # We simulate filling free spans with blocks taken from files on the right

    part1 = 0
    cutoff = total_file_blocks

    # Process file blocks that stay (files entirely or partially before cutoff)
    for (fid = 0; fid <= max_file_id; fid++) {
        if (file_pos[fid] >= cutoff) break
        stay_len = file_len[fid]
        if (file_pos[fid] + stay_len > cutoff) {
            stay_len = cutoff - file_pos[fid]
        }
        part1 += checksum_span(fid, file_pos[fid], stay_len)
    }

    # Now fill free spans (before cutoff) with file blocks from the right
    take_fid = max_file_id
    take_offset = 0  # Blocks already taken from current file

    for (f = 1; f <= num_free; f++) {
        fpos = free_pos[f]
        if (fpos >= cutoff) break

        flen = free_len[f]
        if (fpos + flen > cutoff) {
            flen = cutoff - fpos
        }

        filled = 0
        while (filled < flen) {
            # Find next file to take from (must be at or after cutoff)
            while (take_fid >= 0 && file_pos[take_fid] < cutoff) {
                take_fid--
                take_offset = 0
            }
            if (take_fid < 0) break

            # How many blocks remain in this file?
            # We need blocks from the END of the file (rightmost blocks move first)
            avail = file_len[take_fid] - take_offset
            need = flen - filled
            use = (avail < need) ? avail : need

            # These blocks go to positions fpos+filled to fpos+filled+use-1
            part1 += checksum_span(take_fid, fpos + filled, use)

            filled += use
            take_offset += use
            if (take_offset >= file_len[take_fid]) {
                take_fid--
                take_offset = 0
            }
        }
    }

    print "Part 1:", part1

    # Part 2: Whole-file compaction
    # Make working copies of file positions
    for (fid = 0; fid <= max_file_id; fid++) {
        p2_file_pos[fid] = file_pos[fid]
    }
    # Make working copy of free spans
    for (f = 1; f <= num_free; f++) {
        p2_free_pos[f] = free_pos[f]
        p2_free_len[f] = free_len[f]
    }

    # Process files from highest ID to lowest
    for (fid = max_file_id; fid >= 0; fid--) {
        flen = file_len[fid]
        fpos = p2_file_pos[fid]

        # Find leftmost free span that fits (must be to the left of file)
        best = 0
        for (f = 1; f <= num_free; f++) {
            if (p2_free_pos[f] >= fpos) break  # Only look left
            if (p2_free_len[f] >= flen) {
                best = f
                break
            }
        }

        if (best > 0) {
            # Move file to this free span
            p2_file_pos[fid] = p2_free_pos[best]

            # Shrink or consume the free span
            p2_free_pos[best] += flen
            p2_free_len[best] -= flen
        }
    }

    # Calculate part 2 checksum
    part2 = 0
    for (fid = 0; fid <= max_file_id; fid++) {
        part2 += checksum_span(fid, p2_file_pos[fid], file_len[fid])
    }

    print "Part 2:", part2
}
' ../input.txt
