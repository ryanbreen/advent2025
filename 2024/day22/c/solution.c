#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define MAX_BUYERS 4096
#define SECRET_COUNT 2000
#define PRUNE_MASK 0xFFFFFF  // 16777216 - 1 = 2^24 - 1

// Hash table for tracking 4-change sequences
// Key: 4 changes packed into a 32-bit integer (each change is -9 to 9, fits in 5 bits)
// Value: total bananas for this sequence across all buyers
#define HASH_SIZE 1000003  // Large prime for better distribution

typedef struct {
    int32_t key;      // Packed 4-change sequence
    int32_t value;    // Total bananas
    int occupied;
} HashEntry;

typedef struct {
    HashEntry *entries;
    size_t size;
} HashMap;

HashMap* hash_create(size_t size) {
    HashMap *map = malloc(sizeof(HashMap));
    map->size = size;
    map->entries = calloc(size, sizeof(HashEntry));
    return map;
}

void hash_free(HashMap *map) {
    free(map->entries);
    free(map);
}

// Pack 4 changes into a 32-bit key
// Each change is -9 to 9, add 9 to make it 0 to 18, fits in 5 bits
int32_t pack_sequence(int8_t c1, int8_t c2, int8_t c3, int8_t c4) {
    return ((c1 + 9) << 15) | ((c2 + 9) << 10) | ((c3 + 9) << 5) | (c4 + 9);
}

void hash_add(HashMap *map, int32_t key, int32_t value) {
    size_t idx = (size_t)(key >= 0 ? key : -key) % map->size;

    while (1) {
        if (!map->entries[idx].occupied) {
            map->entries[idx].key = key;
            map->entries[idx].value = value;
            map->entries[idx].occupied = 1;
            return;
        } else if (map->entries[idx].key == key) {
            map->entries[idx].value += value;
            return;
        }
        idx = (idx + 1) % map->size;
    }
}

int32_t hash_max_value(HashMap *map) {
    int32_t max_val = 0;
    for (size_t i = 0; i < map->size; i++) {
        if (map->entries[i].occupied && map->entries[i].value > max_val) {
            max_val = map->entries[i].value;
        }
    }
    return max_val;
}

int32_t next_secret(int32_t secret) {
    // Step 1: multiply by 64 (shift left 6), mix (XOR), prune (mask)
    secret ^= (secret << 6);
    secret &= PRUNE_MASK;

    // Step 2: divide by 32 (shift right 5), mix, prune
    secret ^= (secret >> 5);
    secret &= PRUNE_MASK;

    // Step 3: multiply by 2048 (shift left 11), mix, prune
    secret ^= (secret << 11);
    secret &= PRUNE_MASK;

    return secret;
}

int64_t part1(int32_t *initial_secrets, int count) {
    int64_t total = 0;

    for (int i = 0; i < count; i++) {
        int32_t secret = initial_secrets[i];
        for (int j = 0; j < SECRET_COUNT; j++) {
            secret = next_secret(secret);
        }
        total += secret;
    }

    return total;
}

int32_t part2(int32_t *initial_secrets, int count) {
    HashMap *sequence_totals = hash_create(HASH_SIZE);

    for (int buyer = 0; buyer < count; buyer++) {
        int32_t secret = initial_secrets[buyer];

        // Generate prices and changes
        int8_t prices[SECRET_COUNT + 1];
        int8_t changes[SECRET_COUNT];

        prices[0] = secret % 10;
        for (int i = 0; i < SECRET_COUNT; i++) {
            secret = next_secret(secret);
            prices[i + 1] = secret % 10;
            changes[i] = prices[i + 1] - prices[i];
        }

        // Track first occurrence of each 4-change sequence for this buyer
        // Use a hash set for proper tracking
        HashMap *seen = hash_create(HASH_SIZE);

        for (int i = 0; i < SECRET_COUNT - 3; i++) {
            int32_t seq = pack_sequence(changes[i], changes[i+1], changes[i+2], changes[i+3]);

            // Check if we've seen this sequence for this buyer
            size_t idx = (size_t)(seq >= 0 ? seq : -seq) % seen->size;
            int found = 0;

            // Linear probing to check if exists
            size_t start_idx = idx;
            while (seen->entries[idx].occupied) {
                if (seen->entries[idx].key == seq) {
                    found = 1;
                    break;
                }
                idx = (idx + 1) % seen->size;
                if (idx == start_idx) break;  // Full loop
            }

            if (!found) {
                // Mark as seen
                idx = (size_t)(seq >= 0 ? seq : -seq) % seen->size;
                while (seen->entries[idx].occupied) {
                    idx = (idx + 1) % seen->size;
                }
                seen->entries[idx].key = seq;
                seen->entries[idx].occupied = 1;

                // Price we get is after these 4 changes (at index i+4)
                hash_add(sequence_totals, seq, prices[i + 4]);
            }
        }

        hash_free(seen);
    }

    int32_t result = hash_max_value(sequence_totals);
    hash_free(sequence_totals);

    return result;
}

int main() {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Failed to open input.txt");
        return 1;
    }

    int32_t initial_secrets[MAX_BUYERS];
    int count = 0;

    while (fscanf(fp, "%d", &initial_secrets[count]) == 1) {
        count++;
        if (count >= MAX_BUYERS) break;
    }

    fclose(fp);

    printf("Part 1: %lld\n", part1(initial_secrets, count));
    printf("Part 2: %d\n", part2(initial_secrets, count));

    return 0;
}
