#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NODES 1024
#define MAX_NEIGHBORS 32
#define MAX_NAME_LEN 8
#define HASH_SIZE 2048

// Node structure for graph
typedef struct Node {
    char name[MAX_NAME_LEN];
    char neighbors[MAX_NEIGHBORS][MAX_NAME_LEN];
    int neighbor_count;
    struct Node* next;  // For hash table chaining
} Node;

// Hash table for graph
Node* graph[HASH_SIZE];

// Cache for memoization
typedef struct CacheEntry {
    char from[MAX_NAME_LEN];
    char to[MAX_NAME_LEN];
    unsigned long long count;
    struct CacheEntry* next;
} CacheEntry;

CacheEntry* cache[HASH_SIZE];

// Simple hash function
unsigned int hash(const char* str) {
    unsigned int hash = 5381;
    int c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c;
    return hash % HASH_SIZE;
}

// Find or create node in graph
Node* get_or_create_node(const char* name) {
    unsigned int h = hash(name);
    Node* current = graph[h];

    // Search for existing node
    while (current != NULL) {
        if (strcmp(current->name, name) == 0) {
            return current;
        }
        current = current->next;
    }

    // Create new node
    Node* new_node = (Node*)malloc(sizeof(Node));
    strncpy(new_node->name, name, MAX_NAME_LEN - 1);
    new_node->name[MAX_NAME_LEN - 1] = '\0';
    new_node->neighbor_count = 0;
    new_node->next = graph[h];
    graph[h] = new_node;

    return new_node;
}

// Find node in graph
Node* find_node(const char* name) {
    unsigned int h = hash(name);
    Node* current = graph[h];

    while (current != NULL) {
        if (strcmp(current->name, name) == 0) {
            return current;
        }
        current = current->next;
    }

    return NULL;
}

// Get cached count
int get_cached_count(const char* from, const char* to, unsigned long long* result) {
    char key[MAX_NAME_LEN * 2 + 1];
    snprintf(key, sizeof(key), "%s>%s", from, to);

    unsigned int h = hash(key);
    CacheEntry* current = cache[h];

    while (current != NULL) {
        if (strcmp(current->from, from) == 0 && strcmp(current->to, to) == 0) {
            *result = current->count;
            return 1;
        }
        current = current->next;
    }

    return 0;
}

// Set cached count
void set_cached_count(const char* from, const char* to, unsigned long long count) {
    char key[MAX_NAME_LEN * 2 + 1];
    snprintf(key, sizeof(key), "%s>%s", from, to);

    unsigned int h = hash(key);

    CacheEntry* new_entry = (CacheEntry*)malloc(sizeof(CacheEntry));
    strncpy(new_entry->from, from, MAX_NAME_LEN - 1);
    new_entry->from[MAX_NAME_LEN - 1] = '\0';
    strncpy(new_entry->to, to, MAX_NAME_LEN - 1);
    new_entry->to[MAX_NAME_LEN - 1] = '\0';
    new_entry->count = count;
    new_entry->next = cache[h];
    cache[h] = new_entry;
}

// Clear cache
void clear_cache() {
    for (int i = 0; i < HASH_SIZE; i++) {
        CacheEntry* current = cache[i];
        while (current != NULL) {
            CacheEntry* next = current->next;
            free(current);
            current = next;
        }
        cache[i] = NULL;
    }
}

// Count paths from 'from' to 'to' using memoization
unsigned long long count_paths(const char* from, const char* to) {
    // Base case: reached target
    if (strcmp(from, to) == 0) {
        return 1;
    }

    // Check cache
    unsigned long long cached_result;
    if (get_cached_count(from, to, &cached_result)) {
        return cached_result;
    }

    // Find node
    Node* node = find_node(from);
    if (node == NULL) {
        set_cached_count(from, to, 0);
        return 0;
    }

    // Sum paths through all neighbors
    unsigned long long total = 0;
    for (int i = 0; i < node->neighbor_count; i++) {
        total += count_paths(node->neighbors[i], to);
    }

    // Cache result
    set_cached_count(from, to, total);

    return total;
}

// Parse input file
void parse_input(const char* filename) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        fprintf(stderr, "Error opening file: %s\n", filename);
        exit(1);
    }

    char line[512];
    while (fgets(line, sizeof(line), file)) {
        // Remove trailing newline
        line[strcspn(line, "\n")] = 0;

        // Skip empty lines
        if (strlen(line) == 0) {
            continue;
        }

        // Parse "node: neighbor1 neighbor2 ..."
        char* colon = strchr(line, ':');
        if (colon == NULL) {
            continue;
        }

        // Extract node name
        *colon = '\0';
        char node_name[MAX_NAME_LEN];
        strncpy(node_name, line, MAX_NAME_LEN - 1);
        node_name[MAX_NAME_LEN - 1] = '\0';

        Node* node = get_or_create_node(node_name);

        // Parse neighbors
        char* token = strtok(colon + 1, " ");
        while (token != NULL) {
            if (node->neighbor_count < MAX_NEIGHBORS) {
                strncpy(node->neighbors[node->neighbor_count], token, MAX_NAME_LEN - 1);
                node->neighbors[node->neighbor_count][MAX_NAME_LEN - 1] = '\0';
                node->neighbor_count++;
            }
            token = strtok(NULL, " ");
        }
    }

    fclose(file);
}

// Part 1: Count paths from 'you' to 'out'
unsigned long long part1() {
    clear_cache();
    return count_paths("you", "out");
}

// Part 2: Count paths from 'svr' to 'out' that visit both 'dac' and 'fft'
unsigned long long part2() {
    // Paths that visit dac before fft: svr -> dac -> fft -> out
    clear_cache();
    unsigned long long svr_to_dac = count_paths("svr", "dac");

    clear_cache();
    unsigned long long dac_to_fft = count_paths("dac", "fft");

    clear_cache();
    unsigned long long fft_to_out = count_paths("fft", "out");

    unsigned long long dac_before_fft = svr_to_dac * dac_to_fft * fft_to_out;

    // Paths that visit fft before dac: svr -> fft -> dac -> out
    clear_cache();
    unsigned long long svr_to_fft = count_paths("svr", "fft");

    clear_cache();
    unsigned long long fft_to_dac = count_paths("fft", "dac");

    clear_cache();
    unsigned long long dac_to_out = count_paths("dac", "out");

    unsigned long long fft_before_dac = svr_to_fft * fft_to_dac * dac_to_out;

    return dac_before_fft + fft_before_dac;
}

// Free memory
void cleanup() {
    // Free graph
    for (int i = 0; i < HASH_SIZE; i++) {
        Node* current = graph[i];
        while (current != NULL) {
            Node* next = current->next;
            free(current);
            current = next;
        }
        graph[i] = NULL;
    }

    // Free cache
    clear_cache();
}

int main() {
    // Initialize hash tables
    for (int i = 0; i < HASH_SIZE; i++) {
        graph[i] = NULL;
        cache[i] = NULL;
    }

    // Parse input
    parse_input("../input.txt");

    // Solve both parts
    printf("Part 1: %llu\n", part1());
    printf("Part 2: %llu\n", part2());

    // Cleanup
    cleanup();

    return 0;
}
