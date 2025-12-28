#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_HANDS 1024
#define HAND_SIZE 5

typedef struct {
    char cards[HAND_SIZE + 1];
    int bid;
    int type;
    int card_values[HAND_SIZE];
} Hand;

/* Card strength order for Part 1 (higher index = stronger) */
static const char *CARD_STRENGTH = "23456789TJQKA";

/* Card strength order for Part 2 (J is weakest) */
static const char *CARD_STRENGTH_JOKER = "J23456789TQKA";

static int card_value(char c, const char *strength_order) {
    const char *p = strchr(strength_order, c);
    return p ? (int)(p - strength_order) : -1;
}

static int get_hand_type(const char *hand) {
    int counts[13] = {0};

    /* Count occurrences of each card */
    for (int i = 0; i < HAND_SIZE; i++) {
        int idx = card_value(hand[i], CARD_STRENGTH);
        if (idx >= 0) counts[idx]++;
    }

    /* Get sorted counts (descending) */
    int sorted_counts[5] = {0};
    int num_counts = 0;
    for (int i = 0; i < 13; i++) {
        if (counts[i] > 0) {
            sorted_counts[num_counts++] = counts[i];
        }
    }

    /* Simple bubble sort (small array) */
    for (int i = 0; i < num_counts - 1; i++) {
        for (int j = i + 1; j < num_counts; j++) {
            if (sorted_counts[j] > sorted_counts[i]) {
                int tmp = sorted_counts[i];
                sorted_counts[i] = sorted_counts[j];
                sorted_counts[j] = tmp;
            }
        }
    }

    /* Determine hand type based on sorted counts */
    if (num_counts == 1) return 6;                                    /* Five of a kind */
    if (num_counts == 2 && sorted_counts[0] == 4) return 5;           /* Four of a kind */
    if (num_counts == 2 && sorted_counts[0] == 3) return 4;           /* Full house */
    if (num_counts == 3 && sorted_counts[0] == 3) return 3;           /* Three of a kind */
    if (num_counts == 3 && sorted_counts[0] == 2) return 2;           /* Two pair */
    if (num_counts == 4) return 1;                                    /* One pair */
    return 0;                                                         /* High card */
}

static int get_hand_type_with_jokers(const char *hand) {
    int joker_count = 0;
    for (int i = 0; i < HAND_SIZE; i++) {
        if (hand[i] == 'J') joker_count++;
    }

    if (joker_count == 0) return get_hand_type(hand);
    if (joker_count == 5) return 6;  /* Five of a kind */

    /* Count non-joker cards */
    int counts[13] = {0};
    for (int i = 0; i < HAND_SIZE; i++) {
        if (hand[i] != 'J') {
            int idx = card_value(hand[i], CARD_STRENGTH);
            if (idx >= 0) counts[idx]++;
        }
    }

    /* Get sorted counts (descending) */
    int sorted_counts[5] = {0};
    int num_counts = 0;
    for (int i = 0; i < 13; i++) {
        if (counts[i] > 0) {
            sorted_counts[num_counts++] = counts[i];
        }
    }

    /* Simple bubble sort */
    for (int i = 0; i < num_counts - 1; i++) {
        for (int j = i + 1; j < num_counts; j++) {
            if (sorted_counts[j] > sorted_counts[i]) {
                int tmp = sorted_counts[i];
                sorted_counts[i] = sorted_counts[j];
                sorted_counts[j] = tmp;
            }
        }
    }

    /* Add jokers to highest count */
    sorted_counts[0] += joker_count;

    /* Determine hand type based on sorted counts */
    if (num_counts == 1 || sorted_counts[0] == 5) return 6;           /* Five of a kind */
    if (sorted_counts[0] == 4) return 5;                              /* Four of a kind */
    if (sorted_counts[0] == 3 && sorted_counts[1] == 2) return 4;     /* Full house */
    if (sorted_counts[0] == 3) return 3;                              /* Three of a kind */
    if (sorted_counts[0] == 2 && sorted_counts[1] == 2) return 2;     /* Two pair */
    if (sorted_counts[0] == 2) return 1;                              /* One pair */
    return 0;                                                         /* High card */
}

static int compare_hands_part1(const void *a, const void *b) {
    const Hand *ha = (const Hand *)a;
    const Hand *hb = (const Hand *)b;

    /* Compare by type first */
    if (ha->type != hb->type) {
        return ha->type - hb->type;
    }

    /* Then by card values left to right */
    for (int i = 0; i < HAND_SIZE; i++) {
        if (ha->card_values[i] != hb->card_values[i]) {
            return ha->card_values[i] - hb->card_values[i];
        }
    }
    return 0;
}

static int compare_hands_part2(const void *a, const void *b) {
    const Hand *ha = (const Hand *)a;
    const Hand *hb = (const Hand *)b;

    /* Compare by type first */
    if (ha->type != hb->type) {
        return ha->type - hb->type;
    }

    /* Then by card values left to right */
    for (int i = 0; i < HAND_SIZE; i++) {
        if (ha->card_values[i] != hb->card_values[i]) {
            return ha->card_values[i] - hb->card_values[i];
        }
    }
    return 0;
}

static long long solve(Hand *hands, int num_hands, int part2) {
    /* Calculate type and card values for each hand */
    for (int i = 0; i < num_hands; i++) {
        if (part2) {
            hands[i].type = get_hand_type_with_jokers(hands[i].cards);
            for (int j = 0; j < HAND_SIZE; j++) {
                hands[i].card_values[j] = card_value(hands[i].cards[j], CARD_STRENGTH_JOKER);
            }
        } else {
            hands[i].type = get_hand_type(hands[i].cards);
            for (int j = 0; j < HAND_SIZE; j++) {
                hands[i].card_values[j] = card_value(hands[i].cards[j], CARD_STRENGTH);
            }
        }
    }

    /* Sort hands */
    if (part2) {
        qsort(hands, num_hands, sizeof(Hand), compare_hands_part2);
    } else {
        qsort(hands, num_hands, sizeof(Hand), compare_hands_part1);
    }

    /* Calculate total winnings */
    long long total = 0;
    for (int i = 0; i < num_hands; i++) {
        total += (long long)(i + 1) * hands[i].bid;
    }

    return total;
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        fprintf(stderr, "Could not open input.txt\n");
        return 1;
    }

    Hand hands1[MAX_HANDS];
    Hand hands2[MAX_HANDS];
    int num_hands = 0;

    char line[256];
    while (fgets(line, sizeof(line), fp) && num_hands < MAX_HANDS) {
        char cards[HAND_SIZE + 1];
        int bid;
        if (sscanf(line, "%5s %d", cards, &bid) == 2) {
            strncpy(hands1[num_hands].cards, cards, HAND_SIZE + 1);
            hands1[num_hands].bid = bid;
            strncpy(hands2[num_hands].cards, cards, HAND_SIZE + 1);
            hands2[num_hands].bid = bid;
            num_hands++;
        }
    }
    fclose(fp);

    long long part1 = solve(hands1, num_hands, 0);
    long long part2 = solve(hands2, num_hands, 1);

    printf("Part 1: %lld\n", part1);
    printf("Part 2: %lld\n", part2);

    return 0;
}
