import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

public class Solution {

    // Card strength order (higher index = stronger)
    private static final String CARD_STRENGTH = "23456789TJQKA";
    private static final String CARD_STRENGTH_JOKER = "J23456789TQKA";

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Path.of("../input.txt")).trim();
        String[] lines = input.split("\n");

        System.out.println("Part 1: " + part1(lines));
        System.out.println("Part 2: " + part2(lines));
    }

    private static long part1(String[] lines) {
        List<Hand> hands = new ArrayList<>();
        for (String line : lines) {
            String[] parts = line.split("\\s+");
            hands.add(new Hand(parts[0], Integer.parseInt(parts[1])));
        }

        hands.sort(Comparator.comparingInt((Hand h) -> getHandType(h.cards))
                .thenComparing(h -> getCardValues(h.cards, CARD_STRENGTH)));

        long total = 0;
        for (int rank = 1; rank <= hands.size(); rank++) {
            total += (long) rank * hands.get(rank - 1).bid;
        }
        return total;
    }

    private static long part2(String[] lines) {
        List<Hand> hands = new ArrayList<>();
        for (String line : lines) {
            String[] parts = line.split("\\s+");
            hands.add(new Hand(parts[0], Integer.parseInt(parts[1])));
        }

        hands.sort(Comparator.comparingInt((Hand h) -> getHandTypeWithJokers(h.cards))
                .thenComparing(h -> getCardValues(h.cards, CARD_STRENGTH_JOKER)));

        long total = 0;
        for (int rank = 1; rank <= hands.size(); rank++) {
            total += (long) rank * hands.get(rank - 1).bid;
        }
        return total;
    }

    private static int getHandType(String hand) {
        int[] counts = getCardCounts(hand);
        Arrays.sort(counts);
        return classifyHand(counts);
    }

    private static int getHandTypeWithJokers(String hand) {
        int jokerCount = 0;
        for (char c : hand.toCharArray()) {
            if (c == 'J') jokerCount++;
        }

        if (jokerCount == 0) {
            return getHandType(hand);
        }
        if (jokerCount == 5) {
            return 6; // Five of a kind
        }

        // Count non-joker cards
        int[] counts = new int[13];
        for (char c : hand.toCharArray()) {
            if (c != 'J') {
                int idx = CARD_STRENGTH.indexOf(c);
                counts[idx]++;
            }
        }

        Arrays.sort(counts);
        // Add jokers to the highest count
        counts[12] += jokerCount;

        return classifyHand(counts);
    }

    private static int[] getCardCounts(String hand) {
        int[] counts = new int[13];
        for (char c : hand.toCharArray()) {
            int idx = CARD_STRENGTH.indexOf(c);
            counts[idx]++;
        }
        Arrays.sort(counts);
        return counts;
    }

    private static int classifyHand(int[] sortedCounts) {
        // Get the last 5 non-zero counts (sorted ascending)
        int[] relevant = new int[5];
        int idx = 0;
        for (int i = sortedCounts.length - 1; i >= 0 && idx < 5; i--) {
            if (sortedCounts[i] > 0) {
                relevant[idx++] = sortedCounts[i];
            }
        }
        // Sort descending for comparison
        Arrays.sort(relevant);
        reverse(relevant);

        // Classify based on pattern
        if (relevant[0] == 5) return 6;        // Five of a kind
        if (relevant[0] == 4) return 5;        // Four of a kind
        if (relevant[0] == 3 && relevant[1] == 2) return 4;  // Full house
        if (relevant[0] == 3) return 3;        // Three of a kind
        if (relevant[0] == 2 && relevant[1] == 2) return 2;  // Two pair
        if (relevant[0] == 2) return 1;        // One pair
        return 0;                              // High card
    }

    private static void reverse(int[] arr) {
        for (int i = 0; i < arr.length / 2; i++) {
            int temp = arr[i];
            arr[i] = arr[arr.length - 1 - i];
            arr[arr.length - 1 - i] = temp;
        }
    }

    private static CardValues getCardValues(String hand, String cardStrength) {
        int[] values = new int[5];
        for (int i = 0; i < 5; i++) {
            values[i] = cardStrength.indexOf(hand.charAt(i));
        }
        return new CardValues(values);
    }

    private record Hand(String cards, int bid) {}

    private record CardValues(int[] values) implements Comparable<CardValues> {
        @Override
        public int compareTo(CardValues other) {
            for (int i = 0; i < 5; i++) {
                if (this.values[i] != other.values[i]) {
                    return Integer.compare(this.values[i], other.values[i]);
                }
            }
            return 0;
        }
    }
}
