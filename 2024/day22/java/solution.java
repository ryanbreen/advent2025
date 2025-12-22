import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class solution {

    /**
     * Generate the next secret number using mix and prune operations.
     */
    private static long nextSecret(long secret) {
        // Step 1: multiply by 64, mix, prune
        secret ^= (secret << 6);  // * 64 = << 6
        secret &= 0xFFFFFF;       // % 16777216 = & (2^24 - 1)

        // Step 2: divide by 32, mix, prune
        secret ^= (secret >> 5);  // / 32 = >> 5
        secret &= 0xFFFFFF;

        // Step 3: multiply by 2048, mix, prune
        secret ^= (secret << 11); // * 2048 = << 11
        secret &= 0xFFFFFF;

        return secret;
    }

    /**
     * Generate a sequence of secret numbers.
     */
    private static List<Long> generateSecrets(long initial, int count) {
        List<Long> secrets = new ArrayList<>();
        secrets.add(initial);
        long secret = initial;
        for (int i = 0; i < count; i++) {
            secret = nextSecret(secret);
            secrets.add(secret);
        }
        return secrets;
    }

    /**
     * Part 1: Sum of the 2000th secret number for each buyer.
     */
    private static long part1(List<Long> initialSecrets) {
        long total = 0;
        for (long initial : initialSecrets) {
            long secret = initial;
            for (int i = 0; i < 2000; i++) {
                secret = nextSecret(secret);
            }
            total += secret;
        }
        return total;
    }

    /**
     * Represents a sequence of 4 price changes.
     */
    private static class ChangeSeq {
        final int c1, c2, c3, c4;

        ChangeSeq(int c1, int c2, int c3, int c4) {
            this.c1 = c1;
            this.c2 = c2;
            this.c3 = c3;
            this.c4 = c4;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof ChangeSeq)) return false;
            ChangeSeq that = (ChangeSeq) o;
            return c1 == that.c1 && c2 == that.c2 && c3 == that.c3 && c4 == that.c4;
        }

        @Override
        public int hashCode() {
            int result = c1;
            result = 31 * result + c2;
            result = 31 * result + c3;
            result = 31 * result + c4;
            return result;
        }
    }

    /**
     * Part 2: Find the best sequence of 4 price changes to maximize bananas.
     */
    private static long part2(List<Long> initialSecrets) {
        // Map from (change1, change2, change3, change4) -> total bananas
        Map<ChangeSeq, Long> sequenceTotals = new HashMap<>();

        for (long initial : initialSecrets) {
            // Generate 2001 secrets (initial + 2000 new)
            List<Long> secrets = generateSecrets(initial, 2000);

            // Calculate prices (last digit)
            List<Integer> prices = new ArrayList<>();
            for (long secret : secrets) {
                prices.add((int)(secret % 10));
            }

            // Calculate changes
            List<Integer> changes = new ArrayList<>();
            for (int i = 0; i < prices.size() - 1; i++) {
                changes.add(prices.get(i + 1) - prices.get(i));
            }

            // Track first occurrence of each 4-change sequence for this buyer
            Set<ChangeSeq> seen = new HashSet<>();
            for (int i = 0; i < changes.size() - 3; i++) {
                ChangeSeq seq = new ChangeSeq(
                    changes.get(i),
                    changes.get(i + 1),
                    changes.get(i + 2),
                    changes.get(i + 3)
                );

                if (!seen.contains(seq)) {
                    seen.add(seq);
                    // Price we get is after these 4 changes
                    long price = prices.get(i + 4);
                    sequenceTotals.put(seq, sequenceTotals.getOrDefault(seq, 0L) + price);
                }
            }
        }

        return sequenceTotals.values().stream()
            .mapToLong(Long::longValue)
            .max()
            .orElse(0L);
    }

    public static void main(String[] args) throws IOException {
        String inputText = Files.readString(Paths.get("../input.txt")).strip();
        List<Long> initialSecrets = new ArrayList<>();
        for (String line : inputText.split("\n")) {
            line = line.strip();
            if (!line.isEmpty()) {
                initialSecrets.add(Long.parseLong(line));
            }
        }

        System.out.println("Part 1: " + part1(initialSecrets));
        System.out.println("Part 2: " + part2(initialSecrets));
    }
}
