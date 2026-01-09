import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {

    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get(System.getProperty("user.dir"), "..", "input.txt");
        List<String> rucksacks = Files.readAllLines(inputPath).stream()
            .filter(line -> !line.isEmpty())
            .toList();

        System.out.println("Part 1: " + part1(rucksacks));
        System.out.println("Part 2: " + part2(rucksacks));
    }

    private static int priority(char c) {
        if (Character.isLowerCase(c)) {
            return c - 'a' + 1;
        } else {
            return c - 'A' + 27;
        }
    }

    private static Set<Character> toCharSet(String s) {
        Set<Character> set = new HashSet<>();
        for (char c : s.toCharArray()) {
            set.add(c);
        }
        return set;
    }

    private static int part1(List<String> rucksacks) {
        int total = 0;
        for (String rucksack : rucksacks) {
            int mid = rucksack.length() / 2;
            Set<Character> first = toCharSet(rucksack.substring(0, mid));
            Set<Character> second = toCharSet(rucksack.substring(mid));
            first.retainAll(second);
            char common = first.iterator().next();
            total += priority(common);
        }
        return total;
    }

    private static int part2(List<String> rucksacks) {
        int total = 0;
        for (int i = 0; i < rucksacks.size(); i += 3) {
            Set<Character> common = toCharSet(rucksacks.get(i));
            common.retainAll(toCharSet(rucksacks.get(i + 1)));
            common.retainAll(toCharSet(rucksacks.get(i + 2)));
            char badge = common.iterator().next();
            total += priority(badge);
        }
        return total;
    }
}
