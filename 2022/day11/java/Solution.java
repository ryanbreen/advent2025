import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.regex.*;

public class Solution {

    static class Monkey {
        List<Long> items = new ArrayList<>();
        char operator;
        String operand;
        int divisor;
        int ifTrue;
        int ifFalse;
        long inspections = 0;

        Monkey copy() {
            Monkey m = new Monkey();
            m.items = new ArrayList<>(this.items);
            m.operator = this.operator;
            m.operand = this.operand;
            m.divisor = this.divisor;
            m.ifTrue = this.ifTrue;
            m.ifFalse = this.ifFalse;
            m.inspections = 0;
            return m;
        }
    }

    static List<Monkey> parseMonkeys(String text) {
        List<Monkey> monkeys = new ArrayList<>();
        String[] blocks = text.trim().split("\n\n");

        Pattern numberPattern = Pattern.compile("\\d+");
        Pattern opPattern = Pattern.compile("new = old ([+*]) (\\w+)");

        for (String block : blocks) {
            String[] lines = block.trim().split("\n");
            Monkey monkey = new Monkey();

            // Parse starting items
            Matcher numMatcher = numberPattern.matcher(lines[1]);
            while (numMatcher.find()) {
                monkey.items.add(Long.parseLong(numMatcher.group()));
            }

            // Parse operation
            Matcher opMatcher = opPattern.matcher(lines[2]);
            if (opMatcher.find()) {
                monkey.operator = opMatcher.group(1).charAt(0);
                monkey.operand = opMatcher.group(2);
            }

            // Parse divisor
            numMatcher = numberPattern.matcher(lines[3]);
            if (numMatcher.find()) {
                monkey.divisor = Integer.parseInt(numMatcher.group());
            }

            // Parse if true
            numMatcher = numberPattern.matcher(lines[4]);
            if (numMatcher.find()) {
                monkey.ifTrue = Integer.parseInt(numMatcher.group());
            }

            // Parse if false
            numMatcher = numberPattern.matcher(lines[5]);
            if (numMatcher.find()) {
                monkey.ifFalse = Integer.parseInt(numMatcher.group());
            }

            monkeys.add(monkey);
        }

        return monkeys;
    }

    static long applyOperation(long old, char operator, String operand) {
        long val = operand.equals("old") ? old : Long.parseLong(operand);
        if (operator == '+') {
            return old + val;
        } else {
            return old * val;
        }
    }

    static void simulate(List<Monkey> monkeys, int rounds, int reliefDivisor, boolean useModulo) {
        // Calculate product of all divisors for modulo
        long modValue = 1;
        if (useModulo) {
            for (Monkey m : monkeys) {
                modValue *= m.divisor;
            }
        }

        for (int round = 0; round < rounds; round++) {
            for (Monkey monkey : monkeys) {
                while (!monkey.items.isEmpty()) {
                    long item = monkey.items.remove(0);
                    monkey.inspections++;

                    // Apply operation
                    long newVal = applyOperation(item, monkey.operator, monkey.operand);

                    // Apply relief
                    if (reliefDivisor > 1) {
                        newVal /= reliefDivisor;
                    }

                    // Apply modulo to prevent overflow
                    if (useModulo) {
                        newVal %= modValue;
                    }

                    // Test and throw
                    if (newVal % monkey.divisor == 0) {
                        monkeys.get(monkey.ifTrue).items.add(newVal);
                    } else {
                        monkeys.get(monkey.ifFalse).items.add(newVal);
                    }
                }
            }
        }
    }

    static long monkeyBusiness(List<Monkey> monkeys) {
        List<Long> inspections = new ArrayList<>();
        for (Monkey m : monkeys) {
            inspections.add(m.inspections);
        }
        inspections.sort(Collections.reverseOrder());
        return inspections.get(0) * inspections.get(1);
    }

    static long part1(String text) {
        List<Monkey> monkeys = parseMonkeys(text);
        simulate(monkeys, 20, 3, false);
        return monkeyBusiness(monkeys);
    }

    static long part2(String text) {
        List<Monkey> monkeys = parseMonkeys(text);
        simulate(monkeys, 10000, 1, true);
        return monkeyBusiness(monkeys);
    }

    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get("../input.txt");

        String text = Files.readString(inputPath);

        System.out.println("Part 1: " + part1(text));
        System.out.println("Part 2: " + part2(text));
    }
}
