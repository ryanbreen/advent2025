import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Day 17: Chronospatial Computer - 3-bit VM emulator
 */
public class Solution {

    private long initialA;
    private long initialB;
    private long initialC;
    private int[] program;

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Path.of("../input.txt"));
        Solution solution = new Solution();
        solution.parse(input);

        System.out.println("Part 1: " + solution.part1());
        System.out.println("Part 2: " + solution.part2());
    }

    private void parse(String input) {
        String[] lines = input.strip().split("\n");

        Pattern regPattern = Pattern.compile("Register [ABC]: (\\d+)");

        Matcher matcherA = regPattern.matcher(lines[0]);
        matcherA.find();
        initialA = Long.parseLong(matcherA.group(1));

        Matcher matcherB = regPattern.matcher(lines[1]);
        matcherB.find();
        initialB = Long.parseLong(matcherB.group(1));

        Matcher matcherC = regPattern.matcher(lines[2]);
        matcherC.find();
        initialC = Long.parseLong(matcherC.group(1));

        Pattern progPattern = Pattern.compile("Program: ([\\d,]+)");
        Matcher progMatcher = progPattern.matcher(lines[4]);
        progMatcher.find();
        String[] parts = progMatcher.group(1).split(",");
        program = new int[parts.length];
        for (int i = 0; i < parts.length; i++) {
            program[i] = Integer.parseInt(parts[i]);
        }
    }

    /**
     * Execute the 3-bit computer program and return output.
     */
    private List<Integer> runProgram(long a, long b, long c) {
        List<Integer> output = new ArrayList<>();
        int ip = 0;

        while (ip < program.length) {
            int opcode = program[ip];
            int operand = program[ip + 1];

            switch (opcode) {
                case 0 -> // adv - A = A >> combo
                    a = a >> combo(operand, a, b, c);
                case 1 -> // bxl - B = B XOR literal
                    b = b ^ operand;
                case 2 -> // bst - B = combo % 8
                    b = combo(operand, a, b, c) & 7;
                case 3 -> { // jnz - jump if A != 0
                    if (a != 0) {
                        ip = operand;
                        continue;
                    }
                }
                case 4 -> // bxc - B = B XOR C
                    b = b ^ c;
                case 5 -> // out - output combo % 8
                    output.add((int) (combo(operand, a, b, c) & 7));
                case 6 -> // bdv - B = A >> combo
                    b = a >> combo(operand, a, b, c);
                case 7 -> // cdv - C = A >> combo
                    c = a >> combo(operand, a, b, c);
            }

            ip += 2;
        }

        return output;
    }

    /**
     * Get combo operand value.
     */
    private long combo(int operand, long a, long b, long c) {
        return switch (operand) {
            case 0, 1, 2, 3 -> operand;
            case 4 -> a;
            case 5 -> b;
            case 6 -> c;
            default -> throw new IllegalArgumentException("Invalid combo operand: " + operand);
        };
    }

    /**
     * Part 1: Run the program and return comma-separated output.
     */
    private String part1() {
        List<Integer> output = runProgram(initialA, initialB, initialC);
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < output.size(); i++) {
            if (i > 0) sb.append(",");
            sb.append(output.get(i));
        }
        return sb.toString();
    }

    /**
     * Part 2: Find initial A value that makes program output itself.
     *
     * The program loops, outputting one digit per iteration, dividing A by 8 each time.
     * We need to find A such that output == program.
     * Work backwards from the last digit - build A 3 bits at a time.
     */
    private long part2() {
        Long result = search(program.length - 1, 0);
        return result != null ? result : -1;
    }

    /**
     * Recursively search for A value that produces program output.
     */
    private Long search(int targetIdx, long currentA) {
        if (targetIdx < 0) {
            return currentA;
        }

        // Try all 8 possible 3-bit values for this position
        for (int bits = 0; bits < 8; bits++) {
            long candidateA = (currentA << 3) | bits;

            // A can't be 0 at start (would halt immediately without output)
            if (candidateA == 0 && targetIdx == program.length - 1) {
                continue;
            }

            List<Integer> output = runProgram(candidateA, initialB, initialC);

            // Check if output matches the suffix of the program
            int suffixLen = program.length - targetIdx;
            if (output.size() == suffixLen) {
                boolean matches = true;
                for (int i = 0; i < suffixLen; i++) {
                    if (output.get(i) != program[targetIdx + i]) {
                        matches = false;
                        break;
                    }
                }
                if (matches) {
                    Long result = search(targetIdx - 1, candidateA);
                    if (result != null) {
                        return result;
                    }
                }
            }
        }

        return null;
    }
}
