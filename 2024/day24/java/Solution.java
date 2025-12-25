import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Solution {

    static class Gate {
        String in1;
        String op;
        String in2;
        String out;

        Gate(String in1, String op, String in2, String out) {
            this.in1 = in1;
            this.op = op;
            this.in2 = in2;
            this.out = out;
        }
    }

    static class InputData {
        Map<String, Integer> wires;
        List<Gate> gates;

        InputData(Map<String, Integer> wires, List<Gate> gates) {
            this.wires = wires;
            this.gates = gates;
        }
    }

    static InputData parseInput(String filename) throws IOException {
        String content = Files.readString(Paths.get(filename));
        String[] parts = content.trim().split("\n\n");

        // Parse initial wire values
        Map<String, Integer> wires = new HashMap<>();
        for (String line : parts[0].split("\n")) {
            String[] tokens = line.split(": ");
            wires.put(tokens[0], Integer.parseInt(tokens[1]));
        }

        // Parse gates
        List<Gate> gates = new ArrayList<>();
        for (String line : parts[1].split("\n")) {
            String[] tokens = line.split(" ");
            gates.add(new Gate(tokens[0], tokens[1], tokens[2], tokens[4]));
        }

        return new InputData(wires, gates);
    }

    static Map<String, Integer> simulate(Map<String, Integer> initialWires, List<Gate> gates) {
        Map<String, Integer> wires = new HashMap<>(initialWires);
        List<Gate> remaining = new ArrayList<>(gates);

        while (!remaining.isEmpty()) {
            boolean madeProgress = false;
            List<Gate> newRemaining = new ArrayList<>();

            for (Gate gate : remaining) {
                if (wires.containsKey(gate.in1) && wires.containsKey(gate.in2)) {
                    int v1 = wires.get(gate.in1);
                    int v2 = wires.get(gate.in2);
                    int result;

                    switch (gate.op) {
                        case "AND":
                            result = v1 & v2;
                            break;
                        case "OR":
                            result = v1 | v2;
                            break;
                        case "XOR":
                            result = v1 ^ v2;
                            break;
                        default:
                            throw new IllegalArgumentException("Unknown operation: " + gate.op);
                    }

                    wires.put(gate.out, result);
                    madeProgress = true;
                } else {
                    newRemaining.add(gate);
                }
            }

            remaining = newRemaining;
            if (!madeProgress && !remaining.isEmpty()) {
                throw new RuntimeException("Circuit stuck - missing inputs");
            }
        }

        return wires;
    }

    static long getZValue(Map<String, Integer> wires) {
        List<String> zWires = new ArrayList<>();
        for (String key : wires.keySet()) {
            if (key.startsWith("z")) {
                zWires.add(key);
            }
        }
        Collections.sort(zWires, Collections.reverseOrder());

        long result = 0;
        for (String z : zWires) {
            result = (result << 1) | wires.get(z);
        }
        return result;
    }

    static long part1(Map<String, Integer> wires, List<Gate> gates) {
        Map<String, Integer> finalWires = simulate(wires, gates);
        return getZValue(finalWires);
    }

    static String part2(List<Gate> gates) {
        Set<String> swapped = new HashSet<>();

        // Build lookup: output -> gate
        Map<String, Gate> gateByOutput = new HashMap<>();
        for (Gate gate : gates) {
            gateByOutput.put(gate.out, gate);
        }

        // Build lookup: (inputs, op) -> output
        Map<String, String> gateByInputsOp = new HashMap<>();
        for (Gate gate : gates) {
            Set<String> inputs = new HashSet<>(Arrays.asList(gate.in1, gate.in2));
            String key = inputs.toString() + ":" + gate.op;
            gateByInputsOp.put(key, gate.out);
        }

        // Find the highest bit number
        int maxBit = 0;
        for (Gate gate : gates) {
            if (gate.out.startsWith("z")) {
                int bit = Integer.parseInt(gate.out.substring(1));
                maxBit = Math.max(maxBit, bit);
            }
        }

        // Check structural rules for a correct adder
        for (Gate gate : gates) {
            // Rule: XOR gates that don't take x,y as input should output to z
            if (gate.op.equals("XOR")) {
                boolean isXyXor = (gate.in1.startsWith("x") || gate.in1.startsWith("y")) &&
                                  (gate.in2.startsWith("x") || gate.in2.startsWith("y"));
                if (!isXyXor) {
                    // Second-level XOR (sum XOR carry) should output to z
                    if (!gate.out.startsWith("z")) {
                        swapped.add(gate.out);
                    }
                }
            }

            // Rule: z outputs (except final carry) should come from XOR
            if (gate.out.startsWith("z") && !gate.out.equals(String.format("z%02d", maxBit))) {
                if (!gate.op.equals("XOR")) {
                    swapped.add(gate.out);
                }
            }

            // Rule: AND gates (except x00 AND y00) should feed into OR
            if (gate.op.equals("AND")) {
                boolean isFirstBit = (gate.in1.equals("x00") && gate.in2.equals("y00")) ||
                                     (gate.in1.equals("y00") && gate.in2.equals("x00"));
                if (!isFirstBit) {
                    // This AND output should be input to an OR gate
                    boolean usedByOr = false;
                    for (Gate g2 : gates) {
                        if (g2.op.equals("OR") && (gate.out.equals(g2.in1) || gate.out.equals(g2.in2))) {
                            usedByOr = true;
                            break;
                        }
                    }
                    if (!usedByOr) {
                        swapped.add(gate.out);
                    }
                }
            }

            // Rule: XOR of x,y should feed into another XOR (for z output) and AND (for carry)
            if (gate.op.equals("XOR")) {
                boolean isXyXor = (gate.in1.startsWith("x") || gate.in1.startsWith("y")) &&
                                  (gate.in2.startsWith("x") || gate.in2.startsWith("y"));
                boolean isZ00 = (gate.in1.equals("x00") && gate.in2.equals("y00")) ||
                                (gate.in1.equals("y00") && gate.in2.equals("x00"));
                if (isXyXor && !isZ00) {
                    boolean usedByXor = false;
                    boolean usedByAnd = false;
                    for (Gate g2 : gates) {
                        if (gate.out.equals(g2.in1) || gate.out.equals(g2.in2)) {
                            if (g2.op.equals("XOR")) {
                                usedByXor = true;
                            } else if (g2.op.equals("AND")) {
                                usedByAnd = true;
                            }
                        }
                    }
                    if (!(usedByXor && usedByAnd)) {
                        swapped.add(gate.out);
                    }
                }
            }
        }

        List<String> result = new ArrayList<>(swapped);
        Collections.sort(result);
        return String.join(",", result);
    }

    public static void main(String[] args) throws IOException {
        InputData data = parseInput("../input.txt");

        System.out.println("Part 1: " + part1(data.wires, data.gates));
        System.out.println("Part 2: " + part2(data.gates));
    }
}
