component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();

        // Solve both parts
        var part1Result = part1(inputText);
        var part2Result = part2(inputText);

        systemOutput("Part 1: " & part1Result, true);
        systemOutput("Part 2: " & part2Result, true);
    }

    /**
     * Parse input into array of monkey structs
     */
    function parseMonkeys(required string input) {
        var monkeys = [];
        // Split by double newlines using regex
        var blocks = reMatch("Monkey \d+:[\s\S]*?(?=Monkey \d+:|$)", arguments.input);

        for (var block in blocks) {
            block = trim(block);
            if (len(block) == 0) continue;

            var lines = listToArray(block, chr(10));

            // Parse items from line 2: "  Starting items: 79, 98"
            var itemsLine = trim(lines[2]);
            var itemsPart = listLast(itemsLine, ":");
            var itemsArr = [];
            var itemNums = listToArray(trim(itemsPart), ",");
            for (var num in itemNums) {
                arrayAppend(itemsArr, val(trim(num)));
            }

            // Parse operation from line 3: "  Operation: new = old * 19"
            var opLine = trim(lines[3]);
            // Extract the part after "new = "
            var opMatch = reFind("new = old ([+*]) (\w+)", opLine, 1, true);
            var operator = mid(opLine, opMatch.pos[2], opMatch.len[2]);
            var operand = mid(opLine, opMatch.pos[3], opMatch.len[3]);

            // Parse divisor from line 4: "  Test: divisible by 23"
            var testLine = trim(lines[4]);
            var divisorMatch = reFind("divisible by (\d+)", testLine, 1, true);
            var divisor = val(mid(testLine, divisorMatch.pos[2], divisorMatch.len[2]));

            // Parse if_true from line 5: "    If true: throw to monkey 2"
            var trueLine = trim(lines[5]);
            var trueMatch = reFind("monkey (\d+)", trueLine, 1, true);
            var ifTrue = val(mid(trueLine, trueMatch.pos[2], trueMatch.len[2]));

            // Parse if_false from line 6: "    If false: throw to monkey 3"
            var falseLine = trim(lines[6]);
            var falseMatch = reFind("monkey (\d+)", falseLine, 1, true);
            var ifFalse = val(mid(falseLine, falseMatch.pos[2], falseMatch.len[2]));

            arrayAppend(monkeys, {
                items: itemsArr,
                operator: operator,
                operand: operand,
                divisor: divisor,
                ifTrue: ifTrue,
                ifFalse: ifFalse,
                inspections: 0
            });
        }

        return monkeys;
    }

    /**
     * Apply the monkey's operation to the worry level
     * Use precisionEvaluate for big number math
     */
    function applyOperation(required numeric old, required string operator, required string operand) {
        var val = arguments.old;
        if (arguments.operand != "old") {
            val = val(arguments.operand);
        }

        if (arguments.operator == "+") {
            return precisionEvaluate(arguments.old + val);
        } else {
            return precisionEvaluate(arguments.old * val);
        }
    }

    /**
     * Simulate monkey business for given number of rounds
     */
    function simulate(required array monkeys, required numeric rounds, numeric reliefDivisor = 3, boolean useModulo = false) {
        // For part 2, calculate product of all divisors
        var modValue = 1;
        if (arguments.useModulo) {
            for (var m in arguments.monkeys) {
                modValue = modValue * m.divisor;
            }
        }

        for (var round = 1; round <= arguments.rounds; round++) {
            for (var i = 1; i <= arrayLen(arguments.monkeys); i++) {
                var monkey = arguments.monkeys[i];

                while (arrayLen(monkey.items) > 0) {
                    var item = monkey.items[1];
                    arrayDeleteAt(monkey.items, 1);
                    monkey.inspections++;

                    // Apply operation
                    var newVal = applyOperation(item, monkey.operator, monkey.operand);

                    // Apply relief (divide by 3 for part 1)
                    if (arguments.reliefDivisor > 1) {
                        newVal = int(newVal / arguments.reliefDivisor);
                    }

                    // Apply modulo to prevent overflow (part 2)
                    if (arguments.useModulo) {
                        newVal = newVal mod modValue;
                    }

                    // Test and throw
                    if (newVal mod monkey.divisor == 0) {
                        arrayAppend(arguments.monkeys[monkey.ifTrue + 1].items, newVal);
                    } else {
                        arrayAppend(arguments.monkeys[monkey.ifFalse + 1].items, newVal);
                    }
                }
            }
        }

        return arguments.monkeys;
    }

    /**
     * Calculate monkey business: product of top 2 inspection counts
     */
    function monkeyBusiness(required array monkeys) {
        var inspections = [];
        for (var m in arguments.monkeys) {
            arrayAppend(inspections, m.inspections);
        }

        // Sort descending
        arraySort(inspections, "numeric", "desc");

        // Return product of top 2 using precisionEvaluate for big numbers
        return precisionEvaluate(inspections[1] * inspections[2]);
    }

    /**
     * Part 1: Run 20 rounds with relief (divide by 3)
     */
    function part1(required string input) {
        var monkeys = parseMonkeys(arguments.input);
        simulate(monkeys, 20, 3, false);
        return monkeyBusiness(monkeys);
    }

    /**
     * Part 2: Run 10000 rounds without relief
     */
    function part2(required string input) {
        var monkeys = parseMonkeys(arguments.input);
        simulate(monkeys, 10000, 1, true);
        return monkeyBusiness(monkeys);
    }

}
