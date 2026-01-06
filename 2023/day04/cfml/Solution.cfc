component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();
        var lines = listToArray(inputText, chr(10));

        // Solve both parts
        var part1Result = part1(lines);
        var part2Result = part2(lines);

        writeOutput("Part 1: " & part1Result & chr(10));
        writeOutput("Part 2: " & part2Result & chr(10));
    }

    function parseCards(lines) {
        var cards = [];

        for (var line in lines) {
            // Parse "Card X: winning | have"
            var colonPos = find(":", line);
            var numbers = mid(line, colonPos + 1, len(line) - colonPos);
            var parts = listToArray(numbers, "|");

            // Parse winning numbers into a struct (for set-like lookups)
            var winningNums = {};
            var winningPart = trim(parts[1]);
            for (var num in listToArray(winningPart, " ")) {
                if (len(trim(num)) > 0) {
                    winningNums[val(num)] = true;
                }
            }

            // Parse "have" numbers into an array
            var haveNums = [];
            var havePart = trim(parts[2]);
            for (var num in listToArray(havePart, " ")) {
                if (len(trim(num)) > 0) {
                    arrayAppend(haveNums, val(num));
                }
            }

            arrayAppend(cards, {
                winning: winningNums,
                have: haveNums
            });
        }

        return cards;
    }

    function countMatches(card) {
        var count = 0;
        for (var num in card.have) {
            if (structKeyExists(card.winning, num)) {
                count++;
            }
        }
        return count;
    }

    function part1(lines) {
        var cards = parseCards(lines);
        var total = 0;

        for (var card in cards) {
            var matches = countMatches(card);
            if (matches > 0) {
                // Score is 2^(matches-1)
                total += 2 ^ (matches - 1);
            }
        }

        return total;
    }

    function part2(lines) {
        var cards = parseCards(lines);
        var numCards = arrayLen(cards);

        // Pre-calculate matches for each card
        var matches = [];
        for (var card in cards) {
            arrayAppend(matches, countMatches(card));
        }

        // Track number of copies of each card (1-indexed)
        var copies = [];
        for (var i = 1; i <= numCards; i++) {
            arrayAppend(copies, 1);
        }

        // Process cards and cascade copies
        for (var i = 1; i <= numCards; i++) {
            var m = matches[i];
            // For each copy of this card, win copies of the next m cards
            for (var j = i + 1; j <= min(i + m, numCards); j++) {
                copies[j] += copies[i];
            }
        }

        // Sum total cards
        var total = 0;
        for (var c in copies) {
            total += c;
        }

        return total;
    }
}
