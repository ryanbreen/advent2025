<cfscript>
    solution = new Solution();

    scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
    inputFile = scriptDir & "../input.txt";

    pairs = solution.parseInput(inputFile);

    writeOutput("Part 1: " & solution.part1(pairs) & chr(10));
    writeOutput("Part 2: " & solution.part2(pairs) & chr(10));
</cfscript>
