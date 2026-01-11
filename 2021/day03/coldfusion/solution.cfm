<cfscript>
    solution = new Solution();

    // Get the directory where this script is located
    scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
    inputPath = scriptDir & "../input.txt";
    numbers = solution.parseInput(inputPath);

    part1Result = solution.part1(numbers);
    part2Result = solution.part2(numbers);

    writeOutput("Part 1: " & part1Result & chr(10));
    writeOutput("Part 2: " & part2Result & chr(10));
</cfscript>
