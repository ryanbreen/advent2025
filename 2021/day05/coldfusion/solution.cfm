<cfscript>
    solution = new Solution();

    // Get the directory where this script is located
    scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
    inputPath = scriptDir & "../input.txt";
    segments = solution.parseInput(inputPath);

    part1Result = solution.part1(segments);
    part2Result = solution.part2(segments);

    writeOutput("Part 1: " & part1Result & chr(10));
    writeOutput("Part 2: " & part2Result & chr(10));
</cfscript>
