<cfscript>
    // Get the directory of this script
    scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
    inputPath = scriptDir & "../input.txt";

    // Read the input file
    input = fileRead(inputPath);

    // Create solution instance and solve
    solution = new Solution();

    // Part 1
    result1 = solution.part1(input);
    writeOutput("Part 1: " & result1 & chr(10));

    // Day 25 has no Part 2
    writeOutput("Part 2: No Part 2 on Day 25!" & chr(10));
</cfscript>
