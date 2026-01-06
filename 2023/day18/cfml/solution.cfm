<cfscript>
// Parse dig plan instructions
function parseInput(filename) {
    var inputText = fileRead(filename);
    var lines = listToArray(inputText.trim(), chr(10), false, true);
    var instructions = [];

    for (var line in lines) {
        var parts = listToArray(line, " ");
        var direction = parts[1];
        var distance = val(parts[2]);
        var color = mid(parts[3], 3, len(parts[3]) - 3); // Remove (# and )

        arrayAppend(instructions, {
            direction: direction,
            distance: distance,
            color: color
        });
    }

    return instructions;
}

// Calculate total area using Shoelace formula and Pick's theorem
function calculateArea(vertices, perimeter) {
    var n = arrayLen(vertices);
    var area = 0;

    // Shoelace formula for polygon area
    for (var i = 1; i <= n; i++) {
        var j = (i % n) + 1;
        area += vertices[i].r * vertices[j].c;
        area -= vertices[j].r * vertices[i].c;
    }
    area = abs(area) / 2;

    // Pick's theorem: A = i + b/2 - 1
    // Total points = interior + boundary = area + boundary/2 + 1
    return area + perimeter / 2 + 1;
}

// Part 1: Follow the dig plan directions
function part1(instructions) {
    var directionMap = {
        "R": {r: 0, c: 1},
        "D": {r: 1, c: 0},
        "L": {r: 0, c: -1},
        "U": {r: -1, c: 0}
    };

    var vertices = [];
    arrayAppend(vertices, {r: 0, c: 0});
    var perimeter = 0;
    var r = 0;
    var c = 0;

    for (var inst in instructions) {
        var delta = directionMap[inst.direction];
        r += delta.r * inst.distance;
        c += delta.c * inst.distance;
        arrayAppend(vertices, {r: r, c: c});
        perimeter += inst.distance;
    }

    return calculateArea(vertices, perimeter);
}

// Part 2: Decode instructions from hex color codes
function part2(instructions) {
    // Last digit of hex: 0=R, 1=D, 2=L, 3=U
    // First 5 digits: distance in hex
    var directionMap = {
        "0": {r: 0, c: 1},   // R
        "1": {r: 1, c: 0},   // D
        "2": {r: 0, c: -1},  // L
        "3": {r: -1, c: 0}   // U
    };

    var vertices = [];
    arrayAppend(vertices, {r: 0, c: 0});
    var perimeter = 0;
    var r = 0;
    var c = 0;

    for (var inst in instructions) {
        var color = inst.color;
        var distanceHex = left(color, 5);
        var directionCode = right(color, 1);

        // Convert hex to decimal
        var distance = inputBaseN(distanceHex, 16);
        var delta = directionMap[directionCode];

        r += delta.r * distance;
        c += delta.c * distance;
        arrayAppend(vertices, {r: r, c: c});
        perimeter += distance;
    }

    return calculateArea(vertices, perimeter);
}

// Main execution
var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
var inputPath = scriptDir & "../input.txt";
var instructions = parseInput(inputPath);

writeOutput("Part 1: " & part1(instructions) & chr(10));
writeOutput("Part 2: " & part2(instructions) & chr(10));
</cfscript>
