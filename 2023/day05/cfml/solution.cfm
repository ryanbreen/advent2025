<cfscript>
/**
 * Advent of Code 2023 - Day 5: If You Give A Seed A Fertilizer
 * ColdFusion (CFML) Solution
 *
 * Algorithm Overview:
 * - Part 1: Map each seed through a series of conversion maps to find locations
 * - Part 2: Treat seeds as ranges, split ranges at map boundaries, transform efficiently
 *
 * Key Insight: For Part 2, instead of iterating billions of seeds, we track ranges
 * and split them when they cross map boundaries. This reduces complexity from O(n) to O(maps).
 */

// Read and parse input file
inputPath = getDirectoryFromPath(getCurrentTemplatePath()) & "../input.txt";
inputContent = fileRead(inputPath);

/**
 * Parse the almanac input into seeds and conversion maps.
 *
 * @param rawText The raw input text from the puzzle
 * @return Struct with 'seeds' array and 'maps' array of range definitions
 */
function parseAlmanac(required string rawText) {
    // Normalize line endings (remove carriage returns for cross-platform compatibility)
    var normalizedText = replace(arguments.rawText, chr(13), "", "all");

    // Split input into sections (separated by blank lines)
    var sections = [];
    var currentSection = "";
    var lines = listToArray(normalizedText, chr(10), true);

    for (var line in lines) {
        if (len(trim(line)) == 0) {
            // Blank line marks end of section
            if (len(trim(currentSection)) > 0) {
                arrayAppend(sections, trim(currentSection));
                currentSection = "";
            }
        } else {
            // Accumulate lines into current section
            currentSection = len(currentSection) > 0
                ? currentSection & chr(10) & line
                : line;
        }
    }
    // Don't forget the final section (no trailing blank line)
    if (len(trim(currentSection)) > 0) {
        arrayAppend(sections, trim(currentSection));
    }

    var almanac = {
        seeds: [],
        maps: []
    };

    // Parse seed numbers from first section (format: "seeds: 79 14 55 13")
    var seedLine = sections[1];
    var seedValues = listToArray(trim(listRest(seedLine, ": ")), " ");
    for (var seedValue in seedValues) {
        if (len(trim(seedValue)) > 0) {
            arrayAppend(almanac.seeds, javaCast("long", trim(seedValue)));
        }
    }

    // Parse each conversion map (sections 2 through N)
    for (var sectionIndex = 2; sectionIndex <= arrayLen(sections); sectionIndex++) {
        var sectionContent = sections[sectionIndex];
        var sectionLines = listToArray(trim(sectionContent), chr(10));
        var mapRanges = [];

        // Skip header line (e.g., "seed-to-soil map:"), process range definitions
        for (var lineIndex = 2; lineIndex <= arrayLen(sectionLines); lineIndex++) {
            var rangeParts = listToArray(trim(sectionLines[lineIndex]), " ");
            if (arrayLen(rangeParts) == 3) {
                // Each line: destination_start source_start length
                arrayAppend(mapRanges, {
                    destinationStart: javaCast("long", trim(rangeParts[1])),
                    sourceStart: javaCast("long", trim(rangeParts[2])),
                    rangeLength: javaCast("long", trim(rangeParts[3]))
                });
            }
        }
        arrayAppend(almanac.maps, mapRanges);
    }

    return almanac;
}

/**
 * Apply a single conversion map to transform a value.
 * If the value falls within any range, apply the offset; otherwise return unchanged.
 *
 * @param value The value to transform
 * @param mapRanges Array of range definitions for this map
 * @return The transformed value
 */
function applyConversionMap(required numeric value, required array mapRanges) {
    for (var range in arguments.mapRanges) {
        var sourceEnd = range.sourceStart + range.rangeLength;
        if (arguments.value >= range.sourceStart && arguments.value < sourceEnd) {
            // Value is within this range - apply the offset
            return range.destinationStart + (arguments.value - range.sourceStart);
        }
    }
    // No matching range - value maps to itself
    return arguments.value;
}

/**
 * Convert a seed number to its final location through all conversion maps.
 * Chains through: seed -> soil -> fertilizer -> water -> light -> temperature -> humidity -> location
 *
 * @param seedNumber The initial seed number
 * @param conversionMaps Array of conversion maps to apply in order
 * @return The final location number
 */
function seedToLocation(required numeric seedNumber, required array conversionMaps) {
    var currentValue = arguments.seedNumber;
    for (var mapRanges in arguments.conversionMaps) {
        currentValue = applyConversionMap(currentValue, mapRanges);
    }
    return currentValue;
}

/**
 * Part 1: Find the lowest location for individual seed numbers.
 * Simply maps each seed through all conversions and finds the minimum.
 *
 * @param seedNumbers Array of seed numbers to check
 * @param conversionMaps Array of conversion maps
 * @return The minimum location number
 */
function solvePart1(required array seedNumbers, required array conversionMaps) {
    // Start with the first seed's location as our minimum
    var minimumLocation = seedToLocation(arguments.seedNumbers[1], arguments.conversionMaps);

    // Check remaining seeds for lower locations
    for (var seedIndex = 2; seedIndex <= arrayLen(arguments.seedNumbers); seedIndex++) {
        var location = seedToLocation(arguments.seedNumbers[seedIndex], arguments.conversionMaps);
        if (location < minimumLocation) {
            minimumLocation = location;
        }
    }

    return minimumLocation;
}

/**
 * Apply a conversion map to a list of value ranges.
 * Ranges are split at map boundaries and transformed appropriately.
 *
 * @param inputRanges Array of {rangeStart, rangeEnd} structs representing value ranges
 * @param mapRanges Array of range definitions for this conversion map
 * @return Array of transformed {rangeStart, rangeEnd} structs
 */
function applyMapToRanges(required array inputRanges, required array mapRanges) {
    var transformedRanges = [];

    for (var inputRange in arguments.inputRanges) {
        // Track portions of this range not yet matched by any map range
        var unmappedPortions = [{
            rangeStart: inputRange.rangeStart,
            rangeEnd: inputRange.rangeEnd
        }];

        // Process each map range to find overlaps
        for (var mapRange in arguments.mapRanges) {
            var sourceEnd = mapRange.sourceStart + mapRange.rangeLength;
            var newUnmappedPortions = [];

            for (var portion in unmappedPortions) {
                // Portion before the map range stays unmapped
                if (portion.rangeStart < mapRange.sourceStart) {
                    arrayAppend(newUnmappedPortions, {
                        rangeStart: portion.rangeStart,
                        rangeEnd: min(portion.rangeEnd, mapRange.sourceStart)
                    });
                }

                // Overlapping portion gets transformed
                var overlapStart = max(portion.rangeStart, mapRange.sourceStart);
                var overlapEnd = min(portion.rangeEnd, sourceEnd);
                if (overlapStart < overlapEnd) {
                    var offset = mapRange.destinationStart - mapRange.sourceStart;
                    arrayAppend(transformedRanges, {
                        rangeStart: overlapStart + offset,
                        rangeEnd: overlapEnd + offset
                    });
                }

                // Portion after the map range stays unmapped
                if (portion.rangeEnd > sourceEnd) {
                    arrayAppend(newUnmappedPortions, {
                        rangeStart: max(portion.rangeStart, sourceEnd),
                        rangeEnd: portion.rangeEnd
                    });
                }
            }

            unmappedPortions = newUnmappedPortions;
        }

        // Unmapped portions pass through unchanged (identity mapping)
        for (var remainingPortion in unmappedPortions) {
            arrayAppend(transformedRanges, remainingPortion);
        }
    }

    return transformedRanges;
}

/**
 * Part 2: Find the lowest location when seeds are interpreted as ranges.
 * Seeds are parsed as pairs: (start, length) defining ranges of seed numbers.
 *
 * @param seedNumbers Array of seed numbers (pairs: start, length, start, length, ...)
 * @param conversionMaps Array of conversion maps
 * @return The minimum location number across all seed ranges
 */
function solvePart2(required array seedNumbers, required array conversionMaps) {
    // Convert seed pairs into ranges: each pair is (start, length)
    var seedRanges = [];
    for (var pairIndex = 1; pairIndex <= arrayLen(arguments.seedNumbers); pairIndex += 2) {
        var rangeStart = arguments.seedNumbers[pairIndex];
        var rangeLength = arguments.seedNumbers[pairIndex + 1];
        arrayAppend(seedRanges, {
            rangeStart: rangeStart,
            rangeEnd: rangeStart + rangeLength
        });
    }

    // Apply each conversion map to transform the ranges
    var currentRanges = seedRanges;
    for (var mapRanges in arguments.conversionMaps) {
        currentRanges = applyMapToRanges(currentRanges, mapRanges);
    }

    // Find the minimum starting point among all final ranges
    var minimumLocation = currentRanges[1].rangeStart;
    for (var rangeIndex = 2; rangeIndex <= arrayLen(currentRanges); rangeIndex++) {
        if (currentRanges[rangeIndex].rangeStart < minimumLocation) {
            minimumLocation = currentRanges[rangeIndex].rangeStart;
        }
    }

    return minimumLocation;
}

// Main execution
almanac = parseAlmanac(inputContent);

part1Result = solvePart1(almanac.seeds, almanac.maps);
part2Result = solvePart2(almanac.seeds, almanac.maps);

writeOutput("Part 1: " & part1Result & chr(10));
writeOutput("Part 2: " & part2Result & chr(10));
</cfscript>
