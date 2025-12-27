import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

/**
 * @typedef {Object} MappingRange
 * @property {number} destinationStart - Start of the destination range
 * @property {number} sourceStart - Start of the source range
 * @property {number} rangeLength - Length of the range
 */

/**
 * @typedef {Object} AlmanacData
 * @property {number[]} seeds - List of seed numbers
 * @property {MappingRange[][]} categoryMaps - Array of category mappings (seed-to-soil, soil-to-fertilizer, etc.)
 */

/**
 * @typedef {[number, number]} SeedRange
 * A tuple representing [startInclusive, endExclusive] of a seed range
 */

/**
 * Parses the almanac input text into structured data.
 *
 * @param {string} inputText - Raw input text from the puzzle
 * @returns {AlmanacData} Parsed seeds and category mappings
 */
function parseInput(inputText) {
  const sections = inputText.trim().split('\n\n');

  const seeds = sections[0]
    .split(': ')[1]
    .split(' ')
    .map(Number);

  const categoryMaps = sections.slice(1).map((section) => {
    const lines = section.trim().split('\n');
    return lines.slice(1).map((line) => {
      const [destinationStart, sourceStart, rangeLength] = line.split(' ').map(Number);
      return { destinationStart, sourceStart, rangeLength };
    });
  });

  return { seeds, categoryMaps };
}

/**
 * Applies a single category map to transform a source value to a destination value.
 * If no mapping range contains the value, returns the value unchanged (identity mapping).
 *
 * @param {number} sourceValue - The value to transform
 * @param {MappingRange[]} mappingRanges - Array of mapping ranges for this category
 * @returns {number} The transformed destination value
 */
function applyCategoryMap(sourceValue, mappingRanges) {
  for (const { destinationStart, sourceStart, rangeLength } of mappingRanges) {
    const sourceEnd = sourceStart + rangeLength;
    if (sourceValue >= sourceStart && sourceValue < sourceEnd) {
      const offset = sourceValue - sourceStart;
      return destinationStart + offset;
    }
  }
  return sourceValue;
}

/**
 * Converts a seed number to its final location by applying all category mappings
 * in sequence (seed -> soil -> fertilizer -> water -> light -> temperature -> humidity -> location).
 *
 * @param {number} seedNumber - The initial seed number
 * @param {MappingRange[][]} categoryMaps - All category mappings in order
 * @returns {number} The final location number
 */
function seedToLocation(seedNumber, categoryMaps) {
  let currentValue = seedNumber;
  for (const mappingRanges of categoryMaps) {
    currentValue = applyCategoryMap(currentValue, mappingRanges);
  }
  return currentValue;
}

/**
 * Solves Part 1: Find the lowest location number for any of the individual seed numbers.
 *
 * @param {number[]} seeds - List of individual seed numbers
 * @param {MappingRange[][]} categoryMaps - All category mappings
 * @returns {number} The minimum location number
 */
function part1(seeds, categoryMaps) {
  const locationNumbers = seeds.map((seed) => seedToLocation(seed, categoryMaps));
  return Math.min(...locationNumbers);
}

/**
 * Applies a category map to a collection of seed ranges, transforming them to destination ranges.
 * Handles range splitting when source ranges partially overlap with mapping ranges.
 *
 * @param {SeedRange[]} inputRanges - Array of [start, end) ranges to transform
 * @param {MappingRange[]} mappingRanges - Mapping ranges for this category
 * @returns {SeedRange[]} Transformed destination ranges
 */
function applyCategoryMapToRanges(inputRanges, mappingRanges) {
  const transformedRanges = [];

  for (const [rangeStart, rangeEnd] of inputRanges) {
    let unmappedSegments = [[rangeStart, rangeEnd]];

    for (const { destinationStart, sourceStart, rangeLength } of mappingRanges) {
      const sourceEnd = sourceStart + rangeLength;
      const mappingOffset = destinationStart - sourceStart;
      const nextUnmappedSegments = [];

      for (const [segmentStart, segmentEnd] of unmappedSegments) {
        // Segment portion before the mapping source range (remains unmapped)
        if (segmentStart < sourceStart) {
          const beforeEnd = Math.min(segmentEnd, sourceStart);
          nextUnmappedSegments.push([segmentStart, beforeEnd]);
        }

        // Segment portion overlapping with the mapping source range (gets transformed)
        const overlapStart = Math.max(segmentStart, sourceStart);
        const overlapEnd = Math.min(segmentEnd, sourceEnd);
        if (overlapStart < overlapEnd) {
          transformedRanges.push([
            overlapStart + mappingOffset,
            overlapEnd + mappingOffset,
          ]);
        }

        // Segment portion after the mapping source range (remains unmapped)
        if (segmentEnd > sourceEnd) {
          const afterStart = Math.max(segmentStart, sourceEnd);
          nextUnmappedSegments.push([afterStart, segmentEnd]);
        }
      }

      unmappedSegments = nextUnmappedSegments;
    }

    // Any segments that didn't match any mapping range use identity mapping
    transformedRanges.push(...unmappedSegments);
  }

  return transformedRanges;
}

/**
 * Solves Part 2: Find the lowest location number where seeds are interpreted as ranges.
 * Each pair of numbers represents (start, length) of a seed range.
 *
 * @param {number[]} seeds - Seed data as pairs of [start, length]
 * @param {MappingRange[][]} categoryMaps - All category mappings
 * @returns {number} The minimum location number from any seed in any range
 */
function part2(seeds, categoryMaps) {
  // Convert seed pairs to [start, end) ranges
  const seedRanges = [];
  for (let i = 0; i < seeds.length; i += 2) {
    const rangeStart = seeds[i];
    const rangeLength = seeds[i + 1];
    seedRanges.push([rangeStart, rangeStart + rangeLength]);
  }

  // Apply each category map to transform all ranges through the pipeline
  let currentRanges = seedRanges;
  for (const mappingRanges of categoryMaps) {
    currentRanges = applyCategoryMapToRanges(currentRanges, mappingRanges);
  }

  // Find the minimum starting value across all final location ranges
  const rangeStarts = currentRanges.map(([start]) => start);
  return Math.min(...rangeStarts);
}

// Main execution
const inputPath = join(__dirname, '..', 'input.txt');
const inputText = readFileSync(inputPath, 'utf-8');

const { seeds, categoryMaps } = parseInput(inputText);

console.log('Part 1:', part1(seeds, categoryMaps));
console.log('Part 2:', part2(seeds, categoryMaps));
