(ns solution
  "Advent of Code 2023 Day 5: If You Give A Seed A Fertilizer

   Transforms seed numbers through a series of category mappings
   to find the lowest location number."
  (:require [clojure.string :as str]))

(def input (slurp "../input.txt"))

(defn parse-seeds
  "Parse the seeds line into a sequence of seed numbers."
  [line]
  (->> line
       (re-seq #"\d+")
       (map parse-long)))

(defn parse-map-line
  "Parse a single map line into [dest-start src-start length] vector."
  [line]
  (->> line
       str/trim
       (re-seq #"\d+")
       (mapv parse-long)))

(defn parse-map-section
  "Parse a map section into a vector of range mappings.
   Each mapping is [dest-start src-start length]."
  [section]
  (->> section
       str/split-lines
       rest
       (mapv parse-map-line)))

(defn parse-input
  "Parse the full input text into [seeds maps].

   Returns a vector where:
   - seeds is a sequence of seed numbers
   - maps is a vector of mapping tables, each containing range mappings"
  [text]
  (let [[seeds-section & map-sections] (-> text str/trim (str/split #"\n\n"))]
    [(parse-seeds seeds-section)
     (mapv parse-map-section map-sections)]))

(defn find-mapping
  "Find the applicable mapping for a value, if any.
   Returns the matching [dest-start src-start length] or nil."
  [value ranges]
  (some (fn [[_ src-start length :as mapping]]
          (when (and (>= value src-start)
                     (< value (+ src-start length)))
            mapping))
        ranges))

(defn apply-map-value
  "Apply a single map to transform a value.
   If no mapping matches, returns the value unchanged."
  [value ranges]
  (if-let [[dest-start src-start _] (find-mapping value ranges)]
    (+ dest-start (- value src-start))
    value))

(defn seed-to-location
  "Convert a seed number to its final location through all maps."
  [seed maps]
  (reduce apply-map-value seed maps))

(defn part1
  "Find the lowest location number for any initial seed."
  [seeds maps]
  (transduce (map #(seed-to-location % maps))
             min
             Long/MAX_VALUE
             seeds))

(defn split-range-by-mapping
  "Split an input range by a single mapping range.

   Returns a map with:
   - :mapped - the portion that was mapped (or nil)
   - :unmapped - portions that remain unmapped (may be empty)"
  [[range-start range-end] [dest-start src-start length]]
  (let [src-end (+ src-start length)
        offset  (- dest-start src-start)]
    {:mapped   (let [overlap-start (max range-start src-start)
                     overlap-end   (min range-end src-end)]
                 (when (< overlap-start overlap-end)
                   [(+ overlap-start offset) (+ overlap-end offset)]))
     :unmapped (cond-> []
                 ;; Portion before the mapping
                 (< range-start src-start)
                 (conj [range-start (min range-end src-start)])
                 ;; Portion after the mapping
                 (> range-end src-end)
                 (conj [(max range-start src-end) range-end]))}))

(defn process-range-through-mappings
  "Process a single input range through all mappings in a map section.
   Returns [mapped-ranges unmapped-ranges]."
  [input-range map-ranges]
  (loop [[current-mapping & remaining-mappings] map-ranges
         pending-ranges [input-range]
         mapped-results []]
    (if (or (nil? current-mapping) (empty? pending-ranges))
      [mapped-results pending-ranges]
      (let [split-results (map #(split-range-by-mapping % current-mapping) pending-ranges)
            newly-mapped  (into mapped-results (keep :mapped split-results))
            still-pending (into [] (mapcat :unmapped) split-results)]
        (recur remaining-mappings still-pending newly-mapped)))))

(defn apply-map-to-ranges
  "Apply a mapping table to a collection of ranges.
   Unmapped portions pass through unchanged."
  [input-ranges map-ranges]
  (reduce (fn [acc input-range]
            (let [[mapped unmapped] (process-range-through-mappings input-range map-ranges)]
              (into acc (concat mapped unmapped))))
          []
          input-ranges))

(defn seeds->ranges
  "Convert seed pairs [start length ...] into ranges [[start end] ...]."
  [seeds]
  (->> seeds
       (partition 2)
       (mapv (fn [[start length]] [start (+ start length)]))))

(defn part2
  "Find the lowest location number considering seeds as ranges.

   Seeds are interpreted as pairs [start length], defining ranges
   of consecutive seed numbers."
  [seeds maps]
  (let [seed-ranges (seeds->ranges seeds)
        final-ranges (reduce apply-map-to-ranges seed-ranges maps)]
    (transduce (map first) min Long/MAX_VALUE final-ranges)))

;; Main execution
(let [[seeds maps] (parse-input input)]
  (println "Part 1:" (part1 seeds maps))
  (println "Part 2:" (part2 seeds maps)))
