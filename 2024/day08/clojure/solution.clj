#!/usr/bin/env bb
;; Babashka (bb) provides fast startup and native executable compilation for Clojure scripts

(require '[clojure.string :as str])

(defn parse-input
  "Parse input file into {:rows :cols :antennas} map.
   Returns antennas as map of frequency char to [[r c]] position vectors."
  [filename]
  (let [lines (str/split-lines (slurp filename))
        rows (count lines)
        cols (count (first lines))
        antennas (->> lines
                      (map-indexed vector)
                      (reduce (fn [acc [r row]]
                                (reduce (fn [acc2 [c ch]]
                                          (if (not= ch \.)
                                            (update acc2 ch (fnil conj []) [r c])
                                            acc2))
                                        acc
                                        (map-indexed vector row)))
                              {}))]
    {:rows rows :cols cols :antennas antennas}))

(defn in-bounds?
  "Check if position [r c] is within grid bounds."
  [rows cols [r c]]
  (and (>= r 0) (< r rows) (>= c 0) (< c cols)))

(defn combinations
  "Generate all unique pairs from collection."
  [coll]
  (for [i (range (count coll))
        j (range (inc i) (count coll))]
    [(nth coll i) (nth coll j)]))

(defn find-antinodes-part1
  "Find antinodes for a pair of antennas using 2:1 distance ratio.
   For antennas at p1 and p2, antinodes appear at positions where one antenna
   is twice as far as the other."
  [rows cols [[r1 c1] [r2 c2]]]
  ;; Antinode positions using vector arithmetic:
  ;; - Position beyond p1: 2*p1 - p2 (twice as far from p2 as p1)
  ;; - Position beyond p2: 2*p2 - p1 (twice as far from p1 as p2)
  (let [ar1 (- (* 2 r1) r2)
        ac1 (- (* 2 c1) c2)
        ar2 (- (* 2 r2) r1)
        ac2 (- (* 2 c2) c1)]
    (cond-> []
      (in-bounds? rows cols [ar1 ac1]) (conj [ar1 ac1])
      (in-bounds? rows cols [ar2 ac2]) (conj [ar2 ac2]))))

(defn points-along-line
  "Generate all points along a line from [r c] in direction [dr dc] until out of bounds."
  [rows cols [r c] [dr dc]]
  (->> (iterate (fn [[r c]] [(+ r dr) (+ c dc)]) [r c])
       (take-while #(in-bounds? rows cols %))))

(defn find-antinodes-part2
  "Find antinodes for Part 2 using harmonic resonance.
   Antinodes occur at all grid positions along the line defined by the two antennas."
  [rows cols [[r1 c1] [r2 c2]]]
  ;; Calculate direction vector from p1 to p2
  (let [dr (- r2 r1)
        dc (- c2 c1)]
    ;; Generate points in both directions along the line
    (concat
      (points-along-line rows cols [r1 c1] [dr dc])
      (points-along-line rows cols [(- r1 dr) (- c1 dc)] [(- dr) (- dc)]))))

(defn solve
  "Solve using given antinode-finding function.
   Groups antennas by frequency, finds all pairs, computes antinodes, and counts unique positions."
  [antenna-fn]
  (let [{:keys [rows cols antennas]} (parse-input "../input.txt")]
    (->> antennas
         vals
         (mapcat combinations)
         (mapcat (partial antenna-fn rows cols))
         (into #{})
         count)))

(defn part1
  "Solve Part 1: antinodes at 2:1 distance ratio."
  []
  (solve find-antinodes-part1))

(defn part2
  "Solve Part 2: antinodes at all positions along antenna lines."
  []
  (solve find-antinodes-part2))

(defn -main
  "Main entry point - runs both parts and prints results."
  []
  (println "Part 1:" (part1))
  (println "Part 2:" (part2)))

(-main)
