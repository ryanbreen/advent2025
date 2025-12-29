#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(defn parse-grid
  "Parse the grid and return galaxy positions as [row col] pairs."
  [lines]
  (->> lines
       (map-indexed (fn [r line]
                      (->> line
                           (map-indexed (fn [c ch]
                                          (when (= \# ch) [r c])))
                           (filter some?))))
       (apply concat)))

(defn find-empty-rows
  "Find row indices that contain no galaxies."
  [lines]
  (->> lines
       (map-indexed (fn [idx line]
                      (when (not (str/includes? line "#"))
                        idx)))
       (filter some?)
       set))

(defn find-empty-cols
  "Find column indices that contain no galaxies."
  [lines]
  (let [cols (if (seq lines) (count (first lines)) 0)]
    (->> (range cols)
         (filter (fn [c]
                   (not-any? #(= \# (.charAt ^String % c)) lines)))
         set)))

(defn calculate-distance
  "Calculate Manhattan distance between two galaxies with expansion."
  [[r1 c1] [r2 c2] empty-rows empty-cols expansion-factor]
  (let [min-r (min r1 r2)
        max-r (max r1 r2)
        min-c (min c1 c2)
        max-c (max c1 c2)
        row-expansion (->> (range min-r max-r)
                           (filter empty-rows)
                           count)
        col-expansion (->> (range min-c max-c)
                           (filter empty-cols)
                           count)
        row-dist (+ (- max-r min-r) (* row-expansion (dec expansion-factor)))
        col-dist (+ (- max-c min-c) (* col-expansion (dec expansion-factor)))]
    (+ row-dist col-dist)))

(defn all-pairs
  "Generate all unique pairs from a collection."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (for [i (range n)
          j (range (inc i) n)]
      [(v i) (v j)])))

(defn calculate-distances
  "Calculate sum of Manhattan distances between all pairs of galaxies."
  [galaxies empty-rows empty-cols expansion-factor]
  (->> (all-pairs galaxies)
       (map (fn [[g1 g2]]
              (calculate-distance g1 g2 empty-rows empty-cols expansion-factor)))
       (reduce +)))

(defn solve
  "Solve for a given expansion factor."
  [lines expansion-factor]
  (let [galaxies (parse-grid lines)
        empty-rows (find-empty-rows lines)
        empty-cols (find-empty-cols lines)]
    (calculate-distances galaxies empty-rows empty-cols expansion-factor)))

(defn part1
  "Solve Part 1 - expansion factor of 2."
  [lines]
  (solve lines 2))

(defn part2
  "Solve Part 2 - expansion factor of 1,000,000."
  [lines]
  (solve lines 1000000))

(defn -main []
  (let [input-file (or (first *command-line-args*) "../input.txt")
        content (slurp input-file)
        lines (vec (remove empty? (str/split-lines content)))]
    (println "Part 1:" (part1 lines))
    (println "Part 2:" (part2 lines))))

(-main)
