#!/usr/bin/env bb
;;
;; Day 12: Christmas Tree Farm - Polyomino Packing
;;
;; The solution checks if presents (polyominoes) can fit into rectangular regions.
;; For this problem, the constraint is simply: total cells needed <= available cells.

(require '[clojure.string :as str])

(defn count-cells
  "Count # characters in shape lines."
  [lines]
  (->> lines
       (map #(count (filter #{\#} %)))
       (reduce +)))

(defn parse-shape
  "Parse a shape definition section."
  [lines]
  (let [idx (-> lines first (str/replace #":" "") parse-long)
        cell-count (count-cells (rest lines))]
    {idx cell-count}))

(defn parse-region
  "Parse a single region line (e.g., '12x5: 1 0 1 0 2 2')."
  [line]
  (let [[dims-part counts-part] (str/split line #":")
        [w h] (->> (str/split (str/trim dims-part) #"x")
                   (map parse-long))
        counts (->> (str/split (str/trim counts-part) #"\s+")
                    (mapv parse-long))]
    [w h counts]))

(defn parse-regions
  "Parse all region lines from a section."
  [lines]
  (->> lines
       (filter #(str/includes? % "x"))
       (map parse-region)))

(defn shape-section?
  "Check if a section defines shapes (not regions)."
  [first-line]
  (and (str/includes? first-line ":")
       (not (str/includes? first-line "x"))))

(defn parse-input
  "Parse input into shapes and regions."
  [text]
  (->> (str/split (str/trim text) #"\n\n")
       (map #(str/split-lines (str/trim %)))
       (reduce
        (fn [acc lines]
          (if (shape-section? (first lines))
            (update acc :shapes merge (parse-shape lines))
            (update acc :regions concat (parse-regions lines))))
        {:shapes {} :regions []})))

(defn can-fit-region?
  "Check if all presents can fit in the region."
  [shape-sizes [width height counts]]
  (let [total-cells-needed (->> counts
                                (map-indexed (fn [i cnt]
                                               (* cnt (get shape-sizes i 0))))
                                (reduce +))
        available-cells (* width height)]
    (<= total-cells-needed available-cells)))

(defn part1
  "Count regions that can fit all their presents."
  [shapes regions]
  (->> regions
       (filter (partial can-fit-region? shapes))
       count))

(defn part2
  "Part 2 is just a button click to finish - no computation needed."
  []
  0)

(defn -main []
  (let [text (slurp "../input.txt")
        {:keys [shapes regions]} (parse-input text)]
    (println "Part 1:" (part1 shapes regions))
    (println "Part 2:" (part2))))

(-main)
