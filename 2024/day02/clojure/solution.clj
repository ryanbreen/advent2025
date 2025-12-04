(ns solution
  (:require [clojure.string :as str]))

(defn parse-input [filename]
  "Parse input file into list of reports (each report is a vector of numbers)"
  (let [lines (-> filename slurp str/trim (str/split #"\n"))]
    (mapv (fn [line]
            (mapv parse-long (str/split line #"\s+")))
          lines)))

(defn all-increasing? [levels]
  "Check if all adjacent pairs are increasing by 1-3"
  (every? (fn [[a b]] (and (< a b) (<= (- b a) 3)))
          (partition 2 1 levels)))

(defn all-decreasing? [levels]
  "Check if all adjacent pairs are decreasing by 1-3"
  (every? (fn [[a b]] (and (> a b) (<= (- a b) 3)))
          (partition 2 1 levels)))

(defn is-safe? [levels]
  "Check if a report is safe"
  (or (all-increasing? levels)
      (all-decreasing? levels)))

(defn is-safe-with-dampener? [levels]
  "Check if a report is safe, or can be made safe by removing one level"
  (if (is-safe? levels)
    true
    ;; Try removing each level one at a time
    (some (fn [i]
            (let [modified (vec (concat (subvec levels 0 i)
                                       (subvec levels (inc i))))]
              (is-safe? modified)))
          (range (count levels)))))

(defn part1 [reports]
  "Count safe reports"
  (count (filter is-safe? reports)))

(defn part2 [reports]
  "Count reports that are safe or can be made safe with Problem Dampener"
  (count (filter is-safe-with-dampener? reports)))

(defn -main []
  (let [reports (parse-input "../input.txt")]
    (println (str "Part 1: " (part1 reports)))
    (println (str "Part 2: " (part2 reports)))))

(-main)
