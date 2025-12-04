(ns solution
  (:require [clojure.string :as str]))

(defn parse-input [filename]
  "Parse input file into two lists of numbers"
  (let [lines (-> filename slurp str/trim (str/split #"\n"))
        pairs (map #(str/split % #"\s+") lines)
        left (mapv #(parse-long (first %)) pairs)
        right (mapv #(parse-long (second %)) pairs)]
    [left right]))

(defn part1 [left right]
  "Sort both lists and calculate total distance between paired elements"
  (let [sorted-left (sort left)
        sorted-right (sort right)
        distances (map (fn [l r] (abs (- l r))) sorted-left sorted-right)]
    (reduce + distances)))

(defn part2 [left right]
  "Calculate similarity score: sum of (left number * count in right list)"
  (let [right-freqs (frequencies right)]
    (reduce + (map (fn [num] (* num (get right-freqs num 0))) left))))

(defn -main []
  (let [[left right] (parse-input "../input.txt")]
    (println (str "Part 1: " (part1 left right)))
    (println (str "Part 2: " (part2 left right)))))

(-main)
