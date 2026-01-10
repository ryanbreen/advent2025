(ns solution
  (:require [clojure.string :as str]))

(def depths
  (->> (slurp "../input.txt")
       str/trim
       str/split-lines
       (map #(Integer/parseInt %))))

(defn part1
  "Count the number of times a depth measurement increases from the previous."
  []
  (->> (partition 2 1 depths)
       (filter (fn [[a b]] (> b a)))
       count))

(defn part2
  "Count increases in 3-measurement sliding window sums."
  []
  (let [window-sums (->> (partition 3 1 depths)
                         (map #(apply + %)))]
    (->> (partition 2 1 window-sums)
         (filter (fn [[a b]] (> b a)))
         count)))

(println (str "Part 1: " (part1)))
(println (str "Part 2: " (part2)))
