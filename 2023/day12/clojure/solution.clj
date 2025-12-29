#!/usr/bin/env clojure -M

(ns solution
  "Advent of Code 2023 Day 12: Hot Springs - Spring arrangement counting with DP"
  (:require [clojure.string :as str]))

(defn parse-line
  "Parse a line into [pattern groups] where groups is a vector of integers."
  [line]
  (let [[pattern-str groups-str] (str/split (str/trim line) #"\s+")]
    [pattern-str
     (->> (str/split groups-str #",")
          (mapv #(Integer/parseInt %)))]))

(def operational? #{\. \?})
(def damaged? #{\# \?})

(defn count-arrangements
  "Count valid arrangements using memoized DP.
   State: [position group-index current-run-length]"
  [pattern groups]
  (let [n (count pattern)
        m (count groups)
        cache (atom {})]
    (letfn [(dp [pos group-idx current-run]
              (if-let [cached (find @cache [pos group-idx current-run])]
                (val cached)
                (let [result
                      (if (= pos n)
                        ;; Base case: reached end of pattern
                        (if (or (and (= group-idx m) (zero? current-run))
                                (and (= group-idx (dec m))
                                     (= (groups group-idx) current-run)))
                          1
                          0)
                        ;; Recursive case
                        (let [ch (nth pattern pos)]
                          (+ ;; Option 1: Place operational spring (.)
                             (if (operational? ch)
                               (cond
                                 (zero? current-run)
                                 (dp (inc pos) group-idx 0)

                                 (and (< group-idx m)
                                      (= (groups group-idx) current-run))
                                 (dp (inc pos) (inc group-idx) 0)

                                 :else 0)
                               0)
                             ;; Option 2: Place damaged spring (#)
                             (if (and (damaged? ch)
                                      (< group-idx m)
                                      (< current-run (groups group-idx)))
                               (dp (inc pos) group-idx (inc current-run))
                               0))))]
                  (swap! cache assoc [pos group-idx current-run] result)
                  result)))]
      (dp 0 0 0))))

(defn unfold
  "Unfold pattern and groups by repeating 5 times."
  [[pattern groups]]
  [(str/join "?" (repeat 5 pattern))
   (vec (apply concat (repeat 5 groups)))])

(defn part1
  "Sum of arrangement counts for all rows."
  [lines]
  (->> lines
       (map parse-line)
       (map (fn [[pattern groups]] (count-arrangements pattern groups)))
       (reduce +)))

(defn part2
  "Sum of arrangement counts for all rows after unfolding."
  [lines]
  (->> lines
       (map parse-line)
       (map unfold)
       (map (fn [[pattern groups]] (count-arrangements pattern groups)))
       (reduce +)))

(defn -main []
  (let [input-file (or (first *command-line-args*) "../input.txt")
        lines (->> (slurp input-file)
                   str/split-lines
                   (remove str/blank?)
                   vec)]
    (println "Part 1:" (part1 lines))
    (println "Part 2:" (part2 lines))))

(-main)
