(ns solution
  (:require [clojure.string :as str]))

(defn invalid-id?
  "Check if a number is an invalid ID (a sequence of digits repeated at least twice)."
  [n]
  (let [s (str n)
        len (count s)]
    (when (pos? len)
      ;; Try all possible pattern lengths from 1 to len/2
      (some (fn [pattern-len]
              (when (zero? (mod len pattern-len))
                (let [pattern (subs s 0 pattern-len)
                      repetitions (/ len pattern-len)]
                  ;; Must be repeated at least twice, and pattern must not have leading zeros
                  (and (>= repetitions 2)
                       (not (str/starts-with? pattern "0"))
                       ;; Check if the whole string is this pattern repeated
                       (= s (apply str (repeat repetitions pattern)))))))
            (range 1 (inc (quot len 2)))))))

(defn count-invalid-ids-in-range
  "Count and sum all invalid IDs in the range [start, end]."
  [start end]
  (reduce (fn [sum n]
            (if (invalid-id? n)
              (+ sum n)
              sum))
          0
          (range start (inc end))))

(defn parse-range
  "Parse a range string like '11-22' into [11 22]."
  [range-str]
  (let [[start end] (str/split range-str #"-")]
    [(parse-long start) (parse-long end)]))

(defn solve
  "Solve the problem by parsing input and summing all invalid IDs."
  [input]
  (let [ranges (-> input
                   str/trim
                   (str/split #","))
        invalid-sum (reduce (fn [total range-str]
                              (let [[start end] (parse-range range-str)]
                                (+ total (count-invalid-ids-in-range start end))))
                            0
                            ranges)]
    invalid-sum))

(defn -main []
  (let [input (slurp "/Users/wrb/fun/code/advent2025/day02/input.txt")
        answer (solve input)]
    (println "Part 2 Answer:" answer)))

(-main)
