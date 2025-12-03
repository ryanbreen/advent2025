(ns solution
  (:require [clojure.string :as str]))

;; Part 1: Check if a number is an invalid ID (pattern repeated EXACTLY twice)
(defn invalid-id-part1?
  "Check if a number is an invalid ID (a sequence of digits repeated exactly twice)."
  [n]
  (let [s (str n)
        len (count s)]
    (when (and (pos? len) (even? len))
      (let [half (quot len 2)
            first-half (subs s 0 half)
            second-half (subs s half)]
        (= first-half second-half)))))

;; Part 2: Check if a number is an invalid ID (pattern repeated at least twice)
(defn invalid-id-part2?
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

(defn sum-invalid-ids-in-range
  "Sum all invalid IDs in the range [start, end] using the given predicate."
  [pred start end]
  (reduce (fn [sum n]
            (if (pred n)
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
  [input pred]
  (let [ranges (-> input
                   str/trim
                   (str/split #","))
        invalid-sum (reduce (fn [total range-str]
                              (let [[start end] (parse-range range-str)]
                                (+ total (sum-invalid-ids-in-range pred start end))))
                            0
                            ranges)]
    invalid-sum))

(defn -main []
  (let [input (slurp "../input.txt")
        part1 (solve input invalid-id-part1?)
        part2 (solve input invalid-id-part2?)]
    (println "Part 1:" part1)
    (println "Part 2:" part2)))

(-main)
