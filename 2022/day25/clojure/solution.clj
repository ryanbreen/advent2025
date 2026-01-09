#!/usr/bin/env bb
(ns solution
  (:require [clojure.string :as str]))

(def digit-values
  "Map SNAFU characters to their decimal values."
  {\2 2, \1 1, \0 0, \- -1, \= -2})

(def value-digits
  "Map remainders to SNAFU digits and carry adjustment."
  {0 [\0 0], 1 [\1 0], 2 [\2 0], 3 [\= 1], 4 [\- 1]})

(defn snafu->decimal
  "Convert a SNAFU string to decimal."
  [s]
  (reduce (fn [acc c]
            (+ (* acc 5) (digit-values c)))
          0
          s))

(defn decimal->snafu
  "Convert a decimal number to SNAFU."
  [n]
  (if (zero? n)
    "0"
    (loop [n n
           digits []]
      (if (zero? n)
        (apply str (reverse digits))
        (let [remainder (mod n 5)
              [digit carry] (value-digits remainder)]
          (recur (+ (quot n 5) carry)
                 (conj digits digit)))))))

(defn part1
  "Sum all SNAFU numbers and return result as SNAFU."
  [lines]
  (->> lines
       (map snafu->decimal)
       (reduce +)
       decimal->snafu))

(defn -main []
  (let [script-dir (-> *file*
                       (java.io.File.)
                       (.getParentFile)
                       (.getAbsolutePath))
        input-file (str script-dir "/../input.txt")
        lines (->> (slurp input-file)
                   str/trim
                   str/split-lines)]
    (println "Part 1:" (part1 lines))
    (println "Part 2: No Part 2 on Day 25!")))

(-main)
