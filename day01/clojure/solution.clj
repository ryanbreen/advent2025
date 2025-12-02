(ns solution
  (:require [clojure.string :as str]))

(defn parse-rotation [line]
  "Parse a rotation like 'L47' or 'R26' into [direction distance]"
  (let [direction (first line)
        distance (parse-long (subs line 1))]
    [direction distance]))

(defn rotate-dial [current direction distance]
  "Rotate the dial from current position by distance in given direction.
   The dial has positions 0-99 and wraps around."
  (let [new-pos (if (= direction \L)
                  (- current distance)
                  (+ current distance))]
    (mod new-pos 100)))

(defn count-zeros-during-rotation [start direction distance]
  "Count how many times the dial passes through 0 during a rotation.
   This includes the final position if it's 0."
  (let [clicks (range 1 (inc distance))]
    (count (filter (fn [click]
                     (let [pos (if (= direction \L)
                                 (mod (- start click) 100)
                                 (mod (+ start click) 100))]
                       (zero? pos)))
                   clicks))))

;; Part 1: Count how many times the dial ends at 0 after a rotation
(defn solve-part1 [rotations]
  "Count how many times the dial ends at 0 after any rotation."
  (loop [position 50
         remaining rotations
         zero-count 0]
    (if (empty? remaining)
      zero-count
      (let [[direction distance] (first remaining)
            new-position (rotate-dial position direction distance)
            new-count (if (zero? new-position) (inc zero-count) zero-count)]
        (recur new-position (rest remaining) new-count)))))

;; Part 2: Count how many times the dial passes through 0 during all rotations
(defn solve-part2 [rotations]
  "Count how many times the dial passes through 0 during all rotations."
  (loop [position 50
         remaining rotations
         zero-count 0]
    (if (empty? remaining)
      zero-count
      (let [[direction distance] (first remaining)
            zeros-in-rotation (count-zeros-during-rotation position direction distance)
            new-position (rotate-dial position direction distance)]
        (recur new-position (rest remaining) (+ zero-count zeros-in-rotation))))))

(defn parse-input [input]
  "Parse input into list of rotations."
  (->> input
       str/split-lines
       (remove str/blank?)
       (map parse-rotation)))

(defn -main []
  (let [input (slurp "/Users/wrb/fun/code/advent2025/day01/input.txt")
        rotations (parse-input input)
        part1 (solve-part1 rotations)
        part2 (solve-part2 rotations)]
    (println "Part 1:" part1)
    (println "Part 2:" part2)))

(-main)
