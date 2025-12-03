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
   Uses O(1) mathematical calculation instead of iterating through positions."
  (if (= direction \L)
    ;; Moving left (toward lower numbers)
    ;; We hit 0 after exactly 'start' steps, then every 100 steps after that
    (cond
      (and (pos? start) (>= distance start))
      (inc (quot (- distance start) 100))

      (and (zero? start) (>= distance 100))
      (quot distance 100)

      :else 0)

    ;; Moving right (toward higher numbers)
    ;; We hit 0 after (100 - start) steps, then every 100 steps after that
    (let [steps-to-zero (- 100 start)]
      (cond
        (and (pos? start) (>= distance steps-to-zero))
        (inc (quot (- distance steps-to-zero) 100))

        (and (zero? start) (>= distance 100))
        (quot distance 100)

        :else 0))))

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
