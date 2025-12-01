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

(defn count-zeros [rotations]
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

(defn solve [input]
  "Solve the safe dial problem."
  (->> input
       str/split-lines
       (remove str/blank?)
       (map parse-rotation)
       count-zeros))

(defn -main []
  (let [input (slurp "/Users/wrb/fun/code/advent2025/day01/input.txt")
        answer (solve input)]
    (println "Part 2 Answer:" answer)))

(-main)
