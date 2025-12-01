(ns test-solution
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

(defn count-zeros-with-trace [rotations]
  "Count how many times the dial lands on 0 after processing all rotations, with trace output."
  (println "Starting at position 50")
  (loop [position 50
         remaining rotations
         zero-count 0]
    (if (empty? remaining)
      zero-count
      (let [[direction distance] (first remaining)
            new-position (rotate-dial position direction distance)
            new-zero-count (if (zero? new-position)
                             (inc zero-count)
                             zero-count)]
        (if (zero? new-position)
          (println (str direction distance) "->" new-position "(ZERO!)")
          (println (str direction distance) "->" new-position))
        (recur new-position (rest remaining) new-zero-count)))))

(defn solve-with-trace [input]
  "Solve the safe dial problem with trace output."
  (->> input
       str/split-lines
       (remove str/blank?)
       (map parse-rotation)
       count-zeros-with-trace))

(defn -main []
  (let [example "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"
        answer (solve-with-trace example)]
    (println "\nExpected: 3")
    (println "Got:" answer)))

(-main)
