(ns solution
  (:require [clojure.string :as str]))

(defn read-input []
  (slurp "../input.txt"))

(defn parse-numbers [line]
  (->> (str/split line #":")
       second
       str/trim
       (#(str/split % #"\s+"))
       (map parse-long)))

(defn parse-races [input]
  (let [lines (str/split-lines (str/trim input))
        times (parse-numbers (first lines))
        distances (parse-numbers (second lines))]
    (map vector times distances)))

(defn count-ways-to-win [time record]
  "Count the number of ways to beat the record using the quadratic formula.

   If we hold the button for t ms, we travel t * (time - t) mm.
   We need: t * (time - t) > record
   Solving: -t^2 + time*t - record > 0
   Roots: t = (time +/- sqrt(time^2 - 4*record)) / 2"
  (let [discriminant (- (* time time) (* 4 record))]
    (if (<= discriminant 0)
      0
      (let [sqrt-d (Math/sqrt discriminant)
            t-low (/ (- time sqrt-d) 2)
            t-high (/ (+ time sqrt-d) 2)
            ;; We need integer values strictly between the roots
            first-val (inc (long (Math/floor t-low)))
            last-val (dec (long (Math/ceil t-high)))]
        (if (< last-val first-val)
          0
          (inc (- last-val first-val)))))))

(defn part1 [input]
  (let [races (parse-races input)]
    (reduce * (map (fn [[time record]] (count-ways-to-win time record)) races))))

(defn part2 [input]
  (let [races (parse-races input)
        time (parse-long (apply str (map first races)))
        record (parse-long (apply str (map second races)))]
    (count-ways-to-win time record)))

(defn -main []
  (let [input (read-input)]
    (println "Part 1:" (part1 input))
    (println "Part 2:" (part2 input))))

(-main)
