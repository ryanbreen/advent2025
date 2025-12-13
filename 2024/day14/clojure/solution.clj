(ns solution
  (:require [clojure.string :as str]))

(def WIDTH 101)
(def HEIGHT 103)

(defn parse-robots
  "Parse robot positions and velocities from input text."
  [text]
  (->> (str/split-lines text)
       (map #(re-matches #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)" %))
       (remove nil?)
       (map (fn [[_ px py vx vy]]
              [(parse-long px) (parse-long py) (parse-long vx) (parse-long vy)]))))

(defn simulate
  "Simulate robot movement for given seconds."
  [robots seconds]
  (map (fn [[px py vx vy]]
         (let [new-x (mod (+ px (* vx seconds)) WIDTH)
               new-y (mod (+ py (* vy seconds)) HEIGHT)]
           [new-x new-y]))
       robots))

(defn count-quadrants
  "Count robots in each quadrant, excluding middle row/column."
  [positions]
  (let [mid-x (quot WIDTH 2)   ; 50
        mid-y (quot HEIGHT 2)  ; 51
        quadrants (reduce
                   (fn [[q1 q2 q3 q4] [x y]]
                     (cond
                       (or (= x mid-x) (= y mid-y)) [q1 q2 q3 q4]  ; Skip middle
                       (and (< x mid-x) (< y mid-y)) [(inc q1) q2 q3 q4]  ; Top-left
                       (and (> x mid-x) (< y mid-y)) [q1 (inc q2) q3 q4]  ; Top-right
                       (and (< x mid-x) (> y mid-y)) [q1 q2 (inc q3) q4]  ; Bottom-left
                       (and (> x mid-x) (> y mid-y)) [q1 q2 q3 (inc q4)]  ; Bottom-right
                       :else [q1 q2 q3 q4]))
                   [0 0 0 0]
                   positions)]
    quadrants))

(defn part1
  "Part 1: Safety factor after 100 seconds."
  [robots]
  (let [positions (simulate robots 100)
        [q1 q2 q3 q4] (count-quadrants positions)]
    (* q1 q2 q3 q4)))

(defn has-long-horizontal-line?
  "Check if there's a horizontal line of at least 20 consecutive robots."
  [pos-set]
  (some (fn [y]
          (let [max-consecutive
                (reduce
                 (fn [[max-cons consecutive] x]
                   (if (contains? pos-set [x y])
                     [(max max-cons (inc consecutive)) (inc consecutive)]
                     [(max max-cons consecutive) 0]))
                 [0 0]
                 (range WIDTH))]
            (>= (first max-consecutive) 20)))
        (range HEIGHT)))

(defn part2
  "Part 2: Find when robots form a Christmas tree pattern."
  [robots]
  (loop [seconds 1]
    (if (> seconds (* WIDTH HEIGHT))
      -1
      (let [positions (simulate robots seconds)
            pos-set (set positions)]
        (if (has-long-horizontal-line? pos-set)
          seconds
          (recur (inc seconds)))))))

(defn -main []
  (let [input-text (slurp "../input.txt")
        robots (parse-robots input-text)]
    (println "Part 1:" (part1 robots))
    (println "Part 2:" (part2 robots))))

(-main)
