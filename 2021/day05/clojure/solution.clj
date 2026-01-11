(ns solution
  (:require [clojure.string :as str]))

(defn sign [x]
  (cond
    (pos? x) 1
    (neg? x) -1
    :else 0))

(defn parse-line [line]
  (let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)]
    [(Integer/parseInt x1)
     (Integer/parseInt y1)
     (Integer/parseInt x2)
     (Integer/parseInt y2)]))

(defn parse-input []
  (->> (slurp "../input.txt")
       str/split-lines
       (filter (complement str/blank?))
       (map parse-line)))

(defn line-points [[x1 y1 x2 y2]]
  (let [dx (sign (- x2 x1))
        dy (sign (- y2 y1))]
    (loop [x x1, y y1, points []]
      (let [points (conj points [x y])]
        (if (and (= x x2) (= y y2))
          points
          (recur (+ x dx) (+ y dy) points))))))

(defn count-overlaps [lines include-diagonals?]
  (let [filtered-lines (if include-diagonals?
                         lines
                         (filter (fn [[x1 y1 x2 y2]]
                                   (or (= x1 x2) (= y1 y2)))
                                 lines))
        all-points (mapcat line-points filtered-lines)
        freq-map (frequencies all-points)]
    (count (filter #(>= % 2) (vals freq-map)))))

(defn part1 [lines]
  (count-overlaps lines false))

(defn part2 [lines]
  (count-overlaps lines true))

(defn -main []
  (let [lines (parse-input)]
    (println "Part 1:" (part1 lines))
    (println "Part 2:" (part2 lines))))

(-main)
