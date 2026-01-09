#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(defn parse-coord [s]
  "Parse a coordinate string like '498,4' into [x y]."
  (let [[x y] (str/split (str/trim s) #",")]
    [(Integer/parseInt x) (Integer/parseInt y)]))

(defn draw-line [rocks [x1 y1] [x2 y2]]
  "Add all rock positions along a line from [x1 y1] to [x2 y2]."
  (if (= x1 x2)
    ;; Vertical line
    (reduce (fn [acc y] (conj acc [x1 y]))
            rocks
            (range (min y1 y2) (inc (max y1 y2))))
    ;; Horizontal line
    (reduce (fn [acc x] (conj acc [x y1]))
            rocks
            (range (min x1 x2) (inc (max x1 x2))))))

(defn parse-path [rocks line]
  "Parse a single path line and add all rock positions to the set."
  (let [points (map parse-coord (str/split line #" -> "))]
    (reduce (fn [acc [p1 p2]] (draw-line acc p1 p2))
            rocks
            (partition 2 1 points))))

(defn parse-paths [text]
  "Parse all paths and return set of rock positions."
  (reduce parse-path #{} (str/split-lines text)))

(defn simulate-sand [blocked max-y floor?]
  "Simulate one unit of sand falling. Returns [new-pos hit-floor?] or nil if into abyss."
  (loop [x 500 y 0]
    (cond
      ;; Falling into abyss (Part 1)
      (and (not floor?) (> y max-y))
      nil

      ;; Hit the floor (Part 2)
      (and floor? (= (inc y) (+ max-y 2)))
      [x y]

      ;; Try to move down
      (not (contains? blocked [x (inc y)]))
      (recur x (inc y))

      ;; Try to move down-left
      (not (contains? blocked [(dec x) (inc y)]))
      (recur (dec x) (inc y))

      ;; Try to move down-right
      (not (contains? blocked [(inc x) (inc y)]))
      (recur (inc x) (inc y))

      ;; Sand comes to rest
      :else
      [x y])))

(defn part1 [rocks]
  "Count sand units that come to rest before sand falls into abyss."
  (let [max-y (apply max (map second rocks))]
    (loop [blocked rocks
           count 0]
      (if-let [pos (simulate-sand blocked max-y false)]
        (recur (conj blocked pos) (inc count))
        count))))

(defn part2 [rocks]
  "Count sand units until source is blocked (with floor)."
  (let [max-y (apply max (map second rocks))]
    (loop [blocked rocks
           count 0]
      (let [pos (simulate-sand blocked max-y true)]
        (if (= pos [500 0])
          (inc count)
          (recur (conj blocked pos) (inc count)))))))

(defn -main []
  (let [input-file (str (.getParent (java.io.File. *file*)) "/../input.txt")
        text (slurp input-file)
        rocks (parse-paths text)]
    (println "Part 1:" (part1 rocks))
    (println "Part 2:" (part2 rocks))))

(-main)
