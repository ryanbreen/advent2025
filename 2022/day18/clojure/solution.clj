#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

;; 6 directions: +x, -x, +y, -y, +z, -z
(def directions
  [[1 0 0] [-1 0 0]
   [0 1 0] [0 -1 0]
   [0 0 1] [0 0 -1]])

(defn parse-input [text]
  "Parse cube coordinates from input."
  (set
    (for [line (str/split-lines (str/trim text))]
      (vec (map #(Integer/parseInt %) (str/split line #","))))))

(defn part1 [text]
  "Count total surface area (all exposed faces)."
  (let [cubes (parse-input text)]
    (reduce +
      (for [[x y z] cubes
            [dx dy dz] directions
            :when (not (contains? cubes [(+ x dx) (+ y dy) (+ z dz)]))]
        1))))

(defn part2 [text]
  "Count only exterior surface area (excluding trapped air pockets)."
  (let [cubes (parse-input text)
        ;; Find bounding box with 1 unit padding
        xs (map first cubes)
        ys (map second cubes)
        zs (map #(nth % 2) cubes)
        min-x (dec (apply min xs))
        max-x (inc (apply max xs))
        min-y (dec (apply min ys))
        max-y (inc (apply max ys))
        min-z (dec (apply min zs))
        max-z (inc (apply max zs))
        ;; BFS to find all exterior air cells
        start [min-x min-y min-z]
        exterior (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
                        visited #{start}]
                   (if (empty? queue)
                     visited
                     (let [[x y z] (peek queue)
                           queue' (pop queue)
                           neighbors (for [[dx dy dz] directions
                                          :let [nx (+ x dx)
                                                ny (+ y dy)
                                                nz (+ z dz)]
                                          :when (and (<= min-x nx max-x)
                                                     (<= min-y ny max-y)
                                                     (<= min-z nz max-z)
                                                     (not (contains? cubes [nx ny nz]))
                                                     (not (contains? visited [nx ny nz])))]
                                      [nx ny nz])
                           new-visited (reduce conj visited neighbors)
                           new-queue (reduce conj queue' neighbors)]
                       (recur new-queue new-visited))))]
    ;; Count faces touching exterior air
    (reduce +
      (for [[x y z] cubes
            [dx dy dz] directions
            :when (contains? exterior [(+ x dx) (+ y dy) (+ z dz)])]
        1))))

(defn -main []
  (let [script-dir (-> (java.io.File. *file*) .getParentFile .getAbsolutePath)
        input-file (str script-dir "/../input.txt")
        text (slurp input-file)]
    (println "Part 1:" (part1 text))
    (println "Part 2:" (part2 text))))

(-main)
