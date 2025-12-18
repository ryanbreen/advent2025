(ns solution
  (:require [clojure.string :as str]))

(def grid-size 71)
(def num-bytes 1024)

(defn parse-input [filename]
  "Parse byte positions from input file."
  (->> (slurp filename)
       str/split-lines
       (filter (complement str/blank?))
       (mapv (fn [line]
               (let [[x y] (str/split line #",")]
                 [(Integer/parseInt x) (Integer/parseInt y)])))))

(defn bfs [corrupted size]
  "Find shortest path from (0,0) to (size-1, size-1) using BFS.
   Returns -1 if no path exists."
  (let [start [0 0]
        goal [(dec size) (dec size)]
        directions [[0 1] [0 -1] [1 0] [-1 0]]]
    (if (or (corrupted start) (corrupted goal))
      -1
      (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start 0])
             visited #{start}]
        (if (empty? queue)
          -1
          (let [[pos steps] (peek queue)
                [x y] pos]
            (if (= pos goal)
              steps
              (let [neighbors (for [[dx dy] directions
                                    :let [nx (+ x dx)
                                          ny (+ y dy)
                                          npos [nx ny]]
                                    :when (and (>= nx 0) (< nx size)
                                               (>= ny 0) (< ny size)
                                               (not (visited npos))
                                               (not (corrupted npos)))]
                                npos)
                    new-visited (into visited neighbors)
                    new-queue (reduce (fn [q n] (conj q [n (inc steps)]))
                                      (pop queue)
                                      neighbors)]
                (recur new-queue new-visited)))))))))

(defn part1 [positions]
  "Find shortest path after first num-bytes have fallen."
  (let [corrupted (set (take num-bytes positions))]
    (bfs corrupted grid-size)))

(defn part2 [positions]
  "Find the first byte that blocks all paths using binary search."
  (loop [left 0
         right (count positions)]
    (if (< left right)
      (let [mid (quot (+ left right) 2)
            corrupted (set (take (inc mid) positions))
            result (bfs corrupted grid-size)]
        (if (= result -1)
          (recur left mid)
          (recur (inc mid) right)))
      (let [[x y] (nth positions left)]
        (str x "," y)))))

(defn -main []
  (let [positions (parse-input "../input.txt")]
    (println "Part 1:" (part1 positions))
    (println "Part 2:" (part2 positions))))

(-main)
