#!/usr/bin/env clojure
(ns solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; Directions: 0=East, 1=South, 2=West, 3=North
(def dx [1 0 -1 0])
(def dy [0 1 0 -1])

(defn parse-input [text]
  "Parse the maze and find start/end positions."
  (let [lines (str/split-lines (str/trim text))
        grid (mapv vec lines)
        height (count grid)
        width (count (first grid))]
    (loop [y 0
           start nil
           end nil]
      (if (>= y height)
        {:grid grid :start start :end end}
        (let [row (nth grid y)
              s-idx (.indexOf row \S)
              e-idx (.indexOf row \E)]
          (recur (inc y)
                 (if (>= s-idx 0) [s-idx y] start)
                 (if (>= e-idx 0) [e-idx y] end)))))))

(defn dijkstra-forward [grid start]
  "Run Dijkstra from start facing East.
   Returns map of [x y dir] -> minimum cost to reach that state."
  (let [height (count grid)
        width (count (first grid))
        [sx sy] start
        pq (java.util.PriorityQueue.
             (reify java.util.Comparator
               (compare [_ a b] (compare (first a) (first b)))))
        dist (atom {})]
    ;; Start facing East (direction 0)
    (.offer pq [0 sx sy 0])

    (loop []
      (when-not (.isEmpty pq)
        (let [[cost x y d] (.poll pq)
              state [x y d]]
          (when-not (contains? @dist state)
            (swap! dist assoc state cost)

            ;; Move forward
            (let [nx (+ x (nth dx d))
                  ny (+ y (nth dy d))]
              (when (and (>= ny 0) (< ny height)
                         (>= nx 0) (< nx width)
                         (not= (get-in grid [ny nx]) \#))
                (.offer pq [(+ cost 1) nx ny d])))

            ;; Turn left and right
            (.offer pq [(+ cost 1000) x y (mod (- d 1) 4)])
            (.offer pq [(+ cost 1000) x y (mod (+ d 1) 4)]))

          (recur))))

    @dist))

(defn dijkstra-backward [grid end]
  "Run Dijkstra backward from end (all directions at end have cost 0).
   Returns map of [x y dir] -> minimum cost from that state to reach end."
  (let [height (count grid)
        width (count (first grid))
        [ex ey] end
        pq (java.util.PriorityQueue.
             (reify java.util.Comparator
               (compare [_ a b] (compare (first a) (first b)))))
        dist (atom {})]
    ;; At end, we can arrive facing any direction
    (doseq [d (range 4)]
      (.offer pq [0 ex ey d]))

    (loop []
      (when-not (.isEmpty pq)
        (let [[cost x y d] (.poll pq)
              state [x y d]]
          (when-not (contains? @dist state)
            (swap! dist assoc state cost)

            ;; Reverse of "move forward": come from behind
            (let [px (- x (nth dx d))
                  py (- y (nth dy d))]
              (when (and (>= py 0) (< py height)
                         (>= px 0) (< px width)
                         (not= (get-in grid [py px]) \#))
                (.offer pq [(+ cost 1) px py d])))

            ;; Reverse of turn: came from same position with different direction
            (.offer pq [(+ cost 1000) x y (mod (- d 1) 4)])
            (.offer pq [(+ cost 1000) x y (mod (+ d 1) 4)]))

          (recur))))

    @dist))

(defn part1 [grid start end]
  "Find the lowest score path from start to end."
  (let [dist (dijkstra-forward grid start)
        [ex ey] end]
    (apply min (map #(get dist [ex ey %] Double/POSITIVE_INFINITY) (range 4)))))

(defn part2 [grid start end best-score]
  "Count tiles that are part of any optimal path."
  (let [dist-from-start (dijkstra-forward grid start)
        dist-to-end (dijkstra-backward grid end)
        height (count grid)
        width (count (first grid))
        tiles (atom #{})]

    (doseq [y (range height)
            x (range width)
            :when (not= (get-in grid [y x]) \#)]
      (doseq [d (range 4)]
        (let [state [x y d]
              from-start (get dist-from-start state Double/POSITIVE_INFINITY)
              to-end (get dist-to-end state Double/POSITIVE_INFINITY)]
          (when (== (+ from-start to-end) best-score)
            (swap! tiles conj [x y])))))

    (count @tiles)))

(defn -main []
  (let [input-path (io/file (str (System/getProperty "user.dir") "/../input.txt"))
        text (slurp input-path)
        {:keys [grid start end]} (parse-input text)
        answer1 (part1 grid start end)
        answer2 (part2 grid start end answer1)]
    (println (str "Part 1: " answer1))
    (println (str "Part 2: " answer2))))

(-main)
