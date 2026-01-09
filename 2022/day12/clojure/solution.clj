#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(defn parse-grid
  "Parse grid and find start/end positions.
   Returns [grid start end] where grid has S replaced with 'a' and E replaced with 'z'."
  [text]
  (let [lines (str/split-lines (str/trim text))
        grid (vec (map vec lines))
        rows (count grid)
        cols (count (first grid))]
    (loop [r 0
           start nil
           end nil
           g grid]
      (if (>= r rows)
        [g start end]
        (let [[new-g new-start new-end]
              (loop [c 0
                     g* g
                     s start
                     e end]
                (if (>= c cols)
                  [g* s e]
                  (let [ch (get-in g* [r c])]
                    (cond
                      (= ch \S)
                      (recur (inc c) (assoc-in g* [r c] \a) [r c] e)

                      (= ch \E)
                      (recur (inc c) (assoc-in g* [r c] \z) s [r c])

                      :else
                      (recur (inc c) g* s e)))))]
          (recur (inc r) new-start new-end new-g))))))

(def directions [[-1 0] [1 0] [0 -1] [0 1]])

(defn bfs
  "BFS to find shortest path from any start position to end.
   Movement constraint: destination height at most 1 higher than current."
  [grid starts end]
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [queue (into clojure.lang.PersistentQueue/EMPTY
                       (map (fn [s] [(first s) (second s) 0]) starts))
           visited (set starts)]
      (if (empty? queue)
        -1  ; No path found
        (let [[r c dist] (peek queue)
              queue (pop queue)]
          (if (= [r c] end)
            dist
            (let [current-height (int (get-in grid [r c]))
                  [new-queue new-visited]
                  (reduce
                   (fn [[q v] [dr dc]]
                     (let [nr (+ r dr)
                           nc (+ c dc)]
                       (if (and (>= nr 0) (< nr rows)
                                (>= nc 0) (< nc cols)
                                (not (contains? v [nr nc])))
                         (let [next-height (int (get-in grid [nr nc]))]
                           (if (<= next-height (inc current-height))
                             [(conj q [nr nc (inc dist)]) (conj v [nr nc])]
                             [q v]))
                         [q v])))
                   [queue visited]
                   directions)]
              (recur new-queue new-visited))))))))

(defn part1
  "Find shortest path from S to E."
  [text]
  (let [[grid start end] (parse-grid text)]
    (bfs grid [start] end)))

(defn part2
  "Find shortest path from any 'a' to E."
  [text]
  (let [[grid _ end] (parse-grid text)
        rows (count grid)
        cols (count (first grid))
        ;; Find all cells with elevation 'a'
        starts (for [r (range rows)
                     c (range cols)
                     :when (= (get-in grid [r c]) \a)]
                 [r c])]
    (bfs grid starts end)))

(defn -main []
  (let [script-dir (-> (java.io.File. *file*) .getParentFile .getAbsolutePath)
        input-file (str script-dir "/../input.txt")
        text (slurp input-file)]
    (println "Part 1:" (part1 text))
    (println "Part 2:" (part2 text))))

(-main)
