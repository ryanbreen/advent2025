#!/usr/bin/env -S clojure -M

(require '[clojure.string :as str])
(require '[clojure.java.io :as io])

;; Use Java's PriorityQueue for Dijkstra
(import '[java.util PriorityQueue Comparator])

;; Directions: 0=right, 1=down, 2=left, 3=up
(def dr [0 1 0 -1])
(def dc [1 0 -1 0])

(defn parse-input [text]
  (mapv (fn [line] (mapv #(Character/digit % 10) line))
        (str/split-lines (str/trim text))))

(defn dijkstra
  "Find minimum heat loss path using Dijkstra's algorithm.
   State: [row col direction consecutive-steps]
   Directions: 0=right, 1=down, 2=left, 3=up"
  [grid min-straight max-straight]
  (let [rows (count grid)
        cols (count (first grid))
        ;; Priority queue entries: [heat r c d consec]
        pq (PriorityQueue. 1000
                          (reify Comparator
                            (compare [_ a b]
                              (compare (first a) (first b)))))
        visited (java.util.HashSet.)]
    (.add pq [0 0 0 -1 0])  ; [heat r c d consec]
    (loop []
      (if (.isEmpty pq)
        -1  ; No path found
        (let [[heat r c d consec] (.poll pq)]
          (cond
            ;; Check if we reached the goal
            (and (= r (dec rows))
                 (= c (dec cols))
                 (or (zero? min-straight) (>= consec min-straight)))
            heat

            ;; Already visited this state
            (.contains visited [r c d consec])
            (recur)

            ;; Process this state
            :else
            (do
              (.add visited [r c d consec])
              ;; Try all four directions
              (doseq [nd (range 4)]
                (when (or (= d -1) (not= nd (mod (+ d 2) 4)))  ; Can't reverse
                  (let [nr (+ r (dr nd))
                        nc (+ c (dc nd))]
                    (when (and (>= nr 0) (< nr rows)
                               (>= nc 0) (< nc cols))
                      (let [new-consec (if (= nd d) (inc consec) 1)
                            valid? (if (= nd d)
                                     (<= new-consec max-straight)
                                     (or (= d -1) (>= consec min-straight)))]
                        (when (and valid?
                                   (not (.contains visited [nr nc nd new-consec])))
                          (let [new-heat (+ heat (get-in grid [nr nc]))]
                            (.add pq [new-heat nr nc nd new-consec]))))))))
              (recur))))))))

(defn part1 [grid]
  (dijkstra grid 0 3))

(defn part2 [grid]
  (dijkstra grid 4 10))

(defn -main []
  (let [input-file (io/file (System/getProperty "user.dir") "../input.txt")
        text (slurp input-file)
        grid (parse-input text)]
    (println "Part 1:" (part1 grid))
    (println "Part 2:" (part2 grid))))

(-main)
