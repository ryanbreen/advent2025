#!/usr/bin/env clojure -M

;; Day 21: Step Counter - Garden plot reachability

(require '[clojure.string :as str])

(defn parse-input [filename]
  "Parse grid and find starting position."
  (let [lines (str/split-lines (slurp filename))
        grid (vec lines)
        rows (count grid)
        cols (count (first grid))]
    (loop [r 0]
      (if (>= r rows)
        [grid nil]
        (let [row (nth grid r)
              c (str/index-of row \S)]
          (if c
            [grid [r c]]
            (recur (inc r))))))))

(defn count-reachable [grid start steps]
  "Count cells reachable in exactly 'steps' steps."
  (let [rows (count grid)
        cols (count (first grid))
        directions [[-1 0] [1 0] [0 -1] [0 1]]]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [(first start) (second start) 0])
           visited {start 0}]
      (if (empty? queue)
        ;; Count cells with right parity
        (let [target-parity (mod steps 2)]
          (count (filter (fn [[_ d]] (and (<= d steps) (= (mod d 2) target-parity)))
                        visited)))
        (let [[r c dist] (peek queue)
              queue' (pop queue)]
          (if (>= dist steps)
            (recur queue' visited)
            (let [[queue'' visited'']
                  (reduce (fn [[q v] [dr dc]]
                            (let [nr (+ r dr)
                                  nc (+ c dc)]
                              (if (and (>= nr 0) (< nr rows)
                                       (>= nc 0) (< nc cols)
                                       (not= (get-in grid [nr nc]) \#)
                                       (not (contains? v [nr nc])))
                                [(conj q [nr nc (inc dist)])
                                 (assoc v [nr nc] (inc dist))]
                                [q v])))
                          [queue' visited]
                          directions)]
              (recur queue'' visited''))))))))

(defn count-reachable-infinite-bfs [grid start steps]
  "BFS on infinite tiled grid for small step counts."
  (let [rows (count grid)
        cols (count (first grid))
        directions [[-1 0] [1 0] [0 -1] [0 1]]]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [(first start) (second start) 0])
           visited {[(first start) (second start)] 0}]
      (if (empty? queue)
        ;; Count cells with right parity
        (let [target-parity (mod steps 2)]
          (count (filter (fn [[_ d]] (and (<= d steps) (= (mod d 2) target-parity)))
                        visited)))
        (let [[r c dist] (peek queue)
              queue' (pop queue)]
          (if (>= dist steps)
            (recur queue' visited)
            (let [[queue'' visited'']
                  (reduce (fn [[q v] [dr dc]]
                            (let [nr (+ r dr)
                                  nc (+ c dc)
                                  ;; Map to grid coordinates (infinite tiling)
                                  gr (mod nr rows)
                                  gc (mod nc cols)
                                  ;; Handle negative modulo
                                  gr (if (neg? gr) (+ gr rows) gr)
                                  gc (if (neg? gc) (+ gc cols) gc)]
                              (if (and (not= (get-in grid [gr gc]) \#)
                                       (not (contains? v [nr nc])))
                                [(conj q [nr nc (inc dist)])
                                 (assoc v [nr nc] (inc dist))]
                                [q v])))
                          [queue' visited]
                          directions)]
              (recur queue'' visited''))))))))

(defn count-reachable-infinite [grid start steps]
  "Count cells reachable in exactly 'steps' steps on an infinite tiled grid.
   Uses the quadratic pattern that emerges due to the grid structure."
  (let [rows (count grid)
        size rows
        half (quot size 2)]
    (if (<= steps (* size 2))
      (count-reachable-infinite-bfs grid start steps)
      ;; The number of full grid widths we travel
      (let [n (quot (- steps half) size)
            ;; Calculate reachable counts for n=0, 1, 2
            y0 (count-reachable-infinite-bfs grid start half)
            y1 (count-reachable-infinite-bfs grid start (+ half size))
            y2 (count-reachable-infinite-bfs grid start (+ half (* 2 size)))
            ;; Solve for a, b, c using finite differences
            ;; f(x) = ax^2 + bx + c
            a (quot (- (+ y2 y0) (* 2 y1)) 2)
            b (- y1 y0 a)
            c y0]
        ;; Use BigInt for large calculation
        (+ (* a n n) (* b n) c)))))

(defn part1 [grid start]
  "Part 1: Count plots reachable in exactly 64 steps."
  (count-reachable grid start 64))

(defn part2 [grid start]
  "Part 2: Count plots reachable in exactly 26501365 steps on infinite grid."
  (count-reachable-infinite grid start 26501365))

(defn -main []
  (let [input-file (str (System/getProperty "user.dir") "/../input.txt")
        [grid start] (parse-input input-file)]
    (println "Part 1:" (part1 grid start))
    (println "Part 2:" (part2 grid start))))

(-main)
