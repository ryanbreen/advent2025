(ns solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; Pipe connections: each pipe connects to certain directions
;; Directions: N=[-1,0], S=[1,0], E=[0,1], W=[0,-1]
(def pipe-connections
  {\| [[-1 0] [1 0]]    ; N, S
   \- [[0 -1] [0 1]]    ; W, E
   \L [[-1 0] [0 1]]    ; N, E
   \J [[-1 0] [0 -1]]   ; N, W
   \7 [[1 0] [0 -1]]    ; S, W
   \F [[1 0] [0 1]]})   ; S, E

(defn parse-grid [input]
  (str/split-lines input))

(defn find-start [grid]
  (first
   (for [r (range (count grid))
         c (range (count (nth grid r)))
         :when (= \S (get (nth grid r) c))]
     [r c])))

(defn get-char [grid [r c]]
  (when (and (>= r 0) (< r (count grid))
             (>= c 0) (< c (count (nth grid r))))
    (get (nth grid r) c)))

(defn connects-back? [grid [nr nc] [r c]]
  "Check if the pipe at [nr nc] connects back to [r c]"
  (let [adj-ch (get-char grid [nr nc])]
    (when-let [dirs (pipe-connections adj-ch)]
      (some (fn [[dr dc]]
              (and (= (+ nr dr) r) (= (+ nc dc) c)))
            dirs))))

(defn get-neighbors [grid [r c]]
  "Get valid pipe neighbors that connect to this position."
  (let [ch (get-char grid [r c])
        rows (count grid)
        cols (count (first grid))]
    (if (= ch \S)
      ;; S can connect to any adjacent pipe that connects back to it
      (for [[dr dc] [[-1 0] [1 0] [0 -1] [0 1]]
            :let [nr (+ r dr)
                  nc (+ c dc)]
            :when (and (>= nr 0) (< nr rows)
                       (>= nc 0) (< nc cols)
                       (connects-back? grid [nr nc] [r c]))]
        [nr nc])
      ;; Regular pipe
      (when-let [dirs (pipe-connections ch)]
        (for [[dr dc] dirs
              :let [nr (+ r dr)
                    nc (+ c dc)]
              :when (and (>= nr 0) (< nr rows)
                         (>= nc 0) (< nc cols))]
          [nr nc])))))

(defn find-loop [grid start]
  "BFS to find the main loop and distances from start."
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
         distances {start 0}]
    (if (empty? queue)
      distances
      (let [pos (peek queue)
            queue (pop queue)
            neighbors (get-neighbors grid pos)
            new-neighbors (filter #(not (contains? distances %)) neighbors)]
        (recur
         (into queue new-neighbors)
         (into distances (map (fn [n] [n (inc (distances pos))]) new-neighbors)))))))

(defn determine-start-pipe [grid start loop-positions]
  "Determine what pipe type S actually is based on its connections."
  (let [[r c] start
        connections (set
                     (for [[dr dc] [[-1 0] [1 0] [0 -1] [0 1]]
                           :let [nr (+ r dr)
                                 nc (+ c dc)]
                           :when (and (contains? loop-positions [nr nc])
                                      (connects-back? grid [nr nc] [r c]))]
                       [dr dc]))]
    (or (first (for [[pipe dirs] pipe-connections
                     :when (= (set dirs) connections)]
                 pipe))
        \S)))

(defn part1 [grid]
  (let [start (find-start grid)
        distances (find-loop grid start)]
    (apply max (vals distances))))

(defn part2 [grid]
  "Count tiles enclosed by the loop using ray casting (crossing number)."
  (let [start (find-start grid)
        distances (find-loop grid start)
        loop-positions (set (keys distances))
        start-pipe (determine-start-pipe grid start loop-positions)
        ;; Create modified grid with S replaced by actual pipe
        grid-vec (vec (map vec grid))
        grid-with-start (assoc-in grid-vec start start-pipe)
        rows (count grid-with-start)
        cols (count (first grid-with-start))]
    (reduce
     (fn [total r]
       (let [[_ row-enclosed]
             (reduce
              (fn [[inside enclosed] c]
                (if (contains? loop-positions [r c])
                  (let [ch (get-in grid-with-start [r c])]
                    ;; Count vertical crossings (|, L, J have a north connection)
                    (if (contains? #{\| \L \J} ch)
                      [(not inside) enclosed]
                      [inside enclosed]))
                  (if inside
                    [inside (inc enclosed)]
                    [inside enclosed])))
              [false 0]
              (range cols))]
         (+ total row-enclosed)))
     0
     (range rows))))

(defn -main []
  (let [input (slurp (io/file (-> *file* io/file .getParent) ".." "input.txt"))
        grid (parse-grid (str/trim input))]
    (println "Part 1:" (part1 grid))
    (println "Part 2:" (part2 grid))))

(-main)
