(ns solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; Persistent queue helper
(def empty-queue clojure.lang.PersistentQueue/EMPTY)

;; Pipe connections: each pipe connects to certain directions
;; Directions: N=[-1,0], S=[1,0], E=[0,1], W=[0,-1]
(def pipe-connections
  {\| [[-1 0] [1 0]]    ; N, S
   \- [[0 -1] [0 1]]    ; W, E
   \L [[-1 0] [0 1]]    ; N, E
   \J [[-1 0] [0 -1]]   ; N, W
   \7 [[1 0] [0 -1]]    ; S, W
   \F [[1 0] [0 1]]})   ; S, E

;; Cardinal directions for adjacency checks
(def cardinal-dirs [[-1 0] [1 0] [0 -1] [0 1]])

;; Pipes with north connections (used for ray casting)
(def north-pipes #{\| \L \J})

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
  (let [adj-ch (get-char grid [nr nc])
        dirs   (pipe-connections adj-ch)]
    (some (fn [[dr dc]]
            (= [(+ nr dr) (+ nc dc)] [r c]))
          dirs)))

(defn in-bounds? [rows cols [r c]]
  (and (>= r 0) (< r rows) (>= c 0) (< c cols)))

(defn get-neighbors [grid [r c]]
  "Get valid pipe neighbors that connect to this position."
  (let [ch   (get-char grid [r c])
        rows (count grid)
        cols (count (first grid))]
    (if (= ch \S)
      ;; S can connect to any adjacent pipe that connects back to it
      (for [[dr dc] cardinal-dirs
            :let [neighbor [(+ r dr) (+ c dc)]]
            :when (and (in-bounds? rows cols neighbor)
                       (connects-back? grid neighbor [r c]))]
        neighbor)
      ;; Regular pipe
      (when-let [dirs (pipe-connections ch)]
        (for [[dr dc] dirs
              :let [neighbor [(+ r dr) (+ c dc)]]
              :when (in-bounds? rows cols neighbor)]
          neighbor)))))

(defn find-loop [grid start]
  "BFS to find the main loop and distances from start."
  (loop [queue     (conj empty-queue start)
         distances {start 0}]
    (if (empty? queue)
      distances
      (let [pos           (peek queue)
            rest-queue    (pop queue)
            neighbors     (get-neighbors grid pos)
            new-neighbors (remove distances neighbors)
            dist          (inc (distances pos))]
        (recur
         (into rest-queue new-neighbors)
         (into distances (map #(vector % dist) new-neighbors)))))))

(defn determine-start-pipe [grid start loop-positions]
  "Determine what pipe type S actually is based on its connections."
  (let [[r c] start
        connections (->> cardinal-dirs
                         (filter (fn [[dr dc]]
                                   (let [neighbor [(+ r dr) (+ c dc)]]
                                     (and (loop-positions neighbor)
                                          (connects-back? grid neighbor [r c])))))
                         set)]
    (or (some (fn [[pipe dirs]]
                (when (= (set dirs) connections) pipe))
              pipe-connections)
        \S)))

(defn part1 [grid]
  (->> grid
       find-start
       (find-loop grid)
       vals
       (apply max)))

(defn count-enclosed-in-row
  "Count enclosed tiles in a single row using ray casting algorithm.
   Returns the count of non-loop tiles that are inside the loop."
  [grid-row row-idx loop-positions]
  (let [[_ enclosed]
        (reduce-kv
         (fn [[inside enclosed] col ch]
           (if (loop-positions [row-idx col])
             ;; On loop boundary - toggle if crossing north-connected pipe
             [(if (north-pipes ch) (not inside) inside) enclosed]
             ;; Not on loop - count if inside
             [inside (if inside (inc enclosed) enclosed)]))
         [false 0]
         grid-row)]
    enclosed))

(defn part2 [grid]
  "Count tiles enclosed by the loop using ray casting (crossing number)."
  (let [start           (find-start grid)
        distances       (find-loop grid start)
        loop-positions  (set (keys distances))
        start-pipe      (determine-start-pipe grid start loop-positions)
        grid-with-start (-> (mapv vec grid)
                            (assoc-in start start-pipe))]
    (->> grid-with-start
         (map-indexed (fn [row-idx grid-row]
                        (count-enclosed-in-row grid-row row-idx loop-positions)))
         (reduce +))))

(defn -main []
  (let [input (slurp (io/file (-> *file* io/file .getParent) ".." "input.txt"))
        grid (parse-grid (str/trim input))]
    (println "Part 1:" (part1 grid))
    (println "Part 2:" (part2 grid))))

(-main)
