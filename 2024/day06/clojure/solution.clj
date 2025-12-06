#!/usr/bin/env clojure

(require '[clojure.string :as str])

(defn parse-input [filename]
  "Parse the input file into a grid and find the starting position and direction"
  (let [lines (str/split-lines (slurp filename))
        grid (vec (map vec lines))
        height (count grid)
        width (count (first grid))]
    ;; Find guard position
    (let [positions (for [y (range height)
                          x (range width)
                          :let [cell (get-in grid [y x])]
                          :when (contains? #{\^ \v \< \>} cell)]
                      {:grid grid
                       :start [y x]
                       :dir (case cell
                              \^ :up
                              \v :down
                              \< :left
                              \> :right)
                       :height height
                       :width width})]
      (first positions))))

(defn turn-right [dir]
  "Turn 90 degrees to the right"
  (case dir
    :up :right
    :right :down
    :down :left
    :left :up))

(defn move-forward [[y x] dir]
  "Get the next position in the given direction"
  (case dir
    :up [(dec y) x]
    :down [(inc y) x]
    :left [y (dec x)]
    :right [y (inc x)]))

(defn in-bounds? [[y x] height width]
  "Check if a position is within the map bounds"
  (and (>= y 0) (< y height) (>= x 0) (< x width)))

(defn obstacle? [grid [y x]]
  "Check if a position contains an obstacle"
  (= (get-in grid [y x]) \#))

(defn simulate-patrol [parsed]
  "Simulate the guard's patrol and return visited positions"
  (let [{:keys [grid start dir height width]} parsed]
    (loop [pos start
           direction dir
           visited #{start}]
      (let [next-pos (move-forward pos direction)]
        (cond
          ;; Guard leaves the map
          (not (in-bounds? next-pos height width))
          visited

          ;; Obstacle ahead - turn right
          (obstacle? grid next-pos)
          (recur pos (turn-right direction) visited)

          ;; Move forward
          :else
          (recur next-pos direction (conj visited next-pos)))))))

(defn detect-loop? [parsed]
  "Check if the guard gets stuck in a loop"
  (let [{:keys [grid start dir height width]} parsed]
    (loop [pos start
           direction dir
           states #{}]
      (let [state [pos direction]
            next-pos (move-forward pos direction)]
        (cond
          ;; We've seen this state before - loop detected
          (contains? states state)
          true

          ;; Guard leaves the map - no loop
          (not (in-bounds? next-pos height width))
          false

          ;; Obstacle ahead - turn right
          (obstacle? grid next-pos)
          (recur pos (turn-right direction) (conj states state))

          ;; Move forward
          :else
          (recur next-pos direction (conj states state)))))))

(defn part1 [filename]
  "Solve part 1: count distinct positions visited"
  (let [parsed (parse-input filename)
        visited (simulate-patrol parsed)]
    (count visited)))

(defn part2 [filename]
  "Solve part 2: count positions where adding an obstruction creates a loop"
  (let [parsed (parse-input filename)
        {:keys [grid start height width]} parsed
        ;; First get all positions the guard visits in normal patrol
        visited (simulate-patrol parsed)]
    ;; Count positions where adding an obstruction creates a loop
    (->> visited
         (remove #(= % start))  ;; Can't place obstruction at starting position
         (filter (fn [[y x]]
                   ;; Place obstruction temporarily
                   (let [new-grid (assoc-in grid [y x] \#)
                         new-parsed (assoc parsed :grid new-grid)]
                     ;; Check if this creates a loop
                     (detect-loop? new-parsed))))
         count)))

(defn -main []
  (let [input-file "../input.txt"
        result1 (part1 input-file)
        result2 (part2 input-file)]
    (println (str "Part 1: " result1))
    (println (str "Part 2: " result2))))

(-main)
