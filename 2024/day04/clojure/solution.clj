(ns solution
  (:require [clojure.string :as str]))

(defn parse-input [filename]
  "Parse input file into a grid (vector of strings)"
  (-> filename slurp str/trim (str/split #"\n")))

(def directions
  "8 directions: right, left, down, up, and 4 diagonals"
  [[0 1]    ; right
   [0 -1]   ; left
   [1 0]    ; down
   [-1 0]   ; up
   [1 1]    ; down-right
   [1 -1]   ; down-left
   [-1 1]   ; up-right
   [-1 -1]]) ; up-left

(defn in-bounds? [grid r c]
  "Check if position is within grid bounds"
  (and (>= r 0)
       (< r (count grid))
       (>= c 0)
       (< c (count (first grid)))))

(defn get-char [grid r c]
  "Get character at position (r, c)"
  (get-in grid [r c]))

(defn check-word [grid r c dr dc target]
  "Check if target word exists starting at (r,c) in direction (dr,dc)"
  (every? (fn [i]
            (let [nr (+ r (* dr i))
                  nc (+ c (* dc i))
                  ch (nth target i)]
              (and (in-bounds? grid nr nc)
                   (= (get-char grid nr nc) ch))))
          (range (count target))))

(defn part1 [grid]
  "Count occurrences of XMAS in all 8 directions"
  (let [target "XMAS"
        rows (count grid)
        cols (count (first grid))]
    (reduce +
            (for [r (range rows)
                  c (range cols)
                  [dr dc] directions
                  :when (check-word grid r c dr dc target)]
              1))))

(defn part2 [grid]
  "Find X-MAS patterns: two MAS forming an X with A in center"
  (let [rows (count grid)
        cols (count (first grid))]
    (count
      (for [r (range 1 (dec rows))
            c (range 1 (dec cols))
            :when (= (get-char grid r c) \A)
            :let [top-left (get-char grid (dec r) (dec c))
                  top-right (get-char grid (dec r) (inc c))
                  bottom-left (get-char grid (inc r) (dec c))
                  bottom-right (get-char grid (inc r) (inc c))
                  ;; Check diagonal 1 (top-left to bottom-right): MAS or SAM
                  diag1-ok (or (and (= top-left \M) (= bottom-right \S))
                               (and (= top-left \S) (= bottom-right \M)))
                  ;; Check diagonal 2 (top-right to bottom-left): MAS or SAM
                  diag2-ok (or (and (= top-right \M) (= bottom-left \S))
                               (and (= top-right \S) (= bottom-left \M)))]
            :when (and diag1-ok diag2-ok)]
        1))))

(defn -main []
  (let [grid (parse-input "../input.txt")]
    (println (str "Part 1: " (part1 grid)))
    (println (str "Part 2: " (part2 grid)))))

(-main)
