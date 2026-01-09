#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(defn parse-grid [lines]
  (mapv (fn [line]
          (mapv #(Character/digit % 10) line))
        lines))

(defn is-visible? [grid row col]
  (let [rows (count grid)
        cols (count (first grid))
        height (get-in grid [row col])]
    (or
     ;; Check from left
     (every? #(< (get-in grid [row %]) height) (range col))
     ;; Check from right
     (every? #(< (get-in grid [row %]) height) (range (inc col) cols))
     ;; Check from top
     (every? #(< (get-in grid [% col]) height) (range row))
     ;; Check from bottom
     (every? #(< (get-in grid [% col]) height) (range (inc row) rows)))))

(defn viewing-distance [grid row col dr dc]
  (let [rows (count grid)
        cols (count (first grid))
        height (get-in grid [row col])]
    (loop [r (+ row dr)
           c (+ col dc)
           dist 0]
      (if (or (< r 0) (>= r rows) (< c 0) (>= c cols))
        dist
        (let [tree-height (get-in grid [r c])]
          (if (>= tree-height height)
            (inc dist)
            (recur (+ r dr) (+ c dc) (inc dist))))))))

(defn scenic-score [grid row col]
  (* (viewing-distance grid row col 0 -1)  ; left
     (viewing-distance grid row col 0 1)   ; right
     (viewing-distance grid row col -1 0)  ; up
     (viewing-distance grid row col 1 0))) ; down

(defn part1 [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (count (for [r (range rows)
                 c (range cols)
                 :when (is-visible? grid r c)]
             [r c]))))

(defn part2 [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (apply max (for [r (range rows)
                     c (range cols)]
                 (scenic-score grid r c)))))

(defn -main []
  (let [script-dir (-> (java.io.File. *file*) .getParent)
        input-file (str script-dir "/../input.txt")
        lines (-> (slurp input-file) str/trim (str/split #"\n"))
        grid (parse-grid lines)]
    (println "Part 1:" (part1 grid))
    (println "Part 2:" (part2 grid))))

(-main)
