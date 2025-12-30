#!/usr/bin/env -S clojure -M

(ns solution
  (:require [clojure.string :as str]))

(defn parse-input [text]
  "Parse input into a 2D vector of characters."
  (mapv vec (str/split-lines (str/trim text))))

(defn tilt-north [grid]
  "Tilt the grid north, moving all round rocks up."
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [col 0
           g grid]
      (if (>= col cols)
        g
        (recur (inc col)
               (loop [row 0
                      write-pos 0
                      g' g]
                 (if (>= row rows)
                   g'
                   (let [cell (get-in g' [row col])]
                     (cond
                       (= cell \#)
                       (recur (inc row) (inc row) g')

                       (= cell \O)
                       (recur (inc row)
                              (inc write-pos)
                              (-> g'
                                  (assoc-in [row col] \.)
                                  (assoc-in [write-pos col] \O)))

                       :else
                       (recur (inc row) write-pos g'))))))))))

(defn tilt-south [grid]
  "Tilt the grid south, moving all round rocks down."
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [col 0
           g grid]
      (if (>= col cols)
        g
        (recur (inc col)
               (loop [row (dec rows)
                      write-pos (dec rows)
                      g' g]
                 (if (< row 0)
                   g'
                   (let [cell (get-in g' [row col])]
                     (cond
                       (= cell \#)
                       (recur (dec row) (dec row) g')

                       (= cell \O)
                       (recur (dec row)
                              (dec write-pos)
                              (-> g'
                                  (assoc-in [row col] \.)
                                  (assoc-in [write-pos col] \O)))

                       :else
                       (recur (dec row) write-pos g'))))))))))

(defn tilt-west [grid]
  "Tilt the grid west, moving all round rocks left."
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [row 0
           g grid]
      (if (>= row rows)
        g
        (recur (inc row)
               (loop [col 0
                      write-pos 0
                      g' g]
                 (if (>= col cols)
                   g'
                   (let [cell (get-in g' [row col])]
                     (cond
                       (= cell \#)
                       (recur (inc col) (inc col) g')

                       (= cell \O)
                       (recur (inc col)
                              (inc write-pos)
                              (-> g'
                                  (assoc-in [row col] \.)
                                  (assoc-in [row write-pos] \O)))

                       :else
                       (recur (inc col) write-pos g'))))))))))

(defn tilt-east [grid]
  "Tilt the grid east, moving all round rocks right."
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [row 0
           g grid]
      (if (>= row rows)
        g
        (recur (inc row)
               (loop [col (dec cols)
                      write-pos (dec cols)
                      g' g]
                 (if (< col 0)
                   g'
                   (let [cell (get-in g' [row col])]
                     (cond
                       (= cell \#)
                       (recur (dec col) (dec col) g')

                       (= cell \O)
                       (recur (dec col)
                              (dec write-pos)
                              (-> g'
                                  (assoc-in [row col] \.)
                                  (assoc-in [row write-pos] \O)))

                       :else
                       (recur (dec col) write-pos g'))))))))))

(defn spin-cycle [grid]
  "Perform one spin cycle: N, W, S, E."
  (-> grid
      tilt-north
      tilt-west
      tilt-south
      tilt-east))

(defn calculate-load [grid]
  "Calculate total load on north support beams."
  (let [rows (count grid)]
    (reduce +
            (for [row (range rows)
                  col (range (count (first grid)))
                  :when (= \O (get-in grid [row col]))]
              (- rows row)))))

(defn part1 [grid]
  "Tilt north and calculate load."
  (calculate-load (tilt-north grid)))

(defn part2 [grid]
  "Run 1 billion spin cycles and calculate load."
  (let [target 1000000000]
    (loop [g grid
           cycle-num 0
           seen {}]
      (if (>= cycle-num target)
        (calculate-load g)
        (if-let [cycle-start (seen g)]
          ;; Found a cycle
          (let [cycle-length (- cycle-num cycle-start)
                remaining (mod (- target cycle-num) cycle-length)
                final-grid (nth (iterate spin-cycle g) remaining)]
            (calculate-load final-grid))
          ;; Continue cycling
          (recur (spin-cycle g)
                 (inc cycle-num)
                 (assoc seen g cycle-num)))))))

(defn -main []
  (let [input-file (str (-> *file*
                            java.io.File.
                            .getParent)
                        "/../input.txt")
        text (slurp input-file)
        grid (parse-input text)]
    (println (str "Part 1: " (part1 grid)))
    (println (str "Part 2: " (part2 grid)))))

(-main)
