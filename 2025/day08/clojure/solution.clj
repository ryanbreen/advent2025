#!/usr/bin/env clojure

(ns solution
  (:require [clojure.string :as str]))

;; Union-Find data structure with atoms (idiomatic Clojure)
(defn make-uf [n]
  {:parent (atom (vec (range n)))
   :rank (atom (vec (repeat n 0)))
   :size (atom (vec (repeat n 1)))})

(defn uf-find [uf x]
  (let [parent @(:parent uf)
        px (parent x)]
    (if (not= px x)
      (let [root (uf-find uf px)]
        (swap! (:parent uf) assoc x root)  ; Path compression
        root)
      x)))

(defn uf-union [uf x y]
  (let [px (uf-find uf x)
        py (uf-find uf y)]
    (if (= px py)
      false  ; Already in same set
      (let [parent @(:parent uf)
            rank @(:rank uf)
            size @(:size uf)
            rank-px (rank px)
            rank-py (rank py)
            [parent-root child-root] (if (< rank-px rank-py) [py px] [px py])
            size-pr (size parent-root)
            size-cr (size child-root)]
        (swap! (:parent uf) assoc child-root parent-root)
        (swap! (:size uf) assoc parent-root (+ size-pr size-cr))
        (when (= rank-px rank-py)
          (swap! (:rank uf) update parent-root inc))
        true))))

(defn get-component-sizes [uf]
  (let [parent @(:parent uf)
        size @(:size uf)]
    (->> (range (count parent))
         (filter #(= (parent %) %))
         (map #(size %)))))

;; Parse input
(defn parse-input [filename]
  (->> (slurp filename)
       (str/split-lines)
       (remove str/blank?)
       (mapv (fn [line]
               (let [[x y z] (map #(Integer/parseInt %) (str/split line #","))]
                 [x y z])))))

;; Euclidean distance squared
(defn distance-sq [[x1 y1 z1] [x2 y2 z2]]
  (+ (* (- x1 x2) (- x1 x2))
     (* (- y1 y2) (- y1 y2))
     (* (- z1 z2) (- z1 z2))))

;; Generate all pairs with distances
(defn generate-pairs [points]
  (let [n (count points)]
    (for [i (range n)
          j (range (inc i) n)]
      (let [dist (distance-sq (points i) (points j))]
        [dist i j]))))

;; Part 1: Connect 1000 closest pairs, return product of 3 largest component sizes
(defn part1 [points]
  (let [n (count points)
        pairs (sort (generate-pairs points))
        uf (make-uf n)]
    (doseq [[_ i j] (take 1000 pairs)]
      (uf-union uf i j))
    (let [sizes (sort > (get-component-sizes uf))]
      (* (nth sizes 0) (nth sizes 1) (nth sizes 2)))))

;; Part 2: Connect until all in one circuit, return product of X coordinates of last connection
(defn part2 [points]
  (let [n (count points)
        pairs (sort (generate-pairs points))
        uf (make-uf n)]
    (loop [num-components n
           remaining-pairs pairs]
      (if (empty? remaining-pairs)
        0
        (let [[_ i j] (first remaining-pairs)
              merged? (uf-union uf i j)]
          (if merged?
            (let [new-count (dec num-components)]
              (if (= new-count 1)
                (* (get-in points [i 0]) (get-in points [j 0]))
                (recur new-count (rest remaining-pairs))))
            (recur num-components (rest remaining-pairs))))))))

;; Main
(defn -main [& args]
  (let [input-file (or (first args) "../input.txt")
        points (parse-input input-file)]
    (println "Part 1:" (part1 points))
    (println "Part 2:" (part2 points))))

(-main)
