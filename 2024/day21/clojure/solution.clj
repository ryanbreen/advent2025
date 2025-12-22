#!/usr/bin/env clojure
;;; Day 21: Keypad Conundrum - Robot chain control with shortest path optimization

(require '[clojure.string :as str])
(require '[clojure.java.io :as io])

;; Keypad layouts - positions as [row col]
(def numeric-keypad
  {\7 [0 0] \8 [0 1] \9 [0 2]
   \4 [1 0] \5 [1 1] \6 [1 2]
   \1 [2 0] \2 [2 1] \3 [2 2]
   \0 [3 1] \A [3 2]})

(def numeric-gap [3 0])

(def directional-keypad
  {\^ [0 1] \A [0 2]
   \< [1 0] \v [1 1] \> [1 2]})

(def directional-gap [0 0])

(defn shortest-paths
  "Find all shortest paths from start to end, avoiding gap."
  [keypad gap start end]
  (let [[sr sc] (keypad start)
        [er ec] (keypad end)
        paths (atom [])]
    (letfn [(dfs [r c path]
              (cond
                (= [r c] gap) nil
                (= [r c] [er ec]) (swap! paths conj path)
                :else
                (do
                  ;; Move vertically toward target
                  (cond
                    (< r er) (dfs (inc r) c (str path "v"))
                    (> r er) (dfs (dec r) c (str path "^")))
                  ;; Move horizontally toward target
                  (cond
                    (< c ec) (dfs r (inc c) (str path ">"))
                    (> c ec) (dfs r (dec c) (str path "<"))))))]
      (dfs sr sc "")
      (if (empty? @paths) [""] @paths))))

(def min-presses-for-move
  "Minimum presses needed to move from from-char to to-char and press, at given depth."
  (memoize
   (fn [from-char to-char depth is-numeric]
     (let [[keypad gap] (if is-numeric
                          [numeric-keypad numeric-gap]
                          [directional-keypad directional-gap])
           paths (shortest-paths keypad gap from-char to-char)]
       (if (zero? depth)
         ;; At human level, just return path length + 1 for 'A' press
         (inc (apply min (map count paths)))
         ;; Need to type path + 'A' on the directional keypad above
         (apply min
                (for [path paths]
                  (let [sequence (str path "A")]
                    (loop [chars (seq sequence)
                           current \A
                           cost 0]
                      (if (empty? chars)
                        cost
                        (let [char (first chars)
                              new-cost (+ cost (min-presses-for-move current char (dec depth) false))]
                          (recur (rest chars) char new-cost))))))))))))

(defn solve-code
  "Compute minimum presses to type code on numeric keypad with given robot depth."
  [code depth]
  (loop [chars (seq code)
         current \A
         total 0]
    (if (empty? chars)
      total
      (let [char (first chars)
            new-total (+ total (min-presses-for-move current char depth true))]
        (recur (rest chars) char new-total)))))

(defn complexity
  "Compute complexity: length * numeric part of code."
  [code length]
  (let [numeric-part (Integer/parseInt (str/replace code #"A" ""))]
    (* length numeric-part)))

(defn part1
  "Part 1: 2 intermediate robots (depth = 2)."
  [codes]
  (reduce + (map (fn [code]
                   (let [length (solve-code code 2)]
                     (complexity code length)))
                 codes)))

(defn part2
  "Part 2: 25 intermediate robots (depth = 25)."
  [codes]
  (reduce + (map (fn [code]
                   (let [length (solve-code code 25)]
                     (complexity code length)))
                 codes)))

(defn -main []
  (let [input-path (str (System/getProperty "user.dir") "/../input.txt")
        input-text (slurp input-path)
        codes (->> (str/split-lines input-text)
                   (map str/trim)
                   (remove empty?))]
    (println "Part 1:" (part1 codes))
    (println "Part 2:" (part2 codes))))

(-main)
