#!/usr/bin/env bb

(require '[clojure.string :as str])

(defn parse-line [line]
  "Parse a line like '2-4,6-8' into [a1 b1 a2 b2]"
  (let [[left right] (str/split line #",")
        [a1 b1] (map parse-long (str/split left #"-"))
        [a2 b2] (map parse-long (str/split right #"-"))]
    [a1 b1 a2 b2]))

(defn parse-input [filename]
  (->> (slurp filename)
       str/split-lines
       (filter (complement str/blank?))
       (map parse-line)))

(defn fully-contains? [[a1 b1 a2 b2]]
  "Check if one range fully contains the other"
  (or (and (<= a1 a2) (>= b1 b2))
      (and (<= a2 a1) (>= b2 b1))))

(defn overlaps? [[a1 b1 a2 b2]]
  "Check if ranges overlap at all"
  (and (<= a1 b2) (<= a2 b1)))

(defn part1 [pairs]
  (count (filter fully-contains? pairs)))

(defn part2 [pairs]
  (count (filter overlaps? pairs)))

(defn -main []
  (let [script-dir (-> *file*
                       (java.io.File.)
                       (.getParentFile)
                       (.getAbsolutePath))
        input-file (str script-dir "/../input.txt")
        pairs (parse-input input-file)]
    (println "Part 1:" (part1 pairs))
    (println "Part 2:" (part2 pairs))))

(-main)
