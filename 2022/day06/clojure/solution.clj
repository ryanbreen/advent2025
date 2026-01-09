#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(defn find-marker
  "Find first position where last window-size characters are all unique."
  [data window-size]
  (let [indexed-partitions (map-indexed
                             (fn [idx window] [(+ idx window-size) window])
                             (partition window-size 1 data))]
    (->> indexed-partitions
         (filter (fn [[_ window]] (= (count (set window)) window-size)))
         first
         first)))

(defn part1 [data]
  (find-marker data 4))

(defn part2 [data]
  (find-marker data 14))

(defn -main []
  (let [script-dir (-> (java.io.File. *file*) .getParentFile .getPath)
        input-file (str script-dir "/../input.txt")
        data (str/trim (slurp input-file))]
    (println "Part 1:" (part1 data))
    (println "Part 2:" (part2 data))))

(-main)
