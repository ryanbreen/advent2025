#!/usr/bin/env bb

(ns solution
  (:require [clojure.string :as str]))

(defn parse-input [filename]
  "Parse input into a graph (adjacency list as a map)."
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce
      (fn [graph line]
        (let [line (str/trim line)]
          (if (empty? line)
            graph
            (let [[node neighbors-str] (str/split line #": ")
                  neighbors (if neighbors-str
                              (str/split neighbors-str #" ")
                              [])]
              (assoc graph node (vec neighbors))))))
      {}
      (line-seq rdr))))

(defn count-paths-to-target [graph target]
  "Returns a memoized function that counts paths from any node to target."
  (let [count-paths (atom nil)]
    (reset! count-paths
      (memoize
        (fn [node]
          (cond
            (= node target) 1
            (not (contains? graph node)) 0
            :else (reduce +' 0 (map @count-paths (graph node)))))))
    @count-paths))

(defn part1 [graph]
  "Count all paths from 'you' to 'out' using memoization."
  (let [count-paths (count-paths-to-target graph "out")]
    (count-paths "you")))

(defn part2 [graph]
  "Count paths from 'svr' to 'out' that visit both 'dac' and 'fft'.
   Since it's a DAG, paths either visit dac before fft, or fft before dac.

   Formula:
   - Paths visiting dac before fft: svr→dac × dac→fft × fft→out
   - Paths visiting fft before dac: svr→fft × fft→dac × dac→out"
  (let [paths-to-out (count-paths-to-target graph "out")
        paths-to-dac (count-paths-to-target graph "dac")
        paths-to-fft (count-paths-to-target graph "fft")]

    ;; Paths that visit dac before fft
    (let [dac-before-fft (*' (paths-to-dac "svr")
                             (paths-to-fft "dac")
                             (paths-to-out "fft"))
          ;; Paths that visit fft before dac
          fft-before-dac (*' (paths-to-fft "svr")
                             (paths-to-dac "fft")
                             (paths-to-out "dac"))]
      (+' dac-before-fft fft-before-dac))))

(defn -main [& args]
  (let [input-file (if (seq args)
                     (first args)
                     "../input.txt")
        graph (parse-input input-file)]
    (println (str "Part 1: " (part1 graph)))
    (println (str "Part 2: " (part2 graph)))))

;; Run main if executed as script
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

;; For non-babashka clojure
(when-not (resolve 'bb)
  (-main))
