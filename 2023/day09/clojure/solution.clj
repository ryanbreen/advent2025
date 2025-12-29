(ns solution
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (mapv #(Long/parseLong %) (str/split (str/trim line) #"\s+")))

(defn parse-input [text]
  (->> (str/split-lines text)
       (remove str/blank?)
       (mapv parse-line)))

(defn differences [xs]
  (mapv - (rest xs) xs))

(defn build-difference-pyramid [xs]
  (loop [current xs
         pyramid [xs]]
    (if (every? zero? current)
      pyramid
      (let [diffs (differences current)]
        (recur diffs (conj pyramid diffs))))))

(defn extrapolate-next [xs]
  (let [pyramid (build-difference-pyramid xs)]
    (reduce (fn [acc level]
              (+ (last level) acc))
            0
            (reverse pyramid))))

(defn part1 [histories]
  (reduce + (map extrapolate-next histories)))

(defn part2 [histories]
  (reduce + (map #(extrapolate-next (reverse %)) histories)))

(defn -main []
  (let [input-text (slurp "../input.txt")
        histories (parse-input input-text)]
    (println "Part 1:" (part1 histories))
    (println "Part 2:" (part2 histories))))

(-main)
